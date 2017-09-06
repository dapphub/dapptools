{-# Language RecordWildCards #-}
{-# Language ViewPatterns #-}
{-# Language OverloadedStrings #-}

module Restless.Git
  ( Path (..)
  , File (..)
  , Metadata (..)
  , now
  , make
  , save
  , load
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString        (ByteString)
import Data.Foldable          (toList)
import Data.Maybe             (catMaybes)
import Data.Set               (Set)
import Data.Text              (Text, pack)
import Data.Time.LocalTime    (ZonedTime, getZonedTime)
import Git.Libgit2            (lgFactory)
import Shelly                 (shelly, silently, run_)

import qualified Data.ByteString  as BS
import qualified Data.Set         as Set
import qualified Git              as Git

defaultRef :: Text
defaultRef = "refs/heads/master"

-- A fact path means something like "/0123...abc/storage/0x1",
-- or alternatively "contracts['0123...abc'].storage['0x1']".
data Path = Path [ByteString] ByteString
  deriving (Eq, Ord, Show)

-- We use the word "file" to denote a serialized value at a path.
data File = File { filePath :: Path, fileData :: ByteString }
  deriving (Eq, Ord, Show)

data Metadata = Metadata
  { message :: Text
  , name    :: Text
  , email   :: Text
  , time    :: ZonedTime
  } deriving (Show, Read)

signature :: Metadata -> Git.Signature
signature Metadata {..} = Git.Signature
  { Git.signatureName = name
  , Git.signatureEmail = email
  , Git.signatureWhen = time
  }

now :: MonadIO m => m ZonedTime
now = liftIO getZonedTime

make
  :: (Monad m, MonadIO m)
  => FilePath -> m ()
make dst = do
  shelly . silently $ do
    let git xs = run_ "git" (["-C", pack dst] ++ xs)
    git ["init"]
    git $
      [ "-c", "user.email=git@example.com"
      , "-c", "user.name=Restless Git"
      , "commit", "-am", "init", "--allow-empty"
      ]

save
  :: (Monad m, MonadIO m)
  => FilePath -> Metadata -> Set File -> m ()
save dst meta files = do
  liftIO . Git.withRepository lgFactory dst $ do
    let sig = signature meta

    tree <-
      treeFromFiles files
    parent <-
      latestCommitOid defaultRef
    commit <-
      Git.createCommit [parent] tree sig sig (message meta) Nothing

    let target = Git.commitRefTarget commit
    Git.updateReference defaultRef target

    return ()

load :: (Monad m, MonadIO m) => FilePath -> m (Set File)
load src =
  liftIO . Git.withRepository lgFactory src $ do
    Just (Git.RefObj root) <- Git.lookupReference "refs/heads/master"
    entries <- treeAtOid root >>= Git.listTreeEntries
    fmap (Set.fromList . catMaybes) (mapM f entries)
  where
    f (k, (Git.BlobEntry oid _)) =
      case BS.split 0x2f k of
        [] ->
          -- Empty file name...
          return Nothing
        parts -> do
          bytes <- Git.catBlob oid
          return $
            Just (File (Path (init parts) (last parts)) bytes)
    f _ =
      -- Not a git blob...
      return Nothing

treeFromFiles
  :: (Foldable t, Git.MonadGit r m, MonadIO m, Monad m)
  => t File -> m (Git.TreeOid r)
treeFromFiles (toList -> xs) = do
  let names = map (slashPath . filePath) xs
  blobs <-
    mapM (Git.createBlob . Git.BlobString . fileData) xs
  Git.createTree (mapM_ (uncurry Git.putBlob) (zip names blobs))

slashPath :: Path -> Git.RawFilePath
slashPath (Path xs x) = BS.intercalate "/" (xs ++ [x])

latestCommitOid :: Git.MonadGit r m => Git.RefName -> m (Git.CommitOid r)
latestCommitOid ref = do
  Just (Git.RefObj oid) <- Git.lookupReference ref
  Git.CommitObj commit <- Git.lookupObject oid
  return (Git.commitOid commit)

treeAtOid :: Git.MonadGit r m => Git.Oid r -> m (Git.Tree r)
treeAtOid oid = do
  Git.CommitObj commit <- Git.lookupObject oid
  Git.lookupTree (Git.commitTree commit)
