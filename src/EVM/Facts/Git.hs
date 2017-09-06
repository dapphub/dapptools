{-# Language ViewPatterns #-}

-- This is a backend for the fact representation that uses a Git
-- repository as the store.

module EVM.Facts.Git
  ( saveFacts
  , loadFacts
  , RepoAt (..)
  ) where

import EVM.Facts (Fact (..), File (..), Path (..), Data (..))
import EVM.Facts (fileToFact, factToFile)

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Foldable          (toList)
import Data.Maybe             (catMaybes)
import Data.Monoid            ((<>))
import Data.Set               (Set)
import Data.Text              (Text)
import Data.Time.LocalTime    (getZonedTime)
import Git.Libgit2            (lgFactory)

import qualified Data.ByteString  as BS
import qualified Data.Set         as Set
import qualified Git              as Git
import qualified Git.Libgit2      as Git (MonadLg)

newtype RepoAt = RepoAt String
  deriving (Eq, Ord, Show)

treeFromFiles
  :: (Foldable t, Git.MonadGit r m, MonadIO m, Monad m)
  => t File -> m (Git.TreeOid r)
treeFromFiles (toList -> xs) = do
  let names = map (slashPath . filePath) xs
  blobs <-
    mapM (Git.createBlob . Git.BlobString . (<> "\n") . dataASCII . fileData) xs
  Git.createTree (mapM_ (uncurry Git.putBlob) (zip names blobs))

slashPath :: Path -> Git.RawFilePath
slashPath (Path xs x) = BS.intercalate "/" (xs ++ [x])

commitTreeToRef
  :: (Git.MonadGit r m, MonadIO m)
  => Git.RefName -> Text -> Git.TreeOid r -> m (Git.Oid r)
commitTreeToRef ref message tree = do
  sig <- signature
  parent <- latestCommitOid ref
  commit <- Git.createCommit [parent] tree sig sig message Nothing
  let target@(Git.RefObj oid) = Git.commitRefTarget commit
  Git.updateReference ref target
  return oid

saveFacts :: RepoAt -> Set Fact -> IO ()
saveFacts repo facts = do
  commitFiles repo (map factToFile (Set.toList facts))

commitFiles :: RepoAt -> [File] -> IO ()
commitFiles (RepoAt dst) files = do
  Git.withRepository lgFactory dst $ do
    tree <- treeFromFiles files
    ignore (commitTreeToRef "refs/heads/master" "EVM execution" tree)

ignore :: Functor f => f a -> f ()
ignore = fmap (const ())

signature :: MonadIO m => m Git.Signature
signature = liftIO $ do
  time <- getZonedTime
  return Git.Signature
    { Git.signatureName = "hsevm"
    , Git.signatureEmail = "hsevm@dapphub.com"
    , Git.signatureWhen = time }

latestCommitOid :: Git.MonadGit r m => Git.RefName -> m (Git.CommitOid r)
latestCommitOid ref = do
  Just (Git.RefObj oid) <- Git.lookupReference ref
  Git.CommitObj commit <- Git.lookupObject oid
  return (Git.commitOid commit)

loadFacts
  :: Git.MonadLg m
  => RepoAt -> m (Set Fact)
loadFacts (RepoAt src) =
  Git.withRepository lgFactory src $ do
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
          let file = File (Path (init parts) (last parts))
                          (Data bytes)
          return (fileToFact file)
    f _ =
      -- Not a git blob...
      return Nothing

treeAtOid :: Git.MonadGit r m => Git.Oid r -> m (Git.Tree r)
treeAtOid oid = do
  Git.CommitObj commit <- Git.lookupObject oid
  Git.lookupTree (Git.commitTree commit)
