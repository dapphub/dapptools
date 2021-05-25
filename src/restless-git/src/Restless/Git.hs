{-| This module defines a simple way to serialize state as a nested file
  hierarchy saved in a Git repository.
-}

{-# Language DeriveFunctor #-}
{-# Language OverloadedStrings #-}
{-# Language RecordWildCards #-}
{-# Language TupleSections #-}

module Restless.Git
  ( Path (..)
  , File (..)
  , make
  , save
  , load
  ) where

import Control.Monad          (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString        (ByteString)
import Data.Foldable          (toList)
import Data.Map               (Map)
import Data.Set               (Set)
import Data.Text              (Text, unpack)
import Data.Text.Encoding     (decodeUtf8)
import System.Exit            (ExitCode(..))
import HSH                    (run, (-|-))

import qualified Data.ByteString  as BS
import qualified Data.Set         as Set
import qualified Data.Map         as Map

-- | A fact path is something like "\/0123...abc\/storage\/0x1".
-- It has some number of directories and a file name.
data Path = Path [ByteString] ByteString
  deriving (Eq, Ord, Show)

-- | A file is a serialized value with a path.
data File = File { filePath :: Path, fileData :: ByteString }
  deriving (Eq, Ord, Show)

data Tree a = Tree (Map ByteString (Tree a)) (Map ByteString a)
  deriving (Functor, Show)

instance Semigroup (Tree a) where
  Tree a b <> Tree c d =
    Tree (Map.unionWith mappend a c)
         (Map.union b d)

instance Monoid (Tree a) where
  mempty =
    Tree mempty mempty

-- | Initialize an empty repository at the given path.
make
  :: (Monad m, MonadIO m)
  => FilePath -> m ()
make repo = liftIO $ do
  void $ git repo "init" []
  void $ git repo "commit" ["-am", "initialize", "--allow-empty"]

-- | Save a set of files to an initialized repository and commit this
-- tree to the master branch with a given commit message.
save
  :: (Monad m, MonadIO m)
  => FilePath -> Text -> Set File -> m ()
save dst message files = liftIO $ do
  ref <- defaultRef dst
  tree <- saveTree dst (treeFromFiles files)
  parent <- latestCommitOid dst ref
  commit <- createCommit dst parent tree (unpack message)

  updateReference dst ref commit

  return ()

-- | Load a set of files from a repository's master branch.
load :: (Monad m, MonadIO m) => FilePath -> m (Set File)
load src = liftIO $ do
  ref <- defaultRef src
  ls <- git src "ls-tree" ["-r", "-z", ref]
  Set.fromList <$> mapM (loadFile src) (filter (/= "") (BS.split 0 ls))


-- Internal

treeFromFiles :: Foldable t => t File -> Tree ByteString
treeFromFiles = foldMap singletonTree . toList

singletonTree :: File -> Tree ByteString
singletonTree (File {..}) =
  case filePath of
    Path [] name ->
      Tree mempty (Map.singleton name fileData)
    Path (x:xs) name ->
      let subtree = singletonTree (File (Path xs name) fileData)
      in Tree (Map.singleton x subtree) mempty

data ObjectType = TreeObject | BlobObject
  deriving Show

newtype SHA1 = SHA1 ByteString
  deriving Show

newtype MkTree =
  MkTree (Map ByteString (ObjectType, SHA1))
  deriving Show

serializeMkTree :: MkTree -> ByteString
serializeMkTree (MkTree m) =
  mconcat . map (uncurry mkTreeLine) . Map.toList $ m

mkTreeLine :: ByteString -> (ObjectType, SHA1) -> ByteString
mkTreeLine name (TreeObject, SHA1 sha1) =
  "040000 tree " <> sha1 <> "\t" <> name <> "\0"
mkTreeLine name (BlobObject, SHA1 sha1) =
  "100644 blob " <> sha1 <> "\t" <> name <> "\0"

saveTree :: FilePath -> Tree ByteString -> IO SHA1
saveTree dst (Tree folders files) = do
  trees <- mapM (fmap (TreeObject, ) . saveTree dst) folders
  blobs <- mapM (fmap (BlobObject, ) . createBlob dst) files
  let input = serializeMkTree (MkTree (trees <> blobs))
  asSHA1 <$> run ((\() -> input) -|- git' dst "mktree" ["-z"])

asSHA1 :: ByteString -> SHA1
asSHA1 = SHA1 . fst . BS.break (== 0xa)

defaultRef :: FilePath -> IO String
defaultRef repo = do
  (ref, finish) <- run cmd :: IO (String, IO (String, ExitCode))
  (_, code) <- finish
  pure $ case code of
    ExitFailure _ -> "refs/heads/master"
    ExitSuccess -> "refs/heads/" <> (trimnl ref)
  where
    cmd = git' repo "config" ["--get", "init.defaultBranch"]
    trimnl = reverse . dropWhile (=='\n') . reverse

git :: FilePath -> String -> [String] -> IO ByteString
git repo cmd args = run $ git' repo cmd args

sha1String :: SHA1 -> String
sha1String (SHA1 bs) = unpack (decodeUtf8 bs)

createCommit :: FilePath -> SHA1 -> SHA1 -> String -> IO SHA1
createCommit dst parent tree message =
  asSHA1 <$>
    git dst "commit-tree"
      ["-p", sha1String parent, "-m", message, sha1String tree]

updateReference :: String -> String -> SHA1 -> IO ()
updateReference dst ref next =
  void $ git dst "update-ref" [ref, sha1String next]

git' :: FilePath -> String -> [String] -> (String, [String])
git' repo cmd args =
  ("git" :: String, ["-C", repo] ++ (cmd : args))

createBlob :: String -> ByteString -> IO SHA1
createBlob dst bytes =
  asSHA1 <$> run ((\() -> bytes) -|- git' dst "hash-object" ["--stdin", "-w"])

loadFile :: FilePath -> ByteString -> IO File
loadFile src line = do
  let
    (a, b) = BS.splitAt 52 line
    sha1   = SHA1 (BS.drop 12 a)
    name   = BS.take (BS.length b - 1) (BS.drop 1 b)
    path   =
      case BS.split 0x2f name of
        []    -> error "empty file name"
        parts -> Path (init parts) (last parts)

  bytes <- git src "cat-file" ["blob", sha1String sha1]
  return (File path bytes)

latestCommitOid :: FilePath -> String -> IO SHA1
latestCommitOid dst ref =
  asSHA1 <$> git dst "rev-parse" [ref]
