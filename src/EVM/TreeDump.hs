{-# Language ScopedTypeVariables #-}
{-# Language DeriveFunctor #-}
{-# Language DeriveFoldable #-}
{-# Language DeriveTraversable #-}

module EVM.TreeDump where

import Prelude hiding (Word)

import Control.Monad.IO.Class
import Data.Maybe (catMaybes)
import Control.Arrow (first)
import Data.Time (getZonedTime)
import Control.Lens
import Data.Monoid
import Data.Ord (comparing)
import Data.List (sortBy)
import Data.Text (Text, pack, unpack, splitOn)
import Data.ByteString (ByteString)
import Data.ByteString.Base16 as BS16
import Data.Text.Encoding (encodeUtf8)
import Data.Map (Map)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.Map as Map

import EVM.Concrete
import EVM.Types
import EVM.Machine (w256)
import EVM

import Git
import Git.Libgit2 (lgFactory)

data Forest k v
  = Branches k [Forest k v]
  | Leaf k v
  deriving (Functor, Foldable, Traversable, Show)

type ByteForest = Forest Text ByteString

vmToByteForests :: VM Concrete -> [ByteForest]
vmToByteForests x =
  contractsToByteForests (view (env . contracts) x)

vmToFiles :: VM Concrete -> [(Text, ByteString)]
vmToFiles x = vmToByteForests x >>= flattenByteForest

flattenByteForest :: ByteForest -> [(Text, ByteString)]
flattenByteForest =
  \case
    Leaf k v ->
      [(k, v <> "\n")]
    Branches k ts ->
      map (first ((k <> "/") <>)) (concat (map flattenByteForest ts))

contractToByteForests :: Contract Concrete -> [ByteForest]
contractToByteForests x =
  [ Leaf "code" (BS16.encode (view bytecode x))
  , Branches "storage" (wordMapToByteForests (view storage x))
  , Leaf "balance" (C8.pack (show (view balance x)))
  , Leaf "nonce" (C8.pack (show (view nonce x)))
  ]

contractsToByteForests :: Map Addr (Contract Concrete) -> [ByteForest]
contractsToByteForests = f . fmap contractToByteForests
  where
    f :: Map Addr [ByteForest] -> [ByteForest]
    f = map (\(k, v) -> Branches (pack (show k)) v) . Map.toList

wordMapToByteForests :: Map (Word Concrete) (Word Concrete) -> [ByteForest]
wordMapToByteForests = f
  where
    f :: Map (Word Concrete) (Word Concrete) -> [ByteForest]
    f = map g . Map.toList

    g (C _ k, C _ v) = Leaf (pack (show k)) (C8.pack (show v))

newtype RepoPath = RepoAt String

treeFromFiles
  :: (Foldable t, MonadGit r m, MonadIO m)
  => t (RawFilePath, BlobOid r)
  -> m (TreeOid r)
treeFromFiles =
  createTree . mapM_ (uncurry putBlob)

commitTreeToRef
  :: (MonadGit r m, MonadIO m)
  => RefName -> Text -> TreeOid r -> m (Oid r)
commitTreeToRef ref text tree = do
  sig <- signature
  parent <- latestCommitOid ref
  commit <- createCommit [parent] tree sig sig text Nothing
  let target@(RefObj oid) = commitRefTarget commit
  updateReference ref target
  return oid

commitForests :: RepoPath -> [ByteForest] -> IO ()
commitForests (RepoAt path) forests = do
  let (paths, bytestrings) = unzip (forests >>= flattenByteForest)
  withRepository lgFactory path $ do
    blobs <- mapM (createBlob . BlobString) bytestrings
    tree <- treeFromFiles (zip (map encodeUtf8 paths) blobs)
    _ <- commitTreeToRef "refs/heads/master" "hsevm" tree
    pure ()

commitVm :: RepoPath -> VM Concrete -> IO ()
commitVm path vm = commitForests path (vmToByteForests vm)

signature :: MonadIO m => m Signature
signature = liftIO $ do
  time <- getZonedTime
  return Signature
    { signatureName = "hsevm"
    , signatureEmail = "hsevm@dapphub.com"
    , signatureWhen = time }

latestCommitOid
  :: MonadGit r m
  => RefName -> m (CommitOid r)
latestCommitOid ref = do
  Just (RefObj oid) <- lookupReference ref
  CommitObj commit <- lookupObject oid
  return (commitOid commit)

data Datum
  = SetBalance Addr W256
  | SetCode Addr ByteString
  | SetNonce Addr W256
  | SetStorage Addr W256 W256
  deriving Show

sortData :: [Datum] -> [Datum]
sortData = sortBy (comparing f)
  where
    f :: Datum -> (Int, Addr, W256)
    f (SetCode a _)      = (0, a, 0)
    f (SetBalance a _)   = (1, a, 0)
    f (SetNonce a _)     = (1, a, 0)
    f (SetStorage a x _) = (1, a, x)

applyDatum :: VM Concrete -> Datum -> VM Concrete
applyDatum vm (SetCode a x) =
  vm & env . contracts . at a .~ Just (initialContract x)
applyDatum vm (SetNonce a x) =
  vm & env . contracts . ix a . nonce .~ w256 x
applyDatum vm (SetBalance a x) =
  vm & env . contracts . ix a . balance .~ w256 x
applyDatum vm (SetStorage a x y) =
  vm & env . contracts . ix a . storage . at (w256 x) .~ Just (w256 y)

applyRepo :: FilePath -> VM Concrete -> IO (VM Concrete)
applyRepo path vm =
  foldl applyDatum vm <$> repoData path

repoData :: FilePath -> IO [Datum]
repoData path =
  withRepository lgFactory path $ do
    Just (RefObj oid) <- lookupReference "refs/heads/master"
    sortData <$> allDataAtCommit oid

allDataAtCommit :: forall r m. (Monad m, MonadGit r m) => Oid r -> m [Datum]
allDataAtCommit rootOid =
  catMaybes <$> (treeAtOid rootOid >>= listTreeEntries >>= mapM f)
  where
    f :: (TreeFilePath, TreeEntry r) -> m (Maybe Datum)
    f (path, BlobEntry oid _) =
      case pathComponents path of
        [a, "balance"] -> do
          x <- fmap (read . C8.unpack) (catBlob oid)
          return (Just (SetBalance ((read . unpack) a) x))
        [a, "code"] -> do
          x <- (fst . BS16.decode . mconcat . BS.split 10) <$> catBlob oid
          return (Just (SetCode ((read . unpack) a) x))
        [a, "nonce"] -> do
          x <- fmap (read . C8.unpack) (catBlob oid)
          return (Just (SetNonce ((read . unpack) a) x))
        [a, "storage", b] -> do
          x <- fmap (read . C8.unpack) (catBlob oid)
          return (Just (SetStorage ((read . unpack) a) (read (unpack b)) x))
        _ -> return Nothing
    f (_, _) =
      return Nothing

pathComponents :: TreeFilePath -> [Text]
pathComponents = splitOn "/" . pack . C8.unpack

treeAtOid
  :: MonadGit r m => Oid r -> m (Tree r)
treeAtOid oid = do
  CommitObj commit <- lookupObject oid
  lookupTree (commitTree commit)

allBlobsAtCommit
  :: (Read a, MonadGit r m)
  => Oid r -> m (Text, [a])
allBlobsAtCommit oid = do
  blobs <- treeAtOid oid
    >>= listTreeEntries
    >>= mapM readBlob . blobOids
  return (renderOid oid, catMaybes blobs)

blobOids :: [(a, TreeEntry r)] -> [BlobOid r]
blobOids xs =
  [oid | (_, BlobEntry oid _) <- xs]

readBlob
  :: (Read a, MonadGit r m)
  => BlobOid r -> m (Maybe a)
readBlob x =
  catBlob x >>= return . Just . read . C8.unpack
