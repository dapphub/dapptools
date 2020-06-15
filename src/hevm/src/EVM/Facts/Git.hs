
-- This is a backend for the fact representation that uses a Git
-- repository as the store.

module EVM.Facts.Git
  ( saveFacts
  , loadFacts
  , RepoAt (..)
  ) where

import EVM.Facts (Fact (..), File (..), Path (..), Data (..), fileToFact, factToFile)

import Control.Lens
import Data.Set   (Set)
import Data.Maybe (catMaybes)

import qualified Data.Set         as Set
import qualified Restless.Git     as Git

newtype RepoAt = RepoAt String
  deriving (Eq, Ord, Show)

-- For modularity reasons, we have our own file data type that is
-- isomorphic with the one in the `restless-git` library.  We declare
-- the isomorphism so we can go between them easily.
fileRepr :: Iso' File Git.File
fileRepr = iso f g
  where
    f :: File -> Git.File
    f (File     (Path ps p)     (Data x)) =
      Git.File (Git.Path ps p) x

    g :: Git.File -> File
    g (Git.File (Git.Path ps p) x) =
      File     (Path ps p)     (Data x)

saveFacts :: RepoAt -> Set Fact -> IO ()
saveFacts (RepoAt repo) facts =
  Git.save repo "hevm execution"
    (Set.map (view fileRepr . factToFile) facts)

prune :: Ord a => Set (Maybe a) -> Set a
prune = Set.fromList . catMaybes . Set.toList

loadFacts :: RepoAt -> IO (Set Fact)
loadFacts (RepoAt src) =
  fmap
    (prune . Set.map (fileToFact . view (from fileRepr)))
    (Git.load src)
