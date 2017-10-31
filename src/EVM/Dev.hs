module EVM.Dev where

import System.Directory

import EVM.Dapp
import EVM.Solidity
import EVM.UnitTest

import Control.Lens hiding (indices)

import Data.Aeson
import Data.Aeson.Lens
import Data.Maybe (mapMaybe)

import qualified EVM.Fetch
import qualified EVM.TTY
import qualified EVM.Facts     as Facts
import qualified EVM.Facts.Git as Git

import qualified Data.Map as Map
import Data.Map ((!))

import qualified Data.Graph.Inductive.Graph as Fgl
import qualified Data.Graph.Inductive.PatriciaTree as Fgl
import qualified Data.Graph.Inductive.Query.BFS as Fgl
import qualified Data.Graph.Inductive.Query.DFS as Fgl

import Data.Text (Text)

loadDappInfo :: String -> String -> IO DappInfo
loadDappInfo path file =
  withCurrentDirectory path $
    readSolc file >>=
      \case
        Just (contractMap, cache) -> do
          pure (dappInfo "." contractMap cache)
        _ ->
          error "nope, sorry"

flatten :: DappInfo -> Text -> IO ()
flatten dapp target = do
  let
    astMap  = view (dappSources . sourceAsts) dapp
    names   = Map.keys astMap
    indices = Map.fromList (zip names [1..])

    edges =
      [ (indices ! s, indices ! t, ()) -- edge from s to t
      | (s, v) <- Map.toList astMap    -- for every file s
      , t <- importsFrom v ]           -- and every t imported by s

    importsFrom ast =
      flip mapMaybe (universe ast) $ \x ->
        case preview (key "name") x of
          Just (String "ImportDirective") ->
            preview (key "attributes" . key "absolutePath" . _String) x
          _ ->
            Nothing

    g = Fgl.mkGraph (zip [1..] names) edges :: Fgl.Gr Text ()

  case Map.lookup target indices of
    Nothing ->
      error "didn't find contract"
    Just root -> do
      print (reverse (Fgl.topsort' (Fgl.subgraph (Fgl.bfs root g) g)))

ghciTest :: String -> String -> Maybe String -> IO [Bool]
ghciTest root path state =
  withCurrentDirectory root $ do
    loadFacts <-
      case state of
        Nothing ->
          pure id
        Just repoPath -> do
          facts <- Git.loadFacts (Git.RepoAt repoPath)
          pure (flip Facts.apply facts)
    let
      opts = UnitTestOptions
        { gasForCreating = defaultGasForCreating
        , gasForInvoking = defaultGasForInvoking
        , balanceForCreator = defaultBalanceForCreator
        , balanceForCreated = defaultBalanceForCreated
        , oracle = EVM.Fetch.zero
        , verbose = False
        , vmModifier = loadFacts
        }
    readSolc path >>=
      \case
        Just (contractMap, cache) -> do
          let unitTests = findUnitTests (Map.elems contractMap)
          mapM (runUnitTestContract opts contractMap cache) unitTests
        Nothing ->
          error ("Failed to read Solidity JSON for `" ++ path ++ "'")

ghciTty :: String -> String -> Maybe String -> IO ()
ghciTty root path state =
  withCurrentDirectory root $ do
    loadFacts <-
      case state of
        Nothing ->
          pure id
        Just repoPath -> do
          facts <- Git.loadFacts (Git.RepoAt repoPath)
          pure (flip Facts.apply facts)
    let
      testOpts = UnitTestOptions
        { gasForCreating = defaultGasForCreating
        , gasForInvoking = defaultGasForInvoking
        , balanceForCreator = defaultBalanceForCreator
        , balanceForCreated = defaultBalanceForCreated
        , oracle = EVM.Fetch.zero
        , verbose = False
        , vmModifier = loadFacts
        }
    EVM.TTY.main testOpts root path
