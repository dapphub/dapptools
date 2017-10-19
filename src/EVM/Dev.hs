module EVM.Dev where

import System.Directory

import EVM.Dapp
import EVM.Solidity
import EVM.UnitTest

import qualified EVM.Fetch
import qualified EVM.TTY
import qualified EVM.Facts     as Facts
import qualified EVM.Facts.Git as Git

import qualified Data.Map as Map

loadDappInfo :: String -> String -> IO DappInfo
loadDappInfo path file =
  withCurrentDirectory path $
    readSolc file >>=
      \case
        Just (contractMap, cache) -> do
          pure (dappInfo "." contractMap cache)
        _ ->
          error "nope, sorry"

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
