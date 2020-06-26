module EVM.Dev where

import System.Directory

import EVM.Dapp
import EVM.Solidity
import EVM.UnitTest

import qualified EVM.Fetch
import qualified EVM.TTY
import qualified EVM.Emacs
import qualified EVM.Facts     as Facts
import qualified EVM.Facts.Git as Git
import qualified EVM.Stepper
import qualified EVM.VMTest    as VMTest

import Control.Exception          (evaluate)
import Control.Monad.State.Strict (execState)
import Data.Text (isPrefixOf)

import qualified Data.Map as Map
import qualified Data.ByteString.Lazy   as LazyByteString

loadDappInfo :: String -> String -> IO DappInfo
loadDappInfo path file =
  withCurrentDirectory path $
    readSolc file >>=
      \case
        Just (contractMap, cache) ->
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
    params <- getParametersFromEnvironmentVariables
    let
      opts = UnitTestOptions
        { oracle = EVM.Fetch.zero
        , verbose = Nothing
        , match = ""
        , fuzzRuns = 100
        , replay = Nothing
        , vmModifier = loadFacts
        , testParams = params
        }
    readSolc path >>=
      \case
        Just (contractMap, cache) -> do
          let unitTests = findUnitTests ("test" `isPrefixOf`) (Map.elems contractMap)
          mapM (runUnitTestContract opts contractMap cache) unitTests
        Nothing ->
          error ("Failed to read Solidity JSON for `" ++ path ++ "'")

runBCTest :: (String, VMTest.Case) -> IO Bool
runBCTest (name, x) = do
  let vm0 = VMTest.vmForCase x
  putStr (name ++ " ")
  result <-
    evaluate $
      execState (VMTest.interpret EVM.Stepper.execFully) vm0
  ok <- VMTest.checkExpectation False x result
  putStrLn (if ok then "ok" else "")
  return ok

ghciBCTest :: String -> IO ()
ghciBCTest file = do
  let parser = VMTest.parseBCSuite
  parsed <- parser <$> LazyByteString.readFile file
  case parsed of
     Left "No cases to check." -> putStrLn "no-cases ok"
     Left err -> print err
     Right allTests ->
        mapM_ runBCTest (Map.toList allTests)

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
    params <- getParametersFromEnvironmentVariables
    let
      testOpts = UnitTestOptions
        { oracle = EVM.Fetch.zero
        , verbose = Nothing
        , match = ""
        , fuzzRuns = 100
        , replay = Nothing
        , vmModifier = loadFacts
        , testParams = params
        }
    EVM.TTY.main testOpts root path

ghciEmacs :: IO ()
ghciEmacs =
  EVM.Emacs.main

foo :: IO ()
foo = ghciEmacs
