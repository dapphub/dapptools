module EVM.Dev where

import System.Directory

import EVM.Dapp
import EVM.Solidity
import EVM.UnitTest

import qualified EVM.Fetch
import qualified EVM.TTY

loadDappInfo :: String -> String -> IO DappInfo
loadDappInfo path file =
  withCurrentDirectory path $
    readSolc file >>=
      \case
        Just (contractMap, cache) -> do
          pure (dappInfo "." contractMap cache)
        _ ->
          error "nope, sorry"

ghciTty :: String -> String -> IO ()
ghciTty root path =
  withCurrentDirectory root $ do
    let
      testOpts = UnitTestOptions
        { gasForCreating = defaultGasForCreating
        , gasForInvoking = defaultGasForInvoking
        , balanceForCreator = defaultBalanceForCreator
        , balanceForCreated = defaultBalanceForCreated
        , oracle = EVM.Fetch.zero
        , verbose = False
        , vmModifier = id
        }
    EVM.TTY.main testOpts root path
