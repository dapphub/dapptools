{-# Language BangPatterns #-}
{-# Language DeriveGeneric #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language LambdaCase #-}
{-# Language OverloadedStrings #-}
{-# Language TemplateHaskell #-}

module EVM.Test where

import qualified EVM

import EVM.Types

import Control.Lens
import IPPrint.Colored (cpprint)

import Data.ByteString (ByteString)
import Data.Aeson ((.:), (.:?))
import Data.Aeson (FromJSON (..))
import Data.Map (Map)
import Data.DoubleWord

import qualified Data.Map          as Map
import qualified Data.Aeson        as JSON
import qualified Data.Aeson.Types  as JSON
import qualified Data.ByteString.Lazy  as Lazy

data VMTestCase = VMTestCase
  { testVmOpts      :: EVM.VMOpts
  , testContracts   :: Map Address ContractSpec
  , testExpectation :: VMTestExpectation
  } deriving Show

data ContractSpec = ContractSpec
  { contractBalance :: Hexword
  , contractCode    :: ByteString
  , contractNonce   :: Hexword
  , contractStorage :: Map Word256 Word256
  } deriving Show

data VMTestExpectation = VMTestExpectation
  { expectedOut :: Maybe ByteString
  , expectedContracts :: Maybe (Map Address ContractSpec)
  } deriving Show

checkTestExpectation :: VMTestExpectation -> EVM.VM -> IO Bool
checkTestExpectation x vm = do
  e1 <- maybe (return True) (checkExpectedContracts vm) (expectedContracts x)
          >>= \case True ->
                      return True
                    False -> do
                      putStrLn "failed contract expectations"
                      return False

  e2 <- maybe (return True) (checkExpectedOut vm) (expectedOut x)
          >>= \case True ->
                      return True
                    False -> do
                      putStrLn "failed output value"
                      return False

  return (e1 && e2)

checkExpectedOut :: EVM.VM -> ByteString -> IO Bool
checkExpectedOut vm expected =
  case vm ^. EVM.done of
    Nothing -> error "VM not done"
    Just out ->
      if out == expected
      then
        return True
      else do
        cpprint (out, expected)
        return False

checkExpectedContracts :: EVM.VM -> Map Address ContractSpec -> IO Bool
checkExpectedContracts vm expected =
  if testContractsToVM expected == vm ^. EVM.env . EVM.contracts
  then return True
  else cpprint (testContractsToVM expected, vm ^. EVM.env . EVM.contracts) >> return False

instance FromJSON ContractSpec where
  parseJSON (JSON.Object v) = ContractSpec
    <$> v .: "balance"
    <*> (hexText <$> v .: "code")
    <*> v .: "nonce"
    <*> v .: "storage"
  parseJSON invalid =
    JSON.typeMismatch "VM test case contract" invalid

instance FromJSON VMTestCase where
  parseJSON (JSON.Object v) = VMTestCase
    <$> parseTestVmOpts v
    <*> parseTestContracts v
    <*> parseTestExpectation v
  parseJSON invalid =
      JSON.typeMismatch "VM test case" invalid

parseTestVmOpts :: JSON.Object -> JSON.Parser EVM.VMOpts
parseTestVmOpts v =
  do envV  <- v .: "env"
     execV <- v .: "exec"
     case (envV, execV) of
       (JSON.Object env, JSON.Object exec) ->
         EVM.VMOpts
           <$> dataField exec "code"
           <*> dataField exec "data"
           <*> wordField exec "value"
           <*> addrField exec "address"
           <*> addrField exec "caller"
           <*> addrField exec "origin"
           <*> addrField env  "currentCoinbase"
           <*> wordField env  "currentNumber"
           <*> wordField env  "currentTimestamp"
           <*> wordField env  "currentGasLimit"
           <*> wordField env  "currentDifficulty"
       _ ->
         JSON.typeMismatch "VM test case" (JSON.Object v)

parseTestContracts ::
  JSON.Object -> JSON.Parser (Map Address ContractSpec)
parseTestContracts v =
  v .: "pre" >>= parseJSON

parseTestExpectation :: JSON.Object -> JSON.Parser VMTestExpectation
parseTestExpectation v =
  VMTestExpectation
    <$> (fmap hexText <$> v .:? "out")
    <*> v .:? "post"

parseTestCases ::
  Lazy.ByteString -> Either String (Map String VMTestCase)
parseTestCases = JSON.eitherDecode'

testContractsToVM :: Map Address ContractSpec -> Map Word256 EVM.Contract
testContractsToVM = Map.fromList . map f . Map.toList
  where
    f (a, x) = (addressWord256 a, testContractToVM x)

testContractToVM :: ContractSpec -> EVM.Contract
testContractToVM x =
  EVM.initialContract (contractCode x)
    & EVM.balance .~ hexWord256 (contractBalance x)
    & EVM.nonce   .~ hexWord256 (contractNonce x)
    & EVM.storage .~ contractStorage x
