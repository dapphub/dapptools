{-# Language BangPatterns #-}
{-# Language DeriveGeneric #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language LambdaCase #-}
{-# Language OverloadedStrings #-}
{-# Language TemplateHaskell #-}

module EVM.VMTest
  ( Case
  , parseSuite
  , vmForCase
  , checkExpectation
  ) where

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

data Case = Case
  { testVmOpts      :: EVM.VMOpts
  , testContracts   :: Map Address Contract
  , testExpectation :: Maybe Expectation
  } deriving Show

data Contract = Contract
  { contractBalance :: Hexword
  , contractCode    :: ByteString
  , contractNonce   :: Hexword
  , contractStorage :: Map Word256 Word256
  } deriving Show

data Expectation = Expectation
  { expectedOut :: ByteString
  , expectedContracts :: Map Address Contract
  } deriving Show

checkExpectation :: Case -> EVM.VM -> IO Bool
checkExpectation x vm =
  case (testExpectation x, view EVM.result vm) of
    (Just expectation, EVM.VMSuccess output) -> do
      (&&) <$> checkExpectedContracts vm (expectedContracts expectation)
           <*> checkExpectedOut output (expectedOut expectation)
    (Nothing, EVM.VMSuccess _) ->
      return False
    (Nothing, EVM.VMFailure) ->
      return True
    (Just _, EVM.VMFailure) ->
      return False
    (_, EVM.VMRunning) ->
      error "VMRunning?"

checkExpectedOut :: ByteString -> ByteString -> IO Bool
checkExpectedOut output expected =
  if output == expected
  then
    return True
  else do
    cpprint ("output mismatch" :: String, output, expected)
    return False

checkExpectedContracts :: EVM.VM -> Map Address Contract -> IO Bool
checkExpectedContracts vm expected =
  if realizeContracts expected == vm ^. EVM.env . EVM.contracts
  then return True
  else do
    cpprint (realizeContracts expected, vm ^. EVM.env . EVM.contracts)
    return False

instance FromJSON Contract where
  parseJSON (JSON.Object v) = Contract
    <$> v .: "balance"
    <*> (hexText <$> v .: "code")
    <*> v .: "nonce"
    <*> v .: "storage"
  parseJSON invalid =
    JSON.typeMismatch "VM test case contract" invalid

instance FromJSON Case where
  parseJSON (JSON.Object v) = Case
    <$> parseVmOpts v
    <*> parseContracts v
    <*> parseExpectation v
  parseJSON invalid =
      JSON.typeMismatch "VM test case" invalid

parseVmOpts :: JSON.Object -> JSON.Parser EVM.VMOpts
parseVmOpts v =
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
           <*> wordField env  "currentNumber"
           <*> wordField env  "currentTimestamp"
           <*> addrField env  "currentCoinbase"
           <*> wordField env  "currentDifficulty"
           <*> wordField env  "currentGasLimit"
       _ ->
         JSON.typeMismatch "VM test case" (JSON.Object v)

parseContracts ::
  JSON.Object -> JSON.Parser (Map Address Contract)
parseContracts v =
  v .: "pre" >>= parseJSON

parseExpectation :: JSON.Object -> JSON.Parser (Maybe Expectation)
parseExpectation v =
  do out       <- fmap hexText <$> v .:? "out"
     contracts <- v .:? "post"
     case (out, contracts) of
       (Just x, Just y) ->
         return (Just (Expectation x y))
       _ ->
         return Nothing

parseSuite ::
  Lazy.ByteString -> Either String (Map String Case)
parseSuite = JSON.eitherDecode'

realizeContracts :: Map Address Contract -> Map Word256 EVM.Contract
realizeContracts = Map.fromList . map f . Map.toList
  where
    f (a, x) = (addressWord256 a, realizeContract x)

realizeContract :: Contract -> EVM.Contract
realizeContract x =
  EVM.initialContract (contractCode x)
    & EVM.balance .~ hexWord256 (contractBalance x)
    & EVM.nonce   .~ hexWord256 (contractNonce x)
    & EVM.storage .~ contractStorage x

vmForCase :: Case -> EVM.VM
vmForCase x =
  EVM.makeVm (testVmOpts x)
    & EVM.env . EVM.contracts .~ realizeContracts (testContracts x)
