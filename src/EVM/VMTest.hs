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
import Control.Monad

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
  , testExpectation :: Expectation
  } deriving Show

data Contract = Contract
  { contractBalance :: Hexword
  , contractCode    :: ByteString
  , contractNonce   :: Hexword
  , contractStorage :: Map Word256 Word256
  } deriving Show

data Expectation = Expectation
  { expectedOut :: Maybe ByteString
  , expectedContracts :: Maybe (Map Address Contract)
  } deriving Show

checkExpectation :: Case -> EVM.VM -> IO Bool
checkExpectation x vm = do
  let check f g s =
        do v <- maybe (return True) (f vm) (g (testExpectation x))
           unless v (putStrLn s)
           return v

  e1 <- check checkExpectedContracts expectedContracts
          "failed contract expectations"
  e2 <- check checkExpectedOut expectedOut
          "failed output value"

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
           <*> addrField env  "currentCoinbase"
           <*> wordField env  "currentNumber"
           <*> wordField env  "currentTimestamp"
           <*> wordField env  "currentGasLimit"
           <*> wordField env  "currentDifficulty"
       _ ->
         JSON.typeMismatch "VM test case" (JSON.Object v)

parseContracts ::
  JSON.Object -> JSON.Parser (Map Address Contract)
parseContracts v =
  v .: "pre" >>= parseJSON

parseExpectation :: JSON.Object -> JSON.Parser Expectation
parseExpectation v =
  Expectation
    <$> (fmap hexText <$> v .:? "out")
    <*> v .:? "post"

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
