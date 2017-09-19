{-# Language CPP #-}
{-# Language TemplateHaskell #-}

module EVM.VMTest
  ( Case
#if MIN_VERSION_aeson(1, 0, 0)
  , parseSuite
#endif
  , vmForCase
  , checkExpectation
  ) where

import qualified EVM
import qualified EVM.Concrete as EVM
import qualified EVM.Machine as EVM

import EVM.Types

import Control.Lens

import IPPrint.Colored (cpprint)

import Data.ByteString (ByteString)
import Data.Aeson ((.:), (.:?))
import Data.Aeson (FromJSON (..))
import Data.Map (Map)

import qualified Data.Map          as Map
import qualified Data.Aeson        as JSON
import qualified Data.Aeson.Types  as JSON
import qualified Data.ByteString.Lazy  as Lazy

data Case = Case
  { testVmOpts      :: EVM.VMOpts
  , testContracts   :: Map Addr Contract
  , testExpectation :: Maybe Expectation
  } deriving Show

data Contract = Contract
  { contractBalance :: W256
  , contractCode    :: ByteString
  , contractNonce   :: W256
  , contractStorage :: Map W256 W256
  } deriving Show

data Expectation = Expectation
  { expectedOut :: ByteString
  , expectedContracts :: Map Addr Contract
  , expectedGas :: W256
  } deriving Show

checkExpectation :: Case -> EVM.VM EVM.Concrete -> IO Bool
checkExpectation x vm =
  case (testExpectation x, view EVM.result vm) of
    (Just expectation, Just (EVM.VMSuccess (EVM.B output))) -> do
      t1 <- checkExpectedContracts vm (expectedContracts expectation)
      t2 <- checkExpectedOut output (expectedOut expectation)
      t3 <- checkExpectedGas vm (expectedGas expectation)
      return (t1 && t2 && t3)
    (Nothing, Just (EVM.VMSuccess _)) ->
      return False
    (Nothing, Just (EVM.VMFailure _)) ->
      return True
    (Just _, Just (EVM.VMFailure _)) ->
      return False
    (_, Nothing) -> do
      cpprint (view EVM.result vm)
      error "internal error"

checkExpectedOut :: ByteString -> ByteString -> IO Bool
checkExpectedOut output expected =
  if output == expected
  then
    return True
  else do
    cpprint ("output mismatch" :: String, output, expected)
    return False

checkExpectedContracts :: EVM.VM EVM.Concrete -> Map Addr Contract -> IO Bool
checkExpectedContracts vm expected =
  if realizeContracts expected == vm ^. EVM.env . EVM.contracts
  then return True
  else do
    cpprint (realizeContracts expected, vm ^. EVM.env . EVM.contracts)
    return False

checkExpectedGas :: EVM.VM EVM.Concrete -> W256 -> IO Bool
checkExpectedGas vm expected =
  case vm ^. EVM.state . EVM.gas of
    EVM.C _ x | x == expected -> return True
    y -> do
      cpprint ("gas mismatch" :: String, expected, y)
      return False

#if MIN_VERSION_aeson(1, 0, 0)

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
           <*> wordField exec "gas" -- XXX: correct?
           <*> wordField env  "currentNumber"
           <*> wordField env  "currentTimestamp"
           <*> addrField env  "currentCoinbase"
           <*> wordField env  "currentDifficulty"
           <*> wordField env  "currentGasLimit"
       _ ->
         JSON.typeMismatch "VM test case" (JSON.Object v)

parseContracts ::
  JSON.Object -> JSON.Parser (Map Addr Contract)
parseContracts v =
  v .: "pre" >>= parseJSON

parseExpectation :: JSON.Object -> JSON.Parser (Maybe Expectation)
parseExpectation v =
  do out       <- fmap hexText <$> v .:? "out"
     contracts <- v .:? "post"
     gas       <- v .:? "gas"
     case (out, contracts, gas) of
       (Just x, Just y, Just z) ->
         return (Just (Expectation x y z))
       _ ->
         return Nothing

parseSuite ::
  Lazy.ByteString -> Either String (Map String Case)
parseSuite = JSON.eitherDecode'

#endif

realizeContracts :: Map Addr Contract -> Map Addr (EVM.Contract EVM.Concrete)
realizeContracts = Map.fromList . map f . Map.toList
  where
    f (a, x) = (a, realizeContract x)

realizeContract :: Contract -> EVM.Contract EVM.Concrete
realizeContract x =
  EVM.initialContract (contractCode x)
    & EVM.balance .~ EVM.w256 (contractBalance x)
    & EVM.nonce   .~ EVM.w256 (contractNonce x)
    & EVM.storage .~ (
        Map.fromList .
        map (\(k, v) -> (EVM.w256 k, EVM.w256 v)) .
        Map.toList $ contractStorage x
        )

vmForCase :: Case -> EVM.VM EVM.Concrete
vmForCase x =
  EVM.makeVm (testVmOpts x)
    & EVM.env . EVM.contracts .~ realizeContracts (testContracts x)
