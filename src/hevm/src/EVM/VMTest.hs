{-# Language CPP #-}
{-# Language TemplateHaskell #-}

module EVM.VMTest
  ( Case
#if MIN_VERSION_aeson(1, 0, 0)
  , parseSuite
#endif
  , vmForCase
  , checkExpectation
  , interpret
  ) where

import qualified EVM
import qualified EVM.Concrete as EVM
import qualified EVM.Exec
import qualified EVM.FeeSchedule as EVM.FeeSchedule
import qualified EVM.Stepper as Stepper
import qualified EVM.Fetch as Fetch

import Control.Monad.State.Strict (runState, join)
import qualified Control.Monad.Operational as Operational
import qualified Control.Monad.State.Class as State

import EVM (EVM)
import EVM.Stepper (Stepper)
import EVM.Types

import Control.Lens

import IPPrint.Colored (cpprint)

import Data.ByteString (ByteString)
import Data.Aeson ((.:), (.:?))
import Data.Aeson (FromJSON (..))
import Data.Map (Map)
import Data.List (intercalate)

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

checkExpectation :: Case -> EVM.VM -> IO Bool
checkExpectation x vm =
  case (testExpectation x, view EVM.result vm) of
    (Just expectation, Just (EVM.VMSuccess output)) -> do
      let
        (s1, b1) = ("bad-state", checkExpectedContracts vm (expectedContracts expectation))
        (s2, b2) = ("bad-output", checkExpectedOut output (expectedOut expectation))
        (s3, b3) = ("bad-gas", checkExpectedGas vm (expectedGas expectation))
        ss = map fst (filter (not . snd) [(s1, b1), (s2, b2), (s3, b3)])
      putStr (intercalate " " ss)
      return (b1 && b2 && b3)
    (Nothing, Just (EVM.VMSuccess _)) -> do
      putStr "unexpected-success"
      return False
    (Nothing, Just (EVM.VMFailure _)) ->
      return True
    (Just _, Just (EVM.VMFailure _)) -> do
      putStr "unexpected-failure"
      return False
    (_, Nothing) -> do
      cpprint (view EVM.result vm)
      error "internal error"

checkExpectedOut :: ByteString -> ByteString -> Bool
checkExpectedOut output expected =
  output == expected

checkExpectedContracts :: EVM.VM -> Map Addr Contract -> Bool
checkExpectedContracts vm expected =
  realizeContracts expected == vm ^. EVM.env . EVM.contracts . to (fmap clearZeroStorage)

clearZeroStorage :: EVM.Contract -> EVM.Contract
clearZeroStorage =
  over EVM.storage (Map.filterWithKey (\_ x -> x /= 0))

checkExpectedGas :: EVM.VM -> W256 -> Bool
checkExpectedGas vm expected =
  case vm ^. EVM.state . EVM.gas of
    EVM.C _ x | x == expected -> True
    _ -> False

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
           <*> wordField exec "gasPrice"
           <*> pure (EVM.FeeSchedule.homestead)
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

realizeContracts :: Map Addr Contract -> Map Addr EVM.Contract
realizeContracts = Map.fromList . map f . Map.toList
  where
    f (a, x) = (a, realizeContract x)

realizeContract :: Contract -> EVM.Contract
realizeContract x =
  EVM.initialContract (EVM.RuntimeCode (contractCode x))
    & EVM.balance .~ EVM.w256 (contractBalance x)
    & EVM.nonce   .~ EVM.w256 (contractNonce x)
    & EVM.storage .~ (
        Map.fromList .
        map (\(k, v) -> (EVM.w256 k, EVM.w256 v)) .
        Map.toList $ contractStorage x
        )

vmForCase :: Case -> EVM.VM
vmForCase x =
  EVM.makeVm (testVmOpts x)
    & EVM.env . EVM.contracts .~ realizeContracts (testContracts x)
    & EVM.execMode .~ EVM.ExecuteAsVMTest

interpret :: Stepper a -> EVM a
interpret =
  eval . Operational.view

  where
    eval
      :: Operational.ProgramView Stepper.Action a
      -> EVM a

    eval (Operational.Return x) =
      pure x

    eval (action Operational.:>>= k) =
      case action of
        Stepper.Exec ->
          EVM.Exec.exec >>= interpret . k
        Stepper.Wait q ->
          do join (Fetch.zero q)
             interpret (k ())
        Stepper.Note _ ->
          interpret (k ())
        Stepper.Fail _ ->
          error "VMTest stepper not supposed to fail"
        Stepper.EVM m ->
          State.state (runState m) >>= interpret . k
