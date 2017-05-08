-- Main file of the hsevm CLI program

{-# Language BangPatterns #-}
{-# Language DeriveGeneric #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language LambdaCase #-}
{-# Language OverloadedStrings #-}
{-# Language TemplateHaskell #-}

import qualified EVM as EVM
import qualified EVM.VMTest as VMTest

import EVM.Types

import Control.Lens
import Control.Monad (unless)

import Data.ByteString (ByteString)
import Data.List (intercalate)

import IPPrint.Colored (cpprint)
import Options.Generic
import System.Console.Readline
import System.IO

import qualified Data.ByteString.Lazy  as ByteString
import qualified Data.Map              as Map

-- This record defines the program's command-line options
-- automatically via the `optparse-generic` package.
data Command
  = Exec
      { code       :: ByteString
      , trace      :: Bool
      , calldata   :: Maybe ByteString
      , address    :: Maybe Address
      , caller     :: Maybe Address
      , origin     :: Maybe Address
      , coinbase   :: Maybe Address
      , value      :: Maybe Hexword
      , number     :: Maybe Hexword
      , timestamp  :: Maybe Hexword
      , gaslimit   :: Maybe Hexword
      , difficulty :: Maybe Hexword
      }
  | VMTest
      { file  :: String
      , test  :: [String]
      , debug :: Bool
      }
  deriving (Show, Generic, Eq)

instance ParseRecord Command

data Mode = Debug | Run

main :: IO ()
main = do
  opts <- getRecord "hsevm -- Ethereum evaluator"
  case opts of
    Exec {}   -> print (vmFromCommand opts)
    VMTest {} ->
      VMTest.parseSuite <$> ByteString.readFile (file opts) >>=
       \case
         Left err -> print err
         Right allTests ->
           let testFilter =
                 if null (test opts)
                 then id
                 else filter (\(x, _) -> elem x (test opts))
           in do
             let tests = testFilter (Map.toList allTests)
             putStrLn $ "Running " ++ show (length tests) ++ " tests"
             results <- mapM (runVMTest (optsMode opts)) tests
             let failed = [name | (name, False) <- zip (map fst tests) results]
             unless (null failed) $ do
               putStrLn ""
               putStrLn $ "Failed: " ++ intercalate ", " failed

vmFromCommand :: Command -> EVM.VM
vmFromCommand opts =
  EVM.makeVm $ EVM.VMOpts
    { EVM.vmoptCode       = hexByteString "--code" (code opts)
    , EVM.vmoptCalldata   = maybe "" (hexByteString "--calldata")
                              (calldata opts)
    , EVM.vmoptValue      = word value 0
    , EVM.vmoptAddress    = addr address 1
    , EVM.vmoptCaller     = addr caller 2
    , EVM.vmoptOrigin     = addr origin 3
    , EVM.vmoptCoinbase   = addr coinbase 0
    , EVM.vmoptNumber     = word number 0
    , EVM.vmoptTimestamp  = word timestamp 0
    , EVM.vmoptGaslimit   = word gaslimit 0
    , EVM.vmoptDifficulty = word difficulty 0
    }
  where
    word f def = maybe def hexWord256 (f opts)
    addr f def = maybe def addressWord256 (f opts)

optsMode :: Command -> Mode
optsMode x = if debug x then Debug else Run

exec :: EVM.VM -> IO EVM.VM
exec vm =
  case vm ^. EVM.result of
    EVM.VMRunning -> do
      EVM.exec1 vm >>= exec
    _ ->
      return vm

runVMTest :: Mode -> (String, VMTest.Case) -> IO Bool
runVMTest mode (name, x) = do
  let vm = VMTest.vmForCase x
  case mode of
    Run ->
      do putStr (name ++ ": ")
         hFlush stdout
         vm' <- exec vm
         ok <- VMTest.checkExpectation x vm'
         putStrLn (if ok then "OK" else "FAIL")
         return ok

    Debug ->
      do debugger vm
         return True -- XXX

debugger :: EVM.VM -> IO ()
debugger vm = do
  cpprint (vm ^. EVM.state)
  cpprint (EVM.vmOp vm)
  cpprint (EVM.opParams vm)
  cpprint (vm ^. EVM.frames)
  if vm ^. EVM.result /= EVM.VMRunning
    then do print (vm ^. EVM.result)
    else
    readline "(evm) " >>=
      \case
        Nothing ->
          return ()
        Just line ->
          case words line of
            [] ->
              EVM.exec1 vm >>= debugger
            ["block"] ->
              do cpprint (view EVM.block vm)
                 debugger vm
            ["storage"] ->
              do cpprint (view (EVM.env . EVM.contracts) vm)
            _  -> debugger vm
