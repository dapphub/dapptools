-- Main file of the hsevm CLI program

{-# Language CPP #-}
{-# Language BangPatterns #-}
{-# Language DeriveGeneric #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language LambdaCase #-}
{-# Language OverloadedStrings #-}
{-# Language TemplateHaskell #-}

import qualified EVM as EVM

#if MIN_VERSION_aeson(1, 0, 0)
import qualified EVM.VMTest as VMTest
#endif

import EVM.Types

import Control.Lens
import Control.Monad (unless)
import Control.Monad.State

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
      , address    :: Maybe Addr
      , caller     :: Maybe Addr
      , origin     :: Maybe Addr
      , coinbase   :: Maybe Addr
      , value      :: Maybe W256
      , number     :: Maybe W256
      , timestamp  :: Maybe W256
      , gaslimit   :: Maybe W256
      , difficulty :: Maybe W256
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
#if MIN_VERSION_aeson(1, 0, 0)
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
#else
      putStrLn "Not supported"
#endif

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
    word f def = maybe def id (f opts)
    addr f def = maybe def id (f opts)

optsMode :: Command -> Mode
optsMode x = if debug x then Debug else Run

exec :: State EVM.VM EVM.VMResult
exec =
  use EVM.result >>= \case
    EVM.VMRunning -> EVM.exec1 >> exec
    x -> return x

#if MIN_VERSION_aeson(1, 0, 0)
runVMTest :: Mode -> (String, VMTest.Case) -> IO Bool
runVMTest mode (name, x) = do
  let vm = VMTest.vmForCase x
  case mode of
    Run ->
      do putStr (name ++ ": ")
         hFlush stdout
         let vm' = execState exec vm
         ok <- VMTest.checkExpectation x vm'
         putStrLn (if ok then "OK" else "FAIL")
         return ok

    Debug ->
      do debugger vm
         return True -- XXX
#endif

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
              debugger (execState EVM.exec1 vm)
            ["block"] ->
              do cpprint (view EVM.block vm)
                 debugger vm
            ["storage"] ->
              do cpprint (view (EVM.env . EVM.contracts) vm)
            _  -> debugger vm
