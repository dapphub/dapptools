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
import EVM.Keccak (newContractAddress)

import Control.Lens
import Control.Monad (unless)
import Control.Monad.State

import Data.ByteString (ByteString)
import Data.List (intercalate)

import IPPrint.Colored (cpprint)
import Options.Generic
import System.Console.Readline
import System.IO

import qualified Data.ByteString.Lazy  as LazyByteString
import qualified Data.ByteString       as ByteString
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
      , debug      :: Bool
      }
  | Ethrun
      { code :: ByteString
      , call :: [ByteString]
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
    Exec {} ->
      launchExec opts
    VMTest {} ->
      launchVMTest opts
    Ethrun {} -> do
      cpprint ("calldatas", call opts)
      ethrun (code opts) (map (hexByteString "calldata") (call opts))

ethrun :: ByteString -> [ByteString] -> IO ()
ethrun code calldatas =
  do let creationCode = hexByteString "stdin" code
     case runState exec (vmForEthrunCreation creationCode) of
       (EVM.VMSuccess targetCode, vm1) -> do
         let target = view (EVM.state . EVM.contract) vm1
         doEthrunTransactions (execState (EVM.performCreation targetCode) vm1)
           target targetCode calldatas

doEthrunTransactions :: EVM.VM -> Addr -> ByteString -> [ByteString] -> IO ()
doEthrunTransactions _ _ _ [] = return ()
doEthrunTransactions vm target targetCode (targetData:xs) = do
  vm' <- debugger . flip execState vm $ do
    EVM.resetState
    EVM.loadContract target
    assign (EVM.state . EVM.calldata) targetData
    assign (EVM.state . EVM.caller)   ethrunAddress

  case view EVM.result vm' of
    EVM.VMSuccess out -> do
      cpprint (out, vm')
      doEthrunTransactions vm' target targetCode xs

ethrunAddress :: Addr
ethrunAddress = Addr 0x00a329c0648769a73afac7f9381e08fb43dbea72

vmForEthrunCreation :: ByteString -> EVM.VM
vmForEthrunCreation creationCode =
  EVM.makeVm $ EVM.VMOpts
    { EVM.vmoptCode = creationCode
    , EVM.vmoptCalldata = ""
    , EVM.vmoptValue = 0
    , EVM.vmoptAddress = newContractAddress ethrunAddress 1
    , EVM.vmoptCaller = ethrunAddress
    , EVM.vmoptOrigin = ethrunAddress
    , EVM.vmoptCoinbase = 0
    , EVM.vmoptNumber = 0
    , EVM.vmoptTimestamp = 0
    , EVM.vmoptGaslimit = 0
    , EVM.vmoptDifficulty = 0
    }

launchExec :: Command -> IO ()
launchExec opts =
  let vm = vmFromCommand opts in
    case optsMode opts of
      Run -> print (execState exec vm)
      Debug -> ignore (debugger vm)

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

launchVMTest :: Command -> IO ()
launchVMTest opts =
#if MIN_VERSION_aeson(1, 0, 0)
  VMTest.parseSuite <$> LazyByteString.readFile (file opts) >>=
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
      do _ <- debugger vm
         return True -- XXX
#endif

debugger :: EVM.VM -> IO EVM.VM
debugger vm = do
  cpprint (view EVM.state vm)
  cpprint (view EVM.logs vm)
  cpprint (EVM.vmOp vm)
  cpprint (EVM.opParams vm)
  cpprint (view EVM.frames vm)
  if vm ^. EVM.result /= EVM.VMRunning
    then do
      print (vm ^. EVM.result)
      return vm
    else
    readline "(evm) " >>=
      \case
        Nothing ->
          return vm
        Just line ->
          case words line of
            [] ->
              debugger (execState EVM.exec1 vm)

            ["block"] ->
              do cpprint (view EVM.block vm)
                 debugger vm

            ["storage"] ->
              do cpprint (view (EVM.env . EVM.contracts) vm)
                 debugger vm

            ["disassemble"] ->
              do cpprint (EVM.codeOps (view (EVM.state . EVM.code) vm))
                 debugger vm

            _  -> debugger vm

ignore :: Monad m => m a -> m ()
ignore x = x >> return ()
