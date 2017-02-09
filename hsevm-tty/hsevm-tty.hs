{-# Language OverloadedStrings #-}
{-# Language BangPatterns #-}
{-# Language TemplateHaskell #-}
{-# Language LambdaCase #-}

import EVM
import EVM.Keccak
import EVM.Solidity

import Debug.Trace

import IPPrint.Colored

import Control.Arrow (second)
import Control.Lens
import Control.Monad
import Data.DoubleWord
import Data.Function (fix)
import Data.List
import Data.Map.Strict (Map)
import Data.Sequence (Seq)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Vector.Unboxed (Vector)
import Data.Vector.Unboxed.Mutable (new, write)
import Data.Word
import System.Console.Readline
import System.Directory
import System.Environment
import System.Exit
import System.FilePath

import qualified Data.ByteString as BS 
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Data.Vector.Unboxed as Vector

data UIState = UIState {
  _uiVm :: Maybe VM,
  _uiContracts :: Map Text SolcContract,
  _uiSourceCache :: SourceCache,
  _uiBreakpoints :: [(Word256, Vector Bool)]
} deriving (Show)
makeLenses ''UIState

say :: String -> IO ()
say = putStrLn

repl :: UIState -> IO ()
repl ui = do
  let lbl = case ui ^. uiVm of
              Nothing -> "(evm) "
              Just vm ->
                case currentSrcMap vm >>= srcMapCodePos ui of
                  Nothing -> "(evm@unknown) "
                  Just (x, y) -> "(evm@" ++ Text.unpack x ++ ":" ++ show y ++ ") "
  readline lbl >>= \case
    Nothing -> return ()
    Just line -> do
      case Text.words (Text.pack line) of
        ["help"] -> do
          say "Commands:"
          say "  abi <contract>     -- show an ABI"
          say ""
          repl ui
          
        ["abi", x] -> do
          case ui ^? uiContracts . ix x . abiMap of
            Nothing -> say "error: No such contract."
            Just it -> do
              forM_ (sort (map (\(a, b) -> (b, a)) (Map.toList it))) $ \(k, v) ->
                putStrLn $ "  " ++ Text.unpack k ++ "   hash " ++ show v
          repl ui

        ["break", fileName, lineNo] -> do
          case locateBreakpoint ui fileName (read (Text.unpack lineNo)) of
            Nothing -> do
              say "error: Not found."
              repl ui
            Just bp -> do
              say "Breakpoint set.  Matching code found in contracts:"
              cpprint $
                map (\(h, _) -> ui ^?! uiVm . _Just . solc . ix h . name)
                  (filter (\(_, w) -> Vector.elem True w) bp)
              repl (ui & uiBreakpoints %~ (bp ++))

        ["entry", contractName, abiEntry] -> do
          setEntry ui contractName abiEntry

        ["run"] ->
          let k ui' = step StopOnBreakpoint ui' k in k ui
        ["continue"] ->
          let k ui' = step DoNotStop ui' k in k ui

        []       -> step SingleStep ui repl
        ["step"] -> step SingleStep ui repl
        ["s"]    -> step SingleStep ui repl

        ["vm"] -> do
          case ui ^. uiVm of
            Nothing -> say "VM not created yet."
            Just vm ->
              let
                c = vm ^?! contracts . ix (vm ^. contract)
                solC = vm ^? solc . ix (c ^. codehash)
              in cpprint (
                ("pc", vm ^. pc),
                ("stack", vm ^. stack),
                ("contract", vm ^. contract),
                ("contract-name", solC ^? _Just . name),
                ("calldata", BS.unpack $ vm ^. calldata),
                ("callvalue", vm ^. callvalue),
                ("caller", vm ^. caller),
                ("opIx", (c ^. opIxMap) Vector.! (vm ^. pc))
                )
          repl ui   

        _ -> say "?" >> repl ui

setEntry :: UIState -> Text -> Text -> IO ()
setEntry ui contractName abiEntry =
  let
    vm = initialVm
           (word32Bytes (abiKeccak (encodeUtf8 abiEntry)))
           0
           (ui ^?! uiContracts . ix contractName . runtimeCode)
           (ui ^. uiContracts)
           (ui ^. uiSourceCache)
  in do
    say $ "Entry set to `" ++ Text.unpack contractName ++ "::" ++ Text.unpack abiEntry ++ "'."
    repl (ui & uiVm .~ Just vm)

data Behavior = StopOnBreakpoint | SingleStep | DoNotStop
  deriving (Show, Eq)

step :: Behavior -> UIState -> (UIState -> IO ()) -> IO ()
step behavior ui k =
  case ui ^. uiVm of
    Nothing -> do
      say "error: You must set the entry point."
      repl ui
    Just vm ->
      if vm ^. done
      then do
        say "Execution finished."
        repl ui
      else let
        c = vm ^?! contracts . ix (vm ^. contract)
        solcC = vm ^?! solc . ix (c ^. codehash)
        ch = c ^. codehash
        bps = filter (\(h, _) -> h == ch) (ui ^. uiBreakpoints)
        theOpIx = (c ^. opIxMap) Vector.! (vm ^. pc)
        onBreakpoint = any (\(w, v) -> w == ch && v Vector.! theOpIx) bps
      in
        if behavior == StopOnBreakpoint && onBreakpoint 
        then do
          say $ "Stopped at breakpoint in `" ++ Text.unpack (solcC ^. name) ++ "'."
          repl ui
        else do
          vm' <- exec1 vm
          k (ui & uiVm .~ (Just $! vm'))

currentSrcMap :: VM -> Maybe SrcMap
currentSrcMap vm =
  let
    c = vm ^?! contracts . ix (vm ^. contract)
    theOpIx = (c ^. opIxMap) Vector.! (vm ^. pc)
  in
    vm ^? solc . ix (c ^. codehash) . solcSrcmap . ix (theOpIx)

srcMapCodePos :: UIState -> SrcMap -> Maybe (Text, Int)
srcMapCodePos ui sm =
  fmap (second f) $ ui ^? uiSourceCache . sourceFiles . ix (srcMapFile sm)
  where
    f v = BS.count 0xa (BS.take (srcMapOffset sm - 1) v) + 1
    
locateBreakpoint :: UIState -> Text -> Int -> Maybe [(Word256, Vector Bool)]
locateBreakpoint ui fileName lineNo = do
  (i, (t, s)) <-
    flip find (Map.toList (ui ^. uiSourceCache . sourceFiles))
      (\(_, (t, _)) -> t == fileName)
  let ls = BS.split 0x0a s
      l = ls !! (lineNo - 1)
      offset = 1 + sum (map ((+ 1) . BS.length) (take (lineNo - 1) ls))
      horizon = offset + BS.length l
  return $ Map.elems (ui ^. uiVm . _Just . solc)
    & map (\c -> (
        c ^. solcCodehash,
        Vector.create $ new (Seq.length (c ^. solcSrcmap)) >>= \v -> do
          fst $ foldl' (\(!m, !j) (sm@SM { srcMapOffset = o }) ->
            if srcMapFile sm == i && o >= offset && o < horizon
            then (m >> write v j True, j + 1)
            else (m >> write v j False, j + 1)) (return (), 0) (c ^. solcSrcmap)
          return v
      ))

main :: IO ()
main = do
  say "hsevm 0.1 -- interactive EVM/Solidity debugger"
  say "For info, see https://dev.dapphub.com/hsevm"
  say "For help, type `help'."
  say ""
  xs <- getArgs
  case xs of
    [a] -> do
      setCurrentDirectory (takeDirectory a)
      Just (c, cache) <- readSolc a
      say $ "Loaded " ++ show (Map.size c) ++ " contracts from `" ++ a ++ "':"
      cpprint (Map.keys c)
      repl UIState {
        _uiVm = Nothing,
        _uiContracts = c,
        _uiSourceCache = cache,
        _uiBreakpoints = mempty
      }
    _ -> die "usage: hsevm <solc>"
