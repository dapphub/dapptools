module EVM.Debug where

import EVM
import EVM.Types
import EVM.Exec
import EVM.Solidity

import Control.Arrow (second)

import qualified Data.Vector.Unboxed   as Vector
import Data.Text (Text)
import Data.Map (Map)
import qualified Data.ByteString       as ByteString
import qualified Data.Map              as Map

import Control.Monad.State (execState)
import Control.Lens

import System.Console.Readline
import IPPrint.Colored (cpprint)

import Text.PrettyPrint.ANSI.Leijen

object :: [(Doc, Doc)] -> Doc
object xs =
  group $ lbrace
    <> line
    <> indent 2 (sep (punctuate (char ';') [k <+> equals <+> v | (k, v) <- xs]))
    <> line
    <> rbrace

prettyContract :: Contract -> Doc
prettyContract c =
  object $
    [ (text "codesize", int (ByteString.length (c ^. bytecode)))
    , (text "codehash", text (show (c ^. codehash)))
    , (text "balance", int (fromIntegral (c ^. balance)))
    , (text "nonce", int (fromIntegral (c ^. nonce)))
    ]

prettyContracts :: Map Addr Contract -> Doc
prettyContracts x =
  object $
    (map (\(a, b) -> (text (show a), prettyContract b))
     (Map.toList x))

debugger :: Maybe SourceCache -> VM -> IO VM
debugger maybeCache vm = do
  -- cpprint (view state vm)
  cpprint (view (state . pc) vm)
  cpprint (view (state . stack) vm)
  cpprint (view logs vm)
  cpprint (vmOp vm)
  cpprint (opParams vm)
  cpprint (length (view frames vm))

  putDoc (prettyContracts (view (env . contracts) vm))

  case maybeCache of
    Nothing ->
      return ()
    Just cache ->
      case currentSrcMap vm of
        Nothing -> cpprint "no srcmap"
        Just sm -> cpprint (srcMapCodePos cache sm)

  if vm ^. result /= VMRunning
    then do
      print (vm ^. result)
      return vm
    else
    readline "(evm) " >>=
      \case
        Nothing ->
          return vm
        Just line ->
          case words line of
            [] ->
              debugger maybeCache (execState exec1 vm)

            ["block"] ->
              do cpprint (view block vm)
                 debugger maybeCache vm

            ["storage"] ->
              do cpprint (view (env . contracts) vm)
                 debugger maybeCache vm

            ["disassemble"] ->
              do cpprint (codeOps (view (state . code) vm))
                 debugger maybeCache vm

            _  -> debugger maybeCache vm

currentSrcMap :: VM -> Maybe SrcMap
currentSrcMap vm =
  let
    c = vm ^?! env . contracts . ix (vm ^. state . contract)
    theOpIx = (c ^. opIxMap) Vector.! (vm ^. state . pc)
  in
    vm ^? env . solc . ix (c ^. codehash) . solcSrcmap . ix theOpIx

srcMapCodePos :: SourceCache -> SrcMap -> Maybe (Text, Int)
srcMapCodePos cache sm =
  fmap (second f) $ cache ^? sourceFiles . ix (srcMapFile sm)
  where
    f v = ByteString.count 0xa (ByteString.take (srcMapOffset sm - 1) v) + 1
