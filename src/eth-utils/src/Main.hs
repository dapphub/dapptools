{-# Language DataKinds #-}
{-# Language StandaloneDeriving #-}
{-# Language DeriveAnyClass #-}
{-# Language FlexibleInstances #-}
{-# Language DeriveGeneric #-}
{-# Language GADTs #-}
{-# Language LambdaCase #-}
{-# Language OverloadedStrings #-}
{-# Language TypeOperators #-}
{-# Language RecordWildCards #-}

module Main where

import Data.ByteString (ByteString)
import Options.Generic as Options
import Data.Maybe
import Data.List
import Data.Aeson.Lens
import System.Directory
import Control.Lens
import qualified Data.Text as T
import qualified Data.Vector as V

import EVM.ABI
import EVM.Solidity
import EVM.Types hiding (word)
import EVM.Dapp (dappInfo)
import EVM.RLP (rlpdecode)

import qualified EVM.Flatten

-- This record defines the program's command-line options
-- automatically via the `optparse-generic` package.
data Command w
  = Flatten -- Concat all dependencies for a given source file
    { sourceFile :: w ::: String       <?> "Path to solidity source file e.g. src/contract.sol"
    , jsonFile   :: w ::: Maybe String <?> "Filename or path to dapp build output (default: out/*.solc.json)"
    , dappRoot   :: w ::: Maybe String <?> "Path to dapp project root directory (default: . )"
    }
  | Rlp  -- RLP decode a string and print the result
    { decode :: w ::: ByteString <?> "RLP encoded hexstring"
    }
  | Abiencode
    { abi  :: w ::: Maybe String <?> "Signature of types to decode / encode"
    , arg  :: w ::: [String]     <?> "Values to encode"
    }
  | StripMetadata -- Remove metadata from contract bytecode
    { code        :: w ::: Maybe ByteString       <?> "Program bytecode"
    }

  deriving (Options.Generic)

instance Options.ParseRecord (Command Options.Wrapped) where
  parseRecord =
    Options.parseRecordWithModifiers Options.lispCaseModifiers

main :: IO ()
main = do
  cmd <- Options.unwrapRecord "eth-utils"
  let
    root = fromMaybe "." (dappRoot cmd)
  case cmd of
    Abiencode {} ->
      print . ByteStringS $ abiencode (abi cmd) (arg cmd)
    Flatten {} ->
      withCurrentDirectory root $ do
        theJson <- findJsonFile (jsonFile cmd)
        readSolc theJson >>=
          \case
            Just (contractMap, cache) -> do
              let dapp = dappInfo "." contractMap cache
              EVM.Flatten.flatten dapp (T.pack (sourceFile cmd))
            Nothing ->
              error ("Failed to read Solidity JSON for `" ++ theJson ++ "'")
    Rlp {} ->
      case rlpdecode $ hexByteString "--decode" $ strip0x $ decode cmd of
        Nothing -> error "Malformed RLP string"
        Just c -> print c
    StripMetadata {} -> print .
      ByteStringS . stripBytecodeMetadata . hexByteString "bytecode" . strip0x $ fromJust $ code cmd


findJsonFile :: Maybe String -> IO String
findJsonFile (Just s) = pure s
findJsonFile Nothing = do
  outFiles <- listDirectory "out"
  case filter (isSuffixOf ".sol.json") outFiles of
    [x] -> pure ("out/" ++ x)
    [] ->
      error $ concat
        [ "No `*.sol.json' file found in `./out'.\n"
        , "Maybe you need to run `dapp build'.\n"
        , "You can specify a file with `--json-file'."
        ]
    xs ->
      error $ concat
        [ "Multiple `*.sol.json' files found in `./out'.\n"
        , "Specify one using `--json-file'.\n"
        , "Files found: "
        , intercalate ", " xs
        ]

parseAbi :: (AsValue s) => s -> (Text, [AbiType])
parseAbi abijson =
  (signature abijson, snd
    <$> parseMethodInput
    <$> V.toList
      (fromMaybe (error "Malformed function abi") (abijson ^? key "inputs" . _Array)))

abiencode :: (AsValue s) => Maybe s -> [String] -> ByteString
abiencode Nothing _ = error "missing required argument: abi"
abiencode (Just abijson) args =
  let (sig', declarations) = parseAbi abijson
  in if length declarations == length args
     then abiMethod sig' $ AbiTuple . V.fromList $ zipWith makeAbiValue declarations args
     else error $ "wrong number of arguments:" <> show (length args) <> ": " <> show args
