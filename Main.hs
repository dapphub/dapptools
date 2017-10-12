{-# Language OverloadedStrings #-}

import Jays (jays)

import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO (stdout, stderr)

import Data.Text (pack)
import Data.Monoid ((<>))

import qualified Data.ByteString.Lazy.Char8 as BS

main :: IO ()
main = do
  input <- BS.getContents
  args <- getArgs
  case jays input (map pack args) of
    (output, succeeded) -> do
      let output' = if BS.null output then "" else output <> "\n"
      BS.hPutStrLn (if succeeded then stdout else stderr) output'
      if succeeded then exitSuccess else exitFailure
