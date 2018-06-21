{-# Language OverloadedStrings #-}

import Jays (jays)

import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO (stdout, stderr)
import System.Posix.IO (stdInput)
import System.Posix.Terminal (queryTerminal)

import Data.Text (pack)
import Data.Monoid ((<>))

import qualified Data.ByteString.Lazy.Char8 as BS

main :: IO ()
main = do
  isTty <- queryTerminal stdInput
  input <- if isTty then pure "" else BS.getContents
  args <- getArgs

  if args == ["--version"]
    then
      -- This is just for compatibility with jshon versioning
      putStrLn "20171121"
    else
      case jays input (map pack args) of
        (output, succeeded) -> do
          let output' = if BS.null output then "" else output <> "\n"
          BS.hPutStr (if succeeded then stdout else stderr) output'
          if succeeded then exitSuccess else exitFailure
