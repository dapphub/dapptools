{-# Language LambdaCase #-}

module EVM.Debug where

import EVM
import EVM.Types
import EVM.Exec

import Control.Monad.State (execState)
import Control.Lens

import System.Console.Readline
import IPPrint.Colored (cpprint)

debugger :: VM -> IO VM
debugger vm = do
  cpprint (view state vm)
  cpprint (view logs vm)
  cpprint (vmOp vm)
  cpprint (opParams vm)
  cpprint (view frames vm)
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
              debugger (execState exec1 vm)

            ["block"] ->
              do cpprint (view block vm)
                 debugger vm

            ["storage"] ->
              do cpprint (view (env . contracts) vm)
                 debugger vm

            ["disassemble"] ->
              do cpprint (codeOps (view (state . code) vm))
                 debugger vm

            _  -> debugger vm
