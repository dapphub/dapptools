{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}

module SMT2.Test where

import SMT2.Parse
import SMT2.Syntax.Typed
import SMT2.Type

prog1 :: Command
prog1 = [smt2|(assert (or true (true) false))|]

