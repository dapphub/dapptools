{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}

module SMT2.Test where

import SMT2.Parse
import SMT2.Syntax.Typed

prog1 :: Script
prog1 = [smt2|
  (assert (or true (true) false))
  (assert (or true (true) false))
  (check-sat)
|]

