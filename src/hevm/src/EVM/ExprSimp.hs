{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE TemplateHaskell #-}

module EVM.ExprSimp where

import Data.Text (Text, unpack)
import EVM (VM)
import EVM.Dapp (DappInfo (..))
import EVM.Expr
import EVM.Solidity (Method, StorageItem (..))
import EVM.Types (num)

import Debug.Trace

-------------------------------------------------- knowledge base

data ExprContext = ExprContext
  { isKnownStorageSlot :: ExprC -> Bool,
    simpStorage :: ExprC -> ExprC
  }

-- propagate data from solidity and environment
-- propagateData :: DappInfo ->
--
-- storagelist = Map.toList $ fromMaybe mempty (fromMaybe Nothing (_storageLayout <$> currentSolc ?srcInfo ?vm))
--
-- TODO: whats that?
-- simpW (FromBuff (Literal x) Calldata) =
--   let
--     input = fromMaybe [] $ view methodInputs <$> ?method
--     index = num (((toInteger x) - 4) `div` 32)
--   in if length input > index then Var (cParam $ unpack $ fst (input !! index)) else Todo ("sad" ++ (show index)) []

mkIsKnownStorageSlot :: [(Text, StorageItem)] -> ExprC -> Bool
mkIsKnownStorageSlot
  is
  ( EC
      "WriteWord"
      [ (ECLiteral x),
        (ECLiteral y),
        ( EC
            "WriteWord"
            [ (ECLiteral z),
              _,
              (EC "SEmpty" [])
              ]
          )
        ]
    )
    | x == 0x20 && z == 0x0 && (isKnownSlot is (num y)) = True -- TODO implicitly check if storage (y) slot is known
    | otherwise = False
    where
      isKnownSlot :: [(Text, StorageItem)] -> Int -> Bool
      isKnownSlot [] _ = False
      isKnownSlot ((_, StorageItem _ _ slot) : xs) slot'
        | slot == slot' = True
        | otherwise = isKnownSlot xs slot'
mkIsKnownStorageSlot _ _ = False

-- knownStorageSlots :: [(Text, StorageItem)] -> Int -> Bool
-- knownStorageSlots [] _ = False
-- knownStorageSlots ((_, StorageItem _ _ slot):xs) slot'
--   = slot == slot' || knownStorageSlots xs slot'

mkSimpStorage ::
  [(Text, StorageItem)] -> -- solidity storage info
  ExprC ->                 -- term
  ExprC                    -- found pointer
mkSimpStorage [] expr = expr
mkSimpStorage
  ((text, StorageItem _ _ slot) : xs)
  expr@( EC
      "WriteWord"
      [ _,
        (ECLiteral y),
        ( EC
            "WriteWord"
            [ _,
              word,
              (EC "SEmpty" [])
              ]
          )
        ]
    )
    | (num y) == slot =
      (ECPointer1 (unpack text) word)
    | otherwise =
      mkSimpStorage xs expr

-- simpStorage :: ExprC -> ExprC
-- simpStorage expr = error ""

-------------------------------------------------------- simplify

-- build simplification fixpoint
-- simplify untill nothing changes anymore

fixExpr :: (?ctx :: ExprContext) => ExprC -> ExprC
fixExpr w =
  let w' = simpExpr w
   in if w == w'
        then w
        else fixExpr w'

simpExpr :: (?ctx :: ExprContext) => ExprC -> ExprC
simpExpr e@(EC "FromKeccak" [s])
  | (isKnownStorageSlot ?ctx) s     = (simpStorage ?ctx) s
  | otherwise = EC "FromKeccak" ([simpExpr s])
simpExpr (EC "And" [(ECLiteral x), expr@(EC "And" [(ECLiteral x'), _])])
  | x == x'                         = simpExpr expr
  | otherwise                       = (EC "And" [(ECLiteral x), (simpExpr expr)])

simpExpr (EC "Slice"
  [ ECLiteral from
  , ECLiteral to
  , EC "WriteWord" [ECLiteral x, w, s]
  ])
  | from == x && to == x + 0x20
  = EC "WriteWord"
    [ ECLiteral from
    , simpExpr w
    , EC "SEmpty" []]
  | from == x && x + 0x20 < to
  = EC "WriteWord"
    [ ECLiteral x
    , simpExpr w
    , simpExpr (
      EC "Slice"
        [ ECLiteral (from + 0x20)
        , ECLiteral to
        , s])]
  | from <= x && x + 0x20 == to
  = EC "WriteWord"
    [ ECLiteral x
    , simpExpr w
    , simpExpr (EC "Slice"
      [ ECLiteral from
      , ECLiteral (to - 0x20)
      , s])]
  | otherwise = ECOops $ show (x - from)

-- fold default cases
simpExpr (EC t cs)                  = EC t (simpExpr <$> cs)
simpExpr x                          = x
