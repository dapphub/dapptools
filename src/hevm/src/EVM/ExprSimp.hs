{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE TemplateHaskell #-}

-- TODO:
--       get ridd of oops for todo's
--       replace new_name_needed with debrujin var & freshness
--       implement unification and rewrite simpExpr with it!

--       TYPES!
--       can readword/writeword be rewritten as lambdas
--       does it even make sense?
--       or maybe it makes more sense to rewrite storage in the
--       same way as buffers with readwod writeword?

--       storage is of type W256 -> W256 rn, but if multiple
--       contracts come into play the location of the storage
--       need to be taken into account, so maybe state should be
--       of type Addr -> W256 -> W256

module EVM.ExprSimp where

import Data.Text (Text, unpack)
import qualified Data.Map as Map
import Data.Word (Word32)
import EVM (VM)
import EVM.Dapp (DappInfo (..))
import EVM.Expr
import EVM.Solidity (methodInputs, Method(..), StorageItem (..))
import EVM.Types (num)
import Control.Lens hiding (op, (:<), (|>), (.>))

import Debug.Trace

-------------------------------------------------- knowledge base

data ExprContext = ExprContext
  { _isKnownStorageSlot :: ExprC -> Bool
  , _simpStorage        :: ExprC -> ExprC
  , _abimap             :: Map.Map Word32 Method
  , _methodctx          :: Maybe Method
  }
makeLenses ''ExprContext


-- type inference
-- special cases
inferExpr :: ExprC -> ExprC
inferExpr e@(ECTodo _ _)     = e
inferExpr e@(ECLiteral _)    = e
inferExpr e@(ECPointer1 _ _) = e
inferExpr e@(ECVar _ _)      = e
inferExpr e@(ECSig _ _)      = e
-- inferExpr e@(ECOops _)       = e

-- zero children
inferExpr e@(EC t tt [])
  | t `elem` [ "Calldata", "SEmpty", "UStorage", "Bottom" ]
  = (EC t tt [])
  | otherwise
  = error $ "unknown type found " ++ (show e)


-- single child
inferExpr e@(EC t tt [a])
  | t `elem` [ "IsZero" ]
  = let a'   = inferExpr a
        at   = exprType a'
    in (EC t ECTBool [a'])
  | t `elem` [ "Neg", "Sex", "Sgn", "Cmp", "Bit" ]
  = let a'   = inferExpr a
        at   = exprType a'
    in (EC t tt [a'])
  | t `elem` [ "FromKeccak" ]
  = let a'   = inferExpr a
        at   = exprType a'
    -- TODO - require at 2b buffer
    in (EC t (ECTWord 256) [a'])
  | otherwise
  = error $ "while type infering " ++ (show e)


-- two child
inferExpr e@(EC t tt [a, b])
  | t `elem` [ "And", "Or" ]
  = let a'   = inferExpr a
        at   = exprType a'
        b'   = inferExpr b
        bt   = exprType b'
        maxT = max at bt
    in (EC t maxT [a', b'])
  | t `elem` [ "Eq", "LT", "GT", "SLT", "SGT" ]
  = let a'   = inferExpr a
        at   = exprType a'
        b'   = inferExpr b
        bt   = exprType b'
    in if at == bt then
      (EC t ECTBool [a', b'])
      else trace ("error"++(show t) ++ " " ++ (show at) ++ " " ++ (show bt)) (EC t ECTError [a', b'])
  | t `elem` ["Add", "Sub", "Mul", "Div", "Mod", "Exp",
  -- todo
  "SHL", "SHR", "SAR"]
  = let a'   = inferExpr a
        at   = exprType a'
        b'   = inferExpr b
        bt   = exprType b'
    -- TODO - catch error here if at and bt W256
    in (EC t (max at bt) [a', b'])
  | t `elem` ["ReadStorage"]
  = let a'   = inferExpr a
        at   = exprType a'
        b'   = inferExpr b
        bt   = exprType b'
        tt'  = if at <= ECTWord 256
               && bt <= ECTStorage
          then ECTWord 256
          else trace ("error "++(show t) ++ " " ++ (show at) ++ " " ++ (show bt)) ECTError
    in (EC t tt' [a', b'])
  | t `elem` ["ReadWord"]
  = let a'   = inferExpr a
        at   = exprType a'
        b'   = inferExpr b
        bt   = exprType b'
        tt'  = if at <= ECTWord 256
               && bt == ECTBuffer
          then ECTWord 256
          else trace ("error"++(show t) ++ " " ++ (show at) ++ " " ++ (show bt)) ECTError
    in (EC t tt' [a', b'])
  | t `elem` ["Lambda"]
  = let a'   = inferExpr a
        at   = exprType a'
        b'   = inferExpr b
        bt   = exprType b'
        tt'  = if at <= ECTWord 256
          then ECTFunction at bt
          else trace ("error"++(show t) ++ " " ++ (show at) ++ " " ++ (show bt)) ECTError
    in (EC t tt' [a', b'])
  | t `elem` ["Return"]
  = let a'   = inferExpr a
        at   = exprType a'
        b'   = inferExpr b
        bt   = exprType b'
        tt'  = if at == ECTBuffer
               && bt == ECTStorage
          then ECTAnd at bt
          else trace ("error"++(show t) ++ " " ++ (show at) ++ " " ++ (show bt)) ECTError
    in (EC t tt' [a', b'])
  | otherwise
  = error $ "could not inferr type for " ++ (show e)
  -- = let a'   = inferExpr a
  --       b'   = inferExpr b
  --   -- TODO - catch error here if at and bt W256
  --   in (EC t ECTUnknown [a', b'])

-- three children
inferExpr (EC t tt [a, b, c])
  | t `elem` ["ITE"]
  = let a'   = inferExpr a
        at   = exprType a'
        b'   = inferExpr b
        bt   = exprType b'
        c'   = inferExpr c
        ct   = exprType c'
        tt'  = if at <= ECTWord 256
               && bt < ECTBuffer
               && ct < ECTBuffer
          then max bt ct
          else  trace ("error "++(show t) ++ " \n" ++ (show a') ++ "\n" ++ (show b') ++ "\n" ++ (show c) ++ "\n") ECTError
    in (EC t tt' [a', b', c'])
  | t `elem` ["Slice", "WriteWord"]
  = let a'   = inferExpr a
        at   = exprType a'
        b'   = inferExpr b
        bt   = exprType b'
        c'   = inferExpr c
        ct   = exprType c'
        tt'  = if at <= ECTWord 256
               && bt <= ECTWord 256
               && ct == ECTBuffer
          then ECTBuffer
          else trace ("error "++(show t) ++ " " ++ (show at) ++ " " ++ (show bt) ++ " " ++ (show ct)) ECTError
    in (EC t tt' [a', b', c'])
  | t `elem` ["WriteStorage"]
  = let a'   = inferExpr a
        at   = exprType a'
        b'   = inferExpr b
        bt   = exprType b'
        c'   = inferExpr c
        ct   = exprType c'
        tt'  = if at <= ECTWord 256
               && bt <= ECTWord 256
               && ct == ECTStorage
          then ECTStorage
          else trace ("error "++(show t) ++ " " ++ (show at) ++ " " ++ (show bt) ++ " " ++ (show ct)) ECTError
    in (EC t tt' [a', b', c'])
  | otherwise = trace ("error o abc "++(show t) ++ " " ++ (show a) ++ " " ++ (show b)) (EC t ECTError [a, b, c])

-- TODO
inferExpr (EC "Write" tt [a, b, c, d, e])
  = (EC "Write" ECTError [a, b, c, d, e])












-- (writeword x y (writeword z _ SEmpty))
mkIsKnownStorageSlot :: [(Text, StorageItem)] -> ExprC -> Bool
mkIsKnownStorageSlot
  is
  ( EC
      "WriteWord"
      _
      [ (ECLiteral x),
        (ECLiteral y),
        ( EC
            "WriteWord"
            _
            [ (ECLiteral z),
              _,
              (EC "SEmpty" _ [])
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


mkSimpStorage ::
  [(Text, StorageItem)] -> -- solidity storage info
  ExprC ->                 -- term
  ExprC                    -- found pointer
mkSimpStorage [] expr = expr
mkSimpStorage
  ((text, StorageItem _ _ slot) : xs)
  expr@( EC
      "WriteWord"
      _
      [ _,
        (ECLiteral y),
        ( EC
            "WriteWord"
            _
            [ _,
              word,
              (EC "SEmpty" _ [])
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

-- ABI - semantic information
-- propagate method information
simpExpr (EC "ITE" ta
  [ (EC "IsZero" _
    cs@[ EC "Eq" tb
      [ ECLiteral sigHash
      , EC "SHR" _
        [ EC "ReadWord" _
          [ ECLiteral 0
          , EC "Calldata" _ []]
        , ECLiteral 0xe0 ] ] ])
  , case_notfound
  , case_found ])
  -- todo construct new ?ctx with method
  = case Map.lookup (num sigHash) (view abimap ?ctx) of
  Nothing     -> (EC "ITE" ta (simpExpr <$> cs))
  Just method -> let
    signature = unpack $ _methodSignature method
    clean_sig = signature
    case_notfound_cont = simpExpr case_notfound
    in let
      -- todo how to do this with lenses
      ?ctx = set methodctx (Just method) ?ctx
    in (EC "ITE" ta [EC "Eq" ECTBool [ECVar "signature" (ECTWord 32), ECSig clean_sig sigHash], simpExpr case_found, case_notfound_cont ])

simpExpr (EC "ITE" ta cs@[EC "Eq" tb [ECVar "signature" (ECTWord 32), ECSig clean_sig sigHash], case_found, case_notfound ])
  = case Map.lookup (num sigHash) (view abimap ?ctx) of
    Nothing     -> (EC "ITE" ta (simpExpr <$> cs))
    Just method -> let
      case_notfound_cont = simpExpr case_notfound
      in let
      ?ctx = set methodctx (Just method) ?ctx
      in (EC "ITE" ta [EC "Eq" tb [ECVar "signature" (ECTWord 32), ECSig clean_sig sigHash], simpExpr case_found, case_notfound_cont ])

simpExpr e@(EC "ReadWord" ta
  [ ECLiteral offset
  , EC "Calldata" _ []]) = case (view methodctx ?ctx) of
  Nothing     -> e
  Just method -> let
    inputs = view methodInputs method
    index = num ((offset - 4) `div` 32)
    in if length inputs <= index then
      ECTodo ("sad" ++ (show index)) []
    else let
      pname = unpack $ fst (inputs !! index)
      name = if pname == "" then "param_" ++ (show index) else pname
    in ECVar name ta

-- semantic storage
simpExpr (EC "FromKeccak" ta [s])
  | (view isKnownStorageSlot ?ctx) s
  = (view simpStorage ?ctx) s
  | otherwise = EC "FromKeccak" ta ([simpExpr s])
simpExpr (EC "And" ta [(ECLiteral x), expr@(EC "And" tc [(ECLiteral x'), _])])
  | x == x'
  = simpExpr expr
  | otherwise
  = (EC "And" ta [(ECLiteral x), (simpExpr expr)])
simpExpr (EC "WriteWord" ta
  [ ECLiteral x
  , w
  , EC "WriteWord" tc
    [ ECLiteral y
    , u
    , s ]])
  | x < y
  = (EC "WriteWord" tc
    [ ECLiteral y
    , u
    , simpExpr $ EC "WriteWord" ta
      [ ECLiteral x
      , w
      , s ]])
  | x == y
  = (EC "WriteWord" tc
  [ ECLiteral x
  , w
  , simpExpr s ])
  | otherwise
  = (EC "WriteWord" ta
  [ ECLiteral x
  , w
  , simpExpr $ EC "WriteWord" tc
    [ ECLiteral y
    , u
    , s ]])
simpExpr (EC "Slice" ta
  [ ECLiteral from
  , ECLiteral size
  , EC "WriteWord" td
    [ ECLiteral x
    , w
    , s]])
  -- within
  | from <= x && x <= from + size - 0x20
  = EC "WriteWord" td
    [ ECLiteral (x - from)
    , w
    , EC "Slice" ta
      [ ECLiteral from
      , ECLiteral size
      , s ]]
  -- outside
  | otherwise
  = EC "Slice" ta
    [ ECLiteral from
    , ECLiteral size
    , s ]
-- fill with zeros
simpExpr (EC "Slice" ta [ECLiteral from, ECLiteral size, EC "SEmpty" td []])
  | size <= 0  = EC "SEmpty" ta []
  | otherwise  = EC "WriteWord" ta [
    ECLiteral from,
    ECLiteral 0,
    EC "Slice" ta [
      ECLiteral (from + 0x20),
      ECLiteral (size - 0x20),
      EC "SEmpty" ta []
      ]
    ]
simpExpr (EC "Add" ta [ECLiteral x, ECLiteral y])
  = ECLiteral (x + y)
-- TODO - what if a slice doesn't slice by word chunks but in between
-- TODO - buffer sort by write positions
--        and filter collisions
--
--  TODO - the 'other part' can also be a Bottom
-- simpExpr (EC "ITE" tt [c, a@(EC "Bottom" _ _), b])
--   = trace "s5" $ (EC "ITE" tt [ EC "IsZero" ECTBool [c], b, a ])
simpExpr (EC "ITE" _
  [ c
  , EC "ITE" tt
    [ c'
    , a'
    , (EC "Bottom" _ _)
    ]
  , b@(EC "Bottom" _ _)
  ])
  = (EC "ITE" tt
    [ EC "And" ECTBool [c, c']
    , a'
    , b
    ])
simpExpr (EC "IsZero" _ [EC "IsZero" _ [e@(EC "IsZero" _ _)]])
  = e

-- fold default cases
simpExpr (EC t tt cs)               = EC t  tt (simpExpr <$> cs)
simpExpr x                          = x
