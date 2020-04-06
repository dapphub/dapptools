{-# Language OverloadedStrings #-}
{-# Language QuasiQuotes #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language DataKinds #-}
{-# Language StandaloneDeriving #-}
import Data.Text (Text)
import Data.ByteString (ByteString)

import qualified Data.Text as Text
import Data.Maybe (fromMaybe, fromJust)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Hex

import Test.Tasty
import Test.Tasty.QuickCheck-- hiding (forAll)
import Test.Tasty.HUnit

import Control.Monad.State.Strict (execState, runState, MonadIO, void)
import Control.Lens hiding (List)

import qualified Data.Vector as Vector
import Data.String.Here
import qualified EVM.FeeSchedule as FeeSchedule

import Data.Binary.Put (runPut)
import Data.SBV hiding ((===), forAll_)
import Data.SBV.Control hiding ((===), isTheorem)
--import Data-SBV-Trans
import Data.Binary.Get (runGetOrFail)

import EVM
import EVM.Symbolic
import EVM.ABI
import EVM.Keccak
import EVM.Exec
import EVM.Patricia as Patricia
import EVM.Precompiled
import EVM.RLP
import EVM.Solidity
import EVM.Types

main :: IO ()
main = defaultMain $ testGroup "hevm"
  [ testGroup "ABI"
    [ testProperty "Put/get inverse" $ \x ->
        case runGetOrFail (getAbi (abiValueType x)) (runPut (putAbi x)) of
          Right ("", _, x') -> x' == x
          _ -> False
    ]
  , testGroup "Solidity expressions"
    [ testCase "Trivial" $
        SolidityCall "x = 3;" []
          ===> AbiUInt 256 3

    , testCase "Arithmetic" $ do
        SolidityCall "x = a + 1;"
          [AbiUInt 256 1] ===> AbiUInt 256 2
        SolidityCall "x = a - 1;"
          [AbiUInt 8 0] ===> AbiUInt 8 255

    , testCase "keccak256()" $
        SolidityCall "x = uint(keccak256(abi.encodePacked(a)));"
          [AbiString ""] ===> AbiUInt 256 0xc5d2460186f7233c927e7db2dcc703c0e500b653ca82273b7bfad8045d85a470
    ]

  , testGroup "Precompiled contracts"
      [ testGroup "Example (reverse)"
          [ testCase "success" $
              assertEqual "example contract reverses"
                (execute 0xdeadbeef "foobar" 6) (Just "raboof")
          , testCase "failure" $
              assertEqual "example contract fails on length mismatch"
                (execute 0xdeadbeef "foobar" 5) Nothing
          ]

      , testGroup "ECRECOVER"
          [ testCase "success" $ do
              let
                r = hex "c84e55cee2032ea541a32bf6749e10c8b9344c92061724c4e751600f886f4732"
                s = hex "1542b6457e91098682138856165381453b3d0acae2470286fd8c8a09914b1b5d"
                v = hex "000000000000000000000000000000000000000000000000000000000000001c"
                h = hex "513954cf30af6638cb8f626bd3f8c39183c26784ce826084d9d267868a18fb31"
                a = hex "0000000000000000000000002d5e56d45c63150d937f2182538a0f18510cb11f"
              assertEqual "successful recovery"
                (Just a)
                (execute 1 (h <> v <> r <> s) 32)
          , testCase "fail on made up values" $ do
              let
                r = hex "c84e55cee2032ea541a32bf6749e10c8b9344c92061724c4e751600f886f4731"
                s = hex "1542b6457e91098682138856165381453b3d0acae2470286fd8c8a09914b1b5d"
                v = hex "000000000000000000000000000000000000000000000000000000000000001c"
                h = hex "513954cf30af6638cb8f626bd3f8c39183c26784ce826084d9d267868a18fb31"
              assertEqual "fail because bit flip"
                Nothing
                (execute 1 (h <> v <> r <> s) 32)
          ]
      ]
  , testGroup "Byte/word manipulations"
    [ testProperty "padLeft length" $ \n (Bytes bs) ->
        BS.length (padLeft n bs) == max n (BS.length bs)
    , testProperty "padLeft identity" $ \(Bytes bs) ->
        padLeft (BS.length bs) bs == bs
    , testProperty "padRight length" $ \n (Bytes bs) ->
        BS.length (padLeft n bs) == max n (BS.length bs)
    , testProperty "padRight identity" $ \(Bytes bs) ->
        padLeft (BS.length bs) bs == bs
    , testProperty "padLeft zeroing" $ \(NonNegative n) (Bytes bs) ->
        let x = BS.take n (padLeft (BS.length bs + n) bs)
            y = BS.replicate n 0
        in x == y
    ]

  , testGroup "RLP encodings"
    [ testProperty "rlp decode is a retraction (bytes)" $ \(Bytes bs) ->
--      withMaxSuccess 100000 $
      rlpdecode (rlpencode (BS bs)) == Just (BS bs)
    , testProperty "rlp encode is a partial inverse (bytes)" $ \(Bytes bs) ->
--      withMaxSuccess 100000 $
        case rlpdecode bs of
          Just r -> rlpencode r == bs
          Nothing -> True
    ,  testProperty "rlp decode is a retraction (RLP)" $ \(RLPData r) ->
--       withMaxSuccess 100000 $
       rlpdecode (rlpencode r) == Just r
    ]
  , testGroup "Merkle Patricia Trie"
    [  testProperty "update followed by delete is id" $ \(Bytes r, Bytes s, Bytes t) ->
        whenFail
        (putStrLn ("r:" <> (show (ByteStringS r))) >>
         putStrLn ("s:" <> (show (ByteStringS s))) >>
         putStrLn ("t:" <> (show (ByteStringS t)))) $
--       withMaxSuccess 100000 $
       Patricia.insertValues [(r, BS.pack[1]), (s, BS.pack[2]), (t, BS.pack[3]),
                              (r, mempty), (s, mempty), (t, mempty)]
       === (Just $ Literal Patricia.Empty)
    ]

  , testGroup "Symbolic execution"
      [
     --  -- Currently hangs when checking postcondition...
     --  -- probably because z3 cannot handle the large bitvectors.
     --  -- might work better
     --  -- Somewhat tautological since we are asserting the precondition
     --  -- on the same form as the actual "requires" clause.
     --  testCase "SafeAdd success case" $ do
     --    Just safeAdd <- singleContract "SafeAdd"
     --      [i|
     --       function add(uint x, uint y) public pure returns (uint z) {
     --             require((z = x + y) >= x);
     --        }   
     --      |]
     --    let Just vm = loadVM safeAdd
     --        asWord :: [SWord 8] -> SWord 256
     --        asWord = fromBytes
     --        pre calldata = let (x, y) = splitAt 32 calldata
     --                       in asWord x .<= asWord x + asWord y
     --        post = Just $ \(input, output) -> case output of
     --          (VMSuccess out) -> 
     --            let (x, y) = splitAt 32 input
     --            in (asWord out) .== (asWord y) + (asWord y)
     --          _ -> sFalse
     --    nothing <- runSMT $ query $
     --      verify vm ("add(uint256,uint256)"
     --                       , AbiTupleType $ Vector.fromList
     --                         [AbiUIntType 256, AbiUIntType 256]) pre post
     --    assert $ nothing == Left ()
     -- ,

      testCase "x == y => x + y == 2 * y" $ do
        Just safeAdd <- singleContract "SafeAdd"
          [i|
            function add(uint x, uint y) public pure returns (uint z) {
                 require((z = x + y) >= x);
            }   
          |]
        let Just vm = loadVM safeAdd
            asWord :: [SWord 8] -> SWord 256
            asWord = fromBytes
            pre calldata = let (x, y) = splitAt 32 calldata
                           in (asWord x .<= asWord x + asWord y)
                              .&& (x .== y)
            post = Just $ \(input, output) -> case view result output of
                                                Just (VMSuccess out) ->
                                                  let (x, y) = splitAt 32 input
                                                  in asWord out .== 2 * asWord y
                                                _ -> sFalse
        nothing <- runSMT $ query $
          verify vm ("add(uint256,uint256)"
                           , AbiTupleType $ Vector.fromList
                             [AbiUIntType 256, AbiUIntType 256]) pre post
        assert $ nothing == Left ()
      ,

        testCase "factorize 973013" $ do
        Just factor <- singleContract "factorize"
          [i|
            function factor(uint x, uint y) public pure  {
                   require(1 < x && x < 973013 && 1 < y && y < 973013);
                   assert(x*y != 973013);
            }   
          |]
        let Just vm = loadVM factor
            asWord :: [SWord 8] -> SWord 256
            asWord = fromBytes
            pre = const sTrue
            post = Just $ \(input, output) -> case view result output of
              Just (EVM.VMFailure (EVM.UnrecognizedOpcode 254)) -> sFalse
              _ -> sTrue
        (Right (AbiTuple xy)) <- runSMT $ query $
          verify vm ("factor(uint256,uint256)"
                           , AbiTupleType $ Vector.fromList
                             [AbiUIntType 256, AbiUIntType 256]) pre post
        let (AbiUInt 256 x, AbiUInt 256 y) = (head (Vector.toList xy), head $ tail (Vector.toList xy))
        assert $ x == 953 && y == 1021 || x == 1021 && y == 953
    ]
  ]
  where
    (===>) = assertSolidityComputation

inUIntRange :: SInteger -> SBool
inUIntRange x = 0 .<= x .&& x .< 2 ^ 256 - 1 

runSimpleVM :: ByteString -> [SWord 8] -> Maybe [SWord 8]
runSimpleVM x ins = case loadVM x of
                      Nothing -> Nothing
                      Just vm -> 
                       case runState (assign (state.calldata) ins >> exec) vm of
                         (VMSuccess out, _) -> Just out
                         _ -> Nothing

hex :: ByteString -> ByteString
hex s =
  case Hex.decode s of
    (x, "") -> x
    _ -> error "internal error"

singleContract :: Text -> Text -> IO (Maybe ByteString)
singleContract x s =
  solidity x [i|
    contract ${x} { ${s} }
  |]

defaultDataLocation :: AbiType -> Text
defaultDataLocation t =
  if (case t of
        AbiBytesDynamicType -> True
        AbiStringType -> True
        AbiArrayDynamicType _ -> True
        AbiArrayType _ _ -> True
        _ -> False)
  then "memory"
  else ""

runFunction :: Text -> [SWord 8] -> IO (Maybe [SWord 8])
runFunction c input = do
  Just x <- singleContract "X" c
  return $ runSimpleVM x input

runStatements
  :: Text -> [AbiValue] -> AbiType
  -> IO (Maybe ByteString)
runStatements stmts args t = do
  let params =
        Text.intercalate ", "
          (map (\(x, c) -> abiTypeSolidity (abiValueType x)
                             <> " " <> defaultDataLocation (abiValueType x)
                             <> " " <> Text.pack [c])
            (zip args "abcdefg"))
      s =
        "foo(" <> Text.intercalate ","
                    (map (abiTypeSolidity . abiValueType) args) <> ")"

  output <- runFunction [i|
    function foo(${params}) public pure returns (${abiTypeSolidity t} x) {
      ${stmts}
    }
  |] (litBytes (abiCalldata s (Vector.fromList args)))
  return $ maybe Nothing maybeLitBytes output

newtype Bytes = Bytes ByteString
  deriving Eq

instance Show Bytes where
  showsPrec _ (Bytes x) _ = show (BS.unpack x)

instance Arbitrary Bytes where
  arbitrary = fmap (Bytes . BS.pack) arbitrary

newtype RLPData = RLPData RLP
  deriving (Eq, Show)

-- bias towards bytestring to try to avoid infinite recursion
instance Arbitrary RLPData where
  arbitrary = frequency
   [(5, do
           Bytes bytes <- arbitrary
           return $ RLPData $ BS bytes)
   , (1, do
         k <- choose (0,10)
         ls <- vectorOf k arbitrary
         return $ RLPData $ List [r | RLPData r <- ls])
   ]

data Invocation
  = SolidityCall Text [AbiValue]
  deriving Show

assertSolidityComputation :: Invocation -> AbiValue -> IO ()
assertSolidityComputation (SolidityCall s args) x =
  do y <- runStatements s args (abiValueType x)
     assertEqual (Text.unpack s)
       (fmap Bytes (Just (encodeAbiValue x)))
       (fmap Bytes y)
