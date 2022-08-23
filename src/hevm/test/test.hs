{-# Language OverloadedStrings #-}
{-# Language GADTs #-}
{-# Language ViewPatterns #-}
{-# Language ScopedTypeVariables #-}
{-# Language LambdaCase #-}
{-# Language QuasiQuotes #-}
{-# Language FlexibleInstances #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language DataKinds #-}
{-# Language StandaloneDeriving #-}

module Main where

import Data.Text (Text)
import Data.ByteString (ByteString)

import Prelude hiding (fail)

import Debug.Trace

import qualified Data.Text as Text
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BS (fromStrict)
import qualified Data.ByteString.Base16 as Hex
import Data.Maybe
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Test.Tasty.Runners

import Control.Monad.State.Strict (execState, runState)
import Control.Lens hiding (List, pre, (.>))

import qualified Data.Vector as Vector
import Data.String.Here

import Control.Monad.Fail

import Data.Binary.Put (runPut)
import qualified Data.Map as Map
import Data.Binary.Get (runGetOrFail)

import EVM hiding (Query)
import EVM.SymExec
import EVM.ABI
import EVM.Exec
import qualified EVM.Patricia as Patricia
import EVM.Precompiled
import EVM.RLP
import EVM.Solidity
import EVM.Types
import EVM.SMT
import qualified EVM.Expr as Expr
trace' msg x = trace (msg <> ": " <> show x) x


main :: IO ()
main = defaultMain tests

-- | run a subset of tests in the repl. p is a tasty pattern:
-- https://github.com/UnkindPartition/tasty/tree/ee6fe7136fbcc6312da51d7f1b396e1a2d16b98a#patterns
runSubSet :: String -> IO ()
runSubSet p = defaultMain . applyPattern p $ tests

tests :: TestTree
tests = testGroup "hevm"
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
        SolidityCall "unchecked { x = a - 1; }"
          [AbiUInt 8 0] ===> AbiUInt 8 255

    , testCase "keccak256()" $
        SolidityCall "x = uint(keccak256(abi.encodePacked(a)));"
          [AbiString ""] ===> AbiUInt 256 0xc5d2460186f7233c927e7db2dcc703c0e500b653ca82273b7bfad8045d85a470

    , testProperty "abi encoding vs. solidity" $ withMaxSuccess 20 $ forAll (arbitrary >>= genAbiValue) $
      \y -> ioProperty $ do
          -- traceM ("encoding: " ++ (show y) ++ " : " ++ show (abiValueType y))
          Just encoded <- runStatements [i| x = abi.encode(a);|]
            [y] AbiBytesDynamicType
          let AbiTuple (Vector.toList -> [solidityEncoded]) = decodeAbiValue (AbiTupleType $ Vector.fromList [AbiBytesDynamicType]) (BS.fromStrict encoded)
          let hevmEncoded = encodeAbiValue (AbiTuple $ Vector.fromList [y])
          -- traceM ("encoded (solidity): " ++ show solidityEncoded)
          -- traceM ("encoded (hevm): " ++ show (AbiBytesDynamic hevmEncoded))
          assertEqual "abi encoding mismatch" solidityEncoded (AbiBytesDynamic hevmEncoded)

    , testProperty "abi encoding vs. solidity (2 args)" $ withMaxSuccess 20 $ forAll (arbitrary >>= bothM genAbiValue) $
      \(x', y') -> ioProperty $ do
          -- traceM ("encoding: " ++ (show x') ++ ", " ++ (show y')  ++ " : " ++ show (abiValueType x') ++ ", " ++ show (abiValueType y'))
          Just encoded <- runStatements [i| x = abi.encode(a, b);|]
            [x', y'] AbiBytesDynamicType
          let AbiTuple (Vector.toList -> [solidityEncoded]) = decodeAbiValue (AbiTupleType $ Vector.fromList [AbiBytesDynamicType]) (BS.fromStrict encoded)
          let hevmEncoded = encodeAbiValue (AbiTuple $ Vector.fromList [x',y'])
          -- traceM ("encoded (solidity): " ++ show solidityEncoded)
          -- traceM ("encoded (hevm): " ++ show (AbiBytesDynamic hevmEncoded))
          assertEqual "abi encoding mismatch" solidityEncoded (AbiBytesDynamic hevmEncoded)
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

  , testGroup "Unresolved link detection"
    [ testCase "holes detected" $ do
        let code' = "608060405234801561001057600080fd5b5060405161040f38038061040f83398181016040528101906100329190610172565b73__$f3cbc3eb14e5bd0705af404abcf6f741ec$__63ab5c1ffe826040518263ffffffff1660e01b81526004016100699190610217565b60206040518083038186803b15801561008157600080fd5b505af4158015610095573d6000803e3d6000fd5b505050506040513d601f19601f820116820180604052508101906100b99190610145565b50506103c2565b60006100d36100ce84610271565b61024c565b9050828152602081018484840111156100ef576100ee610362565b5b6100fa8482856102ca565b509392505050565b600081519050610111816103ab565b92915050565b600082601f83011261012c5761012b61035d565b5b815161013c8482602086016100c0565b91505092915050565b60006020828403121561015b5761015a61036c565b5b600061016984828501610102565b91505092915050565b6000602082840312156101885761018761036c565b5b600082015167ffffffffffffffff8111156101a6576101a5610367565b5b6101b284828501610117565b91505092915050565b60006101c6826102a2565b6101d081856102ad565b93506101e08185602086016102ca565b6101e981610371565b840191505092915050565b60006102016003836102ad565b915061020c82610382565b602082019050919050565b6000604082019050818103600083015261023181846101bb565b90508181036020830152610244816101f4565b905092915050565b6000610256610267565b905061026282826102fd565b919050565b6000604051905090565b600067ffffffffffffffff82111561028c5761028b61032e565b5b61029582610371565b9050602081019050919050565b600081519050919050565b600082825260208201905092915050565b60008115159050919050565b60005b838110156102e85780820151818401526020810190506102cd565b838111156102f7576000848401525b50505050565b61030682610371565b810181811067ffffffffffffffff821117156103255761032461032e565b5b80604052505050565b7f4e487b7100000000000000000000000000000000000000000000000000000000600052604160045260246000fd5b600080fd5b600080fd5b600080fd5b600080fd5b6000601f19601f8301169050919050565b7f6261720000000000000000000000000000000000000000000000000000000000600082015250565b6103b4816102be565b81146103bf57600080fd5b50565b603f806103d06000396000f3fe6080604052600080fdfea26469706673582212207d03b26e43dc3d116b0021ddc9817bde3762a3b14315351f11fc4be384fd14a664736f6c63430008060033"
        assertBool "linker hole not detected" (containsLinkerHole code'),
      testCase "no false positives" $ do
        let code' = "0x608060405234801561001057600080fd5b50600436106100365760003560e01c806317bf8bac1461003b578063acffee6b1461005d575b600080fd5b610043610067565b604051808215151515815260200191505060405180910390f35b610065610073565b005b60008060015414905090565b6000809054906101000a900473ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1663f8a8fd6d6040518163ffffffff1660e01b815260040160206040518083038186803b1580156100da57600080fd5b505afa1580156100ee573d6000803e3d6000fd5b505050506040513d602081101561010457600080fd5b810190808051906020019092919050505060018190555056fea265627a7a723158205d775f914dcb471365a430b5f5b2cfe819e615cbbb5b2f1ccc7da1fd802e43c364736f6c634300050b0032"
        assertBool "false positive" (not . containsLinkerHole $ code')
    ]

  , testGroup "metadata stripper"
    [ testCase "it strips the metadata for solc => 0.6" $ do
        let code' = hexText "0x608060405234801561001057600080fd5b50600436106100365760003560e01c806317bf8bac1461003b578063acffee6b1461005d575b600080fd5b610043610067565b604051808215151515815260200191505060405180910390f35b610065610073565b005b60008060015414905090565b6000809054906101000a900473ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1663f8a8fd6d6040518163ffffffff1660e01b815260040160206040518083038186803b1580156100da57600080fd5b505afa1580156100ee573d6000803e3d6000fd5b505050506040513d602081101561010457600080fd5b810190808051906020019092919050505060018190555056fea265627a7a723158205d775f914dcb471365a430b5f5b2cfe819e615cbbb5b2f1ccc7da1fd802e43c364736f6c634300050b0032"
            stripped = stripBytecodeMetadata code'
        assertEqual "failed to strip metadata" (show (ByteStringS stripped)) "0x608060405234801561001057600080fd5b50600436106100365760003560e01c806317bf8bac1461003b578063acffee6b1461005d575b600080fd5b610043610067565b604051808215151515815260200191505060405180910390f35b610065610073565b005b60008060015414905090565b6000809054906101000a900473ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1663f8a8fd6d6040518163ffffffff1660e01b815260040160206040518083038186803b1580156100da57600080fd5b505afa1580156100ee573d6000803e3d6000fd5b505050506040513d602081101561010457600080fd5b810190808051906020019092919050505060018190555056fe"
    ,
      testCase "it strips the metadata and constructor args" $ do
        let srccode =
              [i|
                contract A {
                  uint y;
                  constructor(uint x) public {
                    y = x;
                  }
                }
                |]

        (json, path') <- solidity' srccode
        let Just (solc', _, _) = readJSON json
            initCode :: ByteString
            Just initCode = solc' ^? ix (path' <> ":A") . creationCode
        -- add constructor arguments
        assertEqual "constructor args screwed up metadata stripping" (stripBytecodeMetadata (initCode <> encodeAbiValue (AbiUInt 256 1))) (stripBytecodeMetadata initCode)
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
       === (Just $ Patricia.Literal Patricia.Empty)
    ]

  , testGroup "Symbolic execution"
      [
      -- Somewhat tautological since we are asserting the precondition
      -- on the same form as the actual "requires" clause.
      testCase "SafeAdd success case" $ do
        Just safeAdd <- solcRuntime "SafeAdd"
          [i|
          contract SafeAdd {
            function add(uint x, uint y) public pure returns (uint z) {
                 require((z = x + y) >= x);
            }
          }
          |]
        let pre preVM = let [x, y] = getStaticAbiArgs 2 preVM
                        in (x .<= Expr.add x y)
                           .&& view (state . callvalue) preVM .== Lit 0
            post prestate leaf =
              let [x, y] = getStaticAbiArgs 2 prestate
              in case leaf of
                   Return b _ -> (ReadWord (Lit 0) b) .== (Add x y)
                   _ -> PBool False
        res <- withSolvers Z3 1 $ \s -> verifyContract s safeAdd (Just ("add(uint256,uint256)", [AbiUIntType 256, AbiUIntType 256])) [] SymbolicS (Just pre) (Just post)
        traceShowM res
        --putStrLn $ "successfully explored: " <> show (Expr.numBranches res) <> " paths"
     ,

      testCase "x == y => x + y == 2 * y" $ do
        Just safeAdd <- solcRuntime "SafeAdd"
          [i|
          contract SafeAdd {
            function add(uint x, uint y) public pure returns (uint z) {
                 require((z = x + y) >= x);
            }
          }
          |]
        let pre preVM = let [x, y] = getStaticAbiArgs  2 preVM
                        in (x .<= Expr.add x y)
                           .&& (x .== y)
                           .&& view (state . callvalue) preVM .== Lit 0
            post prestate leaf =
              let [_, y] = getStaticAbiArgs 2 prestate
              in case leaf of
                   Return b _ -> (ReadWord (Lit 0) b) .== (Mul (Lit 2) y)
                   _ -> PBool False
        [Qed res] <- withSolvers Z3 1 $ \s ->
          verifyContract s safeAdd (Just ("add(uint256,uint256)", [AbiUIntType 256, AbiUIntType 256])) [] SymbolicS (Just pre) (Just post)
        putStrLn $ "successfully explored: " <> show (Expr.numBranches res) <> " paths"
      ,
      testCase "summary storage writes" $ do
        Just c <- solcRuntime "A"
          [i|
          contract A {
            uint x;
            function f(uint256 y) public {
               unchecked {
                 x += y;
                 x += y;
               }
            }
          }
          |]
        let pre vm = Lit 0 .== view (state . callvalue) vm
            post prestate leaf =
              let [y] = getStaticAbiArgs 2 prestate
                  this = Expr.litAddr $ view (state . codeContract) prestate
                  prex = Expr.readStorage' this (Lit 0) (view (env . storage) prestate)
              in case leaf of
                Return _ postStore -> Expr.mul (Expr.add (Lit 2) prex) y .== (Expr.readStorage' this (Lit 0) postStore)
                _ -> PBool False
        [Qed res] <- withSolvers Z3 1 $ \s -> verifyContract s c (Just ("f(uint256)", [AbiUIntType 256])) [] SymbolicS (Just pre) (Just post)
        putStrLn $ "successfully explored: " <> show (Expr.numBranches res) <> " paths"
        {-
        ,
        -- tests how whiffValue handles Neg via application of the triple IsZero simplification rule
        -- regression test for: https://github.com/dapphub/dapptools/pull/698
        testCase "Neg" $ do
            let src =
                  [i|
                    object "Neg" {
                      code {
                        // Deploy the contract
                        datacopy(0, dataoffset("runtime"), datasize("runtime"))
                        return(0, datasize("runtime"))
                      }
                      object "runtime" {
                        code {
                          let v := calldataload(4)
                          if iszero(iszero(and(v, not(0xffffffffffffffffffffffffffffffffffffffff)))) {
                            invalid()
                          }
                        }
                      }
                    }
                    |]
            Just c <- yulRuntime "Neg" src
            (Qed res, _) <- runSMTWith cvc4 $ query $ checkAssert defaultPanicCodes c (Just ("hello(address)", [AbiAddressType])) []
            putStrLn $ "successfully explored: " <> show (length res) <> " paths"
        ,

        -- Inspired by these `msg.sender == to` token bugs
        -- which break linearity of totalSupply.
        testCase "catch storage collisions" $ do
        Just c <- solcRuntime "A"
          [i|
          contract A {
            function f(uint x, uint y) public {
               assembly {
                 let newx := sub(sload(x), 1)
                 let newy := add(sload(y), 1)
                 sstore(x,newx)
                 sstore(y,newy)
               }
            }
          }
          |]
        let pre vm = 0 .== view (state . callvalue) vm
            post (prestate, poststate) =
              let [x,y] = getStaticAbiArgs prestate
                  this = view (state . codeContract) prestate
                  (Just preC, Just postC) = both' (view (env . contracts . at this)) (prestate, poststate)
                  --Just postC = view (env.contracts . at this) poststate
                  (Symbolic _ prestore, Symbolic _ poststore) = both' (view storage) (preC, postC)
                  (prex,  prey)  = both' (readArray prestore) (x, y)
                  (postx, posty) = both' (readArray poststore) (x, y)
              in case view result poststate of
                Just (VMSuccess _) -> prex + prey .== postx + (posty :: SWord 256)
                _ -> sFalse
        bs <- runSMT $ query $ do
          (Cex _, vm) <- verifyContract c (Just ("f(uint256,uint256)", [AbiUIntType 256, AbiUIntType 256])) [] SymbolicS pre (Just post)
          case view (state . calldata . _1) vm of
            SymbolicBuffer bs -> BS.pack <$> mapM (getValue.fromSized) bs
            ConcreteBuffer _ -> error "unexpected"

        let [AbiUInt 256 x, AbiUInt 256 y] = decodeAbiValues [AbiUIntType 256, AbiUIntType 256] bs
        assertEqual "Catch storage collisions" x y
        ,
        testCase "Deposit contract loop (z3)" $ do
          Just c <- solcRuntime "Deposit"
            [i|
            contract Deposit {
              function deposit(uint256 deposit_count) external pure {
                require(deposit_count < 2**32 - 1);
                ++deposit_count;
                bool found = false;
                for (uint height = 0; height < 32; height++) {
                  if ((deposit_count & 1) == 1) {
                    found = true;
                    break;
                  }
                 deposit_count = deposit_count >> 1;
                 }
                assert(found);
              }
             }
            |]
          (Qed res, _) <- runSMTWith z3 $ query $ checkAssert defaultPanicCodes c (Just ("deposit(uint256)", [AbiUIntType 256])) []
          putStrLn $ "successfully explored: " <> show (length res) <> " paths"
        ,
                testCase "Deposit contract loop (cvc4)" $ do
          Just c <- solcRuntime "Deposit"
            [i|
            contract Deposit {
              function deposit(uint256 deposit_count) external pure {
                require(deposit_count < 2**32 - 1);
                ++deposit_count;
                bool found = false;
                for (uint height = 0; height < 32; height++) {
                  if ((deposit_count & 1) == 1) {
                    found = true;
                    break;
                  }
                 deposit_count = deposit_count >> 1;
                 }
                assert(found);
              }
             }
            |]
          (Qed res, _) <- runSMTWith cvc4 $ query $ checkAssert defaultPanicCodes c (Just ("deposit(uint256)", [AbiUIntType 256])) []
          putStrLn $ "successfully explored: " <> show (length res) <> " paths"
        ,
        testCase "Deposit contract loop (error version)" $ do
          Just c <- solcRuntime "Deposit"
            [i|
            contract Deposit {
              function deposit(uint8 deposit_count) external pure {
                require(deposit_count < 2**32 - 1);
                ++deposit_count;
                bool found = false;
                for (uint height = 0; height < 32; height++) {
                  if ((deposit_count & 1) == 1) {
                    found = true;
                    break;
                  }
                 deposit_count = deposit_count >> 1;
                 }
                assert(found);
              }
             }
            |]
          bs <- runSMT $ query $ do
            (Cex _, vm) <- checkAssert allPanicCodes c (Just ("deposit(uint8)", [AbiUIntType 8])) []
            case view (state . calldata . _1) vm of
              SymbolicBuffer bs -> BS.pack <$> mapM (getValue.fromSized) bs
              ConcreteBuffer _ -> error "unexpected"

          let [deposit] = decodeAbiValues [AbiUIntType 8] bs
          assertEqual "overflowing uint8" deposit (AbiUInt 8 255)
     ,
        testCase "explore function dispatch" $ do
        Just c <- solcRuntime "A"
          [i|
          contract A {
            function f(uint x) public pure returns (uint) {
              return x;
            }
          }
          |]
        (Qed res, _) <- runSMTWith z3 $ do
          setTimeOut 5000
          query $ checkAssert defaultPanicCodes c Nothing []
        putStrLn $ "successfully explored: " <> show (length res) <> " paths"
        ,

        testCase "injectivity of keccak (32 bytes)" $ do
          Just c <- solcRuntime "A"
            [i|
            contract A {
              function f(uint x, uint y) public pure {
                if (keccak256(abi.encodePacked(x)) == keccak256(abi.encodePacked(y))) assert(x == y);
              }
            }
            |]
          (Qed res, _) <- runSMTWith cvc4 $ query $ checkAssert defaultPanicCodes c (Just ("f(uint256,uint256)", [AbiUIntType 256, AbiUIntType 256])) []
          putStrLn $ "successfully explored: " <> show (length res) <> " paths"
        ,
        testCase "injectivity of keccak (32 bytes)" $ do
          Just c <- solcRuntime "A"
            [i|
            contract A {
              function f(uint x, uint y) public pure {
                if (keccak256(abi.encodePacked(x)) == keccak256(abi.encodePacked(y))) assert(x == y);
              }
            }
            |]
          (Qed res, _) <- runSMTWith z3 $ query $ checkAssert defaultPanicCodes c (Just ("f(uint256,uint256)", [AbiUIntType 256, AbiUIntType 256])) []
          putStrLn $ "successfully explored: " <> show (length res) <> " paths"
       ,

        testCase "injectivity of keccak (64 bytes)" $ do
          Just c <- solcRuntime "A"
            [i|
            contract A {
              function f(uint x, uint y, uint w, uint z) public pure {
                assert (keccak256(abi.encodePacked(x,y)) != keccak256(abi.encodePacked(w,z)));
              }
            }
            |]
          bs <- runSMTWith z3 $ query $ do
            (Cex _, vm) <- checkAssert defaultPanicCodes c (Just ("f(uint256,uint256,uint256,uint256)", replicate 4 (AbiUIntType 256))) []
            case view (state . calldata . _1) vm of
              SymbolicBuffer bs -> BS.pack <$> mapM (getValue.fromSized) bs
              ConcreteBuffer _ -> error "unexpected"

          let [AbiUInt 256 x,
               AbiUInt 256 y,
               AbiUInt 256 w,
               AbiUInt 256 z] = decodeAbiValues [AbiUIntType 256,
                                                 AbiUIntType 256,
                                                 AbiUIntType 256,
                                                 AbiUIntType 256] bs
          assertEqual "x == w" x w
          assertEqual "y == z" y z
       ,

        testCase "calldata beyond calldatasize is 0 (z3)" $ do
          Just c <- solcRuntime "A"
            [i|
            contract A {
              function f() public pure {
                uint y;
                assembly {
                  let x := calldatasize()
                  y := calldataload(x)
                }
                assert(y == 0);
              }
            }
            |]
          Qed res <- runSMTWith z3 $ do
            setTimeOut 5000
            query $ fst <$> checkAssert defaultPanicCodes c Nothing []
          putStrLn $ "successfully explored: " <> show (length res) <> " paths"

       ,

        testCase "keccak soundness" $ do
          Just c <- solcRuntime "C"
            [i|
              contract C {
                mapping (uint => mapping (uint => uint)) maps;

                  function f(uint x, uint y) public view {
                  assert(maps[y][0] == maps[x][0]);
                }
              }
            |]
          -- should find a counterexample
          Cex _ <- runSMTWith cvc4 $ query $ fst <$> checkAssert defaultPanicCodes c (Just ("f(uint256,uint256)", [AbiUIntType 256, AbiUIntType 256])) []
          putStrLn "found counterexample:"


      ,
         testCase "multiple contracts" $ do
          let code' =
                [i|
                  contract C {
                    uint x;
                    A constant a = A(0x35D1b3F3D7966A1DFe207aa4514C12a259A0492B);

                    function call_A() public view {
                      // should fail since a.x() can be anything
                      assert(a.x() == x);
                    }
                  }
                  contract A {
                    uint public x;
                  }
                |]
              aAddr = Addr 0x35D1b3F3D7966A1DFe207aa4514C12a259A0492B
          Just c <- solcRuntime "C" code'
          Just a <- solcRuntime "A" code'
          Cex _ <- runSMT $ query $ do
            vm0 <- abstractVM (Just ("call_A()", [])) [] c SymbolicS
            store <- freshArray (show aAddr) Nothing
            let vm = vm0
                  & set (state . callvalue) 0
                  & over (env . contracts)
                       (Map.insert aAddr (initialContract (RuntimeCode $ ConcreteBuffer a) &
                                           set EVM.storage (EVM.Symbolic [] store)))
            verify vm Nothing Nothing Nothing (Just $ checkAssertions defaultPanicCodes)
          putStrLn "found counterexample:"
      ,
         testCase "calling unique contracts (read from storage)" $ do
          let code' =
                [i|
                  contract C {
                    uint x;
                    A a;

                    function call_A() public {
                      a = new A();
                      // should fail since x can be anything
                      assert(a.x() == x);
                    }
                  }
                  contract A {
                    uint public x;
                  }
                |]
          Just c <- solcRuntime "C" code'
          Cex _ <- runSMT $ query $ do
            vm0 <- abstractVM (Just ("call_A()", [])) [] c SymbolicS
            let vm = vm0 & set (state . callvalue) 0
            verify vm Nothing Nothing Nothing (Just $ checkAssertions defaultPanicCodes)
          putStrLn "found counterexample:"
      ,

         testCase "keccak concrete and sym agree" $ do
          let code' =
                [i|
                  contract C {
                    function kecc(uint x) public pure {
                      if (x == 0) {
                         assert(keccak256(abi.encode(x)) == keccak256(abi.encode(0)));
                      }
                    }
                  }
                |]
          Just c <- solcRuntime "C" code'
          Qed _ <- runSMT $ query $ do
            vm0 <- abstractVM (Just ("kecc(uint256)", [AbiUIntType 256])) [] c SymbolicS
            let vm = vm0 & set (state . callvalue) 0
            verify vm Nothing Nothing Nothing (Just $ checkAssertions defaultPanicCodes)
          putStrLn "found counterexample:"

      , testCase "safemath distributivity (yul)" $ do
          Qed _ <- runSMTWith cvc4 $ query $ do
            let yulsafeDistributivity = hex "6355a79a6260003560e01c14156016576015601f565b5b60006000fd60a1565b603d602d604435600435607c565b6039602435600435607c565b605d565b6052604b604435602435605d565b600435607c565b141515605a57fe5b5b565b6000828201821115151560705760006000fd5b82820190505b92915050565b6000818384048302146000841417151560955760006000fd5b82820290505b92915050565b"
            vm <- abstractVM (Just ("distributivity(uint256,uint256,uint256)", [AbiUIntType 256, AbiUIntType 256, AbiUIntType 256])) [] yulsafeDistributivity SymbolicS
            verify vm Nothing Nothing Nothing (Just $ checkAssertions defaultPanicCodes)
          putStrLn "Proven"

      , testCase "safemath distributivity (sol)" $ do
          let code' =
                [i|
                  contract C {
                      function distributivity(uint x, uint y, uint z) public {
                          assert(mul(x, add(y, z)) == add(mul(x, y), mul(x, z)));
                      }

                      function add(uint x, uint y) internal pure returns (uint z) {
                          unchecked {
                            require((z = x + y) >= x, "ds-math-add-overflow");
                          }
                      }
                      function mul(uint x, uint y) internal pure returns (uint z) {
                          unchecked {
                            require(y == 0 || (z = x * y) / y == x, "ds-math-mul-overflow");
                          }
                      }
                 }
                |]
          Just c <- solcRuntime "C" code'

          Qed _ <- runSMTWith cvc4 $ query $ do
            vm <- abstractVM (Just ("distributivity(uint256,uint256,uint256)", [AbiUIntType 256, AbiUIntType 256, AbiUIntType 256])) [] c SymbolicS
            verify vm Nothing Nothing Nothing (Just $ checkAssertions defaultPanicCodes)
          putStrLn "Proven"
    ]
  , testGroup "Equivalence checking"
    [
      testCase "yul optimized" $ do
        -- These yul programs are not equivalent: (try --calldata $(seth --to-uint256 2) for example)
        Just aPrgm <- yul ""
          [i|
          {
              calldatacopy(0, 0, 32)
              switch mload(0)
              case 0 { }
              case 1 { }
              default { invalid() }
          }
          |]
        Just bPrgm <- yul ""
          [i|
          {
              calldatacopy(0, 0, 32)
              switch mload(0)
              case 0 { }
              case 2 { }
              default { invalid() }
          }
          |]
        runSMTWith z3 $ query $ do
          Cex _ <- equivalenceCheck aPrgm bPrgm Nothing Nothing Nothing
          return ()
          -}

    ]
  ]
  where
    (===>) = assertSolidityComputation


runSimpleVM :: ByteString -> ByteString -> Maybe ByteString
runSimpleVM x ins = case loadVM x of
                      Nothing -> Nothing
                      Just vm -> let calldata' = (ConcreteBuf ins)
                       in case runState (assign (state.calldata) calldata' >> exec) vm of
                            (VMSuccess (ConcreteBuf bs), _) -> Just bs
                            a -> trace (show a) Nothing

loadVM :: ByteString -> Maybe VM
loadVM x =
    case runState exec (vmForEthrunCreation x) of
       (VMSuccess (ConcreteBuf targetCode), vm1) -> do
         let target = view (state . contract) vm1
             vm2 = execState (replaceCodeOfSelf (RuntimeCode (fromJust $ Expr.toList $ ConcreteBuf targetCode))) vm1
         return $ snd $ flip runState vm2
                (do resetState
                    assign (state . gas) 0xffffffffffffffff -- kludge
                    loadContract target)
       _ -> Nothing

hex :: ByteString -> ByteString
hex s =
  case Hex.decode s of
    Right x -> x
    Left e -> error e

singleContract :: Text -> Text -> IO (Maybe ByteString)
singleContract x s =
  solidity x [i|
    pragma experimental ABIEncoderV2;
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

runFunction :: Text -> ByteString -> IO (Maybe ByteString)
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

  runFunction [i|
    function foo(${params}) public pure returns (${abiTypeSolidity t} ${defaultDataLocation t} x) {
      ${stmts}
    }
  |] (abiMethod s (AbiTuple $ Vector.fromList args))

getStaticAbiArgs :: Int -> VM -> [Expr EWord]
getStaticAbiArgs n vm =
  let cd = view (state . calldata) vm
      bs = case cd of
        ConcreteBuf bs' -> ConcreteBuf $ BS.drop 4 bs'
        b -> Expr.drop 4 b
  in decodeStaticArgs n bs

-- includes shaving off 4 byte function sig
decodeAbiValues :: [AbiType] -> ByteString -> [AbiValue]
decodeAbiValues types bs =
  let AbiTuple xy = decodeAbiValue (AbiTupleType $ Vector.fromList types) (BS.fromStrict (BS.drop 4 bs))
  in Vector.toList xy

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

bothM :: (Monad m) => (a -> m b) -> (a, a) -> m (b, b)
bothM f (a, a') = do
  b  <- f a
  b' <- f a'
  return (b, b')

applyPattern :: String -> TestTree  -> TestTree
applyPattern p = localOption (TestPattern (parseExpr p))
