{-# Language OverloadedStrings #-}
{-# Language LambdaCase #-}
{-# Language QuasiQuotes #-}
{-# Language TypeSynonymInstances #-}
{-# Language FlexibleInstances #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language DataKinds #-}
{-# Language StandaloneDeriving #-}
import Data.Text (Text)
import Data.ByteString (ByteString)

import Prelude hiding (fail)

import qualified Data.Text as Text
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BS (fromStrict)
import qualified Data.ByteString.Base16 as Hex
import Test.Tasty
import Test.Tasty.QuickCheck-- hiding (forAll)
import Test.Tasty.HUnit

import Control.Monad.State.Strict (execState, runState, when)
import Control.Lens hiding (List, pre)

import qualified Data.Vector as Vector
import Data.String.Here

import Control.Monad.Fail
import Debug.Trace

import Data.Binary.Put (runPut)
import Data.SBV hiding ((===), forAll, sList)
import Data.SBV.Control
import Data.SBV.Trans (sList)
import Data.SBV.List (implode)
import qualified Data.SBV.List as SL
import qualified Data.Map as Map
import Data.Binary.Get (runGetOrFail)

import EVM hiding (Query)
import EVM.SymExec
import EVM.Symbolic
import EVM.Concrete (w256)
import EVM.ABI
import EVM.Exec
import EVM.Patricia as Patricia
import EVM.Precompiled
import EVM.RLP
import EVM.Solidity
import EVM.Types

instance MonadFail Query where
    fail = io . fail

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

  , testGroup "metadata stripper"
    [ testCase "it strips the metadata for solc => 0.6" $ do
        let code = hexText "0x608060405234801561001057600080fd5b50600436106100365760003560e01c806317bf8bac1461003b578063acffee6b1461005d575b600080fd5b610043610067565b604051808215151515815260200191505060405180910390f35b610065610073565b005b60008060015414905090565b6000809054906101000a900473ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1663f8a8fd6d6040518163ffffffff1660e01b815260040160206040518083038186803b1580156100da57600080fd5b505afa1580156100ee573d6000803e3d6000fd5b505050506040513d602081101561010457600080fd5b810190808051906020019092919050505060018190555056fea265627a7a723158205d775f914dcb471365a430b5f5b2cfe819e615cbbb5b2f1ccc7da1fd802e43c364736f6c634300050b0032"
            stripped = stripBytecodeMetadata code
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

        (json, path) <- solidity' srccode
        let Just (solc, _, _) = readJSON json
            initCode :: ByteString
            Just initCode = solc ^? ix (path <> ":A") . creationCode
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
       === (Just $ Literal Patricia.Empty)
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
        let pre preVM = let [x, y] = getStaticAbiArgs preVM
                        in x .<= x + y
                           .&& view (state . callvalue) preVM .== 0
            post = Just $ \(prestate, poststate) ->
              let [x, y] = getStaticAbiArgs prestate
              in case view result poststate of
                Just (VMSuccess (SymbolicBuffer out)) -> (fromBytes out) .== x + y
                _ -> sFalse
        (Left res, _) <- runSMT $ query $ verifyContract safeAdd (Just ("add(uint256,uint256)", [AbiUIntType 256, AbiUIntType 256])) [] SymbolicS pre post
        putStrLn $ "successfully explored: " <> show (length res) <> " paths"
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
        let pre preVM = let [x, y] = getStaticAbiArgs preVM
                        in (x .<= x + y)
                           .&& (x .== y)
                           .&& view (state . callvalue) preVM .== 0
            post (prestate, poststate) =
              let [_, y] = getStaticAbiArgs prestate
              in case view result poststate of
                      Just (VMSuccess (SymbolicBuffer out)) -> fromBytes out .== 2 * y
                      _ -> sFalse
        (Left res, _) <- runSMTWith z3 $ query $
          verifyContract safeAdd (Just ("add(uint256,uint256)", [AbiUIntType 256, AbiUIntType 256])) [] SymbolicS pre (Just post)
        putStrLn $ "successfully explored: " <> show (length res) <> " paths"
      ,
        testCase "factorize 973013" $ do
        Just factor <- solcRuntime "PrimalityCheck"
          [i|
          contract PrimalityCheck {
            function factor(uint x, uint y) public pure  {
                   require(1 < x && x < 973013 && 1 < y && y < 973013);
                   assert(x*y != 973013);
            }
          }
          |]
        bs <- runSMTWith cvc4 $ query $ do
          (Right _, vm) <- checkAssert factor (Just ("factor(uint256,uint256)", [AbiUIntType 256, AbiUIntType 256])) []
          case view (state . calldata . _1) vm of
            SymbolicBuffer bs -> BS.pack <$> mapM (getValue.fromSized) bs
            ConcreteBuffer _ -> error "unexpected"

        let [AbiUInt 256 x, AbiUInt 256 y] = decodeAbiValues [AbiUIntType 256, AbiUIntType 256] bs
        assertEqual "" True (x == 953 && y == 1021 || x == 1021 && y == 953)
        ,

        testCase "summary storage writes" $ do
        Just c <- solcRuntime "A"
          [i|
          contract A {
            uint x;
            function f(uint256 y) public {
               x += y;
               x += y;
            }
          }
          |]
        let pre vm = 0 .== view (state . callvalue) vm
            post = Just $ \(prestate, poststate) ->
              let [y] = getStaticAbiArgs prestate
                  this = view (state . codeContract) prestate
                  Just preC = view (env.contracts . at this) prestate
                  Just postC = view (env.contracts . at this) poststate
                  Symbolic prestore = _storage preC
                  Symbolic poststore = _storage postC
                  prex = readArray prestore 0
                  postx = readArray poststore 0
              in case view result poststate of
                Just (VMSuccess _) -> prex + 2 * y .== postx
                _ -> sFalse
        (Left res, _) <- runSMT $ query $ verifyContract c (Just ("f(uint256)", [AbiUIntType 256])) [] SymbolicS pre post
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
                  (Just preC, Just postC) = both' (view (env.contracts . at this)) (prestate, poststate)
                  --Just postC = view (env.contracts . at this) poststate
                  (Symbolic prestore, Symbolic poststore) = both' (view storage) (preC, postC)
                  (prex,  prey)  = both' (readArray prestore) (x, y)
                  (postx, posty) = both' (readArray poststore) (x, y)
              in case view result poststate of
                Just (VMSuccess _) -> prex + prey .== postx + (posty :: SWord 256)
                _ -> sFalse
        bs <- runSMT $ query $ do
          (Right _, vm) <- verifyContract c (Just ("f(uint256,uint256)", [AbiUIntType 256, AbiUIntType 256])) [] SymbolicS pre (Just post)
          case view (state . calldata . _1) vm of
            SymbolicBuffer bs -> BS.pack <$> mapM (getValue.fromSized) bs
            ConcreteBuffer bs -> error "unexpected"

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
          (Left res, _) <- runSMTWith z3 $ query $ checkAssert c (Just ("deposit(uint256)", [AbiUIntType 256])) []
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
          (Left res, _) <- runSMTWith cvc4 $ query $ checkAssert c (Just ("deposit(uint256)", [AbiUIntType 256])) []
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
            (Right _, vm) <- checkAssert c (Just ("deposit(uint8)", [AbiUIntType 8])) []
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
        (Left res, _) <- runSMTWith z3 $ do
          setTimeOut 5000
          query $ checkAssert c Nothing []
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
          (Left res, _) <- runSMTWith cvc4 $ query $ checkAssert c (Just ("f(uint256,uint256)", [AbiUIntType 256, AbiUIntType 256])) []
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
          (Left res, _) <- runSMTWith z3 $ query $ checkAssert c (Just ("f(uint256,uint256)", [AbiUIntType 256, AbiUIntType 256])) []
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
            (Right _, vm) <- checkAssert c (Just ("f(uint256,uint256,uint256,uint256)", replicate 4 (AbiUIntType 256))) []
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
          Left res <- runSMTWith z3 $ do
            setTimeOut 5000
            query $ fst <$> checkAssert c Nothing []
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
          Right _ <- runSMTWith cvc4 $ query $ fst <$> checkAssert c (Just ("f(uint256,uint256)", [AbiUIntType 256, AbiUIntType 256])) []
          putStrLn $ "found counterexample:"


      ,
         testCase "multiple contracts" $ do
          let code =
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
          Just c <- solcRuntime "C" code
          Just a <- solcRuntime "A" code
          Right cex <- runSMT $ query $ do
            vm0 <- abstractVM (Just ("call_A()", [])) [] c SymbolicS
            store <- freshArray (show aAddr) Nothing
            let vm = vm0
                  & set (state . callvalue) 0
                  & over (env . contracts)
                       (Map.insert aAddr (initialContract (RuntimeCode a) &
                                           set EVM.storage (Symbolic store)))
            verify vm Nothing Nothing (Just checkAssertions)
          putStrLn $ "found counterexample:"
      ,
         testCase "calling unique contracts (read from storage)" $ do
          let code =
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
          Just c <- solcRuntime "C" code
          Right cex <- runSMT $ query $ do
            vm0 <- abstractVM (Just ("call_A()", [])) [] c SymbolicS
            let vm = vm0 & set (state . callvalue) 0
            verify vm Nothing Nothing (Just checkAssertions)
          putStrLn $ "found counterexample:"
      ,

         testCase "keccak concrete and sym agree" $ do
          let code =
                [i|
                  contract C {
                    function kecc(uint x) public {
                      if (x == 0) {
                         assert(keccak256(abi.encode(x)) == keccak256(abi.encode(0)));
                      }
                    }
                  }
                |]
          Just c <- solcRuntime "C" code
          Left _ <- runSMT $ query $ do
            vm0 <- abstractVM (Just ("kecc(uint256)", [AbiUIntType 256])) [] c SymbolicS
            let vm = vm0 & set (state . callvalue) 0
            verify vm Nothing Nothing (Just checkAssertions)
          putStrLn $ "found counterexample:"

    ]
  , testGroup "Equivalence checking"
    [
      testCase "yul optimized" $ do
        -- These yul programs are not equivalent: (try --calldata $(seth --to-uint256 2) for example)
        --  A:                               B:
        --  {                                {
        --     calldatacopy(0, 0, 32)           calldatacopy(0, 0, 32)
        --     switch mload(0)                  switch mload(0)
        --     case 0 { }                       case 0 { }
        --     case 1 { }                       case 2 { }
        --     default { invalid() }            default { invalid() }
        -- }                                 }
        let aPrgm = hex "602060006000376000805160008114601d5760018114602457fe6029565b8191506029565b600191505b50600160015250"
            bPrgm = hex "6020600060003760005160008114601c5760028114602057fe6021565b6021565b5b506001600152"
        runSMTWith z3 $ query $ do
          Right counterexample <- equivalenceCheck aPrgm bPrgm Nothing Nothing
          return ()

    ]
  ]
  where
    (===>) = assertSolidityComputation


runSimpleVM :: ByteString -> ByteString -> Maybe ByteString
runSimpleVM x ins = case loadVM x of
                      Nothing -> Nothing
                      Just vm -> let calldata' = (ConcreteBuffer ins, literal . num $ BS.length ins)
                       in case runState (assign (state.calldata) calldata' >> exec) vm of
                            (VMSuccess (ConcreteBuffer bs), _) -> Just bs
                            _ -> Nothing

loadVM :: ByteString -> Maybe VM
loadVM x =
    case runState exec (vmForEthrunCreation x) of
       (VMSuccess (ConcreteBuffer targetCode), vm1) -> do
         let target = view (state . contract) vm1
             vm2 = execState (replaceCodeOfSelf (RuntimeCode targetCode)) vm1
         return $ snd $ flip runState vm2
                (do resetState
                    assign (state . gas) 0xffffffffffffffff -- kludge
                    loadContract target)
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
    function foo(${params}) public pure returns (${abiTypeSolidity t} x) {
      ${stmts}
    }
  |] (abiCalldata s (Vector.fromList args))

getStaticAbiArgs :: VM -> [SWord 256]
getStaticAbiArgs vm =
  let SymbolicBuffer bs = ditch 4 $ view (state . calldata . _1) vm
  in fmap (\i -> fromBytes $ take 32 (drop (i*32) bs)) [0..((length bs) `div` 32 - 1)]

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
