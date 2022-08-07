{-# Language DataKinds #-}
{-# Language QuasiQuotes #-}

{- |
Module: EVM.Dev
Description: Helpers for repl driven hevm hacking
-}
module EVM.Dev where

import Data.ByteString hiding (putStrLn, writeFile, zip)
import Control.Monad.State.Strict hiding (state)
import Data.Maybe (fromJust)

import Data.String.Here
import qualified Data.Text as T

import EVM
import EVM.SMT (withSolvers, Solver(..), formatSMT2)
import EVM.Types
import EVM.SymExec
import EVM.Solidity
import EVM.Format (formatExpr)
import qualified EVM.Fetch as Fetch
import qualified EVM.FeeSchedule as FeeSchedule

dumpQueries :: IO ()
dumpQueries = do
  d <- dai
  e <- buildExpr d
  withSolvers Z3 4 $ \s -> do
    qs <- reachableQueries s e
    forM_ (zip ([1..] :: [Int]) qs) $ \(idx, q) -> do
      writeFile ("query_" <> show idx <> ".smt2") (T.unpack $ T.append (formatSMT2 q) "(check-sat)")

doTest :: IO ()
doTest = do
  c <- testContract
  reachable' False c
  --e <- simplify <$> buildExpr c
  --Prelude.putStrLn (formatExpr e)

analyzeDai :: IO ()
analyzeDai = do
  d <- dai
  reachable' False d

analyzeVat :: IO ()
analyzeVat = do
  v <- vat
  reachable' False v

reachable' :: Bool -> ByteString -> IO ()
reachable' smtdebug c = do
  full <- simplify <$> buildExpr c
  putStrLn "Explored contract"
  --putStrLn $ formatExpr full
  writeFile "full.ast" $ formatExpr full
  putStrLn "Dumped to full.ast"
  withSolvers Z3 4 $ \solvers -> do
    putStrLn "Checking reachability"
    (qs, less) <- reachable solvers full
    putStrLn "Checked reachability"
    writeFile "reachable.ast" $ formatExpr less
    putStrLn "Dumped to reachable.ast"
    --putStrLn $ formatExpr less
    when smtdebug $ do
      putStrLn "\n\nQueries\n\n"
      forM_ qs $ \q -> do
        putStrLn "\n\n-- Query --"
        putStrLn $ T.unpack $ formatSMT2 q


testContract :: IO ByteString
testContract = do
  let src =
        [i|
          contract C {
            uint x;
            function set(uint v) public {
              x = v;
            }
          }
          |]
  fmap fromJust (solcRuntime "C" src)

vat :: IO ByteString
vat = do
  let src =
        [i|
          /// vat.sol -- Dai CDP database

          // Copyright (C) 2018 Rain <rainbreak@riseup.net>
          //
          // This program is free software: you can redistribute it and/or modify
          // it under the terms of the GNU Affero General Public License as published by
          // the Free Software Foundation, either version 3 of the License, or
          // (at your option) any later version.
          //
          // This program is distributed in the hope that it will be useful,
          // but WITHOUT ANY WARRANTY; without even the implied warranty of
          // MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
          // GNU Affero General Public License for more details.
          //
          // You should have received a copy of the GNU Affero General Public License
          // along with this program.  If not, see <https://www.gnu.org/licenses/>.

          // FIXME: This contract was altered compared to the production version.
          // It doesn't use LibNote anymore.
          // New deployments of this contract will need to include custom events (TO DO).

          contract Vat {
              // --- Auth ---
              mapping (address => uint) public wards;
              function rely(address usr) external auth { require(live == 1, "Vat/not-live"); wards[usr] = 1; }
              function deny(address usr) external auth { require(live == 1, "Vat/not-live"); wards[usr] = 0; }
              modifier auth {
                  require(wards[msg.sender] == 1, "Vat/not-authorized");
                  _;
              }

              mapping(address => mapping (address => uint)) public can;
              function hope(address usr) external { can[msg.sender][usr] = 1; }
              function nope(address usr) external { can[msg.sender][usr] = 0; }
              function wish(address bit, address usr) internal view returns (bool) {
                  return either(bit == usr, can[bit][usr] == 1);
              }

              // --- Data ---
              struct Ilk {
                  uint256 Art;   // Total Normalised Debt     [wad]
                  uint256 rate;  // Accumulated Rates         [ray]
                  uint256 spot;  // Price with Safety Margin  [ray]
                  uint256 line;  // Debt Ceiling              [rad]
                  uint256 dust;  // Urn Debt Floor            [rad]
              }
              struct Urn {
                  uint256 ink;   // Locked Collateral  [wad]
                  uint256 art;   // Normalised Debt    [wad]
              }

              mapping (bytes32 => Ilk)                       public ilks;
              mapping (bytes32 => mapping (address => Urn )) public urns;
              mapping (bytes32 => mapping (address => uint)) public gem;  // [wad]
              mapping (address => uint256)                   public dai;  // [rad]
              mapping (address => uint256)                   public sin;  // [rad]

              uint256 public debt;  // Total Dai Issued    [rad]
              uint256 public vice;  // Total Unbacked Dai  [rad]
              uint256 public Line;  // Total Debt Ceiling  [rad]
              uint256 public live;  // Active Flag

              // --- Init ---
              constructor() public {
                  wards[msg.sender] = 1;
                  live = 1;
              }

              // --- Math ---
              function _add(uint x, int y) internal pure returns (uint z) {
                  z = x + uint(y);
                  require(y >= 0 || z <= x);
                  require(y <= 0 || z >= x);
              }
              function _sub(uint x, int y) internal pure returns (uint z) {
                  z = x - uint(y);
                  require(y <= 0 || z <= x);
                  require(y >= 0 || z >= x);
              }
              function _mul(uint x, int y) internal pure returns (int z) {
                  z = int(x) * y;
                  require(int(x) >= 0);
                  require(y == 0 || z / y == int(x));
              }
              function _add(uint x, uint y) internal pure returns (uint z) {
                  require((z = x + y) >= x);
              }
              function _sub(uint x, uint y) internal pure returns (uint z) {
                  require((z = x - y) <= x);
              }
              function _mul(uint x, uint y) internal pure returns (uint z) {
                  require(y == 0 || (z = x * y) / y == x);
              }

              // --- Administration ---
              function init(bytes32 ilk) external auth {
                  require(ilks[ilk].rate == 0, "Vat/ilk-already-init");
                  ilks[ilk].rate = 10 ** 27;
              }
              function file(bytes32 what, uint data) external auth {
                  require(live == 1, "Vat/not-live");
                  if (what == "Line") Line = data;
                  else revert("Vat/file-unrecognized-param");
              }
              function file(bytes32 ilk, bytes32 what, uint data) external auth {
                  require(live == 1, "Vat/not-live");
                  if (what == "spot") ilks[ilk].spot = data;
                  else if (what == "line") ilks[ilk].line = data;
                  else if (what == "dust") ilks[ilk].dust = data;
                  else revert("Vat/file-unrecognized-param");
              }
              function cage() external auth {
                  live = 0;
              }

              // --- Fungibility ---
              function slip(bytes32 ilk, address usr, int256 wad) external auth {
                  gem[ilk][usr] = _add(gem[ilk][usr], wad);
              }
              function flux(bytes32 ilk, address src, address dst, uint256 wad) external {
                  require(wish(src, msg.sender), "Vat/not-allowed");
                  gem[ilk][src] = _sub(gem[ilk][src], wad);
                  gem[ilk][dst] = _add(gem[ilk][dst], wad);
              }
              function move(address src, address dst, uint256 rad) external {
                  require(wish(src, msg.sender), "Vat/not-allowed");
                  dai[src] = _sub(dai[src], rad);
                  dai[dst] = _add(dai[dst], rad);
              }

              function either(bool x, bool y) internal pure returns (bool z) {
                  assembly{ z := or(x, y)}
              }
              function both(bool x, bool y) internal pure returns (bool z) {
                  assembly{ z := and(x, y)}
              }

              // --- CDP Confiscation ---
              function grab(bytes32 i, address u, address v, address w, int dink, int dart) external auth {
                  Urn storage urn = urns[i][u];
                  Ilk storage ilk = ilks[i];

                  urn.ink = _add(urn.ink, dink);
                  urn.art = _add(urn.art, dart);
                  ilk.Art = _add(ilk.Art, dart);

                  int dtab = _mul(ilk.rate, dart);

                  gem[i][v] = _sub(gem[i][v], dink);
                  sin[w]    = _sub(sin[w],    dtab);
                  vice      = _sub(vice,      dtab);
              }
          }
          |]
  fmap fromJust (solcRuntime "Vat" src)

initVm :: ByteString -> VM
initVm bs = vm
  where
    contractCode = RuntimeCode $ fmap LitByte (unpack bs)
    c = Contract
      { _contractcode = contractCode
      , _balance      = 0
      , _nonce        = 0
      , _codehash     = keccak (ConcreteBuf bs)
      , _opIxMap      = mkOpIxMap contractCode
      , _codeOps      = mkCodeOps contractCode
      , _external     = False
      }
    vm = makeVm $ VMOpts
      { EVM.vmoptContract      = c
      , EVM.vmoptCalldata      = AbstractBuf "txdata"
      , EVM.vmoptValue         = CallValue 0
      , EVM.vmoptAddress       = Addr 0xffffffffffffffff
      , EVM.vmoptCaller        = Lit 0
      , EVM.vmoptOrigin        = Addr 0xffffffffffffffff
      , EVM.vmoptGas           = 0xffffffffffffffff
      , EVM.vmoptGaslimit      = 0xffffffffffffffff
      , EVM.vmoptStorageBase   = Symbolic
      , EVM.vmoptBaseFee       = 0
      , EVM.vmoptPriorityFee   = 0
      , EVM.vmoptCoinbase      = 0
      , EVM.vmoptNumber        = 0
      , EVM.vmoptTimestamp     = Var "timestamp"
      , EVM.vmoptBlockGaslimit = 0
      , EVM.vmoptGasprice      = 0
      , EVM.vmoptMaxCodeSize   = 0xffffffff
      , EVM.vmoptDifficulty    = 0
      , EVM.vmoptSchedule      = FeeSchedule.berlin
      , EVM.vmoptChainId       = 1
      , EVM.vmoptCreate        = False
      , EVM.vmoptTxAccessList  = mempty
      , EVM.vmoptAllowFFI      = False
      }


-- | Builds the Expr for the given evm bytecode object
buildExpr :: ByteString -> IO (Expr End)
buildExpr bs = evalStateT (interpret (Fetch.oracle Nothing False) Nothing Nothing runExpr) (initVm bs)

dai :: IO ByteString
dai = do
  let src =
        [i|
        contract Dai {
            // --- Auth ---
            mapping (address => uint) public wards;
            function rely(address guy) external auth { wards[guy] = 1; }
            function deny(address guy) external auth { wards[guy] = 0; }
            modifier auth {
                require(wards[msg.sender] == 1, "Dai/not-authorized");
                _;
            }

            // --- ERC20 Data ---
            string  public constant name     = "Dai Stablecoin";
            string  public constant symbol   = "DAI";
            string  public constant version  = "1";
            uint8   public constant decimals = 18;
            uint256 public totalSupply;

            mapping (address => uint)                      public balanceOf;
            mapping (address => mapping (address => uint)) public allowance;

            event Approval(address indexed src, address indexed guy, uint wad);
            event Transfer(address indexed src, address indexed dst, uint wad);

            // --- Math ---
            function add(uint x, uint y) internal pure returns (uint z) {
                require((z = x + y) >= x);
            }
            function sub(uint x, uint y) internal pure returns (uint z) {
                require((z = x - y) <= x);
            }

            // --- EIP712 niceties ---
            constructor() public {
                wards[msg.sender] = 1;
            }

            // --- Token ---
            function transfer(address dst, uint wad) external returns (bool) {
                return transferFrom(msg.sender, dst, wad);
            }
            function transferFrom(address src, address dst, uint wad)
                public returns (bool)
            {
                require(balanceOf[src] >= wad, "Dai/insufficient-balance");
                if (src != msg.sender && allowance[src][msg.sender] != type(uint).max) {
                    require(allowance[src][msg.sender] >= wad, "Dai/insufficient-allowance");
                    allowance[src][msg.sender] = sub(allowance[src][msg.sender], wad);
                }
                balanceOf[src] = sub(balanceOf[src], wad);
                balanceOf[dst] = add(balanceOf[dst], wad);
                emit Transfer(src, dst, wad);
                return true;
            }
            function mint(address usr, uint wad) external auth {
                balanceOf[usr] = add(balanceOf[usr], wad);
                totalSupply    = add(totalSupply, wad);
                emit Transfer(address(0), usr, wad);
            }
            function burn(address usr, uint wad) external {
                require(balanceOf[usr] >= wad, "Dai/insufficient-balance");
                if (usr != msg.sender && allowance[usr][msg.sender] != type(uint).max) {
                    require(allowance[usr][msg.sender] >= wad, "Dai/insufficient-allowance");
                    allowance[usr][msg.sender] = sub(allowance[usr][msg.sender], wad);
                }
                balanceOf[usr] = sub(balanceOf[usr], wad);
                totalSupply    = sub(totalSupply, wad);
                emit Transfer(usr, address(0), wad);
            }
            function approve(address usr, uint wad) external returns (bool) {
                allowance[msg.sender][usr] = wad;
                emit Approval(msg.sender, usr, wad);
                return true;
            }

            // --- Alias ---
            function push(address usr, uint wad) external {
                transferFrom(msg.sender, usr, wad);
            }
            function pull(address usr, uint wad) external {
                transferFrom(usr, msg.sender, wad);
            }
            function move(address src, address dst, uint wad) external {
                transferFrom(src, dst, wad);
            }
        }
        |]
  fmap fromJust (solcRuntime "Dai" src)
