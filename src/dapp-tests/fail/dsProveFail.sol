pragma solidity ^0.6.7;

import "ds-test/test.sol";
import "ds-token/token.sol";
import "ds-math/math.sol";

contract SolidityTest is DSTest, DSMath {
    DSToken token;

    function setUp() public {
        token = new DSToken("TKN");
    }

    function prove_add(uint x, uint y) public {
        assertTrue(x + y >= x);
    }

    function proveFail_shouldFail(address usr) public {
        usr.call("");
    }

    function prove_smtTimeout(uint x, uint y, uint z) public {
        if ((x * y / z) * (x / y) / (x * y) == (x * x * x * y * z / x * z * y)) {
            assertTrue(false);
        } else {
            assertTrue(true);
        }
    }

    function prove_multi(uint x) public {
        if (x == 3) {
            assertTrue(false);
        } else if (x == 9) {
            assertTrue(false);
        } else if (x == 1023423194871904872390487213) {
            assertTrue(false);
        } else {
            assertTrue(true);
        }
    }

    function prove_mul(uint136 x, uint128 y) public {
        mul(x,y);
    }

    function prove_distributivity(uint120 x, uint120 y, uint120 z) public {
        assertEq(add(x, mul(y, z)), mul(add(x, y), add(x, z)));
    }

    function prove_transfer(uint supply, address usr, uint amt) public {
        token.mint(supply);

        uint prebal = token.balanceOf(usr);
        token.transfer(usr, amt);
        uint postbal = token.balanceOf(usr);

        uint expected = usr == address(this)
                        ? 0    // self transfer is a noop
                        : amt; // otherwise `amt` has been transfered to `usr`
        assertEq(expected, postbal - prebal);
    }
}

