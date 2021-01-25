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
        if (x + y < x) return; // no overflow
        assertTrue(x + y >= x);
    }

    function prove_balance(address usr, uint amt) public {
        assertEq(0, token.balanceOf(usr));
        token.mint(usr, amt);
        assertEq(amt, token.balanceOf(usr));
    }

    function prove_supply(uint supply) public {
        token.mint(supply);
        uint actual = token.totalSupply();
        assertEq(supply, actual);
    }

    function proveFail_revertSmoke() public {
        require(false);
    }

    function proveFail_assertSmoke() public {
        assertTrue(false);
    }

    function prove_transfer(uint supply, address usr, uint amt) public {
        if (amt > supply) return; // no underflow

        token.mint(supply);

        uint prebal = token.balanceOf(usr);
        token.transfer(usr, amt);
        uint postbal = token.balanceOf(usr);

        uint expected = usr == address(this)
                        ? 0    // self transfer is a noop
                        : amt; // otherwise `amt` has been transfered to `usr`
        assertEq(expected, postbal - prebal);
    }

    function prove_burn(uint supply, uint amt) public {
        if (amt > supply) return; // no undeflow

        token.mint(supply);
        token.burn(amt);

        assertEq(supply - amt, token.totalSupply());
    }

    function prove_mul(uint128 x, uint128 y) public {
        mul(x,y);
    }

    function prove_distributivity(uint120 x, uint120 y, uint120 z) public {
        assertEq(mul(x, add(y, z)), add(mul(x, y), mul(x, z)));
    }


    function prove_loop(uint n) public {
        uint counter = 0;
        for (uint i = 0; i < n; i++) {
            counter++;
        }
        assertTrue(counter < 100);
    }
}

