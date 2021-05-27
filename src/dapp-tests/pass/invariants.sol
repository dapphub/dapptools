pragma solidity ^0.6.7;

import "ds-test/test.sol";
import "ds-token/token.sol";
import "ds-math/math.sol";

contract InvariantTest is DSTest {
    DSToken token;

    function setUp() public {
        token = new DSToken("TKN");
        token.mint(100 ether);
    }

    function invariantTestThisBal() public {
        assertLe(token.balanceOf(address(this)), 100 ether);
    }
    function invariantTotSupply() public {
        assertEq(token.totalSupply(), 100 ether);
    }
}
