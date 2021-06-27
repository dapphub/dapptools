pragma solidity ^0.6.7;

import "ds-test/test.sol";
import "ds-token/token.sol";
import "ds-math/math.sol";

contract InvariantTest is DSTest {
    DSToken token;
    User user;
    address[] targetContracts_;

    function targetContracts() public returns (address[] memory) {
      return targetContracts_;
    }

    function setUp() public {
        token = new DSToken("TKN");
        token.mint(100 ether);
        user = new User(token);
        token.transfer(address(user), 100 ether);
        targetContracts_.push(address(user));
    }

    function invariantTestThisBal() public {
        assertLe(token.balanceOf(address(user)), 100 ether);
    }
    function invariantTotSupply() public {
        assertEq(token.totalSupply(), 100 ether);
    }
}

contract User {
  DSToken token;
  constructor(DSToken token_) public {
    token = token_;
  }

  function doTransfer(address to, uint amount) public {
    token.transfer(to, amount);
  }

  function doSelfTransfer(uint amount) public {
    token.transfer(address(this), amount);
  }
}
