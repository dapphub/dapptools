pragma solidity ^0.6.7;

import "ds-test/test.sol";

contract Testdapp {
    uint public x;
    function f() public {
        x++;
    }
    function g(uint y) public {
        if (y % 2 == 0) x*=2;
    }
}


contract TestdappTest is DSTest {
    Testdapp testdapp;

    function setUp() public {
        testdapp = new Testdapp();
    }

    function invariantFirst() public {
        assertLt(testdapp.x(), 100);
    }
}

contract BrokenCoin {
  mapping(address=>uint) public balanceOf;
  constructor(uint amount) public {
    balanceOf[msg.sender] = amount;
  }

  function transfer(address to, uint amount) public {
    uint senderBal = balanceOf[msg.sender];
    uint toBal = balanceOf[to];
    require(senderBal >= amount);
    senderBal += amount;
    toBal -= amount;
    balanceOf[msg.sender] = senderBal;
    balanceOf[to] = toBal;
  }
}

contract InvariantTest is DSTest {
    BrokenCoin token;
    User user;
    address[] targetContracts_;

    function targetContracts() public returns (address[] memory) {
      return targetContracts_;
    }
    function setUp() public {
        token = new BrokenCoin(100 ether);
        user = new User(token);
        token.transfer(address(user), 100 ether);
        targetContracts_.push(address(user));
    }

    function invariantTestUserBal() public {
        assertLe(token.balanceOf(address(user)), 100 ether);
    }
}

contract InvariantCount is DSTest {
    BrokenAtStart count;
    address[] targetContracts_;

    function targetContracts() public returns (address[] memory) {
      return targetContracts_;
    }
    function setUp() public {
        count = new BrokenAtStart();
        targetContracts_.push(address(count));
    }

    // this can only fail if we call the invariant method before calling any other method in the target contracts
    function invariantCount() public {
        assertGt(count.count(), 0);
    }
}

contract BrokenAtStart {
    uint public count;

    function inc() public {
        count++;
    }
}

contract User {
  BrokenCoin token;
  constructor(BrokenCoin token_) public {
    token = token_;
  }

  function doTransfer(address to, uint amount) public {
    token.transfer(to, amount);
  }

  function doSelfTransfer(uint amount) public {
    token.transfer(address(this), amount);
  }
}
