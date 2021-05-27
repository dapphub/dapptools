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
