pragma solidity ^0.6.7;

import {DSTest} from "ds-test/test.sol";

// should not be run (no code)
abstract contract MyTest is DSTest {
    function testAbstract() public {
        assertTrue(true);
    }
}

// should run and pass
contract TestMy is MyTest {
    function testTrue() public {
        assertTrue(true);
    }
}
