import "ds-test/test.sol";

library A {
    function f(uint x) public returns (uint256) {
        return x * 2;
    }
}

contract B is DSTest {
    using A for uint256;
    function test_f(uint x) public {
        assertEq(x * 2, x.f());
    }

    function testFail_f() public {
        assertEq(1, uint(1).f());
    }
}
