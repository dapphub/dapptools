import "ds-test/test.sol";

library A {
    function f(uint128 x) public returns (uint256) {
        return uint(x) * 2;
    }
}

contract B is DSTest {
    using A for uint128;
    function test_f(uint128 x) public {
        assertEq(uint(x) * 2, x.f());
    }

    function testFail_f() public {
        assertEq(1, uint128(1).f());
    }
}
