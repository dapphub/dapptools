import "ds-test/test.sol";

contract Env is DSTest {
    // DAPP_TEST_ORIGIN
    function testOrigin() public {
        assertEq(tx.origin, address(256));
    }
}
