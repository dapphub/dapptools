import "ds-test/test.sol";

contract Env is DSTest {
    uint creationGas;
    constructor() public {
        creationGas = gasleft();
    }

    // TODO: why does this fail when address == 0?
    // DAPP_TEST_BALANCE
    function testBalance() public {
        assertEq(address(this).balance, 998877665544 ether);
    }
    // DAPP_TEST_ADDRESS
    function testAddress() public {
        assertEq(address(this), address(256));
    }
    // DAPP_TEST_NONCE
    // we can't test the nonce directly, but can instead check the address of a newly deployed contract
    function testNonce() public {
        uint8 nonce = 100;
        bytes memory payload = abi.encodePacked(hex"d694", address(this), nonce);

        address expected = address(uint160(uint256(keccak256(payload))));
        address actual = address(new Trivial());
        assertEq(actual, expected);

    }
    // DAPP_TEST_CALLER
    function testCaller() public {
        assertEq(msg.sender, address(100));
    }
    // DAPP_TEST_GAS_CREATE
    function testGasCreate() public {
        // we can't be exact since we had to spend some gas to write to storage...
        assertLt(creationGas, 4.20 ether);
        assertGt(creationGas, 4.1999999999999 ether);
    }
    // DAPP_TEST_GAS_CALL
    function testGasCall() public {
        uint gas = gasleft();
        // we can't be exact since we had to spend some gas to get here...
        assertLt(gas, 0.69 ether);
        assertGt(gas, 0.689999999999999 ether);
    }
    // DAPP_TEST_COINBASE
    function testCoinbase() public {
        assertEq(block.coinbase, address(666));
    }
    // DAPP_TEST_NUMBER
    function testBlockNumber() public {
        assertEq(block.number, 420);
    }
    // DAPP_TEST_TIMESTAMP
    function testTimestamp() public {
        assertEq(block.timestamp, 69);
    }
    // DAPP_TEST_GAS_LIMIT
    function testGasLimit() public {
        assertEq(block.gaslimit, 4206966 ether);
    }
    // DAPP_TEST_GAS_PRICE
    function testGasPrice() public {
        assertEq(tx.gasprice, 100);
    }
    // DAPP_TEST_DIFFICULTY
    function testDifficulty() public {
        assertEq(block.difficulty, 600);
    }
}

contract Trivial {}
