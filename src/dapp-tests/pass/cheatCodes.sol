pragma solidity ^0.6.7;
pragma experimental ABIEncoderV2;

import "ds-test/test.sol";

interface Hevm {
    function warp(uint256) external;
    function roll(uint256) external;
    function load(address,bytes32) external returns (bytes32);
    function store(address,bytes32,bytes32) external;
    function sign(uint256,bytes32) external returns (uint8,bytes32,bytes32);
    function addr(uint256) external returns (address);
    function ffi(string[] calldata) external returns (bytes memory);
    function file(bytes32,uint) external;
    function file(bytes32,address) external;
    function file(bytes32,address,uint) external;
    function look(bytes32) external returns (uint);
    function look(bytes32,address) external returns (uint);
    function replace(address,bytes calldata) external;
}

contract HasStorage {
    uint slot0 = 10;
}

contract Old {
    uint constant public x = 10;
}

contract New {
    uint constant public x = 100;
}

contract CheatCodes is DSTest {
    address store = address(new HasStorage());
    Old target = new Old();
    Hevm hevm = Hevm(HEVM_ADDRESS);

    function test_warp_concrete(uint128 jump) public {
        uint pre = block.timestamp;
        hevm.warp(block.timestamp + jump);
        assertEq(block.timestamp, pre + jump);
    }

    function prove_warp_symbolic(uint128 jump) public {
        test_warp_concrete(jump);
    }

    function test_roll_concrete(uint64 jump) public {
        uint pre = block.number;
        hevm.roll(block.number + jump);
        assertEq(block.number, pre + jump);
    }

    function test_store_load_concrete(uint x) public {
        uint ten = uint(hevm.load(store, bytes32(0)));
        assertEq(ten, 10);

        hevm.store(store, bytes32(0), bytes32(x));
        uint val = uint(hevm.load(store, bytes32(0)));
        assertEq(val, x);
    }

    function prove_store_load_symbolic(uint x) public {
        test_store_load_concrete(x);
    }

    function test_sign_addr_digest(uint sk, bytes32 digest) public {
        if (sk == 0) return; // invalid key

        (uint8 v, bytes32 r, bytes32 s) = hevm.sign(sk, digest);
        address expected = hevm.addr(sk);
        address actual = ecrecover(digest, v, r, s);

        assertEq(actual, expected);
    }

    function test_sign_addr_message(uint sk, bytes memory message) public {
        test_sign_addr_digest(sk, keccak256(message));
    }

    function testFail_sign_addr(uint sk, bytes32 digest) public {
        uint badKey = sk + 1;

        (uint8 v, bytes32 r, bytes32 s) = hevm.sign(badKey, digest);
        address expected = hevm.addr(sk);
        address actual = ecrecover(digest, v, r, s);

        assertEq(actual, expected);
    }

    function testFail_addr_zero_sk() public {
        hevm.addr(0);
    }

    function testFFI() public {
        string[] memory inputs = new string[](3);
        inputs[0] = "echo";
        inputs[1] = "-n";
        inputs[2] = "0x000000000000000000000000000000000000000000000000000000000000002000000000000000000000000000000000000000000000000000000000000000046163616200000000000000000000000000000000000000000000000000000000";

        (string memory output) = abi.decode(hevm.ffi(inputs), (string));
        assertEq(output, "acab");
    }

    function testNonce(address who, uint n) public {
        hevm.file("nonce", who, n);
        assertEq(hevm.look("nonce", who), n);
    }

    function testBalance(address who, uint bal) public {
        hevm.file("balance", who, bal);
        assertEq(who.balance, bal);
    }

    function testReplace() public {
        hevm.replace(address(target), type(New).runtimeCode);
        assertEq(New(address(target)).x(), 100);
    }

    function proveCaller(address who) public {
        hevm.file("caller", who);
        assertEq(msg.sender, who);
    }

    function testOrigin(address who) public {
        hevm.file("origin", who);
        assertEq(tx.origin, who);
    }

    function testCoinbase(address who) public {
        hevm.file("coinbase", who);
        assertEq(block.coinbase, who);
    }

    function testGasPrice(uint price) public {
        hevm.file("gasPrice", price);
        assertEq(tx.gasprice, price);
    }

    function testTxGasLimit(uint limit) public {
        hevm.file("txGasLimit", limit);
        assertEq(hevm.look("txGasLimit"), limit);
    }

    function testBlockGasLimit(uint limit) public {
        hevm.file("blockGasLimit", limit);
        assertEq(block.gaslimit, limit);
    }

    function testDifficulty(uint difficulty) public {
        hevm.file(bytes32("difficulty"), difficulty);
        assertEq(block.difficulty, difficulty);
    }
}
