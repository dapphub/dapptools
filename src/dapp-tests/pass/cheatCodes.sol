pragma solidity ^0.6.7;

import "ds-test/test.sol";

interface Hevm {
    function warp(uint256) external;
    function roll(uint256) external;
    function load(address,bytes32) external returns (bytes32);
    function store(address,bytes32,bytes32) external;
}

contract HasStorage {
    uint slot0 = 10;
}

contract CheatCodes is DSTest {
    address store = address(new HasStorage());
    Hevm hevm = Hevm(HEVM_ADDRESS);

    function test_warp_concrete(uint128 jump) public {
        uint pre = block.timestamp;
        hevm.warp(block.timestamp + jump);
        uint post = block.timestamp;
        assertEq(pre + jump, post);
    }

    function prove_warp_symbolic(uint128 jump) public {
        test_warp_concrete(jump);
    }

    function test_roll_concrete(uint64 jump) public {
        uint pre = block.number;
        hevm.roll(block.number + jump);
        uint post = block.number;
        assertEq(pre + jump, post);
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
}
