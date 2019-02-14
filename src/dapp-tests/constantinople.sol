pragma solidity ^0.5.2;

import "ds-test/test.sol";

contract DeadCode{
    function dummy() external returns (uint256);
}

contract ConstantinopleTests is DSTest {

    ConstantinopleTests notmuch;
    // this 5 byte-long initcode simply returns nothing
    // PUSH1  00     PUSH1  00     RETURN
    // 60     00     60     00     f3
    bytes32 zerocode        = 0x60006000f3000000000000000000000000000000000000000000000000000000;
    // this 13 byte-long initcode simply returns 0xdeadbeef:
    // PUSH4  de     ad     be     ef     PUSH1  00     MSTORE PUSH1  32     PUSH1  00     RETURN
    // 63     de     ad     be     ef     60     00     52     60     20     60     00     f3
    bytes32 deadcode        = 0x63deadbeef60005260206000f300000000000000000000000000000000000000;
    // this 25 byte-long initcode returns deadcode (but without the padding)
    // PUSH1  0d     PUSH1  0c     PUSH1  00     CODECO PUSH1  0d     PUSH1  00     RETURN deadcode
    // 60     0d     60     0c     60     00     39     60     0d     60     00     f3
    bytes32 deploysdeadcode = 0x600d600c600039600d6000f363deadbeef60005260206000f300000000000000;

    // EXTCODEHASH of non-existent account is 0
    function test_extcodehash_1() public {
        uint256 h;
        assembly {
            h := extcodehash(0x10)
        }
        assertEq(h, 0);
    }
    // EXTCODEHASH of account with no code is keccak256("")
    function test_extcodehash_2() public {
        address a;
        uint256 h;
        assembly {
            let top := mload(0x40)
            mstore(top, sload(zerocode_slot))
            a := create(0, top, 5)
            h := extcodehash(a)
        }
        assertEq(h, 0xc5d2460186f7233c927e7db2dcc703c0e500b653ca82273b7bfad8045d85a470);
    }
    // EXTCODEHASH of account with code 0xdeadbeef is keccak256(0xdeadbeef)
    function test_extcodehash_3() public {
        address a;
        uint256 h;

        assembly {
          let top := mload(0x40)
          mstore(top, sload(deadcode_slot))
          a := create(0, top, 13)
          h := extcodehash(a)
        }

        uint256 expected_h;
        assembly {
            let top := mload(0x40)
            mstore(top, 0xdeadbeef)
            expected_h := keccak256(top, 32)
        }
        assertEq(h, expected_h);
    }
    // EXTCODEHASH of the notmuch contract should be same as
    // doing EXTCODECOPY and then keccak256
    function test_extcodehash_4() public {
        address a = address(notmuch);
        uint256 h;

        assembly {
            h := extcodehash(a)
        }

        uint256 expected_h;
        assembly {
            let top := mload(0x40)
            let size := extcodesize(a)
            extcodecopy(a, top, 0, size)
            expected_h := keccak256(top, size)
        }
        assertEq(h, expected_h);
    }

    // address of account created by CREATE2 is
    // keccak256(0xff + address + salt + keccak256(init_code))[12:]
    function test_create2_1() public {
        address a;
        uint256 salt = 0xfacefeed;
        assembly {
          let top := mload(0x40)
          mstore(top, sload(deadcode_slot))
          a := create2(0, top, 13, salt)
        }

        address expected_a;

        assembly {
          let top := mload(0x40)
          mstore(top, sload(deadcode_slot))
          let inithash := keccak256(top, 13)
          mstore(sub(top, 11), address)
          mstore8(top, 0xff)
          mstore(add(top, 21), salt)
          mstore(add(top, 53), inithash)
          expected_a := and(keccak256(top, 85), 0x000000000000000000000000ffffffffffffffffffffffffffffffffffffffff)
        }

        assertEq(a, expected_a);
    }
    // calling a CREATE2 contract works as expected
    function test_create2_2() public {
        address a;
        uint256 salt = 0xfacefeed;
        assembly {
          let top := mload(0x40)
          mstore(top, sload(deploysdeadcode_slot))
          a := create2(0, top, 25, salt)
        }

        assertEq(DeadCode(a).dummy(), 0xdeadbeef);
    }
    // TODO: test some SELFDESTRUCT properties of CREATE2
    // TODO: test EXTCODEHASH on self-destructed contract
    // TODO: test EXTCODEHASH on precompiled contracts
}
