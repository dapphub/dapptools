pragma solidity ^0.6.7;

import "ds-test/test.sol";

contract DeadCode{
    function dummy() external returns (uint256) {}
}

contract ConstantinopleTests is DSTest {
    DeadCode notmuch;
    function setUp() public {
      notmuch = new DeadCode();
    }

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
          mstore(sub(top, 11), address())
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

    // SHL, SHR and SAR tests taken from
    // https://github.com/ethereum/EIPs/blob/fde32dfd6b24bac7bfabf6c1ebe3f5a603d5ff4c/EIPS/eip-145.md
    function test_shl() public {
        uint z;

        assembly {
            z := shl(0x00, 0x0000000000000000000000000000000000000000000000000000000000000001)
        }
        assertEq(z, 0x0000000000000000000000000000000000000000000000000000000000000001);

        assembly {
            z := shl(0x01, 0x0000000000000000000000000000000000000000000000000000000000000001)
        }
        assertEq(z, 0x0000000000000000000000000000000000000000000000000000000000000002);

        assembly {
            z := shl(0xff, 0x0000000000000000000000000000000000000000000000000000000000000001)
        }
        assertEq(z, 0x8000000000000000000000000000000000000000000000000000000000000000);

        assembly {
            z := shl(0x0100, 0x0000000000000000000000000000000000000000000000000000000000000001)
        }
        assertEq(z, 0x0000000000000000000000000000000000000000000000000000000000000000);

        assembly {
            z := shl(0x0101, 0x0000000000000000000000000000000000000000000000000000000000000001)
        }
        assertEq(z, 0x0000000000000000000000000000000000000000000000000000000000000000);

        assembly {
            z := shl(0x00, 0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff)
        }
        assertEq(z, 0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff);

        assembly {
            z := shl(0x01, 0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff)
        }
        assertEq(z, 0xfffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffe);

        assembly {
            z := shl(0xff, 0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff)
        }
        assertEq(z, 0x8000000000000000000000000000000000000000000000000000000000000000);

        assembly {
            z := shl(0x0100, 0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff)
        }
        assertEq(z, 0x0000000000000000000000000000000000000000000000000000000000000000);

        assembly {
            z := shl(0x01, 0x0000000000000000000000000000000000000000000000000000000000000000)
        }
        assertEq(z, 0x0000000000000000000000000000000000000000000000000000000000000000);

        assembly {
            z := shl(0x01, 0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff)
        }
        assertEq(z, 0xfffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffe);
    }

    function test_shr() public {
        uint z;

        assembly {
            z := shr(0x00, 0x0000000000000000000000000000000000000000000000000000000000000001)
        }
        assertEq(z, 0x0000000000000000000000000000000000000000000000000000000000000001);

        assembly {
            z := shr(0x01, 0x0000000000000000000000000000000000000000000000000000000000000001)
        }
        assertEq(z, 0x0000000000000000000000000000000000000000000000000000000000000000);

        assembly {
            z := shr(0x01, 0x8000000000000000000000000000000000000000000000000000000000000000)
        }
        assertEq(z, 0x4000000000000000000000000000000000000000000000000000000000000000);

        assembly {
            z := shr(0xff, 0x8000000000000000000000000000000000000000000000000000000000000000)
        }
        assertEq(z, 0x0000000000000000000000000000000000000000000000000000000000000001);

        assembly {
            z := shr(0x0100, 0x8000000000000000000000000000000000000000000000000000000000000000)
        }
        assertEq(z, 0x0000000000000000000000000000000000000000000000000000000000000000);

        assembly {
            z := shr(0x0101, 0x8000000000000000000000000000000000000000000000000000000000000000)
        }
        assertEq(z, 0x0000000000000000000000000000000000000000000000000000000000000000);

        assembly {
            z := shr(0x00, 0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff)
        }
        assertEq(z, 0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff);

        assembly {
            z := shr(0x01, 0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff)
        }
        assertEq(z, 0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff);

        assembly {
            z := shr(0xff, 0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff)
        }
        assertEq(z, 0x0000000000000000000000000000000000000000000000000000000000000001);

        assembly {
            z := shr(0x0100, 0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff)
        }
        assertEq(z, 0x0000000000000000000000000000000000000000000000000000000000000000);

        assembly {
            z := shr(0x01, 0x0000000000000000000000000000000000000000000000000000000000000000)
        }
        assertEq(z, 0x0000000000000000000000000000000000000000000000000000000000000000);
    }

    function test_sar() public {
        uint z;

        assembly {
            z := sar(0x00, 0x0000000000000000000000000000000000000000000000000000000000000001)
        }
        assertEq(bytes32(z), bytes32(0x0000000000000000000000000000000000000000000000000000000000000001));

        assembly {
            z := sar(0x01, 0x0000000000000000000000000000000000000000000000000000000000000001)
        }
        assertEq(bytes32(z), bytes32(0x0000000000000000000000000000000000000000000000000000000000000000));

        assembly {
            z := sar(0x01, 0x8000000000000000000000000000000000000000000000000000000000000000)
        }
        assertEq(bytes32(z), bytes32(0xc000000000000000000000000000000000000000000000000000000000000000));

        assembly {
            z := sar(0xff, 0x8000000000000000000000000000000000000000000000000000000000000000)
        }
        assertEq(bytes32(z), bytes32(0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff));

        assembly {
            z := sar(0x0100, 0x8000000000000000000000000000000000000000000000000000000000000000)
        }
        assertEq(bytes32(z), bytes32(0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff));

        assembly {
            z := sar(0x0101, 0x8000000000000000000000000000000000000000000000000000000000000000)
        }
        assertEq(bytes32(z), bytes32(0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff));

        assembly {
            z := sar(0x00, 0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff)
        }
        assertEq(bytes32(z), bytes32(0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff));

        assembly {
            z := sar(0x01, 0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff)
        }
        assertEq(bytes32(z), bytes32(0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff));

        assembly {
            z := sar(0xff, 0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff)
        }
        assertEq(bytes32(z), bytes32(0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff));

        assembly {
            z := sar(0x0100, 0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff)
        }
        assertEq(bytes32(z), bytes32(0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff));

        assembly {
            z := sar(0x01, 0x0000000000000000000000000000000000000000000000000000000000000000)
        }
        assertEq(bytes32(z), bytes32(0x0000000000000000000000000000000000000000000000000000000000000000));

        assembly {
            z := sar(0xfe, 0x4000000000000000000000000000000000000000000000000000000000000000)
        }
        assertEq(bytes32(z), bytes32(0x0000000000000000000000000000000000000000000000000000000000000001));

        assembly {
            z := sar(0xf8, 0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff)
        }
        assertEq(bytes32(z), bytes32(0x000000000000000000000000000000000000000000000000000000000000007f));

        assembly {
            z := sar(0xfe, 0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff)
        }
        assertEq(bytes32(z), bytes32(0x0000000000000000000000000000000000000000000000000000000000000001));

        assembly {
            z := sar(0xff, 0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff)
        }
        assertEq(bytes32(z), bytes32(0x0000000000000000000000000000000000000000000000000000000000000000));

        assembly {
            z := sar(0x0100, 0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff)
        }
        assertEq(bytes32(z), bytes32(0x0000000000000000000000000000000000000000000000000000000000000000));
    }
}
