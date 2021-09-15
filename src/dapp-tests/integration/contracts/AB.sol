pragma solidity >=0.8;

contract A {
  uint y;
  function set(uint x) public {
    y = x;
  }
}

// B some extra weird noops
contract B {
  uint z;
  function set(uint x) public {
    unchecked { z = x + 2 - 2; }
    if (z == 4291) {
      z = 4291;
    }
  }
}
