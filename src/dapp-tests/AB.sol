pragma solidity >=0.5.15; 
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
    z = x + 2 - 2;
    if (z == 4291) {
      z = 4291;
    }
  }
}
