pragma solidity ^0.4.8;
contract X {
  event Log(uint x);
  function foo() returns (uint) {
    Log(255);
    return 255;
  }
}