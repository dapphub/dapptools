pragma solidity ^0.4.8;
contract X {
  event Log(uint x);
  function foo() {
    Log(255);
  }
}