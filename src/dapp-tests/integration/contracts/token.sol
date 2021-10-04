// SPDX-License-Identifier: AGPL-3.0-only
pragma solidity >=0.8.0;

contract Token {
  mapping(address => uint) public balanceOf;
  uint public totalSupply;

  constructor(uint supply) {
    totalSupply = supply;
    balanceOf[msg.sender] = supply;
  }

  function transfer(address to, uint256 value) public {
    require(to != msg.sender);
    balanceOf[msg.sender] -= value;
    balanceOf[to] += value;
  }
}
