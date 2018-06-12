pragma solidity ^0.4.17;

contract Validators {
	event Changed (address[] indexed validators);

	address[] public validators;

	function Validators (address[] newValidators) public {
		validators = newValidators;
	}

	function setValidators (address[] newValidators) public {
		validators = newValidators;
	}

	function addTwoValidators (address[2] newValidators) public {
		validators.push(newValidators[0]);
		validators.push(newValidators[1]);
	}
}
