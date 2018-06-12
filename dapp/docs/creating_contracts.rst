
##################
Creating Contracts
##################

Creating new contracts is kept very simple in dapp. The command is invoked with ``dapp create`` followed by the contract's type and any constructor parameters.

So if you had this contract:

::

    pragma solidity ^0.4.0;

    contract AppToken {

        mapping( address => uint ) _balances;
        string public name;

        function AppToken(uint initial_balance, string name_) {
            _balances[msg.sender] = initial_balance;
            _supply = initial_balance;

            name = name_;
        }
    }

You would first turn on an Ethereum client and then create it like this:


.. code:: bash

    $ dapp create AppToken 10000 "MyCoolToken"
    + seth send --create out/AppToken.bin 'AppToken(uint256,string)' 10000 MyCoolToken
    seth-send: Published transaction with 1688 bytes of calldata.
    seth-send: 0x4df5fdad614a88702439b10dd3002adb7869b339413011ba8ac69b4f07d9f10d
    seth-send: Waiting for transaction receipt...
    seth-send: Transaction included in block .

And that's it! Dapp will read the correct bytecode file from your ``out`` directory, bundle it up into a transaction with your encoded constructor parameters, and broadcast it to the network by curling your Ethereum client.

Storing Your Contract Addresses
-------------------------------

Coming soon!

 