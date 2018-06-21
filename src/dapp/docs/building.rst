
####################
Building Source Code
####################

Building source code is easy with Dapp:

.. code:: bash

    $ dapp build

This will take all the code in the ``src`` and ``lib`` directories and feed it to the ``solc`` compiler. There are a few build artifacts that are deposited in the ``build`` directory, and it's worth going over each one specifically.


Build Artifacts
---------------


Suppose you had this abridged token contract:

::

    contract DSTokenBase {

        event Transfer( address indexed from, address indexed to, uint value);

        mapping( address => uint ) _balances;
        uint _supply;

        function DSTokenBase( uint initial_balance ) {
            _balances[msg.sender] = initial_balance;
            _supply = initial_balance;
        }

        function balanceOf( address who ) constant returns (uint value) {
            return _balances[who];
        }

        function transfer( address to, uint value) returns (bool ok) {

            if( _balances[msg.sender] < value ) {
                throw;
            }

            _balances[msg.sender] -= value;
            _balances[to] += value;

            Transfer( msg.sender, to, value );

            return true;
        }
    }

Running ``dapp build`` will produce three output files:

.. code:: bash

    $ dapp build
    $ ls out
    DSTokenBase.abi          DSTokenBase.bin          DSTokenBase.bin-runtime

The ``abi`` file is a JSON representation of your contract's `Application Binary Interface <https://github.com/ethereum/wiki/wiki/Ethereum-Contract-ABI>`_ and in this case it will look like this:

::
    
    [
      {
        "anonymous": false,
        "inputs": [
          {
            "indexed": true,
            "name": "from",
            "type": "address"
          },
          {
            "indexed": true,
            "name": "to",
            "type": "address"
          },
          {
            "indexed": false,
            "name": "value",
            "type": "uint256"
          }
        ],
        "name": "Transfer",
        "type": "event"
      },
      {
        "constant": true,
        "inputs": [
          {
            "name": "who",
            "type": "address"
          }
        ],
        "name": "balanceOf",
        "outputs": [
          {
            "name": "value",
            "type": "uint256"
          }
        ],
        "payable": false,
        "type": "function"
      },
      {
        "constant": false,
        "inputs": [
          {
            "name": "to",
            "type": "address"
          },
          {
            "name": "value",
            "type": "uint256"
          }
        ],
        "name": "transfer",
        "outputs": [
          {
            "name": "ok",
            "type": "bool"
          }
        ],
        "payable": false,
        "type": "function"
      }
    ]

This file specifies the public functions of your contract, which your user interface and other applications on the blockchain use to interact with it.

The other two files that were created carry the ``bin`` and ``bin-runtime`` file extensions. The ``bin`` file is the actual bytecode that gets combined with your `encoded constructor arguments <https://github.com/ethereum/wiki/wiki/Ethereum-Contract-ABI#argument-encoding>`_ and included in the ``calldata`` of any contract creating transactions. 

When the EVM evaluates your contract creating transaction, your contract's constructor function is called. The return value of this constructor function is the actual contract that you will interact with on the blockchain, and it is somewhat different that the bytecode that was originally found in your ``bin`` file. For instance, the return value of the constructor function does not in turn include bytecode instructions for a constructor function. This return value can be known in advance when compiling your contract, and its value is included in the ``bin-runtime`` file.