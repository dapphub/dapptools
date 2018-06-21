
.. _testnet:

############
Using the testnet
############

You can quickly spin up an ethereum testnet to experiment with your contracts or connect to a web page interface.

.. code:: bash

    $ dapp testnet

spins up a `geth <https://github.com/ethereum/go-ethereum/>`_ testnet at ``http://127.0.0.1:2000`` with instant mining and keeps stores the data at ``~/.dapp/testnet/2000``. If you want the chain to be saved after you are done, use the flag ``-save=name``. Otherwise, the directory will be removed when the process terminates.

Once the testnet is up and running, you can interact with it through your favorite tool. We like to use `seth <https://dapp.tools/seth/>`_ for slicing and dicing transactions, querying the blockchain, converting between data formats, performing remote calls, and other everyday tasks.

Specifically, you may want to check out the `connecting to the blockchain https://github.com/dapphub/seth#connecting-to-the-blockchain`_ section of the seth documentation.

Test Flags
---------------

+-------------------+-------------------------------------------------------------+
| Flag              | Description                                                 |
+===================+=============================================================+
| --rpc-port        | Change RPC port (default: 8545)                             |
+-------------------+-------------------------------------------------------------+
| --chain-id=number | Change chain ID (default: 99)                               |
+-------------------+-------------------------------------------------------------+
| --period=seconds  | Use a custom block time instead of instamine                |
+-------------------+-------------------------------------------------------------+
| --accounts=number | Create multiple accounts (default: 1)                       |
+-------------------+-------------------------------------------------------------+
| --save=name       | after finishing, save snapshot                              |
+-------------------+-------------------------------------------------------------+
| --load=name       | start from a previously saved snapshot                      |
+-------------------+-------------------------------------------------------------+
