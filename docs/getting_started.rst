
###############
Getting Started
###############

Learning Solidity
-----------------

Without a doubt, the `official documentation website <https://solidity.readthedocs.io>`_ is the most up-to-date and useful resource for learning Solidity. The language subtly changes fairly often, so being up-to-date is particularly valuable when working in this domain. Start from the first page and read to the last. It may take a while, but you will have a good understanding of Solidity by the end.

Initializing A Workspace
------------------------

These are the commands to create a brand new project using dapp:

.. code:: bash

    $ mkdir dapp-tutorial
    $ cd dapp-tutorial
    $ dapp init

You will see that dapp does a couple of things for you:

1. Initializes a git repository if you don't have one.
2. Creates a ``Makefile`` for convenience.

::

    name            dapp-tutorial
    description
    version         0.0.1

    author          amilenius
    license         Apache-2.0

4. Installs a package called ``ds-test``, which is used when :ref:`testing your contracts <unit_testing>`. This package is installed in the ``lib`` folder.
5. Creates a ``src`` directory and ``test.sol``
6. Runs a sanity test with ``dapp test``. This causes the ``Test`` contract from ``test.sol`` to be built. The compiler's output is put in the ``out`` directory.
7. Commits the changes that were made.