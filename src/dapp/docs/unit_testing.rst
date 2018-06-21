
.. _unit_testing:

############
Unit Testing
############

Dapp uses `ethrun <https://github.com/dapphub/ethrun>`_ and `ds-test <https://github.com/dapphub/ds-test>`_ to test your code natively in Solidity. Writing both source code and unit tests in Solidity reduces context-switching, creating a more pleasant experience for the smart contract developer.


Getting Started
===============

First, make sure you are comfortable :ref:`installing packages using dapp <package_management>`. Next, you will need to install the ``ds-test`` Solidity package in your project:

.. code:: bash

    $ dapp install ds-test

.. note::
    Running ``dapp init`` will install ``ds-test`` for you, making this step unnecessary in most cases.

To create a test contract, you simply import ``test.sol`` from ``ds-test`` and make your contract inherit from ``DSTest`` like so:

::

    pragma solidity ^0.4.0;

    import "ds-test/test.sol";

    contract TestContract is DSTest {}


General Testing
===============

Unit testing with dapp should feel familiar to anyone who has used a test harness in other high-level programming languages. The ``DSTest`` parent class provides assertion functions for validating correctness and events for logging data in the console. These are detailed in the API reference guide :ref:`below <unit_test_api>`. Other than those functions, there are only a few basic conventions that you will need to learn to run your tests:

setUp Function
-------------- 

The ``setUp`` function is inherited as a placeholder from ``DSTest``. You can override it and add configuration steps that will be executed before each of your test cases.

::

    pragma solidity ^0.4.0;

    import "ds-test/test.sol";
    import "token.sol";

    contract AppTokenTest is DSTest {

        AppToken token;

        // token will be instantiated before each test case
        function setUp() {
            token = new AppToken();
        }
    }

Testcase Naming
----------------

Any functions that begin with the prefix ``test`` will be evaluated for correctness. In the general case, a testcase is considered correct if it doesn't throw an exception. Additionally, functions that begin with the prefix ``testFail`` are considered correct if they *do* throw an exception.

::

    pragma solidity ^0.4.0;

    import "ds-test/test.sol";
    import "token.sol";

    // A contract that can receive and transfer tokens.
    // Useful for testing a system with multiple users.
    contract TokenUser {
        AppToken  token;

        function TokenUser(AppToken token_) {
            token = token_;
        }

        function doTransfer(address to, uint amount)
            returns (bool)
        {
            return token.transfer(to, amount);
        }
    }

    contract AppTokenTest is DSTest {

        TokenUser user;
        AppToken token;

        // token and user will be instantiated before each test case
        function setUp() {
            token = new AppToken();
            user = new TokenUser(token);
        }

        function testTokenTransfer() {

            // inflate the supply of AppTokens
            token.mint(100);

            // test transfer to user
            assert(token.transfer(user, 100));

            // test transfer back
            assert(user.doTransfer(this, 100));
        }

        function testFailTokenTransfer() {

            // test transfering tokens with a balance of 0.
            // this should throw an exception, making this testcase correct.
            token.transfer(user, 10);
        }
    }

Testing Events
==============

Dapp can also help test the correctness of an emitted sequence of events. To use this feature, call the ``expectEventsExact`` function with the ``address`` of the contract you are testing. Then you simply emit the events that you're expecting, and dapp will ensure that they are called by your contract in the same order and with the same data. This means that in your source code, you will need to split out your event definitions into their own contracts so that your test contracts can inherit from them and have access to the right events. 

Here is an example ``token.sol`` file:

::

    pragma solidity ^0.4.0;

    import "ds-token/token.sol";

    contract AppTokenEvents {

        event BuyerSet(address account, bool value);
        event SellerSet(address account, bool value);
    }

    contract AppToken is DSToken, AppTokenEvents {

        mapping(address => bool) buyers;
        mapping(address => bool) sellers;

        function setBuyer(address buyer, bool value) {
            buyers[buyer] = value;
            BuyerSet(buyer, value);
        }

        function setSeller(address seller, bool value) {
            sellers[seller] = value;
            SellerSet(seller, value);
        }
    }

And here is a contract to test it

::

    pragma solidity ^0.4.0;

    import "ds-test/test.sol";
    import "token.sol";

    contract TokenUser {}

    contract AppTokenTest is DSTest, AppTokenEvents {

        TokenUser user;
        AppToken token;

        function setUp() {
            user = new TokenUser();
            token = new AppToken();
        }

        // This test will pass
        function testEvents() {
            expectEventsExact(token);
            BuyerSet(user, true);
            BuyerSet(user, false);
            SellerSet(this, true);

            token.setBuyer(user, true);
            token.setBuyer(user, false);

            token.setSeller(this, true);
        }

        // This test will fail
        function testEventsIncorrect() {
            expectEventsExact(token);
            BuyerSet(user, true);
            SellerSet(this, true);

            token.setBuyer(user, true);
            token.setBuyer(user, false);

            token.setSeller(this, true);
        }
    }

.. _unit_test_api:

API Reference
=============

These are the modifiers, functions, and events that are available when your test contract inherits from ``DSTest``, and the different flags available when running ``dapp test``

Function Modifiers
------------------

+-------------+-----------------------------------------------------+
| Modifier    | Description                                         |
+=============+=====================================================+
| logs_gas( ) | Logs the amount of gas that your test case consumes |
+-------------+-----------------------------------------------------+

Assertions Functions
--------------------

+-------------------------------------------------+
| Function                                        |  
+=================================================+
|  assert(bool condition)                         |
+-------------------------------------------------+
|  assertEq(address a, address b)                 |
+-------------------------------------------------+
|  assertEq(bytes32 a, bytes32 b)                 |
+-------------------------------------------------+
|  assertEq(int a, int b)                         |
+-------------------------------------------------+
|  assertEq(uint a, uint b)                       |
+-------------------------------------------------+
|  assertEq0(bytes a, bytes b)                    |
+-------------------------------------------------+
|  expectEventsExact(address target)              |
+-------------------------------------------------+

Logging Events
--------------

These events will log information to the console, making them useful for debugging.

+--------------------------------------------------------------+
| Event                                                        |
+==============================================================+
| logs(bytes)                                                  |
+--------------------------------------------------------------+
| log_bytes32(bytes32)                                         |
+--------------------------------------------------------------+
| log_named_bytes32(bytes32 key, bytes32 val)                  |
+--------------------------------------------------------------+
| log_named_address(bytes32 key, address val)                  |
+--------------------------------------------------------------+
| log_named_int(bytes32 key, int val)                          |
+--------------------------------------------------------------+
| log_named_uint(bytes32 key, uint val)                        |
+--------------------------------------------------------------+
| log_named_decimal_int(bytes32 key, int val, uint decimals)   |
+--------------------------------------------------------------+
| log_named_decimal_uint(bytes32 key, uint val, uint decimals) |
+--------------------------------------------------------------+

Dapp Test Flags
---------------

+------------+-------------------------------------------------------------------+
| Flag       | Description                                                       |
+============+===================================================================+
| -v         | Logs events to the command line for failing tests                 |
+------------+-------------------------------------------------------------------+
| -r <REGEX> | Only run tests who's name matches the provided regular expression |
+------------+-------------------------------------------------------------------+

