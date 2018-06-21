.. _package_management:

###################
Package Management
###################

Dapp's package management feature allows you to both download and publish smart contracts for the purposes of code reuse and discovery. Recently, a new standard was created called the `Ethereum Smart Contract Packaging Specification <https://github.com/ethereum/EIPs/issues/190>`_, or EthPM for short. It uses `IPFS <https://ipfs.io/>`_ addresses to identify and distribute smart contract code in a decentralized manner. 

In the near future, a `registry contract <https://www.ethpm.com/>`_ will be deployed to the Ethereum blockchain for easy lookup of package addresses by unique human-friendly names. This will allow for a user experience identical to other package managers you might be more familiar with (e.g. ``npm install my-cool-package``). In the meantime, dapp uses `git submodules <https://git-scm.com/book/en/v2/Git-Tools-Submodules>`_ to mimic the experience of installing EthPM packages.

.. note::
    A good resource for working with git submodules can be found at `Chris Jean's blog <https://chrisjean.com/git-submodules-adding-using-removing-and-updating/>`_


Installing Packages
-------------------

Dapp will install a git submodule for you based on the github path you specify. If you don't specify a github user, dapp will choose dapphub as the default. Thus both of these commands will install code in your ``lib`` folder:

.. code:: bash

    # installing a submodule from  https://github.com/dapphub/ds-auth

    $ dapp install ds-auth

and:

.. code:: bash

    # installing a submodule from  https://github.com/apmilen/my-cool-package
    
    $ dapp install apmilen/my-cool-package 

Files in your installed packages can be imported in your source code by referencing the git submodule name:

::

    pragma solidity ^0.4.0;

    import "ds-auth/auth.sol";

    contract TestContract is DSAuth {}

Publishing Packages
-------------------

Since dapp package management currently works off git submodules, publishing code to the default branch of your repository is equivalent to releasing a new package.


Discovering Packages
--------------------

Coming soon!