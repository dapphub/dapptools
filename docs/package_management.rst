.. _package_management:

###################
Package Management
###################

Dapp's package management feature allows you to install and publish smart contracts for the purposes of code reuse and discovery. It uses the newly-standardized `Ethereum Smart Contract Packaging Specification <https://github.com/ethereum/EIPs/issues/190>`_, or EthPM for short, which makes it easy for you to incorporate anyone's code into your project.


Installing Packages
-------------------

EthPM packages are always identified by their `IPFS <https://ipfs.io/>`_ address. Like Ethereum addresses, IPFS addresses are not easily read by humans. In the near future, a `registry contract <https://www.ethpm.com/>`_ will be deployed to the Ethereum blockchain for easy lookup of package addresses by unique human-friendly names. This will allow for a user experience identical to other package managers you might be more familiar with (e.g. ``npm install my-cool-package``). In the meantime, you can still install packages by referencing their IPFS address:

.. code:: bash

    $ dapp install ipfs://QmYsapuVLeihAANcYTbLxmCT4RWnZTtJ1WnNZ7c4zohCYW

This will cause dapp to pull the package from either DappHub's servers (by default) or any IPFS node that you configure it to talk to. It will install the code in your ``lib`` folder, where it can then be imported into your contracts by its package name (e.g. if I install the ``ds-token`` package, then you can access ``token.sol`` like so: ``import ds-token/token.sol``)

Publishing Packages
-------------------

When you are ready to make your Solidity available to the wider community, you can publish your package:



.. code:: bash

    $ dapp publish

Dapp will bundle up all your source code and depenedencies and start putting them into IPFS. When it has all your files' addresses, it bundles these and your ``Dappfile`` project metadata (e.g. version numbers, authors, deployment addresses) into an `ERC190 lockfile <https://github.com/ethpm/ethpm-spec/blob/master/release-lockfile.spec.md>`_ and puts that into IPFS as well. The returned address can then be used by anyone to locate and install the package.

Resolving Version Conflicts
---------------------------

Coming soon!

Discovering Packages
--------------------

Coming soon!