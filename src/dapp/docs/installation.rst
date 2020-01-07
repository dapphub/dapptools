
###################
Installing Dapp
###################

Installing with Nix
-------------------

Soon dapp will be completely distributed via the `Nix Package Manager <https://nixos.org/nix/>`_, which allows us to provide a consistent experience across all Unix-based operating systems (i.e. various Linux distros and OSX/Mac OS). `Getting started with Nix <https://nixos.org/nix/manual/#chap-quick-start>`_ is very easy and you will be glad you did it. Its purely functional approach is unique to the domain of package management and it makes Nix quite powerful. 

In the meantime, you will need to perform some manual steps to get dapp. This section will be updated as the installation process gets streamlined.

First `download ethrun <https://github.com/dapphub/ethrun/releases>`_, rename it to ``ethrun``, and move it to somewhere on your `$PATH <https://en.wikipedia.org/wiki/PATH_(variable)>`_:

.. code:: bash

    $ cp ethrun-v0.1.0-osx /usr/local/bin/ethrun

The rest of the installation process looks like this:

.. code:: bash

    $ curl https://nixos.org/nix/install | sh
    $ nix-channel --add https://nix.dapphub.com/pkgs/dapphub
    $ nix-channel --update
    $ nix-env -i seth solc
    $ git clone https://github.com/dapphub/dapp
    $ make link -C dapp

Dapp depends on some blockchain development tools that are very useful in their own right. In particular, you may find `seth <https://github.com/dapphub/seth>`_ convenient when working with any Ethereum client's `JSON RPC API <https://github.com/ethereum/wiki/wiki/JSON-RPC>`_. You can explore the full list of dependencies below.


Installing without Nix
----------------------

The program will expect you to have these dependencies installed: 


* `bc <https://www.gnu.org/software/bc/manual/html_mono/bc.html>`_
* `curl <https://curl.haxx.se/docs/manpage.html>`_
* `git <https://git-scm.com/>`_
* `node <https://nodejs.org/en/>`_
* `solc <https://solidity.readthedocs.io/en/develop/installing-solidity.html>`_
* `ethrun <https://github.com/dapphub/ethrun/releases>`_
* `seth <https://github.com/dapphub/seth>`_
* `jshon <http://kmkeen.com/jshon/>`_

None of these programs are too difficult to install, and it's likely that you will have the top half of the list on your computer already. Once you have them, installing dapp is as simple as: 

.. code:: bash

    $ make link

This will create a symlink to your repository in ``/usr/local``.

