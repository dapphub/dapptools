# Dapp tools by DappHub

Hello!

This repository contains the source code of several dapp tools
hand-crafted and maintained by DappHub, along with various Nix
expressions which are all bundled up into a Nix overlay.

To install tools from this repository, you only need to install the
Nix package manager and then run

    make install

which will build and install these tools into your Nix profile.

If you want to cache the binary builds across several computers, Nix
provides several ways of doing this, the most convenient being
the [Cachix](https://cachix.org) service.

We maintain a public cache which you can use by installing Cachix and
then running

    cachix use dapp

followed by restarting your Nix daemon.

## TODO

  [ ] Better installation instructions.
  [ ] Explain how to use the cache without installing Cachix.
  [ ] Make a script that installs Nix and/or configures the cache.
