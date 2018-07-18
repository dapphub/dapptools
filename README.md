# Dapp tools by DappHub

Hello!

This repository contains the source code of several dapp tools
hand-crafted and maintained by DappHub, along with various Nix
expressions which are all bundled up into a Nix overlay.

To install tools from this repository follow these instructions:

### Install Nix

Make sure you are using Nix v2

    curl https://nixos.org/nix/install | sh

    # Run this or login again to use Nix
    . "$HOME/.nix-profile/etc/profile.d/nix.sh"

### Setup Cachix (optional)

If you want to cache the binary builds across several computers, Nix
provides several ways of doing this, the most convenient being
the [Cachix](https://cachix.org) service.

We maintain a public cache which you can use by installing Cachix 

    nix-env -if https://github.com/cachix/cachix/tarball/master --substituters https://cachix.cachix.org --trusted-public-keys cachix.cachix.org-1:eWNHQldwUO7G2VkjpnjDbWwy4KQ/HNxht7H4SSoMckM=

then running

    cachix use dapp

followed by restarting your Nix daemon (if on a multi-user Nix installation)

### Install our tools

To install tools from this repository, you only need to run

    nix-env -iA dapphub.{dapp,seth,hevm,evmdis}

which will build and install these tools into your Nix profile, optionally
fetching the binaries from the cache if you have configured it.

## TODO

  [ ] Better installation instructions.
  [ ] Explain how to use the cache without installing Cachix.
  [ ] Make a script that installs Nix and/or configures the cache.
