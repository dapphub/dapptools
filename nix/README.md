# Dapphub's Nixpkgs channel

This directory can act as a root `<nixpkgs>`.  We use a submodule to
pin a specific version of the upstream, and our `default.nix` loads
that, configured with our overlay.

You can also use the `overlay` directory as an overlay on whatever
version of nixpkgs you wish.
