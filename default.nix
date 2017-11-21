# This file acts like the main file of an entire <nixpkgs>.
# It imports a full nixpkgs from our pinned submodule,
# and extends it with our overlay.
#
# This is meant to be used as a channel.

{ pkgsPath ? null
, overlays ? []
, system ? null
}:

(
  (import ./nixpkgs) ({
    overlays = [(import ./overlay { flavor = "stable"; })];
  } // (
    if system != null then { inherit system; } else {}
  ))
) // {
  master = (import ./nixpkgs) ({
    overlays = [(import ./overlay { flavor = "master"; })];
  } // (
    if system != null then { inherit system; } else {}
  ));
}
