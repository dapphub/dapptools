{ callPackage, buildEnv, ... }:

let

  core = callPackage ./core.nix {};

  tlaps = callPackage ./tlaps.nix {};

  toolbox = callPackage ./toolbox.nix {};

  full = buildEnv {
    name = "tla-plus-full";
    paths = core.all ++ tlaps.all ++ [ toolbox ];
  };

in core // tlaps // { inherit toolbox full; }
