self: super:
let
  stable = import ./overlay.nix { inherit self super; flavor = "stable"; };
  master = import ./overlay.nix { inherit self super; flavor = "master"; };
in
  stable // { inherit master; }

