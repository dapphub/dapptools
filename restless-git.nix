let pkgs = import <nixpkgs> {}; in {
  "restless-git" = pkgs.haskellPackages.callPackage ./default.nix {};
}
