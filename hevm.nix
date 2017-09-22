let pkgs = import <nixpkgs> {};
    profiling = pkgs.haskellPackages.override {
      overrides = self: super: {
        mkDerivation = args: super.mkDerivation (args // {
          enableLibraryProfiling = true;
        });
      };
    };
in rec {
  hevm = (pkgs.haskellPackages.callPackage ./default.nix {}).overrideAttrs (old: rec {
    buildInputs = [pkgs.solc];
  });
  hevmProfiling = (profiling.callPackage ./default.nix {}).overrideAttrs (old: rec {
    buildInputs = [pkgs.solc];
  });
}
