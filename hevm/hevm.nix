let
  pkgs = import <dapphub> {};
  haskellPackages = pkgs.haskellPackages;
  profiling = haskellPackages.override {
    overrides = self: super: {
      mkDerivation = args: super.mkDerivation (args // {
        enableLibraryProfiling = true;
      });
    };
  };
in rec {
  hevm = (haskellPackages.callPackage ./default.nix {}).overrideAttrs (old: rec {
    buildInputs = [pkgs.solc];
  });
  hevmProfiling = (profiling.callPackage ./default.nix {}).overrideAttrs (old: rec {
    buildInputs = [pkgs.solc];
  });
}
