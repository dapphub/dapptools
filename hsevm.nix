let pkgs = import <nixpkgs> {};
    profiling = pkgs.haskellPackages.override {
      overrides = self: super: {
        mkDerivation = args: super.mkDerivation (args // {
          enableLibraryProfiling = true;
        });
      };
    };
in rec {
  hsevm = pkgs.haskellPackages.callPackage ./default.nix {};
  hsevmProfiling = profiling.callPackage ./default.nix {};
  test = pkgs.runCommand "hsevm" {} ''
    ${hsevm}/bin/hsevm
  '';
}
