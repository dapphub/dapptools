{
  description = "dapptools";

  inputs = {
    # same as in default.nix
    nixpkgs.url = "github:NixOS/nixpkgs/aa576357673d609e618d87db43210e49d4bb1789";
    ethereum-hevm.url = "github:ethereum/hevm";
  };

  nixConfig = {
    extra-substituters = [ "https://dapp.cachix.org" ];
    extra-substituters-public-keys = [ "dapp.cachix.org-1:9GJt9Ja8IQwR7YW/aF0QvCa6OmjGmsKoZIist0dG+Rs=" ];
  };

  outputs = { self, nixpkgs, ethereum-hevm }:
    let
      supportedSystems = [
        "aarch64-linux"

        "x86_64-darwin"
        "x86_64-linux"
      ];

      forAllSystems = nixpkgs.lib.genAttrs supportedSystems;
      nixpkgsFor = forAllSystems (system: import nixpkgs {
        inherit system;
        overlays = [
          (import ./overlay.nix {
            hevm = ethereum-hevm.packages.${system}.hevm;
          })
        ];
      });
    in
    {
      packages =
        forAllSystems (system:
          let
            pkgs = nixpkgsFor.${system};
            dapptoolsSrc = pkgs.callPackage (import ./nix/dapptools-src.nix) { };
          in
          rec {
            hevm = pkgs.hevm;
            dapp = pkgs.callPackage (import ./src/dapp) { inherit dapptoolsSrc seth; };
            ethsign = pkgs.callPackage (import ./src/ethsign) { };
            seth = pkgs.callPackage (import ./src/seth) { inherit dapptoolsSrc ethsign; };
          });

      apps =
        forAllSystems (system:
          nixpkgs.lib.genAttrs [ "dapp" "ethsign" "hevm" "seth" ] (name: {
            type = "app";
            program = "${self.packages.${system}.${name}}/bin/${name}";
          }));
    };
}
