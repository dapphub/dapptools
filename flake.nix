{
  description = "dapptools";

  inputs = {
    # same as in default.nix
    nixpkgs.url = "github:NixOS/nixpkgs/2dea8991d89b9f1e78d874945f78ca15f6954289";
    ethereum-hevm.url = "github:ethereum/hevm";
  };

  nixConfig = {
    extra-substituters = [ "https://dapp.cachix.org" ];
    extra-trusted-public-keys = [ "dapp.cachix.org-1:9GJt9Ja8IQwR7YW/aF0QvCa6OmjGmsKoZIist0dG+Rs=" ];
    log-lines = 50;
  };

  outputs = { self, nixpkgs, ethereum-hevm }:
    let
      supportedSystems = [
        "x86_64-darwin"
        "x86_64-linux"
      ];

      forAllSystems = nixpkgs.lib.genAttrs supportedSystems;
      nixpkgsFor = forAllSystems (system: import nixpkgs {
        inherit system;
        overlays = [
          (import ./overlay.nix)
          (final: prev: {
            hevm = ethereum-hevm.packages.${system}.hevm;
          })
        ];
      });
    in
    {
      packages =
        forAllSystems (system: {
          inherit (nixpkgsFor.${system}) dapp ethsign hevm seth solc solc-versions solc-static-versions;
        });

      apps =
        forAllSystems (system:
          nixpkgs.lib.genAttrs [ "dapp" "ethsign" "hevm" "seth" "solc" ] (name: {
            type = "app";
            program = "${self.packages.${system}.${name}}/bin/${name}";
          }));

      devShells =
        forAllSystems (system: {
          default = nixpkgs.legacyPackages.${system}.mkShellNoCC {
            name = "dapp";
            buildInputs = with nixpkgsFor.${system}; [
              dapp
              ethsign
              hevm
              seth
              solc
            ];
          };
        });
    };
}
