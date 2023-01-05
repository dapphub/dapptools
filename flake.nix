{
  description = "dapptools";

  inputs = {
    # same as in default.nix
    nixpkgs.url = "github:NixOS/nixpkgs/aa576357673d609e618d87db43210e49d4bb1789";
  };

  nixConfig = {
    # required to build hevm
    allow-import-from-derivation = true;
    extra-substituters = [ "https://dapp.cachix.org" ];
    extra-trusted-public-keys = [ "dapp.cachix.org-1:9GJt9Ja8IQwR7YW/aF0QvCa6OmjGmsKoZIist0dG+Rs=" ];
  };

  outputs = { self, nixpkgs }:
    let
      supportedSystems = [
        "aarch64-linux"

        "x86_64-darwin"
        "x86_64-linux"
      ];

      forAllSystems = nixpkgs.lib.genAttrs supportedSystems;
      nixpkgsFor = forAllSystems (system: import nixpkgs {
        inherit system;
        overlays = [ (import ./overlay.nix) ];
      });
    in
    {
      packages =
        forAllSystems (system: {
          inherit (nixpkgsFor.${system}) dapp ethsign hevm seth solc;
        });

      apps =
        forAllSystems (system:
          nixpkgs.lib.genAttrs [ "dapp" "ethsign" "hevm" "seth" "solc" ] (name: {
            type = "app";
            program = "${self.packages.${system}.${name}}/bin/${name}";
          }));
    };
}
