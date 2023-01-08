{
  description = "dapptools";

  inputs = {
    # same as in default.nix
    nixpkgs.url = "github:NixOS/nixpkgs/2dea8991d89b9f1e78d874945f78ca15f6954289";
  };

  nixConfig = {
    # required to build hevm
    allow-import-from-derivation = true;
    extra-substituters = [ "https://dapp.cachix.org" ];
    extra-trusted-public-keys = [ "dapp.cachix.org-1:9GJt9Ja8IQwR7YW/aF0QvCa6OmjGmsKoZIist0dG+Rs=" ];
    log-lines = 50;
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
          inherit (nixpkgsFor.${system}) dapp ethsign hevm seth solc solc-static-versions;
        });

      apps =
        forAllSystems (system:
          nixpkgs.lib.genAttrs [ "dapp" "ethsign" "hevm" "seth" "solc" ] (name: {
            type = "app";
            program = "${self.packages.${system}.${name}}/bin/${name}";
          }));
    };
}
