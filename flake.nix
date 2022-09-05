{
  description = ''
    A suite of Ethereum focused CLI tools following the Unix design philosophy,
    favoring composability, configurability and extensibility.
  '';

  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs?rev=aa576357673d609e618d87db43210e49d4bb1789";

  outputs = { self, nixpkgs, flake-utils }:
    let
      systems =
        flake-utils.lib.eachDefaultSystem (system:
          let
            pkgs = import nixpkgs {
              inherit system;
              overlays = [ (import ./overlay.nix) ];
            };
          in
          {
            packages = pkgs.solc-static-versions // {
              inherit (pkgs)
                solidityPackage buildDappPackage
                solc hevm hevmUnwrapped jays jshon
                seth dapp ethsign token go-ethereum-unlimited
                qrtx qrtx-term secp256k1;
            };
          }
        );
    in
    systems // {
      overlays = {
        default = import ./overlay.nix;
      };
    };
}
