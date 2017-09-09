{ }:

let
  system = (system: (import ./default.nix { config = {}; inherit system; }));
  linux = system "x86_64-linux";
  darwin = system "x86_64-darwin";

{
  dapphub-linux = {
    linux.pkgs.dapp
    linux.pkgs.hsevm
  };

  dapphub-darwin = {
    darwin.pkgs.dapp
    darwin.pkgs.hsevm
  };
}
