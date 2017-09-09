{ .. }:

let
  system = (system: (import ./default.nix { inherit system; }));
  linux = system "x86_64-linux";
  darwin = system "x86_64-darwin";

in {
  dapphub-linux = with linux.pkgs; {
    inherit dapp;
    inherit hsevm;
  };

  dapphub-darwin = with darwin.pkgs; {
    inherit dapp;
    inherit hsevm;
  };
}
