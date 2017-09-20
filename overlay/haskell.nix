{ pkgs }: self: super: {
  restless-git =
    pkgs.haskell.lib.dontCheck
      (super.callPackage
        (import (pkgs.fetchFromGitHub {
          owner = "lessrest";
          repo = "restless-git";
          rev = "v0.5.0";
          sha256 = "0l2n556fzgkgnw7rhwfsj7438qyid8y9sghlhd3is154dspg0p9v";
        })) {});
}
