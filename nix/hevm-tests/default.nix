{ pkgs, lib }:
rec {
  solidity = pkgs.fetchFromGitHub {
    owner = "ethereum";
    repo = "solidity";
    rev = "b8d736ae0c506b1b3cf5d2456af67e8dc2c0ca8e"; # v0.6.7
    sha256 = "1zqfcfgy70hmckxb3l59rabdpzj7gf1vzg6kkw4xz0c6lzy7mrpz";
  };

  yulEquivalence = (import ./yul-equivalence.nix) { inherit pkgs lib solidity; };
}
