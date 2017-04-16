let
  pkgs = import <nixpkgs> {};
  stdenv = pkgs.stdenv;
in rec {
  seth = stdenv.mkDerivation rec {
  name = "seth";
  src = ./.;
  buildInputs = [pkgs.makeWrapper];
  makeFlags = ["prefix=$(out)"];
  buildPhase = "true";
  postInstall =
    let
      path = pkgs.lib.makeBinPath [
        pkgs.bc pkgs.curl pkgs.ethabi pkgs.git pkgs.jshon pkgs.solc
      ];
    in
      ''wrapProgram "$out/bin/seth" --prefix PATH : "${path}"'';
  };
}
