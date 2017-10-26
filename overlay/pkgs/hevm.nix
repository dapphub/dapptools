{ pkgs, stdenv }:
  pkgs.lib.overrideDerivation
    (pkgs.haskell.lib.justStaticExecutables
      (pkgs.haskellPackages.callPackage ./cabal/hevm.nix {}))
  (attrs: {
    src = pkgs.fetchFromGitHub {
      owner = "dapphub";
      repo = "hevm";
      rev = "v0.10.9";
      sha256 = "00yvajyfkkhz1df7rd5ib343pdfid4qf3c61pph22jb2hhzas96b";
    };

    postInstall = ''
      wrapProgram $out/bin/hevm \
         --suffix PATH : "${pkgs.lib.makeBinPath [pkgs.bash pkgs.coreutils pkgs.git]}"
    '';

    enableSeparateDataOutput = true;
    buildInputs = attrs.buildInputs ++ [pkgs.solc];
    nativeBuildInputs = attrs.nativeBuildInputs ++ [pkgs.makeWrapper];
    maintainers = [pkgs.lib.maintainers.dbrock];
  })
