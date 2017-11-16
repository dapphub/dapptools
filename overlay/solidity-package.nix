{ pkgs }: let
  remappings = xs:
    builtins.foldl' pkgs.lib.mergeAttrs {}
      (builtins.map
        (x: {
          "${x.name}/" = "${x}/dapp-src/${x.name}/";
         } // x.remappings)
         xs);
  libPaths = xs:
    builtins.foldl' pkgs.lib.mergeAttrs {}
      (builtins.map
        (x: {
          "${x.name}" = "${x}/dapp-src/${x.name}";
         } // x.libPaths)
         xs);
in
  attrs @ { deps ? [], ... }:
    pkgs.stdenv.mkDerivation (rec {
      buildInputs = [pkgs.dapp2.test-hevm pkgs.solc];
      passthru = {
        remappings = remappings deps;
        libPaths = libPaths deps;
      };

      REMAPPINGS =
        pkgs.lib.mapAttrsToList
          (k: v: k + "=" + v)
          passthru.remappings;

      LIBSCRIPT =
        pkgs.lib.mapAttrsToList
          (k: v: ''
            ln -s ${v} lib/${k}
          '')
          passthru.libPaths;

      builder = ./solidity-package.sh;
    } // attrs)
