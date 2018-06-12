{ pkgs }: let
  remappings = xs:
    builtins.foldl' pkgs.lib.mergeAttrs {}
      (builtins.map
        (x: {
          "${x.name}/" = "${x}/dapp/${x.name}/src/";
         } // x.remappings)
         xs);
  libPaths = xs:
    builtins.foldl' pkgs.lib.mergeAttrs {}
      (builtins.map
        (x: {
          "${x.name}" = "${x}/dapp/${x.name}/src";
         } // x.libPaths)
         xs);
in
  pkgs.lib.makeOverridable (
    attrs @ { test ? true, deps ? [], ... }:
      pkgs.stdenv.mkDerivation (rec {
        buildInputs = [pkgs.dapp2.test-hevm pkgs.solc];
        passthru = {
          remappings = remappings deps;
          libPaths = libPaths deps;
        };

        TEST = test;

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
    )
