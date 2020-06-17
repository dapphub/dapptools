{ pkgs }: let
  remappings = xs:
    builtins.foldl' pkgs.lib.mergeAttrs {}
      (builtins.map
        (x: {
          "${x.name}/" = "${x}/dapp/${x.name}/src/";
          "${x.name}" = "${x}/dapp/${x.name}/src/index.sol";
         } // x.remappings)
         xs);
  libPaths = xs:
    builtins.foldl' pkgs.lib.mergeAttrs {}
      (builtins.map
        (x: {
          "${x.name}" = "${x}/dapp/${x.name}";
         } // x.libPaths)
         xs);
in
  pkgs.lib.makeOverridable (
    { doCheck ? true
    , extract ? true
    , deps ? []
    , solc ? pkgs.solc
    , test-hevm ? pkgs.dapp2.test-hevm
    , solcFlags ? ""
    , ...
    } @ attrs:
      pkgs.stdenv.mkDerivation (rec {
        inherit doCheck extract;
        buildInputs = [ solc pkgs.jq ]
          ++ (pkgs.lib.optional (test-hevm != null) test-hevm);
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
    )
