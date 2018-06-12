{ pkgs }: rec {

  remappings = xs:
    builtins.foldl' pkgs.lib.mergeAttrs {}
      (builtins.map
        (x: {
          "${x.name}/" = "${x}/src/";
          "${x.name}" = "${x}/src/index.sol";
         } // x.remappings)
         xs);

  libPaths = xs:
    builtins.foldl' pkgs.lib.mergeAttrs {}
      (builtins.map
        (x: {
          "${x.name}" = "${x}/src";
         } // x.libPaths)
         xs);

  dappPackage = attrs @ { dependencies ? [], ... }:
    pkgs.stdenv.mkDerivation (rec {
      buildInputs = [pkgs.dapp pkgs.solc];
      passthru = {
        remappings = remappings dependencies;
        libPaths = libPaths dependencies;
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

      builder = ./build-dapp.sh;
    } // attrs);

  dappsysPackage = { name, deps ? [], rev, sha256 }:
    dappPackage {
      inherit name;
      src = pkgs.fetchFromGitHub {
        inherit rev sha256;
        owner = "dapphub";
        repo = name;
      };
      dependencies = deps;
    };
}
