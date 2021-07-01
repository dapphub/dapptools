{ pkgs }:

let
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
  {
    name
  , src
  , dapp ? pkgs.dapp
  , deps ? []
  , solc ? "${pkgs.solc}/bin/solc"
  , shouldFail ? false
  , dappFlags ? ""
  , doCheck ? true
  , ... }@args:
    pkgs.stdenv.mkDerivation ( rec {
      inherit doCheck;

      buildInputs = [ pkgs.jq dapp ];
      passthru = {
        remappings = remappings deps;
        libPaths = libPaths deps;
      };

      REMAPPINGS = pkgs.lib.concatStringsSep "\n"
        (pkgs.lib.mapAttrsToList
          (k: v: k + "=" + v)
          passthru.remappings);

      LIBSCRIPT =
        pkgs.lib.mapAttrsToList
          (k: v: ''
            ln -s ${v} lib/${k}
          '')
          passthru.libPaths;
      buildPhase = ''
        mkdir -p out
        export DAPP_SOLC=${solc}
        export DAPP_REMAPPINGS="$REMAPPINGS"
        export DAPP_SRC=$src
        export DAPP_OUT=out
        export DAPP_LINK_TEST_LIBRARIES=1
        mkdir -p lib
        source <(echo "$LIBSCRIPT")
        dapp build
      '';

      checkPhase = let
        cmd = "DAPP_SKIP_BUILD=1 dapp test ${dappFlags}";
      in
        if shouldFail
          then "${cmd} && exit 1 || echo 0"
          else cmd;

      installPhase = ''
        mkdir -p $out/dapp/$name
        cp -r $src $out/dapp/$name/src
        cp -r lib $out/dapp/$name/lib
        cp -r out $out/dapp/$name/out
      '';
    } // args)
