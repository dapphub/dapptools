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
  , deps ? []
  , solc ? pkgs.solc
  , test-hevm ? pkgs.dapp2.test-hevm
  , solcFlags ? ""
  , doCheck ? true
  , extract ? false
  , ... }@args:
    pkgs.stdenv.mkDerivation ( rec {
      inherit doCheck extract;

      buildInputs = [ pkgs.jq solc test-hevm ];
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

      buildPhase = args.buildPhase or ''
        opts=(--combined-json=abi,bin,bin-runtime,srcmap,srcmap-runtime,ast,metadata --overwrite)

        mkdir -p out

        mapfile -t files < <(find "$src" -name '*.sol')
        json_file="out/dapp.sol.json"
        (set -x; ${solc}/bin/solc $REMAPPINGS "''${opts[@]}" $solcFlags "''${files[@]}" > "$json_file")

        mkdir -p lib
        source <(echo "$LIBSCRIPT")

        if [ "$extract" == 1 ]; then
          mapfile -t contracts < <(<"$json_file" jq '.contracts|keys[]' -r | sort -u -t: -k2 | sort)
          data=$(<"$json_file" jq '.contracts' -r)
          total=''${#contracts[@]}
          echo "Extracting build data... [Total: $total]"
          for path in "''${contracts[@]}"; do
            fileName="''${path#*:}"
            contract=$(echo "$data" | jq '.["'"$path"'"]')
            echo "$contract" | jq '.["abi"]' -r > "out/$fileName.abi"
            echo "$contract" | jq '.["bin"]' -r > "out/$fileName.bin"
            echo "$contract" | jq '.["bin-runtime"]' -r > "out/$fileName.bin-runtime"
            echo "$contract" | jq '.["metadata"]' -r > "out/$fileName.metadata"
          done
        fi
      '';

      checkPhase = args.checkPhase or ''
        DAPP_OUT=out dapp2-test-hevm
      '';

      installPhase = args.installPhase or ''
        mkdir -p $out/dapp/$name
        cp -r $src $out/dapp/$name/src
        cp -r lib $out/dapp/$name/lib
        cp -r out $out/dapp/$name/out
      '';
    } // args)
