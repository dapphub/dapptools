{ lib, fetchurl, makeWrapper, stdenv, ocaml, gawk, isabelle2011-1, cvc3, perl
, wget, ... }:

let

  version = "1.4.3";
  src = fetchurl {

    # Originally from "https://tla.msr-inria.inria.fr/tlaps/dist/${version}/tlaps-${version}.tar.gz";
    url = "https://github.com/chris-martin/tla-plus/raw/5c9786746f6a2ba74e031279eb858bd9a1c59613/tlaps-${version}.tar.gz";
    sha256 = "1w5z3ns5xxmhmp8r4x2kjmy3clqam935gmvx82imyxrr1bamx6gf";
  };

  mkModule = { name, meta }: args:
    stdenv.mkDerivation (args // {
      inherit src;
      name = "tlaps-${name}-${version}";
      preConfigure = "cd ${name}";
      meta = {
        homepage = "http://tla.msr-inria.inria.fr/tlaps/content/Home.html";
        # https://tla.msr-inria.inria.fr/tlaps/content/Download/License.html
        license = with lib.licenses; [ bsd2 ];
      } // meta;
    });

  modules = {
    isabelle = mkModule {
      name = "isabelle";
      meta = {};
    } {
      buildInputs = [ ocaml isabelle2011-1 cvc3 perl ];
      buildPhase = "#";
      installPhase = ''
        runHook preBuild

        mkdir -pv "$out"
        export HOME="$out"

        pushd "${isabelle2011-1}/Isabelle2011-1/src/Pure"
        isabelle make
        popd

        # Use a modified version of the command in the Makefile
        # that avoids needing LaTeX dependencies
        isabelle usedir -b -i true Pure TLA+

        runHook postBuild
      '';
    };

    zenon = mkModule {
      name = "zenon";
      meta = {};
    } {
      buildInputs = [ ocaml ];
      configurePhase = ''
        runHook preConfigure
        ./configure --prefix "$out"
        runHook postConfigure
      '';
    };

    tlapm = mkModule {
      name = "tlapm";
      meta = {
        description = "The TLA+ Proof System (TLAPS)";
        longDescription = ''
          Mechanically checks TLA+ proofs. TLA+ is a general-purpose formal specification
          language that is particularly useful for describing concurrent and distributed
          systems. The TLA+ proof language is declarative, hierarchical, and scalable to
          large system specifications. It provides a consistent abstraction over the
          various "backend" verifiers. The current release of TLAPS does not perform
          temporal reasoning, and it does not handle some features of TLA+.
        '';
      };
    } {

      buildInputs = [ makeWrapper ocaml gawk wget ];

      configurePhase = ''
        runHook preConfigure
        ./configure --prefix $out
        runHook postConfigure
      '';

      postInstall = with modules; ''
        wrapProgram "$out/bin/tlapm" \
          --prefix PATH : "${isabelle2011-1}/bin:${zenon}/bin" \
          --prefix ISABELLE_PATH : "${modules.isabelle}/.isabelle/Isabelle2011-1/heaps/polyml-5.5.2_x86-linux"
      '';
    };

  };

  all = with modules; [ tlapm isabelle zenon ];

in modules // { inherit all; }
