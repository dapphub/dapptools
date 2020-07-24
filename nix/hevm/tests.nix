{ pkgs, lib }:
let
  awk = "${pkgs.gawk}/bin/awk";
  cat = "${pkgs.coreutils}/bin/cat";
  echo = "${pkgs.coreutils}/bin/echo";
  grep = "${pkgs.gnugrep}/bin/grep";
  hevm = "${pkgs.hevm}/bin/hevm";
  mktemp = "${pkgs.coreutils}/bin/mktemp";
  timeout = "${pkgs.timeout}/bin/timeout";
  rm = "${pkgs.coreutils}/bin/rm";
  sed = "${pkgs.gnused}/bin/sed";
  solc = "${pkgs.solc-versions.solc_0_6_7}/bin/solc";

  solidity = pkgs.fetchFromGitHub {
    owner = "ethereum";
    repo = "solidity";
    rev = "b8d736ae0c506b1b3cf5d2456af67e8dc2c0ca8e"; # v0.6.7
    sha256 = "1zqfcfgy70hmckxb3l59rabdpzj7gf1vzg6kkw4xz0c6lzy7mrpz";
  };

  compile = pkgs.writeShellScript "compile" ''
    out=$(${mktemp})
    ${solc} --yul --yul-dialect evm $1 > $out 2>&1
    status=$?
    cat $out | ${awk} 'f{print;f=0} /Binary representation:/{f=1}'
    exit $status
  '';

  run-yul-equivalence = pkgs.writeShellScript "run-yul-equivalence" ''
    # Takes two Yul files, compiles them to EVM bytecode and checks equivalence.

    a_bin=$(${compile} $1)
    if [ $? -eq 1 ]
    then
        ${echo} "Could not compile first Yul source."
        ${cat} $1
        exit 1
    fi

    b_bin=$(${compile} $2)
    if [ $? -eq 1 ]
    then
        ${echo} "Could not compile second Yul source."
        ${cat} $1
        exit 1
    fi

    if [[ "$a_bin" == "$b_bin" ]]
    then
        ${echo} "Bytecodes are the same."
        exit 0
    fi

    ${echo} "Checking bytecode equivalence: $a_bin vs $b_bin"
    ${pkgs.coreutils}/bin/timeout 10s ${pkgs.hevm}/bin/hevm equivalence --code-a "$a_bin" --code-b "$b_bin" --smttimeout 1000
    if [[ $? == 124 ]]
    then
        ${echo} "HEVM timeout."
        ${cat} $1
        exit 1
    fi

    exit 0
  '';

  prefixes = {
    yulOptimizerTests = "${solidity}/test/libyul/yulOptimizerTests";
  };

  ignored-tests = {
    yulOptimizerTests = [
      # unbounded loop
      "commonSubexpressionEliminator/branches_for.yul"
      "commonSubexpressionEliminator/loop.yul"
      "conditionalSimplifier/no_opt_if_break_is_not_last.yul"
      "conditionalUnsimplifier/no_opt_if_break_is_not_last.yul"

      # cannot compile
      "commonSubexpressionEliminator/object_access.yul"
      "conditionalSimplifier/add_correct_type.yul"
      "conditionalSimplifier/add_correct_type_wasm.yul"
    ];
  };
in
{
  yulEquivalence = pkgs.runCommand "hevm-equivalence" {} ''
    check_equiv()
    {
        # Takes one file which follows the Solidity Yul optimizer unit tests format,
        # extracts both the nonoptimized and the optimized versions, and checks equivalence.

        ${echo} "---------------------------------------------"
        ${echo} "checking $1"

        ignoredTests=(${toString ignored-tests.yulOptimizerTests})
        testName=$(${echo} "$1" | ${grep} -oP "^${prefixes.yulOptimizerTests}/\K.*")

        if [[ " ''${ignoredTests[@]} " =~ " ''${testName} " ]]; then
            ${echo} "$testName is ignored, skipping"
            return 0
        fi

        nonopt_src=$(${mktemp})
        ${sed} '0,/^\/\/ step:/d' $1 | ${sed} -e 's!\/\/!!' > $nonopt_src
        ${run-yul-equivalence} $1 $nonopt_src
        ${rm} $nonopt_src
    }

    for filename in ${prefixes.yulOptimizerTests}/**/*.yul; do
      check_equiv $filename
    done
  '';
}
