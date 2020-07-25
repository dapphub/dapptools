{ pkgs, lib }:
let
  awk = "${pkgs.gawk}/bin/awk";
  cat = "${pkgs.coreutils}/bin/cat";
  echo = "${pkgs.coreutils}/bin/echo";
  grep = "${pkgs.gnugrep}/bin/grep";
  hevm = "${pkgs.hevm}/bin/hevm";
  mktemp = "${pkgs.coreutils}/bin/mktemp";
  timeout = "${pkgs.timeout}/bin/timeout";
  rev = "${pkgs.utillinux}/bin/rev";
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

  # ensures memory is symbolic by prepending `calldatacopy(0,0,1024)` to
  # every program. (calldata is symbolic, but memory starts empty). This
  # forces the exploration of more branches, and makes the test vectors a
  # little more thorough.
  forceSymbolicMemory = pkgs.writeShellScript "forceSymbolicMemory" ''
    in=$(${cat} /dev/stdin)

    # empty programs
    if [ "$in" == "{ }" ]; then
      echo "$in"
      exit 0
    fi

    # object notation
    if [ "$(${echo} $in | head -n 1 | ${awk} '{print $1;}')" == "object" ]; then
      ${echo} "$in" | ${sed} '/code\ {/ a calldatacopy(0,0,1024)'
      exit 0
    fi

    # simple notation
    ${echo} "$in"                            \
    | ${sed} 's/^{/{\n/'                     \
    | ${sed} '0,/{/a calldatacopy(0,0,1024)' \
    | ${rev} | ${sed} -e 's/}$/\n}/' | ${rev}
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
        ${cat} $2
        exit 1
    fi

    if [[ "$a_bin" == "$b_bin" ]]
    then
        ${echo} "Bytecodes are the same."
        exit 0
    fi

    ${echo} "Checking bytecode equivalence: $a_bin vs $b_bin"
    ${pkgs.coreutils}/bin/timeout 10s ${pkgs.hevm}/bin/hevm equivalence --code-a "$a_bin" --code-b "$b_bin" --smttimeout 1000
    status=$?
    if [[ $status == 1 ]]
    then
        ${echo} hevm execution failed
        ${echo} "file1:"
        ${cat} $1
        ${echo} "-------------"
        ${echo} "file2:"
        ${cat} $2
        exit 1
    fi
    if [[ $status == 124 ]]
    then
        ${echo} "hevm timeout."
        ${echo} "file1:"
        ${cat} $1
        ${echo} "-------------"
        ${echo} "file2:"
        ${cat} $2
        exit 1
    fi

    exit 0
  '';

  prefixes = {
    yulOptimizerTests = "${solidity}/test/libyul/yulOptimizerTests";
  };

  ignored-tests = {
    yulOptimizerTests = [
      # --- timeout investigate ----
      "controlFlowSimplifier/terminating_for_nested.yul"
      "controlFlowSimplifier/terminating_for_nested_reversed.yul"

      # --- unbounded loop ---

      "commonSubexpressionEliminator/branches_for.yul"
      "commonSubexpressionEliminator/loop.yul"
      "conditionalSimplifier/clear_after_if_continue.yul"
      "conditionalSimplifier/no_opt_if_break_is_not_last.yul"
      "conditionalUnsimplifier/clear_after_if_continue.yul"
      "conditionalUnsimplifier/no_opt_if_break_is_not_last.yul"

      # --- unexpected symbolic arg ---

      # OpMstore
      "commonSubexpressionEliminator/function_scopes.yul"
      "commonSubexpressionEliminator/variable_for_variable.yul"

      # --- cannot compile ---

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

        file1=$(${mktemp})
        ${cat} $1                \
        | ${sed} '/^\/\//d'      \
        | ${sed} -e '/^$/d'      \
        | ${forceSymbolicMemory} \
        > $file1

        file2=$(${mktemp})
        cat $1                      \
        | ${sed} '0,/^\/\/ step:/d' \
        | ${sed} -e 's!\/\/!!'      \
        | ${sed} -e '/^$/d'         \
        | ${sed} 's/^.//'           \
        | ${forceSymbolicMemory}    \
        > $file2

        ${run-yul-equivalence} $file1 $file2
        ${rm} $file1 $file2
    }

    for filename in ${prefixes.yulOptimizerTests}/**/*.yul; do
      check_equiv $filename
    done
  '';
}
