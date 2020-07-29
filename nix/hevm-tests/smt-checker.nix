{ pkgs, solidity, solc }:

let

  # --- binaries ---

  awk = "${pkgs.gawk}/bin/awk";
  basename = "${pkgs.coreutils}/bin/basename";
  cat = "${pkgs.coreutils}/bin/cat";
  echo = "${pkgs.coreutils}/bin/echo";
  grep = "${pkgs.gnugrep}/bin/grep";
  hevm = "${pkgs.hevm}/bin/hevm";
  jq = "${pkgs.jq}/bin/jq";
  mkdir = "${pkgs.coreutils}/bin/mkdir";
  mktemp = "${pkgs.coreutils}/bin/mktemp";
  nix = "${pkgs.nix}/bin/nix";
  rev = "${pkgs.utillinux}/bin/rev";
  rm = "${pkgs.coreutils}/bin/rm";
  sed = "${pkgs.gnused}/bin/sed";
  tee = "${pkgs.coreutils}/bin/tee";
  tr = "${pkgs.coreutils}/bin/tr";
  timeout = "${pkgs.coreutils}/bin/timeout";

  # --- test classification ---

  ignored = [

    # constructor arguments

    "functions/constructor_hierarchy_3.sol"
    "functions/constructor_hierarchy_4.sol"
    "functions/constructor_hierarchy_diamond_2.sol"
    "functions/constructor_hierarchy_diamond_3.sol"
    "functions/constructor_hierarchy_diamond_empty_middle.sol"
    "functions/constructor_hierarchy_diamond.sol"
    "functions/constructor_hierarchy_empty_chain.sol"
    "functions/constructor_hierarchy_empty_middle_no_invocation.sol"
    "functions/constructor_hierarchy_empty_middle.sol"
    "functions/constructor_hierarchy_mixed_chain_empty_base.sol"
    "functions/constructor_hierarchy_mixed_chain.sol"
    "functions/constructor_hierarchy_mixed_chain_local_vars.sol"
    "functions/constructor_hierarchy_mixed_chain_with_params.sol"
    "functions/constructor_hierarchy_mixed_chain_with_params_2.sol"
    "functions/constructor_simple.sol"
    "functions/constructor_state_value_inherited.sol"
    "functions/constructor_state_value.sol"

    # unbounded looping

    "functions/functions_recursive.sol"
    "functions/functions_recursive_indirect.sol"
    "functions/recursive_multi_return.sol"
    "functions/functions_trivial_condition_for.sol"
    "functions/functions_trivial_condition_for_only_call.sol"
    "functions/functions_trivial_condition_while.sol"
    "functions/functions_trivial_condition_while_only_call.sol"

    # --- unsupported opcodes ---

    # OpExtcodesize

    "functions/this_fake.sol"
    "functions/functions_external_1.sol"
    "functions/functions_external_2.sol"
    "functions/functions_external_3.sol"
    "functions/functions_external_4.sol"

    # --- contract level knowledge required ---

    "functions/internal_call_with_assertion_1.sol"
    "functions/internal_multiple_calls_with_assertion_1.sol"

  ];

  # --- test scripts ---

  strings = {
    exeucting = "Executing test:";
    pass = "PASS: hevm and SMTChecker agree!";
    ignore = "SKIP: test ignored";
    smtReports = "FAIL: SMTChecker reports assertion violation whereas HEVM reports safe.";
    hevmReports = "FAIL: SMTChecker reports safe whereas HEVM reports assertion violation.";
    timeout = "FAIL: hevm timeout";
    hevmAssertionViolation = "Assertion violation found";
    hevmErrorInBranch = "branch(es) errored while exploring";
    hevmCouldNotExplore = "FAIL: hevm was unable to explore the contract";
  };

  smtCheckerTests = "${solidity}/test/libsolidity/smtCheckerTests";

  testName = pkgs.writeShellScript "testName" ''
    ${echo} "$1" | ${grep} -oP "^${smtCheckerTests}/\K.*"
  '';

  divider = pkgs.writeShellScript "divider" ''
    ${echo}
    ${echo} --------------------------------------------------------------------------------------------
    ${echo}
  '';

  # Takes one Solidity file, compiles it to bytecode and explores `hevm
  # symbolic` on all contracts within.
  # $1 == input file
  # $2 == hevm smt backend
  checkWithHevm = pkgs.writeShellScript "checkWithHevm" ''

    # write json file to store for later debugging
    json=$out/jsonFiles/$(${testName} $1).json
    ${mkdir} -p $(${echo} "''${json%/*}/")

    ${solc} --combined-json=srcmap,srcmap-runtime,bin,bin-runtime,ast,metadata,storage-layout,abi $1 2> /dev/null > $json

    contracts=($(${cat} $json | ${jq} .contracts | ${jq} keys | ${jq} -r '. | @sh' | ${tr} -d \'))

    explore() {
      set -x
      hevm_output=$(${timeout} 10s ${hevm} symbolic --code "$1" --solver "$2" --json-file "$3" $4 2>&1)
      status=$?
      set +x

      # handle timeouts
      if [[ $status == 124 ]]; then ${echo} "${strings.timeout}"; fi

      # handle non assertion related hevm failures
      if [[ $status == 1 ]]; then
        ${grep} -q '${strings.hevmAssertionViolation}' <<< "$hevm_output"
        if [ $? == 0 ]; then return; fi
        ${echo} "${strings.hevmCouldNotExplore}"
      fi

      # handle errors in branches
      if [[ $status == 0 ]]; then
        ${grep} -q '${strings.hevmErrorInBranch}' <<< "$hevm_output"
        if [ $? == 1 ]; then return; fi
        ${echo} "${strings.hevmCouldNotExplore}"
      fi
    }

    for contract in "''${contracts[@]}"; do
      ${echo}
      ${echo} --- exploring $(basename $contract) ---

      ${echo}
      ${echo} exploring init bytecode:
      bin=$(${jq} -r --arg c $contract -c '.contracts[$c]."bin"' $json)
      explore "$bin" "$2" "$json" "--create"

      ${echo}
      ${echo} exploring runtime bytecode:
      bin_runtime=$(${jq} -r --arg c $contract -c '.contracts[$c]."bin-runtime"' $json)
      explore "$bin_runtime" "$2" "$json"
    done

    exit 0
  '';

  # Takes one Solidity file in the format of an SMTChecker test,
  # runs `hevm symbolic` on it and compares results against the SMTChecker expectations.
  # $1 == input file
  # $2 == hevm smt backend
  runSingleTest = pkgs.writeShellScript "runSingleTest" ''
    #!/usr/bin/sh

    testName=$(${testName} $1)

    ${divider}
    ${echo} "${strings.exeucting} $(${testName} $1)"

    ignoredTests=(${toString ignored})
    if [[ " ''${ignoredTests[@]} " =~ " ''${testName} " ]]; then
        ${echo} "${strings.ignore}"
        exit 0
    fi

    hevm_output=$(${checkWithHevm} $1 $2 2>&1)
    echo "$hevm_output"

    ${grep} -q '${strings.timeout}' <<< "$hevm_output"
    if [ $? == 0 ]; then exit; fi
    ${grep} -q '${strings.hevmCouldNotExplore}' <<< "$hevm_output"
    if [ $? == 0 ]; then exit; fi

    ${grep} -q '${strings.hevmAssertionViolation}' <<< "$hevm_output"
    hevm_violation=$?
    ${grep} -q 'Assertion violation happens' $1
    smtchecker_violation=$?

    if [ $hevm_violation -ne 0 ] && [ $smtchecker_violation -eq 0 ]; then
      ${echo}
      ${echo} "${strings.smtReports}"
      exit
    elif [ $hevm_violation -eq 0 ] && [ $smtchecker_violation -ne 0 ]; then
      ${echo}
      ${echo} "${strings.hevmReports}"
      exit
    fi

    ${echo}
    ${echo} ${strings.pass}
  '';
in
pkgs.runCommand "smtCheckerTests" {} ''
  ${mkdir} $out
  results=$out/cvc4

  for filename in ${smtCheckerTests}/functions/*.sol; do
    ${runSingleTest} $filename cvc4 | ${tee} -a $results
  done

  set +e
  total="$(${grep} -c '${strings.exeucting}' $results)"
  passed="$(${grep} -c '${strings.pass}' $results)"
  ignored="$(${grep} -c '${strings.ignore}' $results)"
  hevm_failed="$(${grep} -c '${strings.hevmCouldNotExplore}' $results)"
  smt_reports="$(${grep} -c '${strings.smtReports}' $results)"
  hevm_reports="$(${grep} -c '${strings.hevmReports}' $results)"
  hevm_timeout="$(${grep} -c '${strings.timeout}' $results)"
  set -e


  ${divider}
  ${echo} Summary:
  ${echo} ---------------------------------
  ${echo}
  ${echo} ran: $total
  ${echo} passed: $passed
  ${echo} ignored: $ignored
  ${echo} 'failed (smt reports assertion, hevm reports safe):' $smt_reports
  ${echo} 'failed (hevm reports assertion, smt reports safe):' $hevm_reports
  ${echo} hevm timeout: $hevm_timeout
  ${echo}

  if [ $hevm_timeout != 0 ] || \
     [ $smt_reports != 0 ]  || \
     [ $hevm_reports != 0 ]
  then
    exit 1
  else
    exit 0
  fi
''

