/*
  This file explores the contracts in the test suite for the solc SMTChecker module
  with hevm and compares the results with those found by the SMTChecker
*/
{ pkgs, solidity, solc, solver }:

let

  # --- binaries ---

  bc = "${pkgs.bc}/bin/bc";
  basename = "${pkgs.coreutils}/bin/basename";
  cat = "${pkgs.coreutils}/bin/cat";
  echo = "${pkgs.coreutils}/bin/echo";
  grep = "${pkgs.gnugrep}/bin/grep";
  hevm = "${pkgs.hevm}/bin/hevm";
  jq = "${pkgs.jq}/bin/jq";
  mkdir = "${pkgs.coreutils}/bin/mkdir";
  mktemp = "${pkgs.coreutils}/bin/mktemp";
  sed = "${pkgs.gnused}/bin/sed";
  tee = "${pkgs.coreutils}/bin/tee";
  tr = "${pkgs.coreutils}/bin/tr";
  timeout = "${pkgs.coreutils}/bin/timeout";

  # --- test classification ---

  bounded = [

    "functions/functions_recursive.sol"
    "functions/functions_recursive_indirect.sol"
    "functions/recursive_multi_return.sol"
    "invariants/loop_basic.sol"
    "invariants/loop_basic_for.sol"
    "loops/do_while_1_fail.sol"
    "loops/for_loop_array_assignment_storage_memory.sol"
    "loops/for_loop_array_assignment_storage_storage.sol"
    "loops/while_1_infinite.sol"
    "loops/while_1.sol"
    "loops/while_loop_simple_5.sol"
    "loops/do_while_1_false_positives.sol"
    "loops/while_loop_array_assignment_storage_storage.sol"
    "loops/while_nested_break.sol"
    "loops/while_nested_break_fail.sol"
    "operators/delete_array.sol"
    "operators/delete_array_index_2d.sol"
    "operators/delete_array_2d.sol"
    "types/array_dynamic_3_fail.sol"
  ];

  ignored = [

    # --- constructor arguments ---

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
    "inheritance/constructor_hierarchy_mixed_chain_with_params.sol"
    "inheritance/constructor_state_variable_init_chain_run_all.sol"
    "inheritance/constructor_state_variable_init_chain_run_all_2.sol"

    # --- infinite loops ---

    "functions/functions_trivial_condition_for.sol"
    "functions/functions_trivial_condition_for_only_call.sol"
    "functions/functions_trivial_condition_while.sol"
    "functions/functions_trivial_condition_while_only_call.sol"
    "loops/do_while_continue.sol"
    "loops/for_loop_1.sol"
    "loops/for_loop_2.sol"
    "loops/for_loop_3.sol"
    "loops/for_loop_5.sol"
    "loops/for_loop_6.sol"
    "loops/for_loop_trivial_condition_1.sol"
    "loops/for_loop_trivial_condition_2.sol"
    "loops/for_loop_trivial_condition_3.sol"
    "loops/while_2_fail.sol"
    "loops/while_loop_simple_2.sol"
    "loops/while_loop_simple_3.sol"
    "loops/while_loop_simple_4.sol"
    "loops/while_1.sol"
    "loops/while_1_infinite.sol"

    # --- hevm timeout ---

    "types/array_dynamic_3_fail.sol"
    "types/array_aliasing_storage_1.sol"
    "types/array_aliasing_memory_1.sol"
    "types/array_aliasing_memory_2.sol"
    "types/array_aliasing_memory_3.sol"
    "loops/do_while_1_false_positives.sol"

    # --- unsupported opcodes ---

    # OpExtcodesize
    "functions/this_fake.sol"
    "functions/functions_external_1.sol"
    "functions/functions_external_2.sol"
    "functions/functions_external_3.sol"
    "functions/functions_external_4.sol"
    "typecast/function_type_to_function_type_external.sol"

    # OpJump
    "complex/slither/external_function.sol"

    # OpCalldatacopy
    "loops/for_loop_array_assignment_memory_memory.sol"
    "loops/for_loop_array_assignment_memory_storage.sol"
    "loops/while_loop_array_assignment_memory_memory.sol"
    "loops/while_loop_array_assignment_memory_storage.sol"
    "types/address_call.sol"
    "types/address_delegatecall.sol"
    "types/address_staticcall.sol"
    "types/array_aliasing_storage_2.sol"
    "types/array_aliasing_storage_3.sol"
    "types/array_aliasing_storage_5.sol"
    "types/array_branch_1d.sol"
    "types/array_branches_1d.sol"
    "types/array_dynamic_parameter_1.sol"
    "types/array_dynamic_parameter_1_fail.sol"
    "types/bytes_1.sol"
    "types/bytes_2.sol"
    "types/bytes_2_fail.sol"
    "types/mapping_unsupported_key_type_1.sol"
    "types/function_type_array_as_reference_type.sol"

    # OpBlockhash
    "special/blockhash.sol"

    # OpBalance
    "types/address_balance.sol"
    "types/address_transfer.sol"
    "types/address_transfer_2.sol"
    "types/address_transfer_insufficient.sol"

    # --- missing hevm coverage ---

    # contract level knowledge required
    "functions/internal_call_with_assertion_1.sol"
    "functions/internal_multiple_calls_with_assertion_1.sol"
    "inheritance/constructor_state_variable_init_chain_run_all.sol"
    "inheritance/implicit_only_constructor_hierarchy.sol"
    "inheritance/implicit_constructor_hierarchy.sol"
    "invariants/state_machine_1.sol"
    "types/mapping_4.sol"

    # knowledge of the solidity memory model required
    "types/array_static_aliasing_memory_5.sol"

    # --- missing smt checker coverage ---

    # potential out of bounds array access
    "operators/delete_array_index.sol"
    "operators/delete_array.sol"
    "operators/delete_function.sol"
    "operators/delete_array_2d.sol"
    "types/array_branches_2d.sol"
    "types/array_branches_3d.sol"
    "types/array_dynamic_1.sol"
    "types/array_dynamic_2.sol"
    "types/array_static_2.sol"
    "types/array_static_3.sol"
    "types/tuple_assignment_array.sol"
    "types/array_dynamic_3.sol"
    "types/array_static_1.sol"

    # bounds checking on enum args during abi decoding
    "typecast/enum_to_uint_max_value.sol"
    "types/enum_explicit_values.sol"
    "types/enum_transitivity.sol"

    # --- smt checker false positives ---

    "typecast/cast_different_size_1.sol"
    "typecast/cast_larger_3.sol"
    "typecast/cast_smaller_2.sol"
    "typecast/cast_smaller_3.sol"
    "types/mapping_as_parameter_1.sol"
    "types/mapping_as_local_var_1.sol"

    # --- solc 0.8 ---

    "modifiers/modifier_code_after_placeholder.sol" # assertion can't trigger post 0.8 due to overflow checking
    "special/many.sol" # no longer compiles

  ];

  # --- test scripts ---

  strings = {
    executing = "Executing test:";
    pass = "PASS: hevm and SMTChecker agree!";
    ignore = "SKIP: test ignored";
    smtReports = "FAIL: SMTChecker reports assertion violation whereas HEVM reports safe.";
    hevmReports = "FAIL: SMTChecker reports safe whereas HEVM reports assertion violation.";
    timeout = "FAIL: hevm timeout";
    hevmAssertionViolation = "Assertion violation found";
    hevmErrorInBranch = "branch(es) errored while exploring";
    hevmCouldNotExplore = "FAIL: hevm was unable to explore the contract";
    smtCheckerFailed = "SKIP: smtChecker failed";
  };

  smtCheckerTests = "${solidity}/test/libsolidity/smtCheckerTests";

  testName = pkgs.writeShellScript "testName" ''
    ${echo} "$1" | ${grep} -oP "^${smtCheckerTests}/\K.*"
  '';

  divider = pkgs.writeShellScript "divider" ''
    ${echo}
    ${echo} ============================================================================================
    ${echo}
  '';

  # Takes one Solidity file, compiles it to bytecode and explores `hevm
  # symbolic` on all contracts within.
  # $1 == input file
  # $2 == hevm smt backend
  checkWithHevm = pkgs.writeShellScript "checkWithHevm" ''

    # write json file to store for later debugging
    testName=$(${testName} $1)
    json=$out/jsonFiles/$testName.json
    ${mkdir} -p $(${echo} "''${json%/*}/")

    ${solc} --combined-json=srcmap,srcmap-runtime,bin,bin-runtime,ast,metadata,storage-layout,abi $1 2> /dev/null > $json

    contracts=($(${cat} $json | ${jq} .contracts | ${jq} keys | ${jq} -r '. | @sh' | ${tr} -d \'))

    explore() {
      set -x
      hevm_output=$(${timeout} 90s ${hevm} symbolic --smttimeout 20000 --code "$1" --solver "$2" --json-file "$3" $4 2>&1)
      status=$?
      set +x

      # handle timeouts
      if [[ $status == 124 ]]; then ${echo} && ${echo} "${strings.timeout}"; fi

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

      iterations=""
      boundedTests=(${toString bounded})
      if [[ " ''${boundedTests[@]} " =~ " ''${testName} " ]]; then
        iterations="--max-iterations 3"
      fi

      ${echo}
      ${echo} exploring runtime bytecode:
      bin_runtime=$(${jq} -r --arg c $contract -c '.contracts[$c]."bin-runtime"' $json)
      explore "$bin_runtime" "$2" "$json" "$iterations"
    done

    exit 0
  '';

  # Takes one Solidity file in the format of an SMTChecker test,
  # runs `hevm symbolic` on it and compares results against the SMTChecker expectations.
  # $1 == input file
  # $2 == hevm smt backend
  runSingleTest = pkgs.writeShellScript "runSingleTest" ''
    print_test() {
      ${echo}
      ${echo} "*** Input File: $(${testName} $1) ***"
      ${echo}
      ${cat} "$1"
    }

    testName=$(${testName} $1)

    ${divider}
    ${echo} "${strings.executing} $(${testName} $1)"

    ignoredTests=(${toString ignored})
    if [[ " ''${ignoredTests[@]} " =~ " ''${testName} " ]]; then
        ${echo} "${strings.ignore}"
        exit 0
    fi

    ${grep} -q 'Error trying to invoke SMT solver.' $1
    if [ $? == 0 ]; then ${echo} ${strings.smtCheckerFailed} && exit; fi
    ${grep} -q 'Assertion checker does not yet implement' $1
    if [ $? == 0 ]; then ${echo} ${strings.smtCheckerFailed} && exit; fi
    ${grep} -q 'Assertion checker does not yet support' $1
    if [ $? == 0 ]; then ${echo} ${strings.smtCheckerFailed} && exit; fi

    hevm_output=$(${checkWithHevm} $1 $2 2>&1)
    echo "$hevm_output"

    ${grep} -q '${strings.timeout}' <<< "$hevm_output"
    if [ $? == 0 ]; then print_test "$1" && exit; fi
    ${grep} -q '${strings.hevmCouldNotExplore}' <<< "$hevm_output"
    if [ $? == 0 ]; then print_test "$1" && exit; fi

    ${grep} -q '${strings.hevmAssertionViolation}' <<< "$hevm_output"
    hevm_violation=$?

    ${grep} -q 'Assertion violation happens' $1
    smtchecker_assertion_violation=$?
    ${grep} -q 'Division by zero happens here' $1
    smtchecker_division_by_zero=$?

    smtchecker_violation=1
    if [ $smtchecker_assertion_violation -eq 0 ] || [ $smtchecker_division_by_zero -eq 0 ]; then
      smtchecker_violation=0
    fi

    if [ $hevm_violation -ne 0 ] && [ $smtchecker_violation -eq 0 ]; then
      ${echo}
      ${echo} "${strings.smtReports}"
      print_test "$1"
      exit
    elif [ $hevm_violation -eq 0 ] && [ $smtchecker_violation -ne 0 ]; then
      ${echo}
      ${echo} "${strings.hevmReports}"
      print_test "$1"
      exit
    fi

    ${echo}
    ${echo} ${strings.pass}
  '';

  runAllTests = pkgs.writeShellScript "runAllTests-${solver}" ''
    for filename in ${smtCheckerTests}/**/*.sol; do
      ${runSingleTest} $filename ${solver}
    done

    ${divider}
    ${echo} "Time Taken (${solver}):"
    ${echo} ---------------------------------
    ${echo}

    exit 0
  '';
in
pkgs.runCommand "smtCheckerTests-${solver}" {} ''
  mkdir $out
  results=$out/results

  time ${runAllTests} | ${tee} $results

  set +e
  total="$(${grep} -c '${strings.executing}' $results)"
  passed="$(${grep} -c '${strings.pass}' $results)"
  ignored="$(${grep} -c '${strings.ignore}' $results)"
  smt_failed="$(${grep} -c '${strings.smtCheckerFailed}' $results)"
  hevm_failed="$(${grep} -c '${strings.hevmCouldNotExplore}' $results)"
  smt_reports="$(${grep} -c '${strings.smtReports}' $results)"
  hevm_reports="$(${grep} -c '${strings.hevmReports}' $results)"
  hevm_timeout="$(${grep} -c '${strings.timeout}' $results)"
  set -e

  ${echo}
  ${echo} "Summary (${solver}):"
  ${echo} ---------------------------------
  ${echo}
  ${echo} ran: $total
  ${echo} passed: $passed
  ${echo} skipped: $(${bc} <<< "$ignored + $smt_failed")
  ${echo} 'failed (smt reports assertion, hevm reports safe):' $smt_reports
  ${echo} 'failed (hevm reports assertion, smt reports safe):' $hevm_reports
  ${echo} hevm timeout: $hevm_timeout
  ${echo} hevm failure: $hevm_failed
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

