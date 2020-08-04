/*
  This file runs `hevm equivalence` against the yul optimizer test suite from the solidity repo.
  This is a corpus of ~400 tests used to ensure that the yul optimizer produces equivalent code.
*/

{ pkgs, solidity, solc, solver }:

let

  # --- binaries ---

  awk = "${pkgs.gawk}/bin/awk";
  cat = "${pkgs.coreutils}/bin/cat";
  echo = "${pkgs.coreutils}/bin/echo";
  grep = "${pkgs.gnugrep}/bin/grep";
  hevm = "${pkgs.hevm}/bin/hevm";
  mkdir = "${pkgs.coreutils}/bin/mkdir";
  mktemp = "${pkgs.coreutils}/bin/mktemp";
  rm = "${pkgs.coreutils}/bin/rm";
  sed = "${pkgs.gnused}/bin/sed";
  tee = "${pkgs.coreutils}/bin/tee";
  timeout = "${pkgs.coreutils}/bin/timeout";

  # --- test classification ----

  ignored = [

    # --- timeout (investigate) ----

    "controlFlowSimplifier/terminating_for_nested.yul"
    "controlFlowSimplifier/terminating_for_nested_reversed.yul"

    # --- unbounded loop ---

    "commonSubexpressionEliminator/branches_for.yul"
    "commonSubexpressionEliminator/loop.yul"
    "conditionalSimplifier/clear_after_if_continue.yul"
    "conditionalSimplifier/no_opt_if_break_is_not_last.yul"
    "conditionalUnsimplifier/clear_after_if_continue.yul"
    "conditionalUnsimplifier/no_opt_if_break_is_not_last.yul"
    "expressionSimplifier/inside_for.yul"
    "forLoopConditionIntoBody/cond_types.yul"
    "forLoopConditionIntoBody/simple.yul"
    "fullSimplify/inside_for.yul"
    "fullSuite/devcon_example.yul"
    "fullSuite/loopInvariantCodeMotion.yul"
    "fullSuite/no_move_loop_orig.yul"
    "loadResolver/loop.yul"
    "loopInvariantCodeMotion/multi.yul"
    "loopInvariantCodeMotion/recursive.yul"
    "loopInvariantCodeMotion/simple.yul"
    "redundantAssignEliminator/for_branch.yul"
    "redundantAssignEliminator/for_break.yul"
    "redundantAssignEliminator/for_continue.yul"
    "redundantAssignEliminator/for_decl_inside_break_continue.yul"
    "redundantAssignEliminator/for_deep_noremove.yul"
    "redundantAssignEliminator/for_deep_simple.yul"
    "redundantAssignEliminator/for_multi_break.yul"
    "redundantAssignEliminator/for_nested.yul"
    "redundantAssignEliminator/for_rerun.yul"
    "redundantAssignEliminator/for_stmnts_after_break_continue.yul"
    "rematerialiser/branches_for1.yul"
    "rematerialiser/branches_for2.yul"
    "rematerialiser/for_break.yul"
    "rematerialiser/for_continue.yul"
    "rematerialiser/for_continue_2.yul"
    "rematerialiser/for_continue_with_assignment_in_post.yul"
    "rematerialiser/no_remat_in_loop.yul"
    "ssaTransform/for_reassign_body.yul"
    "ssaTransform/for_reassign_init.yul"
    "ssaTransform/for_reassign_post.yul"
    "ssaTransform/for_simple.yul"

    # --- unexpected symbolic arg ---

    # OpCreate2
    "expressionSimplifier/create2_and_mask.yul"

    # OpCreate
    "expressionSimplifier/create_and_mask.yul"
    "expressionSimplifier/large_byte_access.yul"

    # OpMload
    "yulOptimizerTests/expressionSplitter/inside_function.yul"
    "fullInliner/double_inline.yul"
    "fullInliner/inside_condition.yul"
    "fullInliner/large_function_multi_use.yul"
    "fullInliner/large_function_single_use.yul"
    "fullInliner/no_inline_into_big_global_context.yul"
    "fullSimplify/invariant.yul"
    "fullSuite/abi_example1.yul"
    "ssaAndBack/for_loop.yul"
    "ssaAndBack/multi_assign_multi_var_if.yul"
    "ssaAndBack/multi_assign_multi_var_switch.yul"
    "ssaAndBack/two_vars.yul"
    "ssaTransform/multi_assign.yul"
    "ssaTransform/multi_decl.yul"
    "expressionSplitter/inside_function.yul"
    "fullSuite/ssaReverseComplex.yul"

    # OpMstore
    "commonSubexpressionEliminator/function_scopes.yul"
    "commonSubexpressionEliminator/variable_for_variable.yul"
    "expressionSplitter/trivial.yul"
    "fullInliner/multi_return.yul"
    "fullSimplify/constant_propagation.yul"
    "fullSimplify/identity_rules_complex.yul"
    "fullSuite/medium.yul"
    "loadResolver/memory_with_msize.yul"
    "loadResolver/merge_known_write.yul"
    "loadResolver/merge_known_write_with_distance.yul"
    "loadResolver/merge_unknown_write.yul"
    "loadResolver/reassign_value_expression.yul"
    "loadResolver/second_mstore_with_delta.yul"
    "loadResolver/second_store_with_delta.yul"
    "loadResolver/simple.yul"
    "loadResolver/simple_memory.yul"
    "fullSuite/ssaReverse.yul"
    "rematerialiser/cheap_caller.yul"
    "rematerialiser/non_movable_instruction.yul"
    "ssaAndBack/multi_assign.yul"
    "ssaAndBack/multi_assign_if.yul"
    "ssaAndBack/multi_assign_switch.yul"
    "ssaAndBack/simple.yul"
    "ssaReverser/simple.yul"

    # OpMstore8
    "loadResolver/memory_with_different_kinds_of_invalidation.yul"

    # OpRevert
    "ssaAndBack/ssaReverse.yul"
    "redundantAssignEliminator/for_continue_3.yul"
    "controlFlowSimplifier/terminating_for_revert.yul"

    # --- invalid test ---
    # https://github.com/ethereum/solidity/issues/9500

    "commonSubexpressionEliminator/object_access.yul"
    "expressionSplitter/object_access.yul"
    "fullSuite/stack_compressor_msize.yul"
    "varNameCleaner/function_names.yul"

    # --- stack too deep ---

    "fullSuite/abi2.yul"
    "fullSuite/aztec.yul"
    "stackCompressor/inlineInBlock.yul"
    "stackCompressor/inlineInFunction.yul"
    "stackCompressor/unusedPrunerWithMSize.yul"
    "wordSizeTransform/function_call.yul"
    "fullInliner/no_inline_into_big_function.yul"

    # --- wrong number of args ---

    "wordSizeTransform/functional_instruction.yul"
    "wordSizeTransform/if.yul"
    "wordSizeTransform/or_bool_renamed.yul"
    "wordSizeTransform/switch_1.yul"
    "wordSizeTransform/switch_2.yul"
    "wordSizeTransform/switch_3.yul"
    "wordSizeTransform/switch_4.yul"
    "wordSizeTransform/switch_5.yul"

    # --- typed yul ---

    "expressionSplitter/typed.yul"
    "expressionInliner/simple.yul"
    "expressionInliner/with_args.yul"
    "disambiguator/variables_inside_functions.yul"
    "disambiguator/switch_statement.yul"
    "disambiguator/if_statement.yul"
    "disambiguator/for_statement.yul"
    "disambiguator/funtion_call.yul"
    "disambiguator/long_names.yul"
    "disambiguator/variables.yul"
    "disambiguator/variables_clash.yul"
    "conditionalSimplifier/add_correct_type.yul"
    "conditionalSimplifier/add_correct_type_wasm.yul"
    "fullInliner/multi_return_typed.yul"
    "functionGrouper/empty_block.yul"
    "functionGrouper/multi_fun_mixed.yul"
    "functionGrouper/nested_fun.yul"
    "functionGrouper/single_fun.yul"
    "functionHoister/empty_block.yul"
    "functionHoister/multi_mixed.yul"
    "functionHoister/nested.yul"
    "functionHoister/single.yul"
    "mainFunction/empty_block.yul"
    "mainFunction/multi_fun_mixed.yul"
    "mainFunction/nested_fun.yul"
    "mainFunction/single_fun.yul"
    "ssaTransform/typed.yul"
    "ssaTransform/typed_for.yul"
    "ssaTransform/typed_switch.yul"
    "varDeclInitializer/typed.yul"
  ];

  # --- test scripts ---

  # Compiles a yul file to evm and prints the resulting bytecode to stdout
  # Propogates the status code of the solc invocation
  compile = pkgs.writeShellScript "compile" ''
    out=$(${mktemp})
    ${solc} --yul --yul-dialect evm $1 > $out 2>&1
    status=$?
    ${cat} $out | ${awk} 'f{print;f=0} /Binary representation:/{f=1}'
    exit $status
  '';

  # takes a yul program and ensures memory is symbolic by prepending
  # `calldatacopy(0,0,1024)`. (calldata is symbolic, but memory starts empty).
  # This forces the exploration of more branches, and makes the test vectors a
  # little more thorough.
  forceSymbolicMemory = pkgs.writeShellScript "forceSymbolicMemory" ''
    in=$(${cat} /dev/stdin)

    # empty programs
    if [ "$in" == "{ }" ]; then
      ${echo} "$in"
      exit 0
    fi

    # object notation
    # add a calldatacopy after all 'code {'
    if [ "$(${echo} $in | head -n 1 | ${awk} '{print $1;}')" == "object" ]; then
      ${echo} "$in" | ${sed} '/code\ {/ a calldatacopy(0,0,1024)'
      exit 0
    fi

    # simple notation
    # add a calldatacopy after the first {
    ${echo} "$in" | ${sed} -z '0,/^\s*{/s//{\ncalldatacopy(0,0,1024)/'
  '';

  # Takes two Yul files, compiles them to EVM bytecode and checks equivalence.
  compareTwoFiles = pkgs.writeShellScript "runSingleTest" ''
    a_bin=$(${compile} $1)
    if [ $? -eq 1 ]
    then
        ${echo} "Could not compile first Yul source. ($1)"
        ${cat} $1
        exit
    fi

    b_bin=$(${compile} $2)
    if [ $? -eq 1 ]
    then
        ${echo} "Could not compile second Yul source. ($2)"
        ${cat} $2
        exit
    fi

    if [[ "$a_bin" == "$b_bin" ]]
    then
        ${echo} "Bytecodes are the same."
        exit
    fi

    ${echo} "Checking bytecode equivalence: $a_bin vs $b_bin"
    ${timeout} 30s ${hevm} equivalence --solver $3       \
                                       --code-a "$a_bin" \
                                       --code-b "$b_bin" \
                                       --smttimeout 20000
    status=$?

    if [[ $status == 1 ]]
    then
        ${echo} hevm execution failed
        ${echo} "file1:"
        ${cat} $1
        ${echo} "-------------"
        ${echo} "file2:"
        ${cat} $2
        exit
    fi

    if [[ $status == 124 ]]
    then
        ${echo} "hevm timeout."
        ${echo} "file1:"
        ${cat} $1
        ${echo} "-------------"
        ${echo} "file2:"
        ${cat} $2
        exit
    fi
  '';

  runAllTests = let
    prefix = "${solidity}/test/libyul/yulOptimizerTests";
  in pkgs.writeShellScript "runAllTests" ''
    solver=$1

    check_equiv()
    {
        # Takes one file which follows the Solidity Yul optimizer unit tests format,
        # extracts both the nonoptimized and the optimized versions, and checks equivalence.

        ignoredTests=(${toString ignored})
        testName=$(${echo} "$1" | ${grep} -oP "^${prefix}/\K.*")

        ${echo} "---------------------------------------------"
        ${echo} "executing test: $testName with $2"

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

        ${compareTwoFiles} $file1 $file2 $2
        ${rm} $file1 $file2
    }

    for filename in ${prefix}/**/*.yul; do
      check_equiv $filename $solver
    done

    ${echo}
    ${echo} "Time taken (${solver}):"
    ${echo} -------------------------------------------
  '';

in
pkgs.runCommand "yulEquivalence-${solver}" {} ''
  results=$out
  time ${runAllTests} ${solver} | ${tee} $out

  ${echo}
  ${echo} "Summary (${solver}):"
  ${echo} -------------------------------------------

  set +e
  total="$(${grep} -c 'executing test:' $results)"
  passed="$(${grep} -c 'No discrepancies found' $results)"
  ignored="$(${grep} -c 'is ignored, skipping' $results)"
  same_bytecode="$(${grep} -c 'Bytecodes are the same' $results)"
  no_compile_first="$(${grep} -c 'Could not compile first Yul source' $results)"
  no_compile_second="$(${grep} -c 'Could not compile second Yul source' $results)"
  hevm_timeout="$(${grep} -c 'hevm timeout' $results)"
  hevm_failed="$(${grep} -c 'hevm execution failed' $results)"
  set -e

  ${echo} ran: $total
  ${echo} passed: $passed
  ${echo} ignored: $ignored
  ${echo} same bytecode: $same_bytecode
  ${echo} could not compile first program: $no_compile_first
  ${echo} could not compile second program: $no_compile_second
  ${echo} hevm timeout: $hevm_timeout
  ${echo} hevm execution failed: $hevm_failed
  ${echo}

  if [ $no_compile_first != 0 ]  || \
     [ $no_compile_second != 0 ] || \
     [ $hevm_timeout != 0 ]      || \
     [ $hevm_failed != 0 ]
  then
    exit 1
  else
    exit 0
  fi
''
