{ pkgs, solidity, solc }:

let

  # --- binaries ---

  awk = "${pkgs.gawk}/bin/awk";
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

  # --- test scripts ---

  smtCheckerTests = "${solidity}/test/libsolidity/smtCheckerTests";

  # Takes one Solidity file, compiles it to bytecode and explores `hevm
  # symbolic` on all contracts within.
  # $1 == input file
  # $2 == hevm smt backend
  checkWithHevm = pkgs.writeShellScript "checkWithHevm" ''
    # write json file to store for later debugging
    testName=$(${echo} "$1" | ${grep} -oP "^${smtCheckerTests}/\K.*")
    json=$out/jsonFiles/$testName.json
    ${mkdir} -p $(${echo} "''${json%/*}/")

    ${solc} --combined-json=srcmap,srcmap-runtime,bin,bin-runtime,ast,metadata,storage-layout,abi $1 2> /dev/null > $json

    contracts=($(${cat} $json | ${jq} .contracts | ${jq} keys | ${jq} -r '. | @sh' | ${tr} -d \'))

    for contract in "''${contracts[@]}"; do
      bytecode=$(${jq} -r --arg c $contract -c '.contracts[$c]."bin-runtime"' $json)
      set +e
      set -x
      ${timeout} 10s ${hevm} symbolic --code "$bytecode" --solver $2 --json-file "$json"
      status=$?
      set +x
      set -e

      if [[ $status == 124 ]]; then echo "FAIL: hevm timeout"; fi
    done

    exit 0
  '';

  # Takes one Solidity file in the format of an SMTChecker test,
  # runs `hevm symbolic` on it and compares results against the SMTChecker expectations.
  # $1 == input file
  # $2 == hevm smt backend
  runSingleTest = pkgs.writeShellScript "runSingleTest" ''
    #!/usr/bin/sh

    ${echo}
    ${echo} ----------------------------------------
    ${echo} "Executing test at: $1"

    hevm_output=$(${checkWithHevm} $1 $2)
    echo "$hevm_output"

    ${grep} -q 'FAIL:' <<< "$hevm_output"
    hevm_failure=$?
    if [ $hevm_failure == 0 ]; then exit; fi

    ${grep} -q 'Assertion violation found' <<< "$hevm_output"
    hevm_violation=$?
    ${grep} -q 'Assertion violation happens' $1
    smtchecker_violation=$?

    if [ $hevm_violation -ne 0 ] && [ $smtchecker_violation -eq 0 ]; then
      ${echo} "SMTChecker reports assertion violation whereas HEVM reports safe."
      exit
    elif [ $hevm_violation -eq 0 ] && [ $smtchecker_violation -ne 0 ]; then
      ${echo} "SMTChecker reports safe whereas HEVM reports assertion violation."
      exit
    fi

    ${echo} hevm and SMTChecker agree!
  '';
in
pkgs.runCommand "smtCheckerTests" {} ''
  ${mkdir} $out

  for filename in ${smtCheckerTests}/control_flow/*.sol; do
    ${runSingleTest} $filename z3 | ${tee} $out/z3
  done

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
  ${echo} same bytecode: $same_bytecode
  ${echo} ignored: $ignored
  ${echo} passed: $passed
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
    return 1
  else
    return 0
  fi
''

