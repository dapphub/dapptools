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

  # --- test scripts ---

  smtCheckerTests = "${solidity}/test/libsolidity/smtCheckerTests";

  divider = pkgs.writeShellScript "divider" ''
    ${echo}
    ${echo} ------------------------------------------------------------------------------------------------------------------------------------------
    ${echo}
  '';

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

    explore() {
      set -x
      ${timeout} 10s ${hevm} symbolic --code $1 --solver $2 --json-file $
      status=$?
      set +x
      if [[ $status == 124 ]]; then echo "FAIL: hevm timeout"; fi
    }

    for contract in "''${contracts[@]}"; do
      ${echo}
      ${echo} --- exploring $(basename $contract) ---

      ${echo}
      ${echo} exploring init bytecode:
      bin=$(${jq} -r --arg c $contract -c '.contracts[$c]."bin"' $json)
      explore $bin $2 $json

      ${echo}
      ${echo} exploring runtime bytecode:
      bin_runtime=$(${jq} -r --arg c $contract -c '.contracts[$c]."bin-runtime"' $json)
      explore $bin_runtime $2 $json
    done

    exit 0
  '';

  # Takes one Solidity file in the format of an SMTChecker test,
  # runs `hevm symbolic` on it and compares results against the SMTChecker expectations.
  # $1 == input file
  # $2 == hevm smt backend
  runSingleTest = pkgs.writeShellScript "runSingleTest" ''
    #!/usr/bin/sh

    ${divider}
    ${echo} "Executing test at: $1"

    hevm_output=$(${checkWithHevm} $1 $2 2>&1)
    echo "$hevm_output"

    ${grep} -q 'FAIL:' <<< "$hevm_output"
    hevm_failure=$?
    if [ $hevm_failure == 0 ]; then exit; fi

    ${grep} -q 'Assertion violation found' <<< "$hevm_output"
    hevm_violation=$?
    ${grep} -q 'Assertion violation happens' $1
    smtchecker_violation=$?

    if [ $hevm_violation -ne 0 ] && [ $smtchecker_violation -eq 0 ]; then
      ${echo}
      ${echo} "FAIL: SMTChecker reports assertion violation whereas HEVM reports safe."
      exit
    elif [ $hevm_violation -eq 0 ] && [ $smtchecker_violation -ne 0 ]; then
      ${echo}
      ${echo} "FAIL: SMTChecker reports safe whereas HEVM reports assertion violation."
      exit
    fi

    ${echo}
    ${echo} PASS: hevm and SMTChecker agree!
  '';
in
pkgs.runCommand "smtCheckerTests" {} ''
  ${mkdir} $out
  results=$out/cvc4

  for filename in ${smtCheckerTests}/**/*.sol; do
    ${runSingleTest} $filename cvc4 | ${tee} -a $results
  done

  set +e
  total="$(${grep} -c 'Executing test at:' $results)"
  passed="$(${grep} -c 'PASS: hevm and SMTChecker agree!' $results)"
  smt_reports="$(${grep} -c 'FAIL: SMTChecker reports assertion violation whereas HEVM reports safe.' $results)"
  hevm_reports="$(${grep} -c 'FAIL: SMTChecker reports safe whereas HEVM reports assertion violation.' $results)"
  hevm_timeout="$(${grep} -c 'FAIL: hevm timeout' $results)"
  set -e


  ${divider}
  ${echo} Summary:
  ${echo} ---------------------------------
  ${echo}
  ${echo} ran: $total
  ${echo} passed: $passed
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

