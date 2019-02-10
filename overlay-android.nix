self: super:

let
  # There's a funny bug in the LLVM test suite where some test scripts
  # specify that the instruction "and" should not occur in the result,
  # but this fails because of the "and" in the name "android".
  #
  # We patch these scripts to look for "and" as a separate word.
  patch-llvm = x: x // {
    llvm = x.llvm.overrideAttrs (old: {
      postPatch = (old.postPatch or "") + ''
        for x in $(find test -name '*.ll'); do
          if grep -q -- '-NOT: and' "$x"; then
            echo "Replacing in $x"
            sed -i.bak 's/-NOT: and$/-NOT: {{\\band\\b}}/' "$x"
            grep -- '-NOT: {{' "$x" || true
          fi
        done
      '';
    });
  };

in {
  llvmPackages   = patch-llvm super.llvmPackages;
  llvmPackages_5 = patch-llvm super.llvmPackages_5;
  llvmPackages_6 = patch-llvm super.llvmPackages_6;
  llvmPackages_7 = patch-llvm super.llvmPackages_7;
}
