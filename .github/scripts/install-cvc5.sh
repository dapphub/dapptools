#!/bin/bash


set -eux

mkdir -p $HOME/.local/bin;

travis_retry() {
  cmd=$*
  $cmd || (sleep 2 && $cmd) || (sleep 10 && $cmd)
}

fetch_cvc5_linux() {
  VER="$1"
  wget "https://github.com/cvc5/cvc5/releases/download/$VER/cvc5-$VER-x86_64-linux-opt"
  chmod +x "cvc5-$VER-x86_64-linux-opt"
  mv "cvc5-$VER-x86_64-linux-opt" "$HOME/.local/bin/cvc5"
  echo "Downloaded cvc5 $VER"
}

fetch_cvc5_macos() {
  VER="$1"
  wget "https://github.com/cvc5/cvc5/releases/download/$VER/cvc5-$VER-macos-opt"
  chmod +x "cvc5-$VER-macos-opt"
  mv "cvc5-$VER-macos-opt" "$HOME/.local/bin/cvc5"
  echo "Downloaded cvc5 $VER"
}

if [ "$HOST_OS" = "Linux" ]; then
    travis_retry fetch_cvc5_linux "1.8"
else
    travis_retry fetch_cvc5_macos "1.8"
fi
