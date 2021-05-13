#!/bin/bash


set -eux

mkdir -p $HOME/.local/bin;

travis_retry() {
  cmd=$*
  $cmd || (sleep 2 && $cmd) || (sleep 10 && $cmd)
}

fetch_cvc4_linux() {
  VER="$1"
  wget "https://github.com/CVC4/CVC4/releases/download/$VER/cvc4-$VER-x86_64-linux-opt"
  chmod +x "cvc4-$VER-x86_64-linux-opt"
  mv "cvc4-$VER-x86_64-linux-opt" "$HOME/.local/bin/cvc4"
  echo "Downloaded cvc4 $VER"
}

fetch_cvc4_macos() {
  VER="$1"
  wget "https://github.com/CVC4/CVC4/releases/download/$VER/cvc4-$VER-macos-opt"
  chmod +x "cvc4-$VER-macos-opt"
  mv "cvc4-$VER-macos-opt" "$HOME/.local/bin/cvc4"
  echo "Downloaded cvc4 $VER"
}

if [ "$HOST_OS" = "Linux" ]; then
    travis_retry fetch_cvc4_linux "1.8"
else
    travis_retry fetch_cvc4_macos "1.8"
fi
