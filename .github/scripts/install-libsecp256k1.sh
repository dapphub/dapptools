#! /bin/bash

set -eux

source .github/scripts/host.sh

if [ -f $HOME/.local/lib/libsecp256k1.a ]; then
  echo "libsecp256k1 exists, exiting..."
  exit 0
fi

gitRef="f54c6c5083307b18224c953cf5870ea7ffce070b"
curl -LO "https://github.com/bitcoin-core/secp256k1/archive/$gitRef.zip"

unzip "$gitRef.zip"
cd "secp256k1-$gitRef"

./autogen.sh
# hevm needs reecovery module
# enable pic so static library can link against dynamic correctly
./configure --prefix=$PREFIX --enable-module-recovery --with-pic

make install
