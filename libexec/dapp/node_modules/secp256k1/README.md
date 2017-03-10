# secp256k1-node

[![NPM Package](https://img.shields.io/npm/v/secp256k1.svg?style=flat-square)](https://www.npmjs.org/package/secp256k1)
[![Build Status](https://img.shields.io/travis/cryptocoinjs/secp256k1-node.svg?branch=master&style=flat-square)](https://travis-ci.org/cryptocoinjs/secp256k1-node)
[![AppVeyor](https://img.shields.io/appveyor/ci/fanatid/secp256k1-node.svg?branch=master&style=flat-square)](https://ci.appveyor.com/project/fanatid/secp256k1-node)

[![js-standard-style](https://cdn.rawgit.com/feross/standard/master/badge.svg)](https://github.com/feross/standard)

This module provides native bindings to ecdsa [secp256k1](https://github.com/bitcoin-core/secp256k1) functions.
This library is experimental, so use at your own risk. Works on node version 0.10 or greater and in the Browser via browserify.

## Installation

If you have [gmp](https://gmplib.org/) installed [secp256k1](https://github.com/bitcoin-core/secp256k1) will use it.
* arch `pacman -S gmp`
* ubuntu `sudo apt-get install libgmp-dev`

##### from npm

`npm install secp256k1`

##### from git

```
git clone git@github.com:cryptocoinjs/secp256k1-node.git
cd secp256k1-node
git submodule update --init
npm install
```

##### Windows

Required software:

  * Git: https://git-scm.com/download/win
  * nvm: https://github.com/coreybutler/nvm-windows
  * Python 2.7: https://www.python.org/downloads/release/python-2712/
  * Visual C++ Build Tools: http://landinghub.visualstudio.com/visual-cpp-build-tools (Custom Install, and select both Windows 8.1 and Windows 10 SDKs)

Additional commands:

```
npm config set msvs_version 2015 --global
npm install npm@next -g
```

Useful resources:

  * https://github.com/nodejs/node-gyp/issues/629#issuecomment-153196245
  * https://github.com/nodejs/node-gyp/issues/972

## Usage

* [API Reference (v3.x)](https://github.com/cryptocoinjs/secp256k1-node/blob/master/API.md)
* [API Reference (v2.x)](https://github.com/cryptocoinjs/secp256k1-node/blob/v2.x/API.md)

```js
var crypto = require('crypto')
var secp256k1 = require('secp256k1')
// or require('secp256k1/js')
//   if you want to use pure js implementation in node

// generate message to sign
var msg = crypto.randomBytes(32)

// generate privKey
var privKey
do {
  privKey = crypto.randomBytes(32)
} while (!secp256k1.privateKeyVerify(privKey))

// get the public key in a compressed format
var pubKey = secp256k1.publicKeyCreate(privKey)

// sign the message
var sigObj = secp256k1.sign(msg, privKey)

// verify the signature
console.log(secp256k1.verify(msg, sigObj.signature, pubKey))
```

\* **.verify return false for high signatures**

## Elliptic vs "embedded"

secp256k1-node has pure JavaScript implementation secp256k1 based on [elliptic](http://github.com/indutny/elliptic), [bn.js](http://github.com/indutny/bn.js), [hash.js](http://github.com/indutny/hash.js).
The main purpose of this implementation is more [high performance](#performance), [smaller size](#code-size) and simple code audit.

##### Code size:
|        | browserifiable | + uglified | + gzipped |
|:------:|:--------------:|:----------:|:---------:|
|elliptic|303555          |211777      |62124      |
|embedded|241829          |152989      |35908      |
|diff    |25%             |38%         |73%        |

##### Performance:
```
$ node benchmark/benchmark.js
Set seed: 5120779d9d961dc818363811b3cf44ace2323ccf5e265749206d37442a0deac5
100% (1000/1000), 2.8s elapsed, eta 0.0s
Create 1000 fixtures
++++++++++++++++++++++++++++++++++++++++++++++++++
Benchmarking: publicKeyCreate
--------------------------------------------------
bindings x 13,945 ops/sec ±0.76% (101 runs sampled)
secp256k1js x 967 ops/sec ±0.41% (100 runs sampled)
elliptic x 838 ops/sec ±0.66% (99 runs sampled)
==================================================
Benchmarking: sign
--------------------------------------------------
bindings x 8,219 ops/sec ±0.13% (102 runs sampled)
secp256k1js x 773 ops/sec ±0.47% (98 runs sampled)
elliptic x 615 ops/sec ±0.43% (97 runs sampled)
==================================================
Benchmarking: verify
--------------------------------------------------
bindings x 5,350 ops/sec ±0.11% (103 runs sampled)
secp256k1js x 208 ops/sec ±0.19% (91 runs sampled)
elliptic x 131 ops/sec ±2.05% (87 runs sampled)
==================================================
```

## LICENSE

This library is free and open-source software released under the MIT license.
