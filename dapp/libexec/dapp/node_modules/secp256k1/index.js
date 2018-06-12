'use strict'
try {
  module.exports = require('./bindings')
} catch (err) {
  console.log('Secp256k1 bindings are not compiled. Pure JS implementation will be used.')
  module.exports = require('./elliptic')
}
