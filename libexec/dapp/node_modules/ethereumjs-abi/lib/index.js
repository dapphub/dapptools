const utils = require('ethereumjs-util')
const BN = require('bn.js')

var ABI = function () {
}

// Convert from short to canonical names
// FIXME: optimise or make this nicer?
function elementaryName (name) {
  if (name.startsWith('int[')) {
    return 'int256' + name.slice(3)
  } else if (name === 'int') {
    return 'int256'
  } else if (name.startsWith('uint[')) {
    return 'uint256' + name.slice(4)
  } else if (name === 'uint') {
    return 'uint256'
  } else if (name.startsWith('fixed[')) {
    return 'fixed128x128' + name.slice(5)
  } else if (name === 'fixed') {
    return 'fixed128x128'
  } else if (name.startsWith('ufixed[')) {
    return 'ufixed128x128' + name.slice(6)
  } else if (name === 'ufixed') {
    return 'ufixed128x128'
  }
  return name
}

ABI.eventID = function (name, types) {
  // FIXME: use node.js util.format?
  var sig = name + '(' + types.map(elementaryName).join(',') + ')'
  return utils.sha3(new Buffer(sig))
}

ABI.methodID = function (name, types) {
  return ABI.eventID(name, types).slice(0, 4)
}

// Parse N from type<N>
function parseTypeN (type) {
  return parseInt(/^\D+(\d+)$/.exec(type)[1], 10)
}

// Parse N,M from type<N>x<M>
function parseTypeNxM (type) {
  var tmp = /^\D+(\d+)x(\d+)$/.exec(type)
  return [ parseInt(tmp[1], 10), parseInt(tmp[2], 10) ]
}

// Parse N from type[<N>]
function parseTypeArray (type) {
  var tmp = /^\w+\[(\d*)\]$/.exec(type)[1]
  if (tmp.length === 0) {
    return 0
  } else {
    return parseInt(tmp, 10)
  }
}

function parseNumber (arg) {
  var type = typeof arg
  if (type === 'string') {
    if (utils.isHexPrefixed(arg)) {
      return new BN(utils.stripHexPrefix(arg), 16)
    } else {
      return new BN(arg, 10)
    }
  } else if (type === 'number') {
    return new BN(arg)
  } else if (arg.toArray) {
    // assume this is a BN for the moment, replace with BN.isBN soon
    return arg
  } else {
    throw new Error('Argument is not a number')
  }
}

// someMethod(bytes,uint)
// someMethod(bytes,uint):(boolean)
function parseSignature (sig) {
  var tmp = /^(\w+)\((.+)\)$/.exec(sig)
  if (tmp.length !== 3) {
    throw new Error('Invalid method signature')
  }

  var args = /^(.+)\):\((.+)$/.exec(tmp[2])

  if (args !== null && args.length === 3) {
    return {
      method: tmp[1],
      args: args[1].split(','),
      retargs: args[2].split(',')
    }
  } else {
    return {
      method: tmp[1],
      args: tmp[2].split(',')
    }
  }
}

// Encodes a single item (can be dynamic array)
// @returns: Buffer
function encodeSingle (type, arg) {
  var size, num, ret, i

  if (type === 'address') {
    return encodeSingle('uint160', parseNumber(arg))
  } else if (type === 'bool') {
    return encodeSingle('uint8', arg ? 1 : 0)
  } else if (type === 'string') {
    return encodeSingle('bytes', new Buffer(arg, 'utf8'))
  } else if (type.match(/\w+\[\d+\]/)) {
    // this part handles fixed-length arrays ([2])
    // NOTE: we catch here all calls to arrays, that simplifies the rest
    if (typeof arg.length === 'undefined') {
      throw new Error('Not an array?')
    }

    size = parseTypeArray(type)
    if ((size !== 0) && (arg.length > size)) {
      throw new Error('Elements exceed array size: ' + size)
    }

    type = type.slice(0, type.indexOf('['))

    ret = []
    for (i in arg) {
      ret.push(encodeSingle(type, arg[i]))
    }

    return Buffer.concat(ret)
  } else if (type.match(/\w+\[\]/)) {
    // this part handles variable length ([])
    // NOTE: we catch here all calls to arrays, that simplifies the rest
    if (typeof arg.length === 'undefined') {
      throw new Error('Not an array?')
    }

    type = type.slice(0, type.indexOf('['))

    ret = [ encodeSingle('uint256', arg.length) ]
    for (i in arg) {
      ret.push(encodeSingle(type, arg[i]))
    }

    return Buffer.concat(ret)
  } else if (type === 'bytes') {
    arg = new Buffer(arg)

    ret = Buffer.concat([ encodeSingle('uint256', arg.length), arg ])

    if ((arg.length % 32) !== 0) {
      ret = Buffer.concat([ ret, utils.zeros(32 - (arg.length % 32)) ])
    }

    return ret
  } else if (type.startsWith('bytes')) {
    size = parseTypeN(type)
    if (size < 1 || size > 32) {
      throw new Error('Invalid bytes<N> width: ' + size)
    }

    return utils.setLengthRight(arg, 32)
  } else if (type.startsWith('uint')) {
    size = parseTypeN(type)
    if ((size % 8) || (size < 8) || (size > 256)) {
      throw new Error('Invalid uint<N> width: ' + size)
    }

    num = parseNumber(arg)
    if (num.bitLength() > size) {
      throw new Error('Supplied uint exceeds width: ' + size + ' vs ' + num.bitLength())
    }

    if (num < 0) {
      throw new Error('Supplied uint is negative')
    }

    return num.toArrayLike(Buffer, 'be', 32)
  } else if (type.startsWith('int')) {
    size = parseTypeN(type)
    if ((size % 8) || (size < 8) || (size > 256)) {
      throw new Error('Invalid int<N> width: ' + size)
    }

    num = parseNumber(arg)
    if (num.bitLength() > size) {
      throw new Error('Supplied int exceeds width: ' + size + ' vs ' + num.bitLength())
    }

    return num.toTwos(256).toArrayLike(Buffer, 'be', 32)
  } else if (type.startsWith('ufixed')) {
    size = parseTypeNxM(type)

    num = parseNumber(arg)

    if (num < 0) {
      throw new Error('Supplied ufixed is negative')
    }

    return encodeSingle('uint256', num.mul(new BN(2).pow(new BN(size[1]))))
  } else if (type.startsWith('fixed')) {
    size = parseTypeNxM(type)

    return encodeSingle('int256', parseNumber(arg).mul(new BN(2).pow(new BN(size[1]))))
  }

  throw new Error('Unsupported or invalid type: ' + type)
}

// Decodes a single item (can be dynamic array)
// @returns: array
// FIXME: this method will need a lot of attention at checking limits and validation
function decodeSingle (type, arg) {
  var size, num, ret, i

  if (type === 'address') {
    return decodeSingle('uint160', arg)
  } else if (type === 'bool') {
    return decodeSingle('uint8', arg).toString() === new BN(1).toString()
  } else if (type === 'string') {
    return new Buffer(decodeSingle('bytes', arg), 'utf8').toString()
  } else if (type.match(/\w+\[\d+\]/)) {
    // this part handles fixed-length arrays ([2])
    // NOTE: we catch here all calls to arrays, that simplifies the rest
    size = parseTypeArray(type)
    type = type.slice(0, type.indexOf('['))

    ret = []
    for (i = 0; i < size; i++) {
      ret.push(decodeSingle(type, arg.slice(i * 32)))
    }

    return ret
  } else if (type.match(/\w+\[\]/)) {
    // this part handles variable length ([])
    // NOTE: we catch here all calls to arrays, that simplifies the rest
    type = type.slice(0, type.indexOf('['))
    var count = decodeSingle('uint256', arg.slice(0, 32)).toNumber()

    ret = []
    for (i = 1; i < count + 1; i++) {
      ret.push(decodeSingle(type, arg.slice(i * 32)))
    }

    return ret
  } else if (type === 'bytes') {
    size = decodeSingle('uint256', arg.slice(0, 32)).toNumber()
    return arg.slice(32, 32 + size)
  } else if (type.startsWith('bytes')) {
    size = parseTypeN(type)
    if (size < 1 || size > 32) {
      throw new Error('Invalid bytes<N> width: ' + size)
    }

    return arg.slice(0, size)
  } else if (type.startsWith('uint')) {
    size = parseTypeN(type)
    if ((size % 8) || (size < 8) || (size > 256)) {
      throw new Error('Invalid uint<N> width: ' + size)
    }

    num = new BN(arg.slice(0, 32), 16, 'be')
    if (num.bitLength() > size) {
      throw new Error('Decoded int exceeds width: ' + size + ' vs ' + num.bitLength())
    }

    return num
  } else if (type.startsWith('int')) {
    size = parseTypeN(type)
    if ((size % 8) || (size < 8) || (size > 256)) {
      throw new Error('Invalid uint<N> width: ' + size)
    }

    num = new BN(arg.slice(0, 32), 16, 'be').fromTwos(256)
    if (num.bitLength() > size) {
      throw new Error('Decoded uint exceeds width: ' + size + ' vs ' + num.bitLength())
    }

    return num
  } else if (type.startsWith('ufixed')) {
    size = parseTypeNxM(type)
    size = new BN(2).pow(new BN(size[1]))

    num = decodeSingle('uint256', arg)
    if (!num.mod(size).isZero()) {
      throw new Error('Decimals not supported yet')
    }

    return num.div(size)
  } else if (type.startsWith('fixed')) {
    size = parseTypeNxM(type)
    size = new BN(2).pow(new BN(size[1]))

    num = decodeSingle('int256', arg)
    if (!num.mod(size).isZero()) {
      throw new Error('Decimals not supported yet')
    }

    return num.div(size)
  }

  throw new Error('Unsupported or invalid type: ' + type)
}

// Is a type dynamic?
function isDynamic (type) {
  // FIXME: handle all types? I don't think anything is missing now
  return (type === 'string') || (type === 'bytes') || type.match(/\w+\[\]/)
}

// Encode a method/event with arguments
// @types an array of string type names
// @args  an array of the appropriate values
ABI.rawEncode = function (types, values) {
  var output = []
  var data = []

  var headLength = 32 * types.length

  for (var i in types) {
    var type = elementaryName(types[i])
    var value = values[i]
    var cur = encodeSingle(type, value)

    // Use the head/tail method for storing dynamic data
    if (isDynamic(type)) {
      output.push(encodeSingle('uint256', headLength))
      data.push(cur)
      headLength += cur.length
    } else {
      output.push(cur)
    }
  }

  return Buffer.concat(output.concat(data))
}

ABI.rawDecode = function (types, data) {
  var ret = []

  data = new Buffer(data)

  var offset = 0
  for (var i in types) {
    var type = elementaryName(types[i])
    var cur = data.slice(offset, offset + 32)

    if (isDynamic(type)) {
      var dataOffset = decodeSingle('uint256', cur).toNumber()
      // We will read at least 32 bytes
      if (dataOffset > (data.length - 32)) {
        throw new Error('Invalid offset: ' + dataOffset)
      }

      cur = data.slice(dataOffset)
    } else if (type.match(/\w+\[\d+\]/)) {
      var count = parseTypeArray(type)
      if (count > 1) {
        cur = data.slice(offset, offset + (count * 32))
        offset += (count - 1) * 32
      }
    }

    ret.push(decodeSingle(type, cur))
    offset += 32
  }

  return ret
}

ABI.simpleEncode = function (method) {
  var args = Array.prototype.slice.call(arguments).slice(1)
  var sig = parseSignature(method)

  // FIXME: validate/convert arguments
  if (args.length !== sig.args.length) {
    throw new Error('Argument count mismatch')
  }

  return Buffer.concat([ ABI.methodID(sig.method, sig.args), ABI.rawEncode(sig.args, args) ])
}

ABI.simpleDecode = function (method, data) {
  var sig = parseSignature(method)

  // FIXME: validate/convert arguments
  if (!sig.retargs) {
    throw new Error('No return values in method')
  }

  return ABI.rawDecode(sig.retargs, data)
}

function stringify (type, value) {
  if (type.startsWith('address') || type.startsWith('bytes')) {
    return '0x' + value.toString('hex')
  } else {
    return value.toString()
  }
}

ABI.stringify = function (types, values) {
  var ret = []

  for (var i in types) {
    var type = types[i]
    var value = values[i]

    // if it is an array type, concat the items
    if (/^[^\[]+\[.*\]$/.test(type)) {
      value = value.map(function (item) {
        return stringify(type, item)
      }).join(', ')
    } else {
      value = stringify(type, value)
    }

    ret.push(value)
  }

  return ret
}

ABI.solidityPack = function (types, values) {
  if (types.length !== values.length) {
    throw new Error('Number of types are not matching the values')
  }

  var size, num
  var ret = []

  for (var i = 0; i < types.length; i++) {
    var type = elementaryName(types[i])
    var value = values[i]

    if (type === 'bytes') {
      ret.push(value)
    } else if (type === 'string') {
      ret.push(new Buffer(value, 'utf8'))
    } else if (type === 'bool') {
      ret.push(new Buffer(value ? '01' : '00', 'hex'))
    } else if (type === 'address') {
      ret.push(utils.setLengthLeft(value, 20))
    } else if (type.startsWith('bytes')) {
      size = parseTypeN(type)
      if (size < 1 || size > 32) {
        throw new Error('Invalid bytes<N> width: ' + size)
      }

      return utils.setLengthRight(value, size)
    } else if (type.startsWith('uint')) {
      size = parseTypeN(type)
      if ((size % 8) || (size < 8) || (size > 256)) {
        throw new Error('Invalid uint<N> width: ' + size)
      }

      num = parseNumber(value)
      if (num.bitLength() > size) {
        throw new Error('Supplied uint exceeds width: ' + size + ' vs ' + num.bitLength())
      }

      ret.push(num.toArrayLike(Buffer, 'be', size / 8))
    } else if (type.startsWith('int')) {
      size = parseTypeN(type)
      if ((size % 8) || (size < 8) || (size > 256)) {
        throw new Error('Invalid int<N> width: ' + size)
      }

      num = parseNumber(value)
      if (num.bitLength() > size) {
        throw new Error('Supplied int exceeds width: ' + size + ' vs ' + num.bitLength())
      }

      ret.push(num.toTwos(size).toArrayLike(Buffer, 'be', size / 8))
    } else {
      // FIXME: support all other types
      throw new Error('Unsupported or invalid type: ' + type)
    }
  }

  return Buffer.concat(ret)
}

ABI.soliditySHA3 = function (types, values) {
  return utils.sha3(ABI.solidityPack(types, values))
}

ABI.soliditySHA256 = function (types, values) {
  return utils.sha256(ABI.solidityPack(types, values))
}

ABI.solidityRIPEMD160 = function (types, values) {
  return utils.ripemd160(ABI.solidityPack(types, values), true)
}

// Serpent's users are familiar with this encoding
// - s: string
// - b: bytes
// - b<N>: bytes<N>
// - i: int256
// - a: int256[]

function isNumeric (c) {
  // FIXME: is this correct? Seems to work
  return (c >= '0') && (c <= '9')
}

// For a "documentation" refer to https://github.com/ethereum/serpent/blob/develop/preprocess.cpp
ABI.fromSerpent = function (sig) {
  var ret = []
  for (var i = 0; i < sig.length; i++) {
    var type = sig[i]
    if (type === 's') {
      ret.push('bytes')
    } else if (type === 'b') {
      var tmp = 'bytes'
      var j = i + 1
      while ((j < sig.length) && isNumeric(sig[j])) {
        tmp += sig[j] - '0'
        j++
      }
      i = j - 1
      ret.push(tmp)
    } else if (type === 'i') {
      ret.push('int256')
    } else if (type === 'a') {
      ret.push('int256[]')
    } else {
      throw new Error('Unsupported or invalid type: ' + type)
    }
  }
  return ret
}

ABI.toSerpent = function (types) {
  var ret = []
  for (var i = 0; i < types.length; i++) {
    var type = types[i]
    if (type === 'bytes') {
      ret.push('s')
    } else if (type.startsWith('bytes')) {
      ret.push('b' + parseTypeN(type))
    } else if (type === 'int256') {
      ret.push('i')
    } else if (type === 'int256[]') {
      ret.push('a')
    } else {
      throw new Error('Unsupported or invalid type: ' + type)
    }
  }
  return ret.join('')
}

module.exports = ABI
