function keccakBase64(x) {
  return base64js.fromByteArray(keccak_256.array(base64js.toByteArray(x)))
}
