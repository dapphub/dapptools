#include "ethjet.h"
#include "ethjet-ff.h"
#include "tinykeccak.h"
#include "blake2.h"

#include <secp256k1_recovery.h>

#include <stdlib.h>
#include <stdint.h>
#include <string.h>

struct ethjet_context *
ethjet_init ()
{
  struct ethjet_context *ctx;
  ctx = malloc (sizeof *ctx);
  if (!ctx) return NULL;

  ctx->ec = secp256k1_context_create (SECP256K1_CONTEXT_VERIFY);

  return ctx;
}

void
ethjet_free (struct ethjet_context *ctx)
{
  secp256k1_context_destroy (ctx->ec);
  free (ctx);
}

/* 
 * The example contract at 0xdeadbeef just reverses its input.
 */
int
ethjet_example (struct ethjet_context *ctx,
                uint8_t *in, size_t in_size,
                uint8_t *out, size_t out_size)
{
  if (out_size != in_size)
    return 0;

  for (int i = 0; i < in_size; i++)
    out[i] = in[in_size - i - 1];

  return 1;
}

int
ethjet_ecrecover (secp256k1_context *ctx,
                  uint8_t *in, size_t in_size,
                  uint8_t *out, size_t out_size)
{
  /* Input: H V R S, all 32 bytes. */

  secp256k1_pubkey pubkey;
  secp256k1_ecdsa_recoverable_signature rsig;

  uint8_t *input64;
  uint8_t pubkey_hex[65];
  size_t hexlen = 65;

  int recid;

  if (in_size != 128)
    return 0;

  if (out_size != 32)
    return 0;

  input64 = in + 64;
  recid = in[63] - 27;

  /* higher bytes of V should be zero  */
  static const char z31 [31];
  if (memcmp (z31, in + 32, 31))
    return 0;

  if (recid < 0 || recid > 3)
    return 0;

  if (!secp256k1_ecdsa_recoverable_signature_parse_compact
      (ctx, &rsig, input64, recid))
    return 0;

  if (!secp256k1_ecdsa_recover (ctx, &pubkey, &rsig, in))
    return 0;

  if (!secp256k1_ec_pubkey_serialize
      (ctx, pubkey_hex, &hexlen, &pubkey, SECP256K1_EC_UNCOMPRESSED))
    return 0;

  if (sha3_256 (out, 32, pubkey_hex + 1, 64))
    return 0;

  memset (out, 0, 12);

  return 1;
}

int ethjet_blake2(uint8_t *in, size_t in_size,
                  uint8_t *out, size_t out_size) {
  uint32_t rounds = in[0] << 24 | in[1] << 16 | in[2] << 8 | in[3];
  unsigned char f = in[212];
  uint64_t *h = (uint64_t *)&in[4];
  uint64_t *m = (uint64_t *)&in[68];
  uint64_t *t = (uint64_t *)&in[196];

  blake2b_compress(h, m, t, f, rounds);

  memcpy(out, h, out_size);

  return 1;
}

int
ethjet (struct ethjet_context *ctx,
        enum ethjet_operation op,
        uint8_t *in, size_t in_size,
        uint8_t *out, size_t out_size)
{
  switch (op) {
  case ETHJET_ECRECOVER:
    return ethjet_ecrecover (ctx->ec, in, in_size, out, out_size);
    break;

  case ETHJET_EXAMPLE:
    return ethjet_example (ctx, in, in_size, out, out_size);

  case ETHJET_ECADD:
    return ethjet_ecadd (in, in_size, out, out_size);

  case ETHJET_ECMUL:
    return ethjet_ecmul (in, in_size, out, out_size);

  case ETHJET_ECPAIRING:
    return ethjet_ecpairing (in, in_size, out, out_size);

  case ETHJET_BLAKE2:
    return ethjet_blake2 (in, in_size, out, out_size);

  default:
    return 0;
  }
}
