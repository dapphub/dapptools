#ifndef LIBETHJET_H
#define LIBETHJET_H

#include <stdint.h>

#include <secp256k1.h>

struct ethjet_context
{
  secp256k1_context *ec;
};

enum ethjet_operation
  {
    ETHJET_ECRECOVER = 1,
    ETHJET_ECADD = 6,
    ETHJET_ECMUL = 7,
    ETHJET_ECPAIRING = 8,
    ETHJET_BLAKE2 = 9,
    ETHJET_EXAMPLE = 0xdeadbeef,
  };

struct ethjet_context *
ethjet_init ();

void
ethjet_free (struct ethjet_context *ctx);

int ethjet (struct ethjet_context *ctx,
            enum ethjet_operation op,
            uint8_t *in, size_t in_size,
            uint8_t *out, size_t out_size);

#endif
