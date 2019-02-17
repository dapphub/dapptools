#include "ethjet.h"
#include "ethjet-ff.h"

#include <libff/algebra/curves/bn128/bn128_pairing.hpp>

extern "C" {
  // Do stuff here to make relevant functionality available to
  // `ethjet.c' -- I just have no clue what's going on with this
  // cryptic crypto nonsense.

  int
  ethjet_ecadd (ethjet_ff_context *ctx,
                uint8_t *in, size_t in_size,
                uint8_t *out, size_t out_size) {
    return 0;
  }

  int
  ethjet_ecpairing (ethjet_ff_context *ctx,
                    uint8_t *in, size_t in_size,
                    uint8_t *out, size_t out_size) {
    return 0;
  }
}
