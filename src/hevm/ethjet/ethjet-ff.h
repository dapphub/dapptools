#ifndef ETHJET_FF_H
#define ETHJET_FF_H

#include <stdint.h>

extern "C" {
  // Just a dummy example context type; I don't know if we need any
  // persistent context for these precompiles.
  typedef void ethjet_ff_context;

  int
  ethjet_ecadd (ethjet_ff_context *ctx,
                uint8_t *in, size_t in_size,
                uint8_t *out, size_t out_size);

  int
  ethjet_ecpairing (ethjet_ff_context *ctx,
                    uint8_t *in, size_t in_size,
                    uint8_t *out, size_t out_size);
}

#endif
