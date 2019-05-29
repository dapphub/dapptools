#ifndef ETHJET_FF_H
#define ETHJET_FF_H

#include <stdint.h>

int
ethjet_ecadd (uint8_t *in, size_t in_size,
              uint8_t *out, size_t out_size);

int
ethjet_ecmul (uint8_t *in, size_t in_size,
              uint8_t *out, size_t out_size);

int
ethjet_ecpairing (uint8_t *in, size_t in_size,
                  uint8_t *out, size_t out_size);

#endif
