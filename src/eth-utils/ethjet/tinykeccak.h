#ifndef KECCAK_H
#define KECCAK_H

#include <stdint.h>
#include <stdlib.h>

int sha3_256 (uint8_t *out, size_t out_size,
              const uint8_t *in, size_t in_size);

#endif
