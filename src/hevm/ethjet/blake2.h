#ifndef BLAKE2_H
#define BLAKE2_H

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

void
blake2b_compress(uint64_t *h, uint64_t *m, uint64_t *t, char f, uint32_t rounds);

#ifdef __cplusplus
}
#endif

#endif
