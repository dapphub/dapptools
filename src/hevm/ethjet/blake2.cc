#include <cstdint>
#include "blake2.h"

#define ROTR64(x, y) (((x) >> (y)) ^ ((x) << (64 - (y))))

#define B2B_G(a, b, c, d, x, y)   \
  v[a] = v[a] + v[b] + x;         \
  v[d] = ROTR64(v[d] ^ v[a], 32); \
  v[c] = v[c] + v[d];             \
  v[b] = ROTR64(v[b] ^ v[c], 24); \
  v[a] = v[a] + v[b] + y;         \
  v[d] = ROTR64(v[d] ^ v[a], 16); \
  v[c] = v[c] + v[d];             \
  v[b] = ROTR64(v[b] ^ v[c], 63);

static const uint64_t blake2b_iv[8] = {
  0x6A09E667F3BCC908, 0xBB67AE8584CAA73B,
  0x3C6EF372FE94F82B, 0xA54FF53A5F1D36F1,
  0x510E527FADE682D1, 0x9B05688C2B3E6C1F,
  0x1F83D9ABFB41BD6B, 0x5BE0CD19137E2179
};

static const uint8_t sigma[10][16] = {
  { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15 },
  { 14, 10, 4, 8, 9, 15, 13, 6, 1, 12, 0, 2, 11, 7, 5, 3 },
  { 11, 8, 12, 0, 5, 2, 15, 13, 10, 14, 3, 6, 7, 1, 9, 4 },
  { 7, 9, 3, 1, 13, 12, 11, 14, 2, 6, 5, 10, 4, 0, 15, 8 },
  { 9, 0, 5, 7, 2, 4, 10, 15, 14, 1, 11, 12, 6, 8, 3, 13 },
  { 2, 12, 6, 10, 0, 11, 8, 3, 4, 13, 7, 5, 15, 14, 1, 9 },
  { 12, 5, 1, 15, 14, 13, 4, 10, 0, 7, 6, 3, 9, 2, 8, 11 },
  { 13, 11, 7, 14, 12, 1, 3, 9, 5, 0, 15, 4, 8, 6, 2, 10 },
  { 6, 15, 14, 9, 11, 3, 0, 8, 12, 2, 13, 7, 1, 4, 10, 5 },
  { 10, 2, 8, 4, 7, 6, 1, 5, 15, 11, 9, 14, 3, 12, 13, 0 }
};

void blake2b_compress(uint64_t *h, uint64_t *m, uint64_t *t, char f, uint32_t rounds) {
  uint32_t i;
  uint64_t v[16];

  for (i = 0; i < 8; i++) {
    v[i] = h[i];
    v[i+8] = blake2b_iv[i];
  }

  v[12] ^= t[0];
  v[13] ^= t[1];

  if (f) v[14] = ~v[14];

  int index;
  for (i = 0; i < rounds; i++) {
    index = i % 10;
    B2B_G( 0, 4,  8, 12, m[sigma[index][ 0]], m[sigma[index][ 1]]);
    B2B_G( 1, 5,  9, 13, m[sigma[index][ 2]], m[sigma[index][ 3]]);
    B2B_G( 2, 6, 10, 14, m[sigma[index][ 4]], m[sigma[index][ 5]]);
    B2B_G( 3, 7, 11, 15, m[sigma[index][ 6]], m[sigma[index][ 7]]);
    B2B_G( 0, 5, 10, 15, m[sigma[index][ 8]], m[sigma[index][ 9]]);
    B2B_G( 1, 6, 11, 12, m[sigma[index][10]], m[sigma[index][11]]);
    B2B_G( 2, 7,  8, 13, m[sigma[index][12]], m[sigma[index][13]]);
    B2B_G( 3, 4,  9, 14, m[sigma[index][14]], m[sigma[index][15]]);
  }

  for (i = 0; i < 8; ++i)
    h[i] ^= v[i] ^ v[i+8];
}
