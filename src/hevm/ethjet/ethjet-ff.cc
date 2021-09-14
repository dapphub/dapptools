#include "ethjet.h"

#include <gmp.h>
#include <stdio.h>

#include <libff/algebra/fields/bigint.hpp>
#include <libff/algebra/curves/alt_bn128/alt_bn128_init.hpp>
#include <libff/algebra/curves/alt_bn128/alt_bn128_g1.hpp>
#include <libff/algebra/curves/alt_bn128/alt_bn128_g2.hpp>
#include <libff/algebra/curves/alt_bn128/alt_bn128_pairing.hpp>
#include <libff/common/profiling.hpp>

using namespace libff;

namespace ethjet_ff {
  void init() {
    libff::inhibit_profiling_info     = true;
    libff::inhibit_profiling_counters = true;
    init_alt_bn128_params();
  }

  // for loading an element of F_q (a coordinate of G_1)
  // consumes 32 bytes
  alt_bn128_Fq read_Fq_element (uint8_t *in) {
    mpz_t x_data;
    mpz_init(x_data);
    mpz_import(x_data, 32, 1, sizeof(in[0]), 1, 0, in);

    mpz_t q;
    mpz_init(q);
    alt_bn128_modulus_q.to_mpz(q);
    const mp_size_t limbs = alt_bn128_q_limbs;

    if (mpz_cmp(x_data, q) >= 0)
      throw 0;

    return Fp_model<limbs, alt_bn128_modulus_q>(bigint<limbs>(x_data));
  }

  // for loading an element of F_{q^2} (a coordinate of G_2)
  // consumes 64 bytes
  alt_bn128_Fq2 read_Fq2_element (uint8_t *in) {
    // surprising "big-endian" encoding
    alt_bn128_Fq x0 = read_Fq_element(in+32);
    alt_bn128_Fq x1 = read_Fq_element(in);

    mpz_t q;
    mpz_init(q);
    alt_bn128_modulus_q.to_mpz(q);

    return Fp2_model<alt_bn128_q_limbs, alt_bn128_modulus_q>(x0, x1);
  }

  // for loading an element of F_r (a scalar for G_1)
  // consumes 32 bytes
  alt_bn128_Fr read_Fr_element (uint8_t *in) {
    mpz_t x_data;
    mpz_init(x_data);
    mpz_import(x_data, 32, 1, sizeof(in[0]), 1, 0, in);

    mpz_t r;
    mpz_init(r);
    alt_bn128_modulus_r.to_mpz(r);
    const mp_size_t limbs = alt_bn128_r_limbs;

    return Fp_model<limbs, alt_bn128_modulus_r>(bigint<limbs>(x_data));
  }

  // for loading a point in G_1
  // consumes 64 bytes
  alt_bn128_G1 read_G1_point (uint8_t *in) {
    alt_bn128_Fq ax = read_Fq_element(in);
    alt_bn128_Fq ay = read_Fq_element(in+32);
    alt_bn128_G1 a;
    // create curve point from affine coordinates
    // the point at infinity (0,0) is a special case
    if (ax.is_zero() && ay.is_zero()) {
      a = alt_bn128_G1::G1_zero;
    }
    else {
      a = alt_bn128_G1(ax, ay, alt_bn128_Fq::one());
    }
    if (! a.is_well_formed()) {
      throw 0;
    }
    return a;
  }

  // for loading a point in G_2
  // consumes 128 bytes
  alt_bn128_G2 read_G2_point (uint8_t *in) {
    alt_bn128_Fq2 ax = read_Fq2_element(in);
    alt_bn128_Fq2 ay = read_Fq2_element(in+64);
    alt_bn128_G2  a;
    // create curve point from affine coordinates
    // the point at infinity (0,0) is a special case
    if (ax.is_zero() && ay.is_zero()) {
      a = alt_bn128_G2::G2_zero;
      return a;
    }
    a = alt_bn128_G2(ax, ay, alt_bn128_Fq2::one());
    if (! a.is_well_formed()) {
      throw 0;
    }
    // additionally check that the element has the right order
    if (-alt_bn128_Fr::one() * a + a != alt_bn128_G2::G2_zero) {
      throw 0;
    }
    return a;
  }

  // writes an element of Fq
  // produces 32 bytes
  void write_Fq_element(uint8_t *out, alt_bn128_Fq x) {
    mpz_t x_data;
    size_t x_size;
    mpz_init(x_data);

    x.as_bigint().to_mpz(x_data);
    uint8_t *x_arr = (uint8_t *)mpz_export(NULL, &x_size, 1, 1, 1, 0, x_data);
    if (x_size > 32) {
      throw 0;
    }
    // copy the result to the output buffer
    // with padding
    for (int i = 1; i <= 32; i++) {
      if (i <= x_size)
        out[32-i] = x_arr[x_size-i];
      else
        out[32-i] = 0;
    }
    return;
  }

  // writes an element of F_{q^2}
  // produces 64 bytes
  void write_Fq2_element(uint8_t *out, alt_bn128_Fq2 x) {
    // surprising "big-endian" encoding
    write_Fq_element(out+32, x.c0);
    write_Fq_element(out,    x.c1);
    return;
  }

  // writes a point of G1
  // produces 64 bytes
  void write_G1_point(uint8_t *out, alt_bn128_G1 a) {
    // point at infinity is represented as (0,0)
    // so treat it as a special case
    if (a.is_zero()) {
      write_Fq_element(out,    alt_bn128_Fq::zero());
      write_Fq_element(out+32, alt_bn128_Fq::zero());
      return;
    }
    a.to_affine_coordinates();
    write_Fq_element(out,    a.X);
    write_Fq_element(out+32, a.Y);
    return;
  }

  // writes a point of G2
  // produces 128 bytes
  void write_G2_point(uint8_t *out, alt_bn128_G2 a) {
    // point at infinity is represented as (0,0)
    // so treat it as a special case
    if (a.is_zero()) {
      write_Fq2_element(out,    alt_bn128_Fq2::zero());
      write_Fq2_element(out+64, alt_bn128_Fq2::zero());
      return;
    }
    a.to_affine_coordinates();
    write_Fq2_element(out,    a.X);
    write_Fq2_element(out+64, a.Y);
    return;
  }

  // writes a bool
  // produces 32 bytes
  void write_bool(uint8_t *out, bool p) {
    out[31] = (int)(p);
    for (int i = 2; i <= 32; i++) {
      out[32-i] = 0;
    }
  }
}

extern "C" {
  using namespace ethjet_ff;

  int
  ethjet_ecadd (uint8_t *in, size_t in_size,
                uint8_t *out, size_t out_size) {

    if (in_size != 128) {
      return 0;
    }
    if (out_size != 64) {
      return 0;
    }

    init();

    try {
      alt_bn128_G1 a = read_G1_point(in);
      alt_bn128_G1 b = read_G1_point(in+64);
      alt_bn128_G1 sum = (a + b);

      write_G1_point(out, sum);
    }
    catch (int e) {
      return 0;
    }

    return 1;
  }

  int
  ethjet_ecmul (uint8_t *in, size_t in_size,
                uint8_t *out, size_t out_size) {

    if (in_size != 96) {
      return 0;
    }
    if (out_size != 64) {
      return 0;
    }

    init();

    try {
      alt_bn128_G1 a = read_G1_point(in);
      alt_bn128_Fr n = read_Fr_element(in+64);
      alt_bn128_G1 na = n * a;

      write_G1_point(out, na);
    }
    catch (int e) {
      return 0;
    }

    return 1;
  }

  int
  ethjet_ecpairing (uint8_t *in, size_t in_size,
                    uint8_t *out, size_t out_size) {

    if (in_size % 192 != 0)
      return 0;

    if (out_size != 32)
      return 0;

    init();
    int pairs = in_size / 192;

    try {
      alt_bn128_Fq12 x = libff::alt_bn128_Fq12::one();
      for (int i = 0; i < pairs; i++) {
        alt_bn128_G1 a = read_G1_point(in + i*192);
        alt_bn128_G2 b = read_G2_point(in + i*192 + 64);
        if (a.is_zero() || b.is_zero())
          continue;
        x = x * alt_bn128_miller_loop(alt_bn128_precompute_G1(a), alt_bn128_precompute_G2(b));
      }
      bool result;
      if (pairs == 0)
        result = true;
      else
        result = (alt_bn128_final_exponentiation(x) == alt_bn128_GT::one());
      write_bool(out, result);
    }
    catch (int e) {
      return 0;
    }

    return 1;
  }
}
