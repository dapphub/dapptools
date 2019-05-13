#include "ethjet.h"
//#include "ethjet-ff.h"

#include <gmp.h>
#include <stdio.h>

#include <libff/algebra/fields/bigint.hpp>
#include <libff/algebra/curves/alt_bn128/alt_bn128_init.hpp>
#include <libff/algebra/curves/alt_bn128/alt_bn128_g1.hpp>
#include <libff/algebra/curves/alt_bn128/alt_bn128_pairing.hpp>

using namespace libff;

extern "C" {
  // Do stuff here to make relevant functionality available to
  // `ethjet.c' -- I just have no clue what's going on with this
  // cryptic crypto nonsense.

  typedef void ethjet_ff_context;

  int
  ethjet_ecadd (ethjet_ff_context *ctx,
                uint8_t *in, size_t in_size,
                uint8_t *out, size_t out_size) {

    if (in_size != 128) {
      // printf("in_size=%d\n", int(in_size));
      return 0;
    }
    if (out_size != 64) {
      // printf("out_size=%d\n", int(out_size));
      return 0;
    }

    mpz_t ax_data;
    mpz_t ay_data;
    mpz_t bx_data;
    mpz_t by_data;
    mpz_init(ax_data);
    mpz_init(ay_data);
    mpz_init(bx_data);
    mpz_init(by_data);
    mpz_import(ax_data, 32, 1, sizeof(in[0]), 1, 0, in);
    mpz_import(ay_data, 32, 1, sizeof(in[0]), 1, 0, in+32);
    mpz_import(bx_data, 32, 1, sizeof(in[0]), 1, 0, in+64);
    mpz_import(by_data, 32, 1, sizeof(in[0]), 1, 0, in+96);

    // gmp_printf("ax=%Zd, ay=%Zd\n", ax_data, ay_data);
    // gmp_printf("bx=%Zd, by=%Zd\n", bx_data, by_data);

    init_alt_bn128_params();
    const mp_size_t limbs      = alt_bn128_q_limbs;
    const alt_bn128_Fq Fq_zero = alt_bn128_Fq::zero();
    const alt_bn128_Fq Fq_one  = alt_bn128_Fq::one();
    mpz_t q;
    mpz_init(q);
    alt_bn128_modulus_q.to_mpz(q);
    // gmp_printf("q=%Zd\n", q);

    // check coordinates are in range
    if ((mpz_cmp(ax_data, q) >= 0) ||
        (mpz_cmp(ay_data, q) >= 0) ||
        (mpz_cmp(bx_data, q) >= 0) ||
        (mpz_cmp(by_data, q) >= 0)) {
      // printf("One of the coordinates was out of range!\n");
      return 0;
    }

    const alt_bn128_Fq ax = Fp_model<alt_bn128_q_limbs, alt_bn128_modulus_q>(bigint<limbs>(ax_data));
    const alt_bn128_Fq ay = Fp_model<alt_bn128_q_limbs, alt_bn128_modulus_q>(bigint<limbs>(ay_data));
    const alt_bn128_Fq bx = Fp_model<alt_bn128_q_limbs, alt_bn128_modulus_q>(bigint<limbs>(bx_data));
    const alt_bn128_Fq by = Fp_model<alt_bn128_q_limbs, alt_bn128_modulus_q>(bigint<limbs>(by_data));

    alt_bn128_G1 a;
    alt_bn128_G1 b;
    // create curve points from affine coordinates
    // the point at infinity (0,0) is a special case
    if (ax.is_zero() && ay.is_zero()) {
      a = alt_bn128_G1(Fq_zero, Fq_one, Fq_zero);
    }
    else {
      a = alt_bn128_G1(ax, ay, Fq_one);
    }
    if (bx.is_zero() && by.is_zero()) {
      b = alt_bn128_G1(Fq_zero, Fq_one, Fq_zero);
    }
    else {
      b = alt_bn128_G1(bx, by, Fq_one);
    }

    // printf("a=");
    // a.print();
    // printf("b=");
    // b.print();

    // check points are on curve
    if (! a.is_well_formed()) {
      // printf("a is ill-formed!\n");
      return 0;
    }
    if (! b.is_well_formed()) {
      // printf("b is ill-formed!\n");
      return 0;
    }

    alt_bn128_G1 sum = (a + b);
    // remember to convert the Jacobian (projective)
    // coordinates back to affine before extracting
    sum.to_affine_coordinates();
    // printf("after normalisation, a+b=");
    // sum.print();

    mpz_t sumX;
    mpz_t sumY;
    size_t sumX_size;
    size_t sumY_size;
    mpz_init(sumX);
    mpz_init(sumY);
    // point at infinity is represented as (0,0)
    // so leave the arrays empty in that case
    if (!sum.is_zero()) {
      sum.X.as_bigint().to_mpz(sumX);
      sum.Y.as_bigint().to_mpz(sumY);
    }
    // gmp_printf("sum.X=%Zd, sum.Y=%Zd\n", sumX, sumY);

    uint8_t *sumX_arr = (uint8_t *)mpz_export(NULL, &sumX_size, 1, 1, 1, 0, sumX);
    uint8_t *sumY_arr = (uint8_t *)mpz_export(NULL, &sumY_size, 1, 1, 1, 0, sumY);

    if (sumX_size > 32 || sumY_size > 32) {
      // printf("Output is too large!\n");
      return 0;
    }
    // printf("sumX_size=%d, sumY_size=%d\n", int(sumX_size), int(sumY_size));
    // copy the result to the output buffer
    for (int i = 1; i <= 32; i++) {
      if (i <= sumX_size)
        out[32-i] = sumX_arr[sumX_size-i];
      else
        out[32-i] = 0;
    }
    for (int i = 1; i <= 32; i++) {
      if (i <= sumY_size)
        out[64-i] = sumY_arr[sumY_size-i];
      else
        out[64-i] = 0;
    }

    // printf("Sum was: 0x");
    // for (int i = 0; i < 32; i++) {
    //   printf("%02x", sumX_arr[i]);
    // }
    // printf("\n");
    // for (int i = 0; i < 32; i++) {
    //   printf("%02x", sumY_arr[i]);
    // }
    // printf("\n");

    // printf("Output was: 0x");
    // for (int i = 0; i < 64; i++) {
    //   printf("%02x", out[i]);
    // }
    // printf("\n");

    return 1;
  }

  int
  ethjet_ecmul (ethjet_ff_context *ctx,
                uint8_t *in, size_t in_size,
                uint8_t *out, size_t out_size) {

    if (in_size != 96) {
      // printf("in_size=%d\n", int(in_size));
      return 0;
    }
    if (out_size != 64) {
      // printf("out_size=%d\n", int(out_size));
      return 0;
    }

    mpz_t n_data;
    mpz_t ax_data;
    mpz_t ay_data;
    mpz_init(n_data);
    mpz_init(ax_data);
    mpz_init(ay_data);
    mpz_import(ax_data, 32, 1, sizeof(in[0]), 1, 0, in);
    mpz_import(ay_data, 32, 1, sizeof(in[0]), 1, 0, in+32);
    mpz_import(n_data,  32, 1, sizeof(in[0]), 1, 0, in+64);

    // gmp_printf("ax=%Zd, ay=%Zd\n", ax_data, ay_data);
    // gmp_printf("n=%Zd\n", n_data);

    init_alt_bn128_params();
    const mp_size_t q_limbs      = alt_bn128_q_limbs;
    const mp_size_t r_limbs      = alt_bn128_r_limbs;
    const alt_bn128_Fq Fq_zero = alt_bn128_Fq::zero();
    const alt_bn128_Fq Fq_one  = alt_bn128_Fq::one();
    mpz_t q;
    mpz_init(q);
    alt_bn128_modulus_q.to_mpz(q);
    // gmp_printf("q=%Zd\n", q);

    // reduce n mod r
    // mpz_t r;
    // mpz_init(r);
    // alt_bn128_modulus_r.to_mpz(r);
    // mpz_mod(n_data, n_data, r);
    // gmp_printf("after reducing n=%Zd\n", n_data);

    // check coordinates are in range
    // n.b. the scalar needn't be in range
    if ((mpz_cmp(ax_data, q) >= 0) ||
        (mpz_cmp(ay_data, q) >= 0)) {
      // printf("One of the coordinates was out of range!\n");
      return 0;
    }

    const alt_bn128_Fr n = Fp_model<alt_bn128_r_limbs, alt_bn128_modulus_r>(bigint<r_limbs>(n_data));
    const alt_bn128_Fq ax = Fp_model<alt_bn128_q_limbs, alt_bn128_modulus_q>(bigint<q_limbs>(ax_data));
    const alt_bn128_Fq ay = Fp_model<alt_bn128_q_limbs, alt_bn128_modulus_q>(bigint<q_limbs>(ay_data));

    alt_bn128_G1 a;
    // create curve point from affine coordinates
    // the point at infinity (0,0) is a special case
    if (ax.is_zero() && ay.is_zero()) {
      a = alt_bn128_G1(Fq_zero, Fq_one, Fq_zero);
    }
    else {
      a = alt_bn128_G1(ax, ay, Fq_one);
    }

    // printf("a=");
    // a.print();

    // check point is on curve
    if (! a.is_well_formed()) {
      // printf("a is ill-formed!\n");
      return 0;
    }

    alt_bn128_G1 na = n * a;
    // remember to convert the Jacobian (projective)
    // coordinates back to affine before extracting
    na.to_affine_coordinates();
    // printf("after normalisation, n*a=");
    // na.print();

    mpz_t naX;
    mpz_t naY;
    size_t naX_size;
    size_t naY_size;
    mpz_init(naX);
    mpz_init(naY);
    // point at infinity is represented as (0,0)
    // so leave the arrays empty in that case
    if (!na.is_zero()) {
      na.X.as_bigint().to_mpz(naX);
      na.Y.as_bigint().to_mpz(naY);
    }
    // gmp_printf("sum.X=%Zd, sum.Y=%Zd\n", sumX, sumY);

    uint8_t *naX_arr = (uint8_t *)mpz_export(NULL, &naX_size, 1, 1, 1, 0, naX);
    uint8_t *naY_arr = (uint8_t *)mpz_export(NULL, &naY_size, 1, 1, 1, 0, naY);

    if (naX_size > 32 || naY_size > 32) {
      // printf("Output is too large!\n");
      return 0;
    }
    // printf("sumX_size=%d, sumY_size=%d\n", int(sumX_size), int(sumY_size));
    // copy the result to the output buffer
    for (int i = 1; i <= 32; i++) {
      if (i <= naX_size)
        out[32-i] = naX_arr[naX_size-i];
      else
        out[32-i] = 0;
    }
    for (int i = 1; i <= 32; i++) {
      if (i <= naY_size)
        out[64-i] = naY_arr[naY_size-i];
      else
        out[64-i] = 0;
    }

    // printf("Sum was: 0x");
    // for (int i = 0; i < 32; i++) {
    //   printf("%02x", sumX_arr[i]);
    // }
    // printf("\n");
    // for (int i = 0; i < 32; i++) {
    //   printf("%02x", sumY_arr[i]);
    // }
    // printf("\n");

    // printf("Output was: 0x");
    // for (int i = 0; i < 64; i++) {
    //   printf("%02x", out[i]);
    // }
    // printf("\n");

    return 1;
  }

  int
  ethjet_ecpairing (ethjet_ff_context *ctx,
                    uint8_t *in, size_t in_size,
                    uint8_t *out, size_t out_size) {
    return 0;
  }
}
