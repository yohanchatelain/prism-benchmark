#ifndef STOCHRNDDW_H_
#define STOCHRNDDW_H_

#include <assert.h>
#include <math.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <type_traits>

namespace Double {

using FPTYPE = double;
using INTTYPE = uint64_t;
using SINTTYPE = int64_t;
constexpr int DEFPREC = 53;
constexpr int DEFSIGNIFICAND = 52;
constexpr int EMAX = 1023;
constexpr int NBITS = 64;
constexpr int NLEADBITS = 12;
constexpr int NRNDBITS = 30;
constexpr uint64_t FULLMASK = 0xFFFFFFFFFFFFFFFF;
constexpr uint64_t ABSMASK = 0x7FFFFFFFFFFFFFFF;
constexpr uint64_t EXPMASK = 0x7FF0000000000000;
constexpr uint64_t FRACMASK = 0x000FFFFFFFFFFFFF;
constexpr uint64_t SIGNMASK = 0x8000000000000000;

typedef union {
  FPTYPE fpval;
  INTTYPE intval;
} STRUCTNAME;

constexpr INTTYPE INTCONST(int x) { return static_cast<INTTYPE>(x); }
constexpr INTTYPE INTOF(const FPTYPE x) {
  STRUCTNAME _s = {.fpval = x};
  return _s.intval;
}
constexpr FPTYPE FPOF(INTTYPE x) {
  STRUCTNAME _s = {.intval = x};
  return _s.fpval;
}
constexpr int SIGN(FPTYPE x) { return (x > 0) - (x < 0); }
constexpr INTTYPE FASTSIGN(const FPTYPE x) { return SIGNMASK & INTOF(x); }
constexpr INTTYPE INTABS(const FPTYPE x) { return ABSMASK & INTOF(x); }
constexpr FPTYPE ABS(const FPTYPE x) { return FPOF(ABSMASK & INTOF(x)); }

/* Compute IEEE floating-point exponent of x. */
template <typename T> static inline int get_exponent(T x) {
  return (((INTOF(x) & EXPMASK) >> DEFSIGNIFICAND) - EMAX);
}

/* Compute absolute value of ulp(x). */
template <typename T> static inline INTTYPE ulp_abs(T x) {
  SINTTYPE tmp = get_exponent(x);
  if (tmp <= (-EMAX + DEFSIGNIFICAND)) // ulp(x) is subnormal
    return INTCONST(1);
  else // ulp(x) i normal
    return INTCONST(tmp + (+EMAX - DEFSIGNIFICAND)) << DEFSIGNIFICAND;
}

/* A helper function for SR algorithms without the change of rounding mode. */
template <typename T> static inline T sr_round(const T s, const T t) {
  T ulp;
  if (FASTSIGN(t) != FASTSIGN(s))
    //    ulp = FPOF(FASTSIGN(&t) | ulp_abs(s*(1 - ldexp(1, -53))));
    ulp = SIGN(t) *
          ldexp(1, get_exponent(s * (1 - ldexp(1, -DEFPREC))) - DEFSIGNIFICAND);
  else
    //    ulp = FPOF(FASTSIGN(&t) | ulp_abs(s));
    ulp = SIGN(t) * ldexp(1, get_exponent(s) - DEFSIGNIFICAND);

  T p = (rand() / (T)RAND_MAX) * ulp;
  T round = 0;
  if (fabs(t + p) >= fabs(ulp))
    round = ulp;
  return round;
}

} // namespace Double

namespace Float {

using FPTYPE = float;
using INTTYPE = uint32_t;
using SINTTYPE = int32_t;
constexpr int DEFPREC = 24;
constexpr int DEFSIGNIFICAND = 23;
constexpr int EMAX = 127;
constexpr int NBITS = 32;
constexpr int NLEADBITS = 9;
constexpr int NRNDBITS = 23;
constexpr uint32_t FULLMASK = 0xFFFFFFFF;
constexpr uint32_t ABSMASK = 0x7FFFFFFF;
constexpr uint32_t EXPMASK = 0x7F800000;
constexpr uint32_t FRACMASK = 0x007FFFFF;
constexpr uint32_t SIGNMASK = 0x80000000;

typedef union {
  FPTYPE fpval;
  INTTYPE intval;
} STRUCTNAME;

constexpr INTTYPE INTCONST(int x) { return static_cast<INTTYPE>(x); }
constexpr INTTYPE INTOF(const FPTYPE x) {
  STRUCTNAME _s = {.fpval = x};
  return _s.intval;
}
constexpr FPTYPE FPOF(INTTYPE x) {
  STRUCTNAME _s = {.intval = x};
  return _s.fpval;
}
constexpr int SIGN(FPTYPE x) { return (x > 0) - (x < 0); }
constexpr INTTYPE FASTSIGN(const FPTYPE x) { return SIGNMASK & INTOF(x); }
constexpr INTTYPE INTABS(const FPTYPE x) { return ABSMASK & INTOF(x); }
constexpr FPTYPE ABS(const FPTYPE x) { return FPOF(ABSMASK & INTOF(x)); }

/* Compute IEEE floating-point exponent of x. */
template <typename T> static inline int get_exponent(T x) {
  return (((INTOF(x) & EXPMASK) >> DEFSIGNIFICAND) - EMAX);
}

/* Compute absolute value of ulp(x). */
template <typename T> static inline INTTYPE ulp_abs(T x) {
  SINTTYPE tmp = get_exponent(x);
  if (tmp <= (-EMAX + DEFSIGNIFICAND)) // ulp(x) is subnormal
    return INTCONST(1);
  else // ulp(x) i normal
    return INTCONST(tmp + (+EMAX - DEFSIGNIFICAND)) << DEFSIGNIFICAND;
}

/* A helper function for SR algorithms without the change of rounding mode. */
template <typename T> static inline T sr_round(const T s, const T t) {
  T ulp;
  if (FASTSIGN(t) != FASTSIGN(s))
    //    ulp = FPOF(FASTSIGN(&t) | ulp_abs(s*(1 - ldexp(1, -53))));
    ulp = SIGN(t) *
          ldexp(1, get_exponent(s * (1 - ldexp(1, -DEFPREC))) - DEFSIGNIFICAND);
  else
    //    ulp = FPOF(FASTSIGN(&t) | ulp_abs(s));
    ulp = SIGN(t) * ldexp(1, get_exponent(s) - DEFSIGNIFICAND);

  T p = (rand() / (T)RAND_MAX) * ulp;
  T round = 0;
  if (fabs(t + p) >= fabs(ulp))
    round = ulp;
  return round;
}

} // namespace Float

template <typename T> static inline T sr_round(const T s, const T t) {
  if (std::is_same<T, Double::FPTYPE>::value)
    return Double::sr_round(s, t);
  else
    return Float::sr_round(s, t);
}

template <typename T> static inline T SIGN(T x) {
  if (std::is_same<T, Double::FPTYPE>::value)
    return Double::SIGN(x);
  else
    return Float::SIGN(x);
}

template <typename T, typename T> static inline I get_exponent(T x) {
  if (std::is_same<T, Double::FPTYPE>::value)
    return Double::get_exponent(x);
  else
    return Float::get_exponent(x);
}

/************************
 * AUGMENTED OPERATIONS *
 ************************/

/* Compute s and t such that s+t = a+b. */
template <typename T>
static inline void two_sum(T *s, T *t, const T a, const T b) {
  *s = a + b;
  T ap = *s - b;
  T bp = *s - ap;
  ap = a - ap;
  bp = b - bp;
  *t = ap + bp;
}

/* If |a| > |b|, compute s and t such that s+t = a+b. */
template <typename T>
static inline void fast_two_sum(T *s, T *t, const T a, const T b) {
  assert(fabs(a) > fabs(b));
  *s = a + b;
  *t = (*s - a) - b;
}

/* Compute s and t such that s+t = a*b. */
template <typename T>
static inline void two_prod_fma(T *s, T *t, const T a, const T b) {
  *s = a * b;
  *t = fma(a, b, -*s);
}

/****************************************************
 * ALGORITHMS WITH EXPLICIT CHANGE OF ROUNDING MODE *
 ****************************************************/

template <typename T> T sr_add(const T, const T);
template <typename T> T sr_mul_fma(const T, const T);
template <typename T> T sr_div(const T, const T);
template <typename T> T sr_sqrt(const T);

template <> double sr_add(const double, const double);
template <> double sr_mul_fma(const double, const double);
template <> double sr_div(const double, const double);
template <> double sr_sqrt(const double);

template <> float sr_add(const float, const float);
template <> float sr_mul_fma(const float, const float);
template <> float sr_div(const float, const float);
template <> float sr_sqrt(const float);

/*****************************************************
 * ALGORITHMS WITH SIMULATED CHANGE OF ROUNDING MODE *
 *****************************************************/

template <typename T> T sr_add_rn_only(const T a, const T b);
template <typename T> T sr_mul_fma_rn_only(const T, const T);
template <typename T> T sr_div_rn_only(const T, const T);
template <typename T> T sr_sqrt_rn_only(const T);

template <> double sr_add_rn_only(const double, const double);
template <> double sr_mul_fma_rn_only(const double, const double);
template <> double sr_div_rn_only(const double, const double);
template <> double sr_sqrt_rn_only(const double);

template <> float sr_add_rn_only(const float, const float);
template <> float sr_mul_fma_rn_only(const float, const float);
template <> float sr_div_rn_only(const float, const float);

#endif // STOCHRNDDW_H_
