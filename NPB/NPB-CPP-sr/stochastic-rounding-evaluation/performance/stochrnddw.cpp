#include "stochrnddw.hpp"
#include <fenv.h>
#include <stdio.h>

/* Compute stochastic rounding of a+b (new method). */
template <typename T> T _sr_add(const T a, const T b) {
  int prevround = fegetround();
  fesetround(FE_TONEAREST);

  // Compute floating-point approximation of sum and error.
  T s, t;
  two_sum(&s, &t, a, b);

  // Compute exponent with truncation.
  int exponent;
  fesetround(FE_TOWARDZERO);
  frexp(a + b, &exponent); // Exponent of fraction / 2 (i.e. in [0.5, 1)).
  /* assert(exponent-1 == floor(log2(fabs(a+b)))); */

  // Compute and renormalize random number.
  T p = ldexp(SIGN(t) * (rand() / (T)RAND_MAX), exponent - 53);

  // Compute and return result.
  if (t >= 0)
    fesetround(FE_DOWNWARD);
  else
    fesetround(FE_UPWARD);
  T r = (t + p) + s;
  fesetround(prevround);
  return r;
}

/* Compute approximate stochastic rounding of a+b (new method) */
template <typename T> T _fast_sr_add(const T a, const T b) {
  int prevround = fegetround();
  fesetround(FE_TOWARDZERO);

  // Compute floating-point approximation of sum and error.
  T s, t;
  two_sum(&s, &t, a, b);

  // Compute exponent with truncation.
  int exponent;
  frexp(s, &exponent);

  // Compute and renormalize random number.
  T p = ldexp(SIGN(t) * (rand() / (T)RAND_MAX), exponent - 53);

  // Compute and return result.
  T r = (t + p) + s;
  fesetround(prevround);
  return r;
}

/* Compute stochastic rounding of a*b (new method). */
template <typename T> T _sr_mul_fma(const T a, const T b) {
  int prevround = fegetround();
  fesetround(FE_TOWARDZERO);

  // Compute floating-point approximation of product and error.
  T s, t;
  two_prod_fma(&s, &t, a, b);

  // Compute exponent with truncation.
  int exponent;
  frexp(s, &exponent);

  // Compute and renormalize random number.
  T p = ldexp(SIGN(t) * (rand() / (T)RAND_MAX), exponent - 53);

  // Compute and return result.
  T r = (t + p) + s;
  fesetround(prevround);
  return r;
}

/* Compute stochastic rounding of a/b (new method). */
template <typename T> T _sr_div(const T a, const T b) {
  int prevround = fegetround();
  fesetround(FE_TOWARDZERO);

  // Compute floating-point quotient, remainder, and residual.
  T s, t;
  s = a / b;
  t = fma(-s, b, a);
  t = t / b;

  // Compute exponent with truncation.
  int exponent;

  frexp(s, &exponent);

  // Compute and renormalize random number.
  T p = ldexp(SIGN(t) * (rand() / (T)RAND_MAX), exponent - 53);

  // Compute and return result.
  T r = (t + p) + s;
  fesetround(prevround);
  return r;
}

/* double instances */
template <> double sr_add(const double a, const double b) {
  return _sr_add(a, b);
}

template <> double fast_sr_add(const double a, const double b) {
  return _fast_sr_add(a, b);
}

template <> double sr_mul_fma(const double a, const double b) {
  return _sr_mul_fma(a, b);
}

template <> double sr_div(const double a, const double b) {
  return _sr_div(a, b);
}

/* float instances */
template <> float sr_add(const float a, const float b) { return _sr_add(a, b); }

template <> float fast_sr_add(const float a, const float b) {
  return _fast_sr_add(a, b);
}

template <> float sr_mul_fma(const float a, const float b) {
  return _sr_mul_fma(a, b);
}

template <> float sr_div(const float a, const float b) {
  return _sr_div<float>(a, b);
}
