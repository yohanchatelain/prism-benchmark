#include "stochrnddw.hpp"
#include <stdio.h>

/* Compute stochastic rounding of a+b (new method). */
template <typename T> T _sr_add_rn_only(const T a, const T b) {
  // Compute floating-point approximation of sum and error.
  T s, t;
  two_sum(&s, &t, a, b);

  // Check whether SR rounds up or down.
  T round = sr_round(s, t);

  // Compute and return result.
  T r = round + s;
  return r;
}

/* Compute stochastic rounding of a*b (new method). */
template <typename T> T _sr_mul_fma_rn_only(const T a, const T b) {
  // Compute floating-point approximation of product and error.
  T s, t;
  two_prod_fma(&s, &t, a, b);

  // Check whether SR rounds up or down.
  T round = sr_round(s, t);

  // Compute and return result.
  T r = round + s;
  return r;
}

/* Compute stochastic rounding of a/b (new method). */
template <typename T> T _sr_div_rn_only(const T a, const T b) {
  // Compute floating-point quotient, remainder, and residual.
  T s, t;
  s = a / b;
  t = fma(-s, b, a);
  t = t / b;

  // Check whether SR rounds up or down.
  T round = sr_round(s, t);

  // Compute and return result.
  T r = round + s;
  return r;
}

/* Compute stochastic rounding of sqrt(a) (new method). */
template <typename T> T _sr_sqrt_rn_only(const T a) {
  T s, t;
  s = sqrt(a);
  t = fma(-s, s, a);
  t = t / (2 * s);

  // Check whether SR rounds up or down.
  T round = sr_round(s, t);

  // Compute and return result.
  T r = round + s;
  return r;
}

template <> float sr_add_rn_only<float>(const float a, const float b) {
  return _sr_add_rn_only(a, b);
}

template <> double sr_add_rn_only<double>(const double a, const double b) {
  return _sr_add_rn_only(a, b);
}

template <> double sr_mul_fma_rn_only<double>(const double a, const double b) {
  return _sr_mul_fma_rn_only<double>(a, b);
}
template <> float sr_mul_fma_rn_only<float>(const float a, const float b) {
  return _sr_mul_fma_rn_only<float>(a, b);
}

template <> double sr_div_rn_only<double>(const double a, const double b) {
  return _sr_div_rn_only<double>(a, b);
}
template <> float sr_div_rn_only<float>(const float a, const float b) {
  return _sr_div_rn_only<float>(a, b);
}

template <> double sr_sqrt_rn_only<double>(const double a) {
  return _sr_sqrt_rn_only<double>(a);
}
template <> float sr_sqrt_rn_only<float>(const float a) {
  return _sr_sqrt_rn_only<float>(a);
}