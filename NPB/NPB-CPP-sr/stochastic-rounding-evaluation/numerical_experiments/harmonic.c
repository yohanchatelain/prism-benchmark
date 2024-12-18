#include <stdio.h>
#include <stdlib.h>

float harmonic_series(int n) {
  float sum = 0.0;
  for (int i = 1; i <= n; i++) {
    sum += 1.0f / i;
  }
  return sum;
}

int main(int argc, char *argv[]) {
  if (argc != 2) {
    printf("Usage: %s <number_of_terms>\n", argv[0]);
    return 1;
  }

  int n = atoi(argv[1]);
  if (n <= 0) {
    printf("Please enter a positive integer for the number of terms.\n");
    return 1;
  }

  float result = harmonic_series(n);
  printf("The harmonic series sum of %d terms is: %f  (%.13a)\n", n, result,
         result);

  return 0;
}