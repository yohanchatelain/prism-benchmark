#include <cadna.h>
using namespace std;

#include <stdio.h>
#include <stdlib.h>

float_st harmonic_series(int n) {
  float_st sum = 0.0;
  for (int i = 1; i <= n; i++) {
    float_st i_st = i;
    sum += 1.0f / i_st;
  }
  return sum;
}

int main(int argc, char *argv[]) {
  cadna_init(-1);

  if (argc != 2) {
    printf("Usage: %s <number_of_terms>\n", argv[0]);
    return 1;
  }

  int n = atoi(argv[1]);
  if (n <= 0) {
    printf("Please enter a positive integer for the number of terms.\n");
    return 1;
  }

  float_st result = harmonic_series(n);
  printf("The harmonic series sum of %d terms is: %s  (%.13a)\n", n,
         strp(result), result);
  result.display("RESULT: ");
  printf("\n");

  return 0;

  cadna_end();
}