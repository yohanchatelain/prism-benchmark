#include <stdio.h>
#include <stdlib.h>

int main(int argc, char *argv[]) {
  if (argc != 2) {
    printf("Usage: %s <number_of_iterations>\n", argv[0]);
    return 1;
  }

  int iterations = atoi(argv[1]);
  double sum = 0.0;

  for (int i = 0; i < iterations; i++) {
    sum += 0.1 * i + 0.01 * i * i;
  }

  printf("Sum after adding 0.1 for %d iterations: %13a\n", iterations, sum);

  return 0;
}
