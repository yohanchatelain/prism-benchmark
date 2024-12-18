#include <math.h>
#include <stdio.h>

#define LEARNING_RATE 0.001   // Small learning rate
#define NUM_ITERATIONS 1000   // Number of iterations
#define STARTING_POINT 100000 // Starting point for gradient descent

// Function definition: f(x) = log(x^2 + 10^10) - log(10^10)
double f(double x) { return log(pow(x, 2) + pow(10, 10)) - log(pow(10, 10)); }

// Gradient (derivative) of the function
double grad_f(double x) { return 2 * x / (pow(x, 2) + pow(10, 10)); }

// Gradient Descent function
void gradient_descent(double x0) {
  double x = x0;
  for (int i = 0; i < NUM_ITERATIONS; i++) {
    double grad = grad_f(x);
    x = x - LEARNING_RATE * grad; // Update the value of x

    // Optionally print the iteration and value of x
    if (i % 100 == 0) {
      printf("Iteration %d: x = %f, f(x) = %f\n", i, x, f(x));
    }
  }
  printf("Final result: x = %f, f(x) = %f\n", x, f(x));
}

int main() {
  printf("Starting gradient descent with initial point %f\n", STARTING_POINT);
