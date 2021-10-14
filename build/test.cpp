#include <iostream>

int apply (int a, int (*func)(int)) {
  return (*func)(a);
}

int by2 (int a) { return 2 * a; }

int main () {
  int res = apply(85, *by2);
  printf("%d\n", res);
  return 0;
}