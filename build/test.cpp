#include <iostream>

int add (int nb[3]) {
  return nb[0] + nb[1] + nb[2];
}

int main () {
  int res [3] = {0,1,3};
  printf("%d\n", res[1]);
  res[1] = 23;
  printf("%d\n", add(res));
  return res[0];
}