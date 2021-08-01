# Ocaml LLVM IR Wrapper

Use `./build/bc_to_llvm input_file.bc [output_file]` to convert *llvm ir* bytecode to human-readable *llvm ir*


Use `cd build && make && ./main [output_file]` to generate two little *llvm ir* programs:

1. `sum.ll` corresponds to:
```c
#include <stdio.h>

void sum (int a, int b, int*res) {
  *res = a + b;
}

int main () {
  int a;
  int b;
  int res;
  printf("Type number 1: ");
  scanf("%d", &a);
  printf("Type number 2: ");
  scanf("%d", &b);
  sum(a, b, &res);
  printf("Result: %d\n", res);
}
```

2. `comps.ll` corresponds to:
```c
#include <stdio.h>

int comps (int a, int b) {
  return a > b || a == b;
}

int main () {
  int a;
  int b;
  printf("Type number 1: ");
  scanf("%d", &a);
  printf("Type number 2: ");
  scanf("%d", &b);
  int res = comps(a, b);
  printf("Result: %d\n", res);
}
```

## Dependencies
+ [llvm](https://opam.ocaml.org/packages/llvm/)

## License
[GPL v3](https://choosealicense.com/licenses/gpl-3.0/)
