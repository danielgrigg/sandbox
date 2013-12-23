#include <stdio.h>
#include "bar.h"

int foo(int x) {
  return x * x;
}

int bar(int x, int y) { 
  return x + y;
}

int main() {
  printf("foo 4 = %d\n", foo(4));
  printf("bar 5 7 = %d\n", bar(5, 7));al
  return 0;
}
