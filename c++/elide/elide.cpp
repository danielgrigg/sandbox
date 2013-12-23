#include <stdio.h>

int n = 0;
struct C {
  C(int) {}
  C(const C&) { ++n; } // the copy constructor has a visible side effect
};                     // it modifies an object with static storage duration

int foo(int x, int y);
{
  return x + y;
}
 
int main() {
  C c1(42); // direct-initialization, calls C::C(42)
  C c2 = 42; // copy-initialization, calls C::C(42) _or_ C::C( C(42) )

  foo();
 
  printf("n = %d\n", n);
  return n; // returns either 0 or 1, depending on whether the copy was elided
}

