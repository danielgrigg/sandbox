#include "myproc.h"

typedef struct {
  float a;
  float b;
} substruct;

struct {
  int i;
  substruct r;
} c;

int main() {
  int n = 4;
  int m[10] = {0,1,4,9,16,25,36,49,64,81};
  printf("m4 %d\n", m[4]);
  n = 5;
  myproc(n);
  c.i = 1;
  c.r.a = 0.5;
  c.r.b = 0.25;
  return 0;
}

