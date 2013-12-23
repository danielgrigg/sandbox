#include <stdio.h>
#include "myproc.h"

void myproc(int n)
{
  int p;
  p = 2*n;
  printf("Two times %d is %d\n", n, p);
}

