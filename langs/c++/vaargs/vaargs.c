#include <stdarg.h>

int Foo(int l, ...)
{
  va_list vl;
  va_start(vl, l);
  int a = va_arg(vl, int);
  va_end(vl);
  return a;
}

int main()
{
  return Foo(1, 42);
}

