#include <stdio.h>

int main()
{
  int result = rename("foo.txt", "foo.txt");
  printf("result = %d\n", result);
  return result;
}
