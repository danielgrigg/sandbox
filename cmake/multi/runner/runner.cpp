#include "mylib.hpp"
#include <iostream>

int main()
{
  cout << "Calling into MyLib...\n";

  MyLib a("World!");
  a.hello();

  return 0;
}

