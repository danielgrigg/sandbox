#include <algorithm>
#include <fstream>
#include <functional>
#include <iomanip>
#include <iostream>
#include <iterator>
#include <map>
#include <numeric>
#include <sstream>
#include <stdint.h>
#include <string>
#include <vector>

using namespace std;


#include <execinfo.h>
#include <stdio.h>

int MyDFunc()
{
  cout << "In D!\n";
  void* callstack[128];
  int i, frames = backtrace(callstack, 128);
  char** strs = backtrace_symbols(callstack, frames);
  for (i = 0; i < frames; ++i) {
    printf("%s\n", strs[i]);
  }
  free(strs);
  return 4;
}

int MyCFun()
{
  cout << "In C!\n";
  return 2*MyDFunc();
}
int Bar()
{
  cout << "In B!\n";
  return 3*MyCFun();
}

int Foo()
{
  cout << "In A!\n";
  return 4*Bar();
}

int main(int argc, char **argv)
{
  cout << "Calling Foo" << endl;
  cout << "Foo == " << Foo() << endl;
  return 0;
}
