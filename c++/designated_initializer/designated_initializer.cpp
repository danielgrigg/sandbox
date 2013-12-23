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

struct A
{
  int foo;
  int bar; 
};

int main(int argc, char **argv)
{
  A a = {.foo = 3, .bar = 7};

  cout << "A.foo = " << a.foo << ", " << "A.bar = " << a.bar << endl;
  return 0;
}
