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

template<int N>
struct A
{
  int foo() { return N; }
};

int main(int argc, char **argv)
{
  A<3> x;
  std::cout << "x: " << x.foo() << std::endl;
  A<5> y;
  std::cout << "y: " << y.foo() << std::endl;
  return 0;
}
