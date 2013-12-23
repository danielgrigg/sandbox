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

struct Foo {
  int a;
  float b;
  std::string c;
};

struct Qux {
  int a;
  double b;
};

struct MySeq {
  std::vector<int> vals;

  MySeq(std::initializer_list<int> list):
    vals(list)
  {}
};

std::ostream& operator<<(ostream& os, const Foo& f) {
  os << f.a << ", " << f.b << ", " << f.c;
  return os;
}

int bar(std::initializer_list<float> list) {
  return std::accumulate(list.begin(), list.end(), 0);
}

Qux make_qux(int x) { return {x, 2.0 * x}; }

int main(int argc, char **argv)
{
  Foo fs[] = {{3, 4.0, "five"}, {6, 8.8, "eight"}};
  cout << fs[0] << "\n" << fs[1] << endl;

  MySeq s = {1, 2, 3, 5};

  cout << "bar " << bar({1.0, 4.0, 7.0}) << endl;
  
  // Uniform initialization
  //
  Foo f2 {2, 4.4, "f2"};
  Qux q1 {7, 9.9};

  cout << "f2 " << f2 << endl;
  cout << "make_qux(3) " << make_qux(3).a << ", " << make_qux(3).b << endl;
  return 0;
}
