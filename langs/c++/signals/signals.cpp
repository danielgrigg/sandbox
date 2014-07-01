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
#include <boost/bind.hpp>
#include <boost/function.hpp>

using namespace std;
using namespace boost;

struct A
{
  int foo;
};

int main(int argc, char **argv)
{
  A a;
  a.foo = 2;

  int bar = 7;

  function<int()> myFoo = bind(&A::foo, a);
  cout << "a::foo " << bind(&A::foo, a)() << endl;
  a.foo = 5;
  cout << "a::foo " << myFoo() << endl;

  function<int()> myBar = bind(&bar);
  cout << "bar " << myBar << endl;
  return 0;
}
