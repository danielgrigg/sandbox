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

template <typename A>
struct Foo
{
  A _member;
};

template <typename A>
Foo<A> makeFoo(int x);

template <>
Foo<int> makeFoo(int x)
{
  Foo<int> f;
  f._member = x;
  return f;
}

int main(int argc, char **argv)
{
  Foo<int> x = makeFoo<int>(32);
  return 0;
}
