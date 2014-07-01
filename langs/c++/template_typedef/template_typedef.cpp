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
#include <tr1/memory>

using namespace std;

template <class T>
class Foo
{
  public:
    Foo(T x):bar(x){}

  T bar;
};

// The below won't work, pity. Wait for c++0x
//typedef tr1::shared_ptr<Foo<T> > FooPtr<T>;
typedef tr1::shared_ptr<Foo<int> > FooPtr;

int main(int argc, char **argv)
{
  FooPtr f(new Foo<int>(42));
  cout << "f: " << f->bar << endl;
  return 0;
}
