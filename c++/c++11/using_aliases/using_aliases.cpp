#include <iostream>

using namespace std;

template <typename A, typename B, int C>
class MyType {
};

template <typename B>
using MyTypeAlias = MyType<int, B, 8>;

int foo(double x) { return x * 2.0; }

int main(int argc, char **argv)
{
  MyType<int, int, 3> a;
  MyTypeAlias<float> b;

  using MyFunAlias = int(*)(double);
  MyFunAlias f = foo;
  cout << "f(3.8) " << f(3.8) << endl;
  return 0;
}
