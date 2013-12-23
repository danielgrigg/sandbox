#include <iostream>

using namespace std;

class Foo {
  public:
  int _x;
  int _y;
  int _z = 9; // Member initialization

  Foo(int x, int y):_x(x), _y(y) {}

  Foo(int x):Foo(x, 7) {}

  Foo():Foo(3, 7) {}
};

std::ostream& operator<< (std::ostream& os, const Foo& f) {
  os << "(" << f._x << " " << f._y << " " << f._z << ")";
  return os;
}

#if 0
// Inheriting constructors not supported..booo
class DerivedFoo : public Foo {
  public:
  using Foo::Foo;
};
#endif

int main(int argc, char **argv)
{
  cout << "Foo(1,2) " << Foo(1,2) << endl;
  cout << "Foo(8) " << Foo(8) << endl;
  cout << "Foo() " << Foo() << endl;

  return 0;
}

