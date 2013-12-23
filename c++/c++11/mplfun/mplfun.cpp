#include <iostream>
#include <string>

using namespace std;

template <class, class> struct cons { };
struct nil {};
template <int x> struct Int {
  static const int v = x;
};

template <typename x, typename y>
struct add {
  typedef Int<x::v + y::v> v;
};

int main(int argc, char **argv)
{
  auto xs = cons<Int<3>, nil>();
  auto xs2 = cons<cons<Int<2>,Int<3>>, nil>();
  add<Int<3>, Int<4>>::v y;
  cout << "y: " << y.v << endl;

  return 0;
}
