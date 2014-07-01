#include <iostream>

using namespace std;

template <class A, class B>
  auto my_adder(A a, B b) -> decltype(a + b) { return a + b; }

auto bar(int x, int y) -> int {
  return x + y;
}

int main(int argc, char **argv)
{
  auto x = my_adder(2.0, 3.0);
  auto y = my_adder(5, 7);
  auto z = my_adder(11, 13.0);

  cout << "x " << x << ", y " << y << ", z " << z 
    << ", bar(3,4) " << bar(3,4) << endl;
  return 0;
}
