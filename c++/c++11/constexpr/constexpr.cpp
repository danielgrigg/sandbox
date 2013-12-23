#include <iostream>
#include <algorithm>

using namespace std;

constexpr int array_size() { return 3 + 4; }
int foo[array_size()];

class A {
  constexpr static int foo() { return 3 + 5; }
};

int main(int argc, char **argv) {
  std::fill(foo, foo+array_size(), 9);
  std::copy(foo, foo+array_size(), std::ostream_iterator<int>(cout));
  cout << endl;
  return 0;
}
