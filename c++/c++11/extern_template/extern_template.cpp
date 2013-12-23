#include <iostream>
#include <vector>

using namespace std;

class Foo {
  int a;
};

extern template class std::vector<Foo>;

int main(int argc, char **argv)
{
  cout << "hello world" << endl;
  return 0;
}
