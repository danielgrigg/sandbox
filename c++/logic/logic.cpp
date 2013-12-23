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

int foo = 1;

int main(int argc, char **argv)
{
  cout << "foo: " << foo << endl;
  foo = !foo;
  cout << "foo: " << foo << endl;
  return 0;
}
