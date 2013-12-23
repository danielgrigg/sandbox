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

template <int v>
struct Int2Type
{
  enum { value = v };
};

template <typename T>
struct Type2Type
{
  typedef T OriginalType;
};

int main(int argc, char **argv)
{
  class Local
  {
    // stuff here
  };
  Local l;

  const Local& r = l;

  cout << "hello world" << endl;
  return 0;
}
