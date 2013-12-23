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

template <typename T, T n, T m>
class FixedInterval
{
  public:

  private:
  T value;

};

int main(int argc, char **argv)
{
  FixedInterval<int, 0, 100> a;
  a.value = 20;

  FixedInterval<int, -100, 100> b;
  a = b;
  
  return 0;
}
