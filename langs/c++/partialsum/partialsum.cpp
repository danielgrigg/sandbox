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

int main(int argc, char **argv)
{
  int a[] = {1,1,1,1,1,
             1,1,1,1,1};
  int b[10];
  std::partial_sum(a, a+10, b);
  std::ostream_iterator<int> osi(cout, " ");
  std::copy(b, b+10, osi);
  std::cout << std::endl;
  return 0;
}
