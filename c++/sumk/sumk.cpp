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

int myrand()
{
  return rand() % 20;
}

int main(int argc, char **argv)
{
  const int n = 10, k = 5;
  int a[n];
  std::generate(a, a+n, myrand);
  std::ostream_iterator<int> osi(cout, " ");
  std::copy(a, a+n, osi);
  cout << "\n";

  std::partial_sort(a, a + k, a + n, std::greater<int>());
  std::copy(a, a+n, osi);
  cout << "\n";
  int sum = std::accumulate(a, a+k, 0);
  std::cout << "sum: " << sum << std::endl;

  return 0;
}
