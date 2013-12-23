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
#include <boost/lambda/lambda.hpp>
#include <boost/lambda/bind.hpp>

using namespace std;
using namespace boost::lambda;

int square(int x)
{
  return x * x;
}

int main(int argc, char **argv)
{
  int x[] = {1, 2, 3, 5, 7, 11, 13, 17};

  int sum = 0;
  vector<int> y(x, x+8);
  for_each(y.begin(), y.end(), std::cout << constant("< ") << _1 << " >");
  for_each(y.begin(), y.end(), std::cout << constant("< ") << bind(square, _1) << " >");
  for_each(x, x+8, sum += _1);
  std::cout << "\nsum: " << sum << endl;
  vector<int> z;
  for_each(y.begin(), y.end(), bind(&vector<int>::push_back, &z, _1));
  std::cout <<"\nz: ";
  for_each(z.begin(), z.end(), std::cout << _1 << ' ');
  return 0;
}
