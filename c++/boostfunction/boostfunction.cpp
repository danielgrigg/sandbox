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
#include <boost/function.hpp>
#include <boost/bind.hpp>

using namespace std;

struct int_div { 
  float operator()(int x, int y) const { return ((float)x)/y; }; 
};

void do_sum_avg(int values[], int n, int& sum, float& avg)
{
  sum = 0;
  for (int i = 0; i < n; i++)
    sum += values[i];
  avg = (float)sum / n;
}

struct X {
  int foo(int i){ return i*i; }
};

int main(int argc, char **argv)
{
  boost::function<float (int x, int y)> f;
 // boost::function2<float, int, int> f2;
  f = int_div();
  std::cout << f(5, 3) << endl;

  boost::function<void (int values[], int n, int& sum, float& avg)> sum_avg;
//  boost::function4<void, int*, int, int&, float&> sum_avg;

  sum_avg = &do_sum_avg;
  int values[] = {2, 3, 5, 7, 11, 13};
  int sum;
  float avg;
  sum_avg(values, 6, sum, avg);
  std::cout << "sum = " << sum << ", avg = " << avg << endl;

  boost::function<int (X*, int)> g;
  // boost::function2<int, X*, int> g;
  g = &X::foo;
  X x;
  cout << "g(7) = " << g(&x, 7) << endl;

  boost::function<int (int)> h(boost::bind(&X::foo, &x, _1));
  cout << "h(9) = " << h(9) << endl;
  return 0;
}
