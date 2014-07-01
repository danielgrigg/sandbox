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

  template<typename T, typename U>
T convert(U value)
{
  std::cout << "generic convert!\n" << endl;
  return value;
}

 double convert(uint32_t value)
{
  std::cout << "uint32_t->double convert!\n" << endl;
  return (double)value / 3600000.0;
}

 uint32_t convert(double deg)
{
  std::cout << "double->uint32_t convert!\n" << endl;
  return deg * 3600000.0;
}


int main(int argc, char **argv)
{
  uint32_t x = convert(34.3);
  double y = convert(22U);

  cout << x << ", " << y << endl;
  return 0;
}
