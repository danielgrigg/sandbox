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
  return rand() % 10;
}

int main(int argc, char **argv)
{
  int myInts[10] = {0};
  std:generate(myInts, myInts, myrand);

  uint32_t y = ~0;
  std::cout << "~0 " << y << endl;
  return 0;
}
