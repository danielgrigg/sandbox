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

#include "Tga.h"

using namespace std;

int main(int argc, char **argv)
{
  int32_t width, height, components;
  uint32_t format;
  vector<int8_t> pixels;
  bool loadResult = gltLoadTGA("image2.tga", &width, &height, &components, &format, pixels);

  if (!loadResult)
  {
    std::cout << "load failed\n";
    return 1;
  }
  std::cout << "load success\n";
  bool writeResult = gltWriteTGA("image2_copy.tga", width, height, pixels);
  if (!writeResult)
  {
    std::cout << "write failed\n";
    return 1;
  }
  std::cout << "write success\n";
  return 0;
}
