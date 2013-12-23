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
  std::cin >> std::noskipws >> std::cout.rdbuf();

  std::cout << std::cin.rdbuf();
  
  std::copy(istreambuf_iterator<char>(std::cin), 
            istreambuf_iterator<char>(), 
            ostreambuf_iterator<char>(std::cout));

  return 0;
}
