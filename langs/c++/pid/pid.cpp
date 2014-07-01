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
#include <sys/types.h>

using namespace std;

int main(int argc, char **argv)
{
  pid_t p = getpid();
  std::cout << "my pid: " << p << std::endl;
  return 0;
}
