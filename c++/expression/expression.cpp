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

#include "expression.h"

using namespace std;

int main(int argc, char **argv)
{
  Order o;
  Contract c;

  std::cout << "Doing Order invoice\n";
  invoice(o);

  std::cout << "Doing Contract invoice\n";
  invoice(o);
  return 0;
}
