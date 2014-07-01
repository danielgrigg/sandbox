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

#include <tr1/memory>

using namespace std;

int main(int argc, char **argv)
{
  tr1::shared_ptr<int> a(new int(32));  
  
  cout << "a: " << *a << endl;
  return 0;
}
