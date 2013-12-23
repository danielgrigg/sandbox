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
#include <stdlib.h>

using namespace std;

int main(int argc, char **argv)
{
  srandom(time(NULL));

  int _min = 11;
  int _max = 18;
  for (int i = 0; i < 100000; ++i)
  {
    int r = _min + random() % (_max + 1 - _min);
    cout << r << endl;
  }
  return 0;
}
