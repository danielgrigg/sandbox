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
  int *data = NULL;
  for (int i = 0; i < 120; i++)
  {
    data = new int[1000];
    memset(data, 0xAC, 1000*sizeof(int));
    cout << '.' << flush;
    sleep(1);
  }
  return 0;
}
