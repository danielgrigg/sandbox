
#include <fstream>
#include <iostream>
#include <algorithm>
#include <iterator>
#include <iomanip>
#include <dispatch/dispatch.h>

using namespace std;

int main(int argc, char *argv[])
{
  int count = 100;
  dispatch_apply(count, dispatch_get_global_queue(0, 0), ^(size_t index)
      {
      int r = rand() % 5000;
      cout << "block " << index << " started and idling for " << r * 1E+6 << " iterations" << endl;
      for (int i = 0; i < 1000000 * r; ++i)
      {
        double a = 23.34;
        int b = (int)a / (i+1);
      }
      cout << "block " << index << " done!" << endl;
    });
  cout << "All done!" << endl;

  return 0;
}


