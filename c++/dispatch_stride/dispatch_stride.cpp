
#include <fstream>
#include <iostream>
#include <algorithm>
#include <iterator>
#include <iomanip>
#include <dispatch/dispatch.h>
#include <stdint.h>

using namespace std;

int main(int argc, char *argv[])
{
  int count = 16;
  int stride = 4;
  dispatch_apply(count / stride, dispatch_get_global_queue(0, 0), ^(size_t index) {
      size_t j = index * stride;
      size_t jEnd = j + stride;
      while (j < jEnd)
      {
        printf("%u\n", (uint32_t)j);
        ++j;
        }
    });
  cout << "All done!" << endl;

  return 0;
}


