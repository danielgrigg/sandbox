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

#include <time.h>

using namespace std;

uint64_t tomillis(const timespec& ts)
{
  uint64_t ret = (uint64_t)ts.tv_sec * 1000ul + ts.tv_nsec / 1000000;  
  return ret;
}


int main(int argc, char **argv)
{
  timespec ts;
  clock_gettime(CLOCK_MONOTONIC, &ts);
  uint64_t start = tomillis(ts);
  for (int i = 0; i < 5; ++i)
  {
    // CLOCK_MONOTONIC_RAW
    if (0 != clock_gettime(CLOCK_MONOTONIC, &ts))
    {
      cerr << "clock_gettime error";
      return 1;
    }

    cout << "tv_sec: " << ts.tv_sec << ", tv_nsec: " << ts.tv_nsec << endl;
    cout << "millis: " << tomillis(ts) << endl;
    cout << "deltamillis: " << tomillis(ts) - start << endl;
    uint64_t sleepMillis = rand() % 1000;
    cout << "  sleeping " << sleepMillis << "ms" << endl;
    timespec sleepts;
    sleepts.tv_sec = 0; sleepts.tv_nsec = sleepMillis * 1000000;
    nanosleep(&sleepts, NULL);
  }
  return 0;
}
