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
#include <sys/time.h>

using namespace std;

void MLog(const char* message)
{
  struct timeval tv;
  struct timezone tz;
  struct tm *tm;
  gettimeofday(&tv, &tz);
  tm = localtime(&tv.tv_sec);
  fprintf(stdout, "%d:%02d:%02d.%d %s", 
      tm->tm_hour, tm->tm_min, tm->tm_sec, tv.tv_usec/1000, message);
}

int main(int argc, char **argv)
{
  MLog("1\n"); for (int i = 0; i < 90000000; ++i) ;
  MLog("2\n"); for (int i = 0; i < 90000000; ++i) ;
  MLog("3\n"); for (int i = 0; i < 90000000; ++i) ;
  MLog("4\n"); for (int i = 0; i < 90000000; ++i) ;
  MLog("5\n"); for (int i = 0; i < 90000000; ++i) ;
  MLog("6\n"); for (int i = 0; i < 90000000; ++i) ;
  MLog("7\n");
  return 0;
}
