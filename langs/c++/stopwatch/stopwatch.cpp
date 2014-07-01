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
#include <unistd.h>

using namespace std;

#ifdef __APPLE__ && __MACH__
#define DARWIN
#endif

#ifdef DARWIN
#include <mach/mach_time.h>
#endif

class Stopwatch
{
  public:
    Stopwatch();

    void reset();

    float timeElapsed_s()const;
  private:
#ifdef DARWIN
    uint64_t m_timeStarted;  // Mach-time-units
    uint32_t m_freqDenom;
    uint32_t m_freqNumer;
#endif
    
};

Stopwatch::Stopwatch()
{
#ifdef DARWIN
  mach_timebase_info_data_t info;
  mach_timebase_info(&info);
  m_freqNumer = info.numer;
  m_freqDenom = info.denom;
#endif
  reset();
}

void Stopwatch::reset()
{
#ifdef DARWIN
  m_timeStarted = mach_absolute_time();
#endif
}

float Stopwatch::timeElapsed_s()const
{
#ifdef DARWIN
  uint64_t timeNow = mach_absolute_time();
  uint64_t dt = timeNow - m_timeStarted;
  uint64_t dt_ns = dt * m_freqNumer / m_freqDenom;
  return dt_ns * 1E-9;
#endif
}

int main(int argc, char **argv)
{
  Stopwatch sw;
  if (1)
  {
    cout << "Counting 10s..." << endl;
    sw.reset();
    while (sw.timeElapsed_s() < 10.0f)
    {
      ;
    }
    cout << "10s is up!" << endl;

    cout << "Timing pid" << endl;
    double total_s = 0.0;
    sw.reset();
    for (int i = 0; i < 10000; ++i)
    {
      getpid();
      float s = sw.timeElapsed_s();
      total_s += s;
    }
    total_s /= 100.0;
    cout << "~pid s: " << total_s << endl;
  
  }
  else
  {
    sw.reset();
    char a;
    cin >> a;
    cout << "elapsed_s: " << sw.timeElapsed_s() << endl;
  }

  return 0;
}
