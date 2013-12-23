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
#include <boost/thread.hpp>
#include <boost/chrono.hpp>

using namespace std;

typedef boost::mutex Mutex;
typedef boost::unique_lock<Mutex> ReadLock;
typedef boost::unique_lock<Mutex> WriteLock;

int main(int argc, char **argv)
{
  Mutex mutex;
  std::cout << "Locking...";
  WriteLock lock(mutex);
  cout << "locked.\n";
  boost::condition_variable event;

  {
  cout << "timed waiting 3s...\n";
  event.timed_wait(lock, boost::posix_time::seconds(3));
  cout << "timed wait over\n";
  }

  {
  cout << "timed waiting 4ms...\n";
  boost::chrono::system_clock::time_point start = boost::chrono::system_clock::now();
  boost::chrono::milliseconds wait_time_bc_ms(231);
  event.timed_wait(lock, boost::posix_time::milliseconds(4));
  //event.timed_wait(lock, boost::posix_time::milliseconds(4));

  boost::chrono::milliseconds waited_for_ms = 
    boost::chrono::duration_cast<boost::chrono::milliseconds>(boost::chrono::system_clock::now() - start);
  boost::chrono::duration<double> sec = 
    boost::chrono::system_clock::now() - start;
  cout << "timed wait over\n";
  std::cout << "duration: " << sec << "\n";
  }

  typedef boost::chrono::nanoseconds duration_t; 
  typedef duration_t::rep rep_t;
  rep_t d = boost::chrono::duration_cast<duration_t>(boost::chrono::milliseconds(1317)).count();
  rep_t sec = d/1000000000; 
  rep_t nsec = d%1000000000; 
  boost::posix_time::time_duration td = 
    boost::posix_time::seconds(static_cast<long>(sec))
    + boost::posix_time::microseconds((nsec+500) / 1000);

  std::cout << "waiting " << td.total_milliseconds() << "...";
  event.timed_wait(lock, td);
  std::cout << "waited " << td.total_milliseconds() << std::endl;

  std::cout << "count: " << 
    boost::chrono::duration_cast<boost::chrono::microseconds>(boost::chrono::milliseconds(878)).count()
    << endl;

  cout << "waiting...\n";
  event.wait(lock);
  cout << "wait over\n";

  return 0;
}
