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
#include <boost/signal.hpp>
#include <boost/chrono.hpp>
#include <boost/function.hpp>
#include <boost/date_time/posix_time/posix_time.hpp>
#include <boost/thread/thread.hpp>
#include <boost/lambda/bind.hpp>
#include <boost/lambda/lambda.hpp>

using namespace std;
namespace bc = boost::chrono;
namespace bl = boost::lambda;
namespace pt = boost::posix_time;

template <typename T>
class Watchdog
{
  public:
  
    Watchdog(bc::milliseconds expireTime,
        boost::function<bool(T const&, T const&)> predicate, 
        boost::function<void()> handler):
      _expireTime(expireTime),
      _lastUpdatedValue(T())
    {
      _expired.connect(handler);
      _predicate = predicate;
    }

    void update(const T& newValue)
    {
      if (_predicate(_lastUpdatedValue, newValue))
      {
        _lastUpdated = bc::steady_clock::now();
        return;
      }

      if (bc::steady_clock::now() - _lastUpdated > _expireTime)
      {
        _expired();
      }
    }
  private:
    bc::milliseconds _expireTime;
    boost::signal<void()> _expired;
    bc::steady_clock::time_point _lastUpdated;
    T _lastUpdatedValue;
    boost::function<bool(T const&, T const&)> _predicate;
};

void handler()
{
  cout << "expired" << endl;
}

int main(int argc, char **argv)
{
  using boost::lambda::_1;
  using boost::lambda::_2;

  boost::function<int(int, int)> f = _1 + _2;

  boost::function<bool(bool,bool)> myEqual = _1 == _2;
  Watchdog<bool> w(bc::seconds(3), _1 == _2, handler);
  w.update(false);

  pt::time_duration myDuration = pt::milliseconds(200);
  bc::steady_clock::time_point start = bc::steady_clock::now();
  bc::steady_clock::time_point end = start + bc::seconds(5);
  {
    while (bc::steady_clock::now() < end)
    {
      cout << bc::duration_cast<bc::seconds>(bc::steady_clock::now() - start) << endl;
      w.update(true);
      boost::this_thread::sleep(myDuration);
    }
  }
  w.update(false);
  cout << "reset!\n";

  {
    bc::steady_clock::time_point end = bc::steady_clock::now() + bc::seconds(4);
    while (bc::steady_clock::now() < end)
    {
      cout << bc::duration_cast<bc::seconds>(bc::steady_clock::now() - start) << endl;
      w.update(true);
      boost::this_thread::sleep(myDuration);
    }
  }


  return 0;
}
