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
#include <boost/chrono.hpp>
#include <cmath>
#include <boost/date_time/posix_time/posix_time.hpp>
#include <boost/thread/thread.hpp>


using namespace std;
using namespace boost::chrono;
namespace pt = boost::posix_time;

int main(int argc, char **argv)
{
  cout << "default-system-time " << system_clock::time_point() << endl;
  {
    system_clock::time_point start = system_clock::now();
    cout << "start_time " << start << endl;
    for ( long i = 0; i < 10000000; ++i )
      std::sqrt( 123.456L ); // burn some time

    duration<double> sec = system_clock::now() - start;
    cout << "sec " << sec << endl;
    std::cout << "took " << sec.count() << " seconds\n";
  }
  {
    steady_clock::time_point start = steady_clock::now();
    steady_clock::time_point end = start + seconds(7); //steady_clock::duration(seconds(5));
    cout << "start " << start << "\n  end " << end << "\n diff " << end - start << endl;
    cout << "seconds_diff " << duration_cast<seconds>(end - start) << endl;
    cout << "minutes " << duration_cast<minutes>(seconds(200)) << endl;

    typedef duration<double, boost::ratio<60> > dminutes; 
    dminutes dm = seconds(200);
    cout << "minutesf " << dm << endl;


    seconds s = duration_cast<seconds>(end - start);
    cout << "s " << s << endl;
  }

  pt::time_duration myDuration = pt::seconds(3);
  
  cout << "sleeping for " << myDuration << " seconds" << endl;
  boost::this_thread::sleep(myDuration);

  return 0;
}
