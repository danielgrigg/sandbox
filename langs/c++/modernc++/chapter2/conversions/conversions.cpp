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

template <class T, class U>
class Conversion 
{
  typedef char Small;
  class Big { char dummy[2]; };
  static Small Test( const U& );
  static Big Test(...);
  static T MakeT();
  public:
  enum { exists = sizeof(Test(MakeT())) == sizeof(Small) };
};

int main(int argc, char **argv)
{
  cout << Conversion<double, int>::exists << ' ' << 
    Conversion<size_t, vector<int> >::exists << std::endl;
  return 0;
}
