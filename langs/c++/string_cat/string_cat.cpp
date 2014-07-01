#include <iostream>
#include <algorithm>
#include <boost/lexical_cast.hpp>

using namespace std;
using namespace boost;

int main()
{
  int baud = 115200;
  string command = "COM,0;THISPORT," + 
    lexical_cast<string>(baud) + ",N,8,1,N,OFF,ON";

  cout << "command: " << command << endl;
  return 0;
}

