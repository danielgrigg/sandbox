
#include <iostream>
#include <boost/bind.hpp>
#include <string>
#include <algorithm>
#include <vector>
#include <iterator>
#include <iomanip>

using namespace boost;
using namespace std;

int main()
{
  cout << "Foo:" << setw(10) << 77 << endl;

  cout << setprecision(8) << 1234567.123456 << endl;
  cout << fixed <<  setprecision(8) << 1234567.123456 << endl;
  cout << scientific << showpoint << setprecision(8) << 0.123456789 << endl;
  return 0;
}

