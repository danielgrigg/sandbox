#include <boost/lexical_cast.hpp>
#include <iostream>

using namespace std;
using namespace boost;

int main()
{
  std::string x = "3.14159265358979";

  double a;
  std::istringstream is(x);
  is >> a;

  double b;
  b = lexical_cast<double>(x);

  cout << "a = " << a << ", b = " << b << endl;
  
  return 0;
}

