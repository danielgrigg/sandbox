#include <iostream>
#include <boost/format.hpp>

using namespace std;
using namespace boost;

int main()
{
  boost::format f = boost::format("%1%_%2%_%3%.tga") % "foo" % 14 % 96;
  cout << "f: " << f << endl;


  cout << format("(x,y) = (%+5d,%+5d) \n") % -23 % 35;
  cout << format("(x,y) = (%|+5|,%|+5|) \n") % -23 % 35;

  cout << format("(x,y) = (%1$+5d,%2$+5d) \n") % -23 % 35;
  cout << format("(x,y) = (%|1$+5|,%|2$+5|) \n") % -23 % 35;

  // padding
  cout << format("x = %|1$9|\n") % 123;
  cout << format("x = %|1$9|\n") % 123.456;
  cout << format("x = %|1$09|\n") % 123.456;

  cout << format("x = %|1$07.5|\n") % 1234567.0;
  cout << format("x = %|1$07.5f|\n") % 1234567.0;
  cout << format("x = %|1$07.5f|\n") % 1234567.123456;
  return 0;
}

