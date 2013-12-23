#include <iostream>
#include <sstream>
#include <iomanip>

using namespace std;


struct Fraction {
  int numerator;
  int denominator;

  Fraction(int n, int d):
    numerator(n),
    denominator(d)
  {}
};


// Normally we might overload the io operators using a signature like
//
// ostream operator<<(ostream& x, ...)
// but this only works for char and has formatting issues
//
// instead we can use:
//

template <class charT, class traits>
inline std::basic_ostream<charT, traits>&
operator << (std::basic_ostream<charT, traits>& strm,
    const Fraction& f) {

  std::basic_ostringstream<charT, traits> s;

  // copy format flags from strm
  s.copyfmt(strm);
  s.width(0);

  s << f.numerator << '/' << f.denominator;

  strm << s.str();
  return strm;
}

template <class charT, class traits>
inline std::basic_istream<charT, traits>&
operator >> (std::basic_istream<charT, traits>& strm, Fraction& f) {
  int n, d;
  strm >> n;

  if (strm.peek() == '/') { 
    strm.ignore();
    strm >> d;
  } else {
    d = 1;
  }
  if (d == 0) { 
    strm.setstate(std::ios::failbit);
    return strm;
  }
  if (strm) {
    f = Fraction(n, d);
  }
  return strm;
}


int main(int argc, char **argv)
{
  Fraction vat(16, 100);
  std::cout << "VAT: \"" << std::left << std::setw(8) 
    << vat << '"' << std::endl;

  Fraction f(1,1);
  std::cout << "Enter fraction: ";
  std::cin >> f;
  std::cout << f << endl;

  return 0;
}
