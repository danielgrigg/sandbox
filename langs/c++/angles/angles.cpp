
#include <iostream>
#include <vector>
#include <math.h>
using namespace std;

double Normalise(double angle, double lowerBound, double upperBound)                                                                                                                                        
{
      double modulus = fmod(angle - lowerBound, upperBound - lowerBound);
          return modulus + ((modulus < 0.0) ? upperBound : lowerBound);
}

inline double Radians(const double deg)
{
  return deg * M_PI / 180.0;
}

/** @brief Convert a value from radians to degrees.
 *    */
inline double Degrees(const double rad)                                                                                                                                                                         
{
  return rad * 180.0 / M_PI;
}

const double PI = M_PI;

int main()
{
  double a = PI / 4;      // 45
  double b = -PI / 4;     // -45
  double c = PI + PI / 4; // 225
  double d = 2 * PI - PI / 4; // 315
  double e = -PI - PI / 4;  // -225
  double f = Radians(-405);

  double a_norm = Normalise(a, -PI, PI);
  double b_norm = Normalise(b, -PI, PI);
  double c_norm = Normalise(c, -PI, PI);
  double d_norm = Normalise(d, -PI, PI);
  double e_norm = Normalise(e, -PI, PI);
  double f_norm = Normalise(f, -PI, PI);

  cout << "a = " << Degrees(a) << ", a_norm = " << Degrees(a_norm) << endl;
  cout << "b = " << Degrees(b) << ", b_norm = " << Degrees(b_norm) << endl;
  cout << "c = " << Degrees(c) << ", c_norm = " << Degrees(c_norm) << endl;
  cout << "d = " << Degrees(d) << ", d_norm = " << Degrees(d_norm) << endl;
  cout << "e = " << Degrees(e) << ", e_norm = " << Degrees(e_norm) << endl;
  cout << "f = " << Degrees(f) << ", f_norm = " << Degrees(f_norm) << endl;
  return 0;
}

