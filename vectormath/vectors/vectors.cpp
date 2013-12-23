// 'hello-world' example of using the vectormath library.

// This will pull the SSE implementation if __SSE__ is defined,
// which it is on Mac OS X.  Unfortunately, the implementation
// doesn't support OSX :/
//#include <vectormath/cpp/vectormath_aos.h>

// For print
#define _VECTORMATH_DEBUG

// Assuming vectormath has been installed and in your include path.
#include <vectormath/scalar/cpp/vectormath_aos.h>

using namespace Vectormath::Aos;

int main(int argc, char **argv)
{
  Vector3 a(5);
  Vector3 b(7);

  Vector3 c = a + b;
  print(c); // 12 12 12

  Matrix3 S = Matrix3::scale(Vector3(1,2,3));
  Vector3 d = S * Vector3(2,2,2);
  print(d); // 2, 4, 6

  const float kPI = 3.141592653589793;
  Transform3 T = Transform3::rotationZ(kPI / 2.0) * 
    Transform3::translation(Vector3(1,0,0));
  Point3 p = T * Point3(2, 0, 0);
  print(p); // 0 3 0
  return 0;
}
