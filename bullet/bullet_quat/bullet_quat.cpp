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

#define _VECTORMATH_DEBUG
#include <bullet/vectormath/vmInclude.h>

using namespace std;
using namespace Vectormath::Aos;

const Vector3 rotate2( const Quat & quat, const Vector3 & vec )
{
    float tmpX, tmpY, tmpZ, tmpW;
    tmpX = ( ( ( quat.getW() * vec.getX() ) + ( quat.getY() * vec.getZ() ) ) - ( quat.getZ() * vec.getY() ) );
    tmpY = ( ( ( quat.getW() * vec.getY() ) + ( quat.getZ() * vec.getX() ) ) - ( quat.getX() * vec.getZ() ) );
    tmpZ = ( ( ( quat.getW() * vec.getZ() ) + ( quat.getX() * vec.getY() ) ) - ( quat.getY() * vec.getX() ) );
    tmpW = ( ( ( quat.getX() * vec.getX() ) + ( quat.getY() * vec.getY() ) ) + ( quat.getZ() * vec.getZ() ) );
    printf("tmpX %f tmpY %f tmpZ %f tmpW %f\n",
      tmpX, tmpY, tmpZ, tmpW);
    return Vector3(
        ( ( ( ( tmpW * quat.getX() ) + ( tmpX * quat.getW() ) ) - ( tmpY * quat.getZ() ) ) + ( tmpZ * quat.getY() ) ),
        ( ( ( ( tmpW * quat.getY() ) + ( tmpY * quat.getW() ) ) - ( tmpZ * quat.getX() ) ) + ( tmpX * quat.getZ() ) ),
        ( ( ( ( tmpW * quat.getZ() ) + ( tmpZ * quat.getW() ) ) - ( tmpX * quat.getY() ) ) + ( tmpY * quat.getX() ) )
    );
}

int main(int argc, char **argv)
{
  Quat q{normalize(Quat{1.0, 0.0, 0.0, 1.0})};
  Quat p{normalize(Quat(1.0, 1.0, 0.0, 1.0))};

  print(q, "q");
  print(p, "p");
  cout << endl;

  for (auto t : {0.0, 0.2, 0.4, 0.6, 0.8, 1.0}) {
    auto s = slerp(t, q, p);
    cout << t << ": ";
    print(s, "s");
  }

  Quat a{3, -1, 4, 5};
  Quat b{-2, 4, 3, 2};

  auto c = a * b;
  print(c, "c");

  print(squad(0.6, 
      normalize(Quat{1, 0, 0, 1}),
      normalize(Quat(1, 0, 1, 1)),
      normalize(Quat(0, 1, 1, 1)),
      normalize(Quat(1, 1, 0, 1))), "squad");

  print(normalize(Quat(0,0,0,0))
      .rotation(Vector3(1, 0.5, 0.3), Vector3(0.7, -0.2, 2.0)),
      "rotation vectors");

  print(Quat().rotation(1, normalize(Vector3(1.0, 1.0, 1.0))), 
      "rotation");

  print(Quat(1.0, -2, 3, 5) * Quat(4, 3, -1, 1), "mul");

  print(rotate(Quat(2.0, -1, 3, 1), Vector3(-1, 1, 2)), "rotate quat vec");
  print(rotate(Quat(8.0, 22, -31, 11), Vector3(-11, 13, 17)), "rotate quat vec");
  return 0;
}
