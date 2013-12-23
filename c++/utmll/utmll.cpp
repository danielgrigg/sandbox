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

void foo(double phi1Rad, )
{

  double Lat = 
    phi1Rad - 
    (N1*tan(phi1Rad)/R1) * 
    (D*D/2 - 
     (5 + 3*T1 + 10*C1 - 4*C1*C1-9*eccPrimeSquared)*D*D*D*D/24 + 
     (61 + 90*T1 + 298*C1 + 45*T1*T1-252*eccPrimeSquared-3*C1*C1)*D*D*D*D*D*D/720);
}



int main(int argc, char **argv)
{

  return 0;
}
