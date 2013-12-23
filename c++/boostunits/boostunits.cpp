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

#include <boost/units/systems/si.hpp>
#include <boost/units/systems/angle/revolutions.hpp>
#include <boost/units/systems/angle/degrees.hpp>
#include <boost/units/conversion.hpp>
#include <boost/units/pow.hpp>
#include <boost/units/io.hpp>

using namespace std;

int main(int argc, char **argv)
{
  boost::units::quantity<boost::units::si::plane_angle> theta = 3.14 * boost::units::si::radians;

  namespace units = boost::units;
  namespace si = boost::units::si;
  units::quantity<si::angular_velocity> nu = theta / (2.0 * si::seconds);

  std::cout << "theta " << theta << std::endl;
  std::cout << "nu " << nu << std::endl;

  units::quantity<units::degree::plane_angle> omega = 
    static_cast<units::quantity<units::degree::plane_angle> >(theta);

  std::cout << "omega " << omega << std::endl;
  return 0;
}

