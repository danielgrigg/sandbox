#include <stdint.h>
#include <iostream>
#include <fstream>
#include <algorithm>
#include <iterator>
#include <iomanip>
#include <string>
#include <vector>
#include <map>
#include <sstream>
#include <functional>
#include <numeric>

using namespace std;

struct RmcData
{
  double latitude;
  double longitude;
  double speed_kph;
  double heading;
  double magneticVariance;
  double magneticVarianceDir;
  time_t time;
  int date;
  char status;
  char latitudeDir;
  char longitudeDir;
};

inline char xor_op(char a, char b) { return a ^ b; }

std::ostream & operator<<(std::ostream &os, const RmcData &rhs)
{
  //  GPRMC,00445,A,3223.1527,S,15101.4518,E,000.0,305.9,070111,012.1,E
  std::ostringstream str_os;
  str_os << "GPRMC," << std::fixed 
      << rhs.time << ',' 
      << rhs.status << ',' 
      << std::setprecision(4) << rhs.latitude << ','
      << rhs.latitudeDir << ','
      << std::setprecision(4) << rhs.longitude << ','
      << rhs.longitudeDir << ',' << std::setprecision(1)
      << setfill('0') << setw(5) << rhs.speed_kph << ','
      << setfill('0') << setw(5) << rhs.heading << ','
      << rhs.date << ','
      << setfill('0') << setw(4) << rhs.magneticVariance << ','
      << rhs.magneticVarianceDir;
  std::string body = str_os.str();
  int checksum = std::accumulate(body.begin(), body.end(), 0, xor_op);
  os << '$' << body << '*' << checksum;
  return os;
}

int main(int argc, char **argv)
{
  cout << setfill('0') << setw(5) << 1.3 << endl;

  // expert 6A from "$str*6A"
//  string str = "GPRMC,123519,A,4807.038,N,01131.000,E,022.4,084.4,230394,003.1,W";
//  string str = "$GPGGA,004458,3223.1527,S,15101.4518,E,1,08,1.1,11.4,M,25.1,M,,";
//  string str = "12";

  string str = "GPRMC,152243.17,A,2729.7528,S,15301.5664,E,022.4,008.4,260311,010.0,E";
  
  int result = 0;
  result = std::accumulate(str.begin(), str.end(), 0, xor_op);
  cout << "checksum: 0x" << std::hex << result << std::dec << endl;

  RmcData r;
  r.latitude =  31.123456; 
  r.longitude = 123.45678; 
  r.speed_kph = 34.6;
  r.heading = 89.2;
  r.magneticVariance = 3.1;
  r.magneticVarianceDir = 'W';
  r.time = 123519;
  r.date = 230394;
  r.status = 'A';
  r.latitudeDir = 'E';
  r.longitudeDir = 'N';
  cout << "RMC: " << r << endl; 
  return 0;
}
