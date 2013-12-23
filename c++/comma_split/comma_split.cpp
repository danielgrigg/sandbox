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

using namespace std;

struct gga
{
  int m_time;
  double m_lat;
  double m_lng;
};

vector<string> tokens;

template <typename T>
T lex_cast(const std::string &str)
{
  std::istringstream tss(str);
  T result;
  tss >> result;
  return result;
}

int main(int argc, char **argv)
{
  string str = "$GPGGA,004851,3223.1442,S,15101.4481,E,1,10,0.9,11.8,M,25.1,M,,";

  std::istringstream iss(str);
  string token;

  while (getline(iss, token, ','))
  {
    tokens.push_back(token); 
  }
  
  gga a;
  a.m_time = lex_cast<int>(tokens[1]);
  a.m_lat = lex_cast<double>(tokens[2]);
  a.m_lng = lex_cast<double>(tokens[4]);

  cout << "time: " << a.m_time << ", lat: " << a.m_lat << ", lng: " << a.m_lng << endl;
  return 0;
}
