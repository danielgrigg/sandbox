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
#include <tr1/unordered_map>
#include <boost/algorithm/string.hpp>

using namespace std;
using namespace std::tr1;
using namespace boost;

class Config
{
  public:
  typedef unordered_map<string,string> OptionsMap;
  bool init(const string& filename)
  {
    std::fstream fs (filename.c_str(), std::fstream::in);
    if (!fs.is_open()) 
    {
      std::cerr << "Error reading Config-file '" << filename << "'\n";
      return false;
    }
    string line;
    while (getline(fs, line))
    {
      size_t firstAt = line.find_first_of("=:");
      if (firstAt == std::string::npos)
      {
        std::cerr << "Option-line: '" << line << "' not '=' delimited." << std::endl;
        continue;
      }
      std::pair<OptionsMap::iterator, bool> res = 
        _options.insert(make_pair(trim_copy(line.substr(0, firstAt)),
              trim_copy(line.substr(firstAt, line.length() - firstAt))));
      if (!res.second)
      {
        std::cerr << "Option " << res.first->first << " already set to "
          << res.first->second << std::endl;
      }
    }
    return true;
  }

  std::string get(const std::string& option, const std::string& defaultValue)
  {
    OptionsMap::iterator i = _options.find(option);
    if (i != _options.end()) return i->second;
    return defaultValue;
  }
  private:

  unordered_map<string, string> _options;
};


int main(int argc, char **argv)
{
  cout << "starting options" << endl;

  Config cfg;
  cfg.init("foo.cfg");

  std::cout << "log_type: " <<  cfg.get("log_type", "stdout") << std::endl;
  cout << "swing_seconds: " << cfg.get("swing_seconds", "12.57") << endl;
  cout << "fill_time: " << cfg.get("fill_time", "99") << endl;

  return 0;
}
