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

std::string extension(const std::string& path)
{
      if(path.find_last_of(".") != std::string::npos)
                return path.substr(path.find_last_of(".")+1);
          return "";
}

int main(int argc, char **argv)
{
  string pathsIn[9] = {
    "a.vp",
    "a.fp",
    "a.gp",
    "b.vp",
    "b.fp",
    "b.gp",
    "c.vp",
    "c.fp",
    "c.gp",
  };
  vector<string> paths(pathsIn, pathsIn + 9);

  random_shuffle(paths.begin(), paths.end());
  ostream_iterator<string> out_it(cout, "\n");
  cout << "Unsorted: "; copy(paths.begin(), paths.end(), out_it);

  vector<string>::iterator second = partition(paths.begin(), paths.end(), 
      ^(const string &path) { return extension(path) == "vp"; });
  vector<string>::iterator third = partition(second, paths.end(),
      ^(const string &path) { return extension(path) == "fp"; });

  cout << "VP (" << (second - paths.begin()) << "):\n"; copy(paths.begin(), second, out_it);
  cout << "FP:\n"; copy(second, third, out_it);
  cout << "Others:\n"; copy(third, paths.end(), out_it);

  return 0;
}
