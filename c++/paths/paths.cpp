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

#include <boost/filesystem/operations.hpp> // includes boost/filesystem/path.hpp
#include <boost/filesystem/fstream.hpp>    // ditto
#include <boost/filesystem/convenience.hpp>

using namespace std;
using namespace boost::filesystem;  

int main(int argc, char **argv)
{
  if (argc < 2)
  {
    cerr << "Usage: paths <some-path>" << endl;
    return 1;
  }
  path p(argv[1]);

  cout << "Path: " << p << endl;
  std::cout << "Filename: " << p.filename() << std::endl;
  std::cout << "stem: " << p.stem() << std::endl;
  std::cout << "extension: " << p.extension() << std::endl;
  std::cout << "root_name: " << p.root_name() << std::endl;
  std::cout << "root_directory: " << p.root_directory() << std::endl;
  std::cout << "root_path: " << p.root_path() << std::endl;
  std::cout << "relative_path: " << p.relative_path() << std::endl;
  std::cout << "parent_path: " << p.parent_path() << std::endl;
  return 0;
}
