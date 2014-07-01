
#include <iostream>
#include <fstream>
#include <algorithm>
#include <iterator>
#include <iomanip>
#include <string>

using namespace std;

bool fileExists(const std::string &path)
{
  std::ifstream ifs(path.c_str());
  return ifs.is_open();
}

int main(int argc, char **argv)
{
  cout << "FileExists 'file_exists.cpp'?..." << fileExists("file_exists.cpp") << endl;
  cout << "FileExists 'random34783file.txt'..." << fileExists("random34783file.txt") << endl;
  return 0;
}
