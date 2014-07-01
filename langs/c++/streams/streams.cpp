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

string viaIterator(char *f)
{
  fstream ifs(f, ifstream::in);
  istreambuf_iterator<char> eos;  // end of range iterator
  string result(istreambuf_iterator<char>(ifs), eos);
  return result;
}

string viaRead(char * f)
{
//  fstream is(f, ifstream::in | ifstream::binary);
  fstream fs;
  fs.open(f, fstream::binary | ifstream::in);
  if (!fs.is_open())
  {
    cout << "failed to open " << f << endl;
    return "";
  }
  fs.seekg (0, ios::end);
  uint64_t length = fs.tellg();
  fs.seekg (0, ios::beg);
  char *buffer = new char [length];

  // read data as a block:
  fs.read (buffer,length);
  fs.close();

  string result(buffer, length);
  delete[] buffer;
  return result;
}

int main(int argc, char **argv)
{
  if (argc < 2) return 1;

  string result = viaRead(argv[1]);
  cout << "Read:\n" << result << endl; 
  return 0;
}
