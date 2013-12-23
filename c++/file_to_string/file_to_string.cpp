
#include <iostream>
#include <fstream>
#include <algorithm>
#include <iterator>
#include <iomanip>
#include <string>

using namespace std;

int main(int argc, char **argv)
{
  ifstream ifs("file_to_string.cpp", ifstream::in);
  istreambuf_iterator<char> eos;  // end of range iterator
  string str(istreambuf_iterator<char>(ifs), eos);
  
  cout << "File contains:\n" << str << endl;

  return 0;
}
