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
#include <boost/tokenizer.hpp>

using namespace std;

int main(int argc, char **argv)
{
  char str[] = "1//2";
  char* p = strtok(str, "/");
  cout << "str: " << str << endl;
  char first[128], second[128], third[128];
  while (p != NULL)
  {
    cout << p << endl;
    p = strtok(NULL, "/");
  }

  std::string str2 = "3//4";
  boost::tokenizer<> tok(str2);
  for (boost::tokenizer<>::const_iterator iter = tok.begin();
        iter != tok.end(); ++iter)
  {
    cout << "T: " << *iter << endl;
  }
  return 0;
}
