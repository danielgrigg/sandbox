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

int main(int argc, char **argv) {

  cin.imbue(locale::classic());
//  cout.imbue(locale("de_DE"));
  locale langLocale("");
 // cout << "locale name: " << langLocale.name() << endl;

  double value;
  while (cin >> value) { 
    cout << value << endl;
  }
  
  /*
  
  std::copy(std::istream_iterator<double>(cin),
            std::istream_iterator<double>(),
            std::ostream_iterator<double>(cout));
            */
  return 0;
}
