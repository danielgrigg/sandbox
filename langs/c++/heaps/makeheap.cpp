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

int myrand() { return random() % 100; }

int main(int argc, char **argv)
{
  vector<int> v(20);
  generate(v.begin(), v.end(), myrand);
  cout << "v: ";
  ostream_iterator<int> osi(cout, " ");
  copy(v.begin(), v.end(), osi);

  make_heap (v.begin(),v.end());
  cout << "\nv: ";
  copy(v.begin(), v.end(), osi);
  return 0;
}
