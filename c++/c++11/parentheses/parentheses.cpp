// Implement an algorithm to print all valid combinations of n-pairs of 
// parentheses.
//
// Example:
// input: 3
// output: ((())), (()()), (())(), ()(()), ()()()

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
#include <sstream>

using namespace std;

void add_paren(vector<std::string>& ss, string s, int nleft, int nright) {

  if (nleft == 0 && nright == 0) {
    ss.push_back(s);
    return;
  }

  if (nleft > 0) {
//    s += '(';
    add_paren(ss, s + '(', nleft - 1, nright);
  }

  if (nright > nleft) {
    add_paren(ss, s + ')', nleft, nright - 1);
  }
}

vector<std::string> paren_pairs(int n) {
  vector<std::string> ret(1);
  string s;
  add_paren(ret, s, n, n);
  return ret;
}


//  if (paren_pairs(3) == "((())), (()()), (())(), ()(()), ()()()") {
//    if (paren_pairs(2) == "(()), ()()") {


int main(int argc, char **argv) {

  istringstream is(argv[1]);
  int n;
  is >> n;
  auto ps = paren_pairs(n);
  std::copy(ps.begin(), ps.end(), ostream_iterator<string>(cout, ", "));
  cout << endl;
  return 0;
}
