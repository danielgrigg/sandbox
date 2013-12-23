#include <iostream>
#include <vector>
#include <utility>
#include <string>
#include <iterator>
#include <boost/algorithm/string/predicate.hpp>
#include <boost/tokenizer.hpp>
#include <boost/bind.hpp>
#include <functional>
#include <numeric>

using namespace std;
using namespace boost;

int main()
{
  vector<string> a;

  string s("The quick brown fox jumps over the lazy dog");
  tokenizer<> tok(s);
  a.assign(tok.begin(), tok.end());
  ostream_iterator<string> out_it(cout, ", ");
  copy(a.begin(), a.end(), out_it); 
  cout << endl;

//  sort(a.begin(), a.end(), ilexicographical_compare<string, string>);

  cout << "sorted: ";
  copy(a.begin(), a.end(), out_it); 
  
  cout << endl;
  return 0;
}
