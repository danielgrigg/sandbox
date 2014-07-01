#include <algorithm>
#include <fstream>
#include <functional>
#include <iomanip>
#include <iostream>
#include <iterator>
#include <map>
#include <numeric>
#include <sstream>
#include <string>
#include <vector>
#include <deque>

#include <boost/iterator/counting_iterator.hpp>
#include <boost/iterator/function_input_iterator.hpp>
#include <boost/iterator/filter_iterator.hpp>
//#include <boost/bind.hpp>
//
namespace ph=std::placeholders;

using namespace std;

void init_flags(deque<bool>& flags) {
  if (flags.size() > 0) flags[0] = false;
  if (flags.size() > 1) flags[1] = false;
}

void cross_off(deque<bool>& flags, int prime) {
  for (int i = prime * prime; i < flags.size(); i += prime) {
    flags[i] = false;
  }
}

int get_next_prime(deque<bool>& flags, int prime) {
  int next = prime + 1;
  while (next < flags.size() && !flags[next]) {
    next++;
  }
  return next;
}

void sieve_of_eratos_thenes(int max, std::deque<bool>& flags) {
  flags.assign(max, true);
  init_flags(flags);

  int prime = 2;
  int count = 0;

  while (prime <= max) {
    cross_off(flags, prime);
    prime = get_next_prime(flags, prime);
    if (prime >= flags.size()) {
      break;
    }
  }
}

typedef std::pair<int, bool> PrimePair;

int my_f(int a, int b, int c) { return a * b * c; }

PrimePair my_make_pair(int a, bool b) {
  return std::make_pair(a, b);
}

void write_primes(int max) {
  std::deque<bool> flags;
  sieve_of_eratos_thenes(max, flags);

  std::vector<PrimePair> pairs;
  std::transform(boost::counting_iterator<int>(0),
            boost::counting_iterator<int>(max),
            flags.begin(),
            back_inserter(pairs),
            make_pair<const int&, const bool&>);

  std::transform(
      boost::make_filter_iterator(std::bind(&PrimePair::second, ph::_1), pairs.begin(), pairs.end()),
      boost::make_filter_iterator(std::bind(&PrimePair::second, ph::_1), pairs.end(), pairs.end()),
      ostream_iterator<int>(cout, " "),
      std::bind(&PrimePair::first, ph::_1));
}

int main(int argc, char **argv) {
  write_primes(1000);
  cout << endl;
  
  return 0;
}
