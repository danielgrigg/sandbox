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
#include <random>
#include <memory>
#include <chrono>

using namespace std;


using RandomEnginePtr = std::shared_ptr<std::default_random_engine>;

std::function<int(int, int)> make_uniform_random_generator() {
  unsigned seed = std::chrono::system_clock::now().time_since_epoch().count();
  RandomEnginePtr G(new std::default_random_engine(seed));
  return [G](int a, int b) {
    return (std::uniform_int_distribution<int>(a, b))(*G);
  };
}

void shuffle(std::vector<int>& v, std::function<int(int,int)> random_gen ) {
  for (int first = 0; first < v.size() - 1; ++first) {
    int idx = random_gen(first, v.size()-1);
    std::swap(v[first], v[idx]);
    
    cout << first << "(" << idx << "): ";
    for (auto &x : v) { cout << x << " "; } cout << endl;
  }
}


int main(int argc, char **argv)
{
  std::vector<int> v{1,2,3,4,5,6,7,8,9};

  for (auto &x : v) { cout << x << " "; } cout << endl;

  auto gen = make_uniform_random_generator();
  shuffle(v, gen);

  /*
  std::generate_n(ostream_iterator<int>(cout, " "),
      30,
      std::bind(gen, 0, 9));
  cout << endl;
  */

  return 0;
}
