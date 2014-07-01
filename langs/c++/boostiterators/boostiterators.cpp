#include <algorithm>
#include <iostream>
#include <boost/iterator/counting_iterator.hpp>
#include <boost/iterator/function_input_iterator.hpp>

using namespace boost;
using namespace std;

struct generator {
  typedef int result_type;
  generator() { srand(time(0)); }
  result_type operator() () const {
    return rand();
  }
};

int f() { return rand() % 10; }

int main(int argc, char * argv[]) {
  copy(
      make_function_input_iterator(f, 0),
      make_function_input_iterator(f, 10),
      ostream_iterator<int>(cout, " ")
      );

  generator g;

  copy(
      make_function_input_iterator(g, infinite()),
      make_function_input_iterator(g, infinite()),
      ostream_iterator<int>(cout, " ")
      );

  return 0;
}


