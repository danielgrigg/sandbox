#include <algorithm>
#include <iostream>
#include <stdint.h>
#include <vector>
#include <boost/iterator/function_input_iterator.hpp>
#include <boost/lambda/lambda.hpp>
#include <boost/lambda/bind.hpp>

using namespace std;
using namespace boost;
using namespace boost::lambda;

int f() { return rand() % 10; }

// If you're unfamiliar with boost iterator, definitely check it out!

int main(int argc, char * argv[]) 
{
  vector<int> v;
  copy(make_function_input_iterator(f, 0),
      make_function_input_iterator(f, 90),
      back_inserter(v)
      );

  for_each(v.begin(), v.end(), cout << _1 << ' ');
  cout << endl;

  // We're aiming to group-by aka chunk the items, in this case, their values 
  // in 0..9. We could just as easily group by a set of names for instance
  // against more complex objects.
  vector<int>::iterator lower = v.begin();
  while (lower != v.end())
  {
    int n = *lower;
    vector<int>::iterator upper = partition(lower, v.end(), _1 == n);

    // Per-group computation here
    cout << n << ": ";
    for_each(lower, upper, cout << _1 << ' ');
    cout << endl;

    lower = upper;
  }

  return 0;
}


