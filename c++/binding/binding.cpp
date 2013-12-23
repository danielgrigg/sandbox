#include <iostream>
#include <boost/bind.hpp>
#include <string>
#include <algorithm>
#include <vector>
#include <iterator>

using namespace boost;
using namespace std;

class Foo
{
  public:
    void foo(int x)
    {
      cout << "x = " << 2 * x << "\n";
    }
};

void freefunc(int x)
{
      cout << "x = " << x * x << "\n";
}

int main()
{
  int y[] = {1, 2, 3, 4, 5};
  vector<int> v;
  copy(y, y+5, back_inserter(v));

  ostream_iterator<int> out_iter (cout, " ");
  copy(v.begin(), v.end(), out_iter);
  cout << endl;

  Foo f;
  for_each(v.begin(), v.end(), bind(&Foo::foo, ref(f), _1));

  for_each(v.begin(), v.end(), bind(freefunc, _1));
  return 0;
}

