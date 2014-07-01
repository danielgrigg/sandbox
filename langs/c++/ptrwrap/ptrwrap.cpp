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
#include <utility>
#include <boost/lambda/lambda.hpp>

using namespace std;
using namespace boost::lambda;

template <typename T, int N>
struct Weave 
{
  T v[N];
};

template <typename T, int N>
std::ostream& operator<<(std::ostream& os, const Weave<T, N>& rhs)
{
  os << '[';
  for (int i = 0; i < N-1; ++i) os << rhs.v[i] << ' ';
  os << rhs.v[N-1] << ']';
  return os;
}

template <typename T>
class Range
{
  public:
  Range(T* a, size_t count):
    _begin(a),
    _end(a + count)
  {}
  T* begin(){ return _begin; }
  const T* begin()const{ return _begin; }
  T* end(){ return _end; }
  const T* end()const{ return _end; }
  private:
  T* _begin;
  T* _end;
};

template <typename T, typename U>
Range<T> make_range(const U* p, size_t elements)
{
  return Range<T>((T*)p, elements / (sizeof(T) / sizeof(U)));
}

int main(int argc, char **argv)
{
  Weave<int, 3> w;
  int x[12] = {1,2,3, 4,5,6 ,7,8,9, 10, 11, 12};
  Range<Weave<int,4> > w2 = make_range<Weave<int,4> >(x, 12);
  for_each(w2.begin(), w2.end(), cout << _1 << ' ');
  cout << endl;

  Range<Weave<int,3> > w3 = make_range<Weave<int,3> >(x, 12);
  for_each(w3.begin(), w3.end(), cout << _1 << ' ');
  return 0;
}
