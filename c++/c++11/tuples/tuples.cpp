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

template <typename T>
struct Result {
  T value;
  bool ok;
};

template <typename T>
Result<T> make_result(T value, bool ok) { return {value, ok }; }

template <typename T>
bool is_ok(Result<T> value) { return value.ok; }

//template<class... Args>
//void print(const std::tuple<Args...>& t) 
//{
//     std::cout << "(";
//         TuplePrinter<decltype(t), sizeof...(Args)>::print(t);
//             std::cout << ")\n";
//}

template <typename Tuple, std::size_t N>
struct TuplePred {
  static bool all_ok_n(Tuple& t) {
    printf("ok?(%ld): %d\n", N-1, is_ok(get<N-1>(t)));
    return TuplePred<Tuple, N-1>::all_ok_n(t) && is_ok(std::get<N-1>(t));
  }
};

template <typename Tuple>
struct TuplePred<Tuple, 1> {
  static bool all_ok_n(Tuple& t) {
    cout << "ok(0)? " << is_ok(std::get<0>(t)) << endl;
    return is_ok(std::get<0>(t));
  }
};

template <typename... TS>
bool all_ok(const std::tuple<TS...>& t) {
  return TuplePred<decltype(t), sizeof...(TS)>::all_ok_n(t);
}


int main(int argc, char **argv)
{
  auto foo = make_tuple(make_result(3, true), make_result(4, false));
  auto tuple_ok = all_ok(foo);
  cout << "tuple_ok? " << std::boolalpha << tuple_ok << endl;
  return 0;
}
