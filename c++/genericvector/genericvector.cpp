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
void foo(T t)
{
  std::vector<T> x;
  x.push_back(t);
  x.push_back(T());
  x.push_back(t);
  typename std::vector<T>::iterator i;

}

int main(int argc, char **argv)
{
  foo<int>(42);
  return 0;
}
