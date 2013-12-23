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
#include <stdlib.h>

using namespace std;

struct A
{
  A(){x = 0; }
  A(int y)
  {x = y; }
  int x;

  bool operator<(const A& rhs)const
  {
    return x < rhs.x;
  }
};

int main(int argc, char **argv)
{
  A a[10];
  std::generate(a, a+10, rand);

  for (int i = 0; i < 10; i++) cout << a[i].x << " ";
  cout << endl;

  std::sort(a, a+10);

  for (int i = 0; i < 10; i++) cout << a[i].x << " ";
  cout << endl;
  return 0;
}
