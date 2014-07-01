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

int AA(int l)
{
  cout << "In AA()" << l << endl;
  sleep(2);
  return 11;
}

int A()
{
  int a = 5;
  cout << "In A()" << a << endl;
  AA(42);
  sleep(5);
  return 1;
}

int B()
{
  cout << "In B()" << endl;
  sleep(5);
  return 2;
}

int C()
{
  cout << "In C()" << endl;
  sleep(5);
  return 3;
}

int main(int argc, char **argv)
{
  A();
  B();
  C();
  return 0;
}
