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
int someVar = 0;
void foo()
{
//  std::cout << "in foo" << endl;
  sleep(1);
}

void bar()
{
  //std::cout << "in bar" << endl;
  sleep(1);
}

void handleSignal(int x)
{
  cout << "got signal" << endl;
}

int main(int argc, char **argv)
{
  signal(SIGABRT, handleSignal);
  for (int i = 0; i < 10; ++i)
  {
   someVar = rand();
    foo();
    bar();
  }
  return 0;
}
