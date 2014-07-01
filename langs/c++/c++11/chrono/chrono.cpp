#include <iostream>
#include <chrono>
#include <unistd.h>

using namespace std;

int main(int argc, char **argv)
{
  using namespace std::chrono;
  auto t0 = high_resolution_clock::now();
  cout << "hello world" << endl;
  sleep(1);
  auto t1 = high_resolution_clock::now();
  cout << duration_cast<milliseconds>(t1 - t0).count() << "msec\n";
  return 0;
}
