#include <iostream>
#include <stdint.h>

using namespace std;

int main()
{

  uint64_t my64 = 0x37FFFFFFFFLLU;

  cout << "my64 = 0x" << hex << my64 << ", sizeof(my64) = " << sizeof(my64) << endl;
  return 0;
}
