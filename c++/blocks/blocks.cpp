
#include <fstream>
#include <iostream>
#include <algorithm>
#include <iterator>
#include <iomanip>

using namespace std;

#include <stdio.h>
#include <Block.h>
typedef int (^IntBlock)();

IntBlock MakeCounter(int start, int increment) 
{
  __block int i = start;

  return Block_copy( ^ {
      int ret = i;
      i += increment;
      return ret;
      });

}

int main(void) 
{
  IntBlock mycounter = MakeCounter(5, 2);
  printf("First call: %d\n", mycounter());
  printf("Second call: %d\n", mycounter());
  printf("Third call: %d\n", mycounter());

  printf ("i = %d\n", i);
  /* because it was copied, it must also be released */
  Block_release(mycounter);

  return 0;
}

