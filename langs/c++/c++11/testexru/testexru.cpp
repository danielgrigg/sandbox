#include <string>
#include <vector>
#include <exru/exru.h>
#include <iostream>

using namespace std;

int main(int argc, char **argv)
{
  float pixels[16] = 
  { 1, 0, 0, 1,
    0, 1, 0, 1,
    0, 0, 1, 1,
    1, 1, 0, 1};

  write_rgba(2, 2, "foo.exr", pixels);

  return 0;
}
