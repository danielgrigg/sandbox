#include <fstream>
#include <iostream>

int main(int argc, char* argv[])
{
  using namespace std;
  (argc > 2 ? ofstream(argv[2], ios::out | ios::binary) : cout)
    <<
    (argc > 1 ? ifstream(argv[1], ios::in | ios::binary) : cin).rdbuf();
  return 0;
}

