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

  template<typename T>
void print_comma_separated_list(T value)
{
  std::cout<<value<<std::endl;
}

  template<typename First,typename ... Rest>
void print_comma_separated_list(First first,Rest ... rest)
{
  std::cout<<first<<",";
  print_comma_separated_list(rest...);
}

int main(int argc, char **argv)
{
  print_comma_separated_list(42,"hello",2.3,'a');
  return 0;
}
