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
#include <tr1/unordered_map>


int main(int argc, char **argv)
{
  typedef std::tr1::unordered_map<uint64_t, uint32_t> IndexMap;
  IndexMap m;

  m.insert(IndexMap::value_type(32, 4));
  m.insert(IndexMap::value_type(123456789, 23));
  m.insert(IndexMap::value_type(999, 99));

  
  for (IndexMap::const_iterator it = m.begin(); it != m.end(); ++it)
  {
    std::cout << "[" << it->first << ", " << it->second << "]";
  }
  std::cout << std::endl;

  std::pair<IndexMap::iterator, bool> pib = m.insert(IndexMap::value_type(32, 88));
  std::cout << "Tried insert, result = " << pib.second << ", at = " << pib.first->second << std::endl;
  return 0;
}
