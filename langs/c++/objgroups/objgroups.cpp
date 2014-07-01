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
#include <boost/lambda/lambda.hpp>
#include <boost/lambda/bind.hpp>

using namespace std;
using namespace boost::lambda;


struct Group;

std::ostream& operator<<(std::ostream& os, const Group& rhs);
struct Group
{
  Group(const std::string& name, uint32_t start, uint32_t count = 0):
    _start(start),
    _count(count),
    _name(name)
  {}
  uint32_t begin()const { return _start; }
  uint32_t end()const { return _start + _count; }
  uint32_t count()const { return _count; }
  const std::string& name()const { return _name; }

  bool operator<(const Group& b)const
  {
    return begin() < b.begin() || (begin() == b.begin() && b.end() < end());
  }


  private:
  uint32_t _start;
  uint32_t _count;
  std::string _name;
};

std::ostream& operator<<(std::ostream& os, const Group& rhs)
{
  return os << rhs.name() << ", " << rhs.begin() << ", " << rhs.end();
}

Group styleObjGroup(const Group& g)
{
  return Group(std::string("g ") + g.name(), g.begin(), g.count());
}

Group styleObjMaterial(const Group& g)
{
  return Group(std::string("usemtl ") + g.name(), g.begin(), g.count());
}

int main(int argc, char **argv)
{
  vector<Group> G;
  G.push_back(Group("groupA", 0, 30));
  G.push_back(Group("groupB", 30, 30));

  vector<Group> MG;
  MG.push_back(Group("materialA", 0, 10));
  MG.push_back(Group("materialD", 30, 20));
  MG.push_back(Group("materialC", 20, 10));
  MG.push_back(Group("materialB", 10, 10));
  MG.push_back(Group("materialE", 50, 10));

  std::vector<Group> sorted;
  sorted.reserve(G.size() + MG.size());
  std::transform(G.begin(), G.end(), back_inserter(sorted), bind(styleObjGroup, boost::ref(_1)));
  std::transform(MG.begin(), MG.end(), back_inserter(sorted), bind(styleObjMaterial, boost::ref(_1)));
  sort(sorted.begin(), sorted.end());

  std::vector<Group>::const_iterator i = sorted.begin();
  cout << *i << endl;
  cout << *(i+1) << endl;
  uint32_t c = i->count() + (i+1)->count();
  cout << "c: " << c << endl;

  //  vector<Group> all = G;
  //  all.insert(all.end(), MG.begin(), MG.end());

  //for_each(all.begin(), all.end(), cout << _1 << '\n');
  for_each(sorted.begin(), sorted.end(), cout << _1 << '\n');
  cout << endl;
  return 0;
}
