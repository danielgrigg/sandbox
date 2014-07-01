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

#include <lexi/Geometry/LQuadTree.h>

using namespace std;

int main(int argc, char **argv)
{
  LPoint p;
  vector<LPoint> ps;
  ps.push_back(LPoint(-1,-1,-1));
  ps.push_back(LPoint(1,1,1));

  LPointQuadTreePtr tree = createLPointQuadTree(ps);
  cout << "Tree:\n" << *tree << flush;
  return 0;
}
