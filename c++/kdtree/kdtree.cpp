#include <algorithm>
#include <iostream>
#include <stdint.h>
#include <string>
#include <vector>
#include "kdtree.h"
#include <cstdlib>
#include <boost/lambda/lambda.hpp>
#include <boost/lambda/bind.hpp>

using namespace std;
using namespace boost::lambda;

vec3f makeVec3f(float x, float y, float z)
{
  vec3f a;
  a[0] = x;
  a[1] = y;
  a[2] = z;
  return a;
}

vec3f makeRandomPoint(int size)
{
  vec3f a;
  a[0] = random() % size;
  a[1] = random() % size;
  a[2] = random() % size;
  return a;
}

int main(int argc, char **argv)
{
  time_t t = time(NULL);
  srandom(t);
  cout << ctime(&t) << endl;

  std::vector<vec3f> points(3);
  std::generate(points.begin(), points.end(), bind(makeRandomPoint, 100));
  std::for_each(points.begin(), points.end(), std::cout << _1 << "\n");

  BoundingBox<vec3f> bbox(&points[0], points.size());
  cout << bbox << endl;
  cout << endl;

  KdTree<vec3f> tree;
  tree.build(points);

  cout << tree << endl;
  cout << bbox.size(0) << ", " << bbox.size(1) << ", " << bbox.size(2) << endl;
  return 0;
}
