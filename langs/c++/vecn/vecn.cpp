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
#include <cfloat>
#include <tr1/unordered_map>
#include <boost/lambda/lambda.hpp>
#include <boost/lambda/bind.hpp>
#include <limits>
#include <lap/lap.h>

#include <boost/static_assert.hpp>

using namespace boost::lambda;
using namespace std;
using namespace lap;

template <typename P, typename N>
struct PositionNormal
{
  vec<P,3> position;
  vec<N,3> normal;

  PositionNormal()
  {}

  template <typename U> PositionNormal(const U& rhs): 
    position(rhs.position), normal(rhs.normal) { }
};

template <>
struct PositionNormal<int16_t, int16_t>
{
  vec<int16_t, 3> position;
  uint8_t _padding[2];
  vec<int16_t, 3> normal;
  PositionNormal(){}

  template <typename U> PositionNormal(const U& rhs): 
    position(rhs.position), normal(rhs.normal) { }
};

template <typename P, typename N, typename T>
struct PositionNormalColorUV
{
  vec<P,3> position;
  vec<N,3> normal;
  vec<T,2> uv;
  vec<uint8_t,4> color;

  PositionNormalColorUV()
  {}

  template <typename U>
    PositionNormalColorUV(const U& rhs):
      position(rhs.position),
      normal(rhs.normal),
      uv(rhs.uv),
      color(rhs.color)
    { }
};

template <typename P, typename N, typename T>
struct PositionNormalUV
{
  vec<P,3> position;
  vec<N,3> normal;
  vec<T,2> uv;
  PositionNormalUV()
  {}

  template <typename U>
    PositionNormalUV(const U& rhs):
      position(rhs.position),
      normal(rhs.normal),
      uv(rhs.uv)
    { }
};


void vertexAttribPointer(uint32_t index, int size, uint32_t type, bool normalized, size_t stride,
                                  const void * pointer)
{
  std::cout << "vertexAttribPointer("
    << index << ", "
    << size << ", "
    << type << ", "
    << normalized << ", "
    << stride << ", "
    << pointer << ")\n";

}
template <typename T>
uint32_t componentType();

template <> uint32_t componentType<float>() { return 123; }
template <> uint32_t componentType<short>() { return 245; }


#define BUFFER_OFFSET(i) ((char *)NULL + (i))

template <typename P, typename N>
void vertexPointer(PositionNormal<P,N>* vs)
{
  vertexAttribPointer(0, 3, componentType<P>(), false, sizeof(PositionNormal<P,N>), 
    BUFFER_OFFSET(0));

  vertexAttribPointer(1, 3, componentType<N>(), false, sizeof(PositionNormal<P,N>), 
    BUFFER_OFFSET(sizeof(P)));
}

void testVertexPointer()
{
  PositionNormal<float,short> v;
  v.position = float3(1, -2, 3);
  v.normal = vec<short,3>(0, 1, 0);
  vertexPointer(&v);
}

typedef PositionNormal<float,float> PositionFNormalF;
typedef PositionNormal<int16_t,int16_t> Position16Normal16;

template <typename P, typename N>
std::ostream& operator<<(std::ostream& os, const PositionNormal<P, N>& rhs)
{
  os << rhs.position << ", ";
  os << rhs.normal;
  return os;
}

template <typename U, int N>
vec<float, N> compress(const vec<U, N>& rhs)
{
  vec<float, N> result;
  for (uint32_t i = 0; i < N; ++i)
  {
    result[i] = (float)rhs[i] / std::numeric_limits<U>::max();
  }
  return result;
}

template <typename T, int N>
vec<T, N> expand(const vec<float, N>& rhs)
{
  vec<T, N> result;
  for (uint32_t i = 0; i < N; ++i)
  {
    result[i] = rhs[i] * std::numeric_limits<T>::max();
  }
  return result;
}

void testAlignment()
{
  PositionNormal<short, short> pn16[2];
  pn16[0].position = vec<short, 3>(12, 15, 20);
  pn16[0].normal = vec<short, 3>(0, 1, 0);

  pn16[1].position = vec<short, 3>(5, 7, 9);
  pn16[1].normal = vec<short, 3>(1, 0, 0);


  cout << "&pn16[0] " << &pn16[0] << ", &pn16[0].p " 
    << (uint8_t*)&pn16[0].position - (uint8_t*)&pn16[0] << ", &pn16[0].n "
        << (uint8_t*)&pn16[0].normal - (uint8_t*)&pn16[0] << endl;

  cout << "&pn16[1] " << &pn16[1] << ", &pn16[1].p " 
    << (uint8_t*)&pn16[1].position - (uint8_t*)&pn16[1] << ", &pn16[1].n "
        << (uint8_t*)&pn16[1].normal - (uint8_t*)&pn16[1] << endl;

}

void testMeshAlignCopy()
{
  const char* modelFile = "/Users/daniel/content/models/test/box_pn.obj";
   shared_ptr<Mesh<VertexPN> > m = 
     meshFromObj<VertexPN>(obj::ObjTranslator().importFile(modelFile));

   cout << "m\n" << *m << endl;

   Mesh<PositionNormal<short, short> > m16 = *m;
   cout << "m16\n" << m16 << endl;

}

float rand16()
{
  return (2.0f * ((random() % 0xFFFF) / 65535.0f) - 1.0f) * 32767.0f;
      
}

PositionNormal<float, float> makeRandPN()
{
  PositionNormal<float, float> pn;
  pn.position = vec<float, 3>(rand16(), rand16(), rand16());
  pn.normal = vec<float, 3>(rand16(), rand16(), rand16());
  return pn;
}

void testAlignCopy()
{
  PositionNormal<float, float> pn;
  pn.position = vec<float, 3>(-3.5, 10, 999);
  pn.normal = vec<float, 3>(43, -10, 77.7);

  cout << "pn " << pn << endl;

  PositionNormal<short, short> pn16(pn);
  cout << "pn16 " << pn16 << endl;

  std::vector<PositionNormal<float, float> > pns(4);
  generate(pns.begin(), pns.end(), makeRandPN);
  for_each(pns.begin(), pns.end(), cout << _1 << ' ');
  cout << endl; 
  std::vector<PositionNormal<short, short> > pns16(pns.begin(), pns.end());
  for_each(pns16.begin(), pns16.end(), cout << _1 << ' ');
  cout << endl; 

}

void testExpand()
{
  vec<float, 2> a(-1.0f, 1.0f);
  vec<int16_t, 2> b = expand<int16_t>(a);
  cout << "b " << b << endl;
}

int main(int argc, char **argv)
{
  float3 a;
  a[0] = 3; a[1] = 3; a[2] = 3;
  float3 b;

  b[0] = 2; b[1] = 3; b[2] = 2;

  float3 c;
  c[0] = 3; c[1] = 3; c[2] = 3;


  cout << "a " << a << ", b " << b << ", c " << c << endl;
  cout << "a == b: " << (a == b) << ", b == c: " << (a == c) << endl;

  cout << "b < a: " << (b < a) << endl;

  typedef std::tr1::unordered_map<float3, int> Float3Map;

  //  Float3Map float3Map;
  //  std::map<float3, int> myMap;
  std::tr1::unordered_map<float3, int> float3Map;

  for (int i = 0; i < 10; ++i)
  {
    float3 z; 
    z[0] = rand() % 100 - 50;
    z[1] = rand() % 100 - 50;
    z[2] = rand() % 100 - 50;
    float3Map[z] = i;
  }

  //  for_each(float3Map.begin(), float3Map.end(), 
  //     cout << bind(&std::pair<float3,int>::first, _1) << endl);

  for (Float3Map::const_iterator i = float3Map.begin(); i != float3Map.end(); ++i)
  {
    cout << i->first << endl;
  }

  vector<vec<short, 3> > s3;
  std::transform(float3Map.begin(), float3Map.end(), std::back_inserter(s3),
      bind(&Float3Map::value_type::first, _1));

  cout << endl;
  for_each(s3.begin(), s3.end(), cout << _1 << '\n');

  {
    vec<float, 2> x(3.0f, -5.0f);
    cout << "x " << x << endl;
    float3 y(-2, 1, 99.0f);
    cout << "y " << y << endl;

    vec<float, 4> w(1, -2, 8, -7.0f);
    vec<float, 4> z = w;
    z[2] = .3f;
    cout << "z " << z << endl;
  }

  {
    PositionFNormalF x;
    x.position = float3(1, 2, 3);
    x.normal = float3(1, 0, 0);
    cout << "x " << x << endl;
    Position16Normal16 y = x;
    cout << "y " << y << endl;

  }
  testExpand();

  testVertexPointer();

  testAlignment();

  testAlignCopy();

  testMeshAlignCopy();
  return 0;
}
