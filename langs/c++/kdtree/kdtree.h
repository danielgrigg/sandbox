#ifndef KDTREE_H
#define KDTREE_H

#include <vector>
#include <algorithm>
#include <float.h>
#include <boost/lambda/bind.hpp>
#include <boost/lambda/lambda.hpp>
#include <iostream>
#include <cmath>

const float epsilon = 5E-4;

using namespace std;
using namespace boost::lambda;

  inline bool floatEquals(const float a, const float b)
  {
    return (fabs(a - b) < epsilon);
  }


template<int N>
class vec
{
  public:
    vec()
    {
      std::fill(v, v+N, float());
    }

    bool operator==(const vec<N>& rhs)const
    {
      return std::equal(v, v+N, rhs.v);
    }

    bool equalsWithError(const vec<N>& rhs)
    {
      return std::equal(v, v+N, rhs.v, floatEquals);
    }

    vec<N>& operator=(const vec<N>& rhs)
    {
      std::copy(rhs.v, rhs.v + N, v);
      return *this;
    }

    float operator[](int i)const { return v[i]; }
    float& operator[](int i) { return v[i]; }

  private:
    float v[N];
};

template <int N>
vec<N> vectorMin(const vec<N>& a, const vec<N>& b)
{
  vec<N> c;
  for (int i = 0; i < N; ++i) c[i] = std::min(a[i], b[i]);
  return c;
}

template <int N>
vec<N> vectorMax(const vec<N>& a, const vec<N>& b)
{
  vec<N> c;
  for (int i = 0; i < N; ++i) c[i] = std::max(a[i], b[i]);
  return c;
}

typedef vec<3> vec3f;

  template <int N>
std::ostream& operator<<(std::ostream& os, const vec<N>& rhs)
{
  os << '[';
  for (int i = 0; i < N-1; ++i) os << rhs[i] << ' ';
  os << rhs[N-1] << ']';
}

typedef vec<3> vec3f;
typedef vec<2> vec2f;

const float infinity = FLT_MAX;
template <typename P>
class BoundingBox
{
  public:
    BoundingBox()
    {
      _min[0] = infinity; _min[1] = infinity; _min[2] = infinity; 
      _max[0] = -infinity; _max[1] = -infinity; _max[2] = -infinity; 
    }

    BoundingBox(const P *points, int numPoints)
    {
      _min[0] = infinity; _min[1] = infinity; _min[2] = infinity; 
      _max[0] = -infinity; _max[1] = -infinity; _max[2] = -infinity; 
      std::for_each(points, points + numPoints, 
          boost::lambda::bind(&BoundingBox::unionPoint, this, boost::lambda::_1));
    }

    const P & min()const { return _min; }
    const P & max()const { return _max; }
    float size(int i)const { return _max[i] - _min[i]; }
    P centre()const
    {
      P p;
      p[0] = _min[0] + size(0) / 2.0f;
      p[1] = _min[1] + size(1) / 2.0f;
      p[2] = _min[2] + size(2) / 2.0f;
      return p;
    }
    void unionPoint(const P &p)
    {
      _min = vectorMin(_min, p);
      _max = vectorMax(_max, p);
    }
    int largestAxis()const
    {
      return std::max(make_pair(size(0), 0), 
          std::max(make_pair(size(1), 1), make_pair(size(2), 2))).second;
    }
  private:
    P _min;
    P _max;
};

  template <typename P>
std::ostream & operator<<(std::ostream &os, const BoundingBox<P> &rhs)
{
  os << '[' << rhs.min() << ", " << rhs.max() << ']';  
  return os;
}

template<typename T>
bool compareSplit(const T&a, const T& b, int splitAxis, float splitPosition)
{
  return (a[splitAxis] - splitPosition) < (b[splitAxis] - splitPosition);
}

#if 0
template <typename T>
class KdNode
{
  public:
    static const int kMaxLeaf = 1;
    static const int kMaxDepth = 8;

    typedef std::vector<T> PointVector;
    KdNode():
      _splitAxis(0),
      _splitPosition(float()),
      _left(NULL),
      _right(NULL),
      _points(NULL),
      _pointsSize(0)
  {}
    ~KdNode()
    {
      delete _left;
      delete _right;
    }

    void build(T* points, uint32_t pointsSize, uint32_t depth)
    {
      if (pointsSize <= kMaxLeaf || depth == kMaxDepth)
      {
        _points = points;
        _pointsSize = pointsSize;

        cout << depth << "leaf : [";
        for_each(_points, _points + _pointsSize, cout << _1 << ", ");
        cout << "]\n";
        return;
      }

      BoundingBox<vec3f> bbox(points, pointsSize);
      cout << depth << " interior : " << bbox << endl;
      _splitAxis = bbox.largestAxis();
      _splitPosition = bbox.centre()[_splitAxis];

      uint32_t mid = pointsSize / 2;
      nth_element(points, points + mid, points + pointsSize,
          bind(compareSplit<T>, _1, _2, _splitAxis, _splitPosition));

      _left = new KdNode<T>();
      _left->build(points, mid, depth+1);
      _right = new KdNode<T>();
      _right->build(points + mid, pointsSize - mid,depth+1);

    }
    //  private:
    uint32_t _splitAxis;
    float _splitPosition;
    KdNode<T>* _left;
    KdNode<T>* _right;
    T* _points;
    uint32_t _pointsSize;
};

template <typename T>
class KdTree
{
  public:
    void build(std::vector<T>& points)
    {
      _points = points;
      _root.build(&_points[0], _points.size(), 0);
    }
    //  private:
    KdNode<vec3f> _root;
    std::vector<T> _points;
};

  template <typename T>
std::ostream& operator<<(std::ostream& os, const KdNode<T>& rhs)
{
  os << "split [" << rhs._splitAxis << ", " << rhs._splitPosition << "]\n";
  for_each(rhs._points, rhs._points + rhs._pointsSize, os << _1 << ", ");
  if (rhs._left) os << *rhs._left << "\n";
  if (rhs._right) os << *rhs._right << "\n";
  return os;
}


  template <typename T>
std::ostream& operator<<(std::ostream& os, const KdTree<T>& rhs)
{
  os << "\nTree:\n";
  os << rhs._root;
}
#endif

template <typename T>
class KdNode
{
  public:
    static const int kMaxLeaf = 1;
    static const int kMaxDepth = 8;

    typedef std::vector<T> PointVector;
    KdNode():
      _splitAxis(0),
      _splitPosition(float()),
      _left(NULL),
      _right(NULL),
      _points(NULL),
      _pointsSize(0)
  {}
    ~KdNode()
    {
      delete _left;
      delete _right;
    }

    void addPoint(const T& pt)
    {
      if (!_points.empty() && 
          (fabsf(pt[_splitAxis] - _splitPosition) < epsilon))
      {
//        for (PointVector::const
      }
    }
  private:
    uint32_t _splitAxis;
    float _splitPosition;
    KdNode<T>* _left;
    KdNode<T>* _right;
    T* _points;
    uint32_t _pointsSize;
};

template <typename T>
class KdTree
{
  public:
    void addPoint(const T& pt)
    {
      _root.addPoint(pt);
    }
  private:
    KdNode<T> _root;
};


#endif
