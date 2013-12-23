#ifndef GEOMETRY_BATCH_H
#define GEOMETRY_BATCH_H

#include "glforward.h"
#include <vector>
#include <lexi/Color/LColor.hpp>
#include <lexi/Geometry/LPoint.hpp>
#include <lexi/Geometry/LNormal.hpp>
#include <iostream>
#include <lexi/Geometry/LAABB.h>

struct UV
{
  UV():
    u(0.0f), v(0.0f)
  {}
  UV(float up, float vp):
    u(up),
    v(vp)
  {}
  float u;
  float v;
};

inline std::ostream & operator <<(std::ostream &os, const UV &rhs)
{
  os << '[' << rhs.u << ',' << rhs.v << ']';
  return os;
}

const int kUVMax = 4;

struct GeometryBatch
{
  typedef uint32_t ElementType;
//  typedef uint16_t ElementType;
  enum
  {
    kPosOffset,
    kColorOffset,
    kNormalOffset,
    kUV0Offset,
    kUV1Offset,
    kUV2Offset,
    kUV3Offset
  };

  enum Usage
  {
    kStatic,
    kDynamic,
    kUsageMax
  };

  GLuint arrayObject;
  GLuint vertexObject;
  GLuint colorObject;
  GLuint normalObject;
  GLuint elementObject;
  GLuint uvObjects[kUVMax];
  GLuint primitiveType;
  vector<LPoint> ps;
  vector<LColor> cs;
  vector<LNormal> ns;
  vector<UV> uvs[kUVMax];
  vector<ElementType> elements;
  string name;
  bool bound;
  LAABB boundingBox;
  GLuint m_usage;

  GeometryBatch(){ bound = false; }

  void init(GLuint prim, const string &name, Usage usage = kStatic);
  void bind();
  void draw();
  void drawRange(uint32_t offset, uint32_t count);
  void drawRange(uint32_t offset, uint32_t count, 
    uint32_t minElement, uint32_t maxElement);
  void setDirty() { bound = false; }
};

#endif
