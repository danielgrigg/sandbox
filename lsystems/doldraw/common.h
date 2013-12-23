#ifndef COMMON_H
#define COMMON_H

#include <lap/lap.h>

inline lap::VertexP makeVertex(float x, float y, float z)
{
  lap::float3 p; p[0] = x; p[1] = y; p[2] = z;
  return lap::VertexP(p);
}

#endif // COMMON_H
