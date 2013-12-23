/*
 *  scene.h
 *  pointview
 *
 *  Created by Daniel Grigg on 6/02/11.
 *  Copyright 2011 Daniel Grigg. All rights reserved.
 *
 */
#ifndef SCENE_H
#define SCENE_H

#include <vector>
#include <lexi/Geometry/LAABB.h>
#include <stdint.h>
#include "Accelerator.h"
#include <tr1/memory>

struct Coloru8
{
  uint8_t rgba[4];
};

struct ViewRect
{
  ViewRect(float width, float height, uint32_t attribVertex, uint32_t attribColor)
  {
    m_attribVertex = attribVertex;
    m_attribColor = attribColor;
    m_bounds = LAABB(LPoint(-width / 2.0f, -width / 2.0f, -1.0f), LPoint(width/2.0f, height/2.0f, 1.f));
    m_viewBounds = m_bounds;
  }
  
  void translateTo(float x, float y)
  {
    LVector dt(x, y, 0.0);
    m_viewBounds = LAABB(m_bounds.min() + dt - LVector(0.0001, 0.0001, 0.0001),
                     m_bounds.max() + dt + LVector(0.0001, 0.0001, 0.0001));
  }
  const LAABB & bounds() {
    return m_viewBounds;
  }
  
  LAABB m_bounds;
  LAABB m_viewBounds;
  uint32_t m_attribVertex;
  uint32_t m_attribColor;
  void draw();
};

struct Triangle
{
  Triangle(const LPoint &v1, 
           const LPoint &v2, 
           const LPoint &v3, 
           const Coloru8 &c1,
           const Coloru8 &c2, 
           const Coloru8 &c3)
  {
    v[0] = v1; v[1] = v2; v[2] = v3;
    c[0] = c1; c[1] = c2; c[2] = c3;
  }
  LPoint v[3];
  Coloru8 c[3];
};

class TriangleBatch
{
public:
  TriangleBatch(uint32_t attribVertex, uint32_t attribColor)
  {
    m_attribVertex = attribVertex;
    m_attribColor = attribColor;
  }
  
  void addTriangle(const Triangle &t)
  {
    m_vertices.push_back(t.v[0]);
    m_vertices.push_back(t.v[1]);
    m_vertices.push_back(t.v[2]);
    m_colors.push_back(t.c[0]);
    m_colors.push_back(t.c[1]);
    m_colors.push_back(t.c[2]);
  }
  void draw();
  void clear()
  {
    m_vertices.clear();
    m_colors.clear();
  }
private:
  uint32_t m_attribVertex;
  uint32_t m_attribColor;
  std::vector<LPoint> m_vertices;
  std::vector<Coloru8> m_colors;
  
  LAABB m_viewBox;
};

class Scene
{
public:
  Scene(unsigned int attribVertex, uint32_t attribColor);
  ~Scene();
  bool init();
  
  void draw();
private:
  unsigned int m_attribVertex;
  uint32_t m_attribColor;
  TriangleBatch m_pointsBatch; // All points
  std::tr1::shared_ptr<Accelerator> m_sortedPoints;
//  Accelerator *m_sortedPoints;
  ViewRect m_viewRect;
};

#endif
