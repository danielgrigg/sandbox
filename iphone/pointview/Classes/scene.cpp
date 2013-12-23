/*
 *  scene.cpp
 *  pointview
 *
 *  Created by Daniel Grigg on 6/02/11.
 *  Copyright 2011 Daniel Grigg. All rights reserved.
 *
 */


#include <stdint.h>

#import <OpenGLES/ES2/gl.h>
#import <OpenGLES/ES2/glext.h>

#include "scene.h"
#include "ListAccelerator.h"
#include "QuadTreeAccelerator.h"

void ViewRect::draw()
{
  LPoint v[4];
  v[0] = LPoint(bounds().min().x(), bounds().min().y(), 0.0);
  v[1] = LPoint(bounds().max().x(), bounds().min().y(), 0.0);
  v[2] = LPoint(bounds().max().x(), bounds().max().y(), 0.0);
  v[3] = LPoint(bounds().min().x(), bounds().max().y(), 0.0);
  Coloru8 c[4] = {255,255,255,255, 255,255,255,255, 255,255,255,255, 255,255,255,255};
  glVertexAttribPointer(m_attribVertex, 4, GL_FLOAT, 0, 0, &v[0]);
  glEnableVertexAttribArray(m_attribVertex);
  glVertexAttribPointer(m_attribColor, 4, GL_UNSIGNED_BYTE, 1, 0, &c[0]);
  glEnableVertexAttribArray(m_attribColor);
  glDrawArrays(GL_LINE_LOOP, 0, 4);
}

Triangle triangleFromPoint(const LPoint &p, const Coloru8 &pointColor)
{
  const float w = 0.012f;
  Coloru8 c = {
    (uint8_t)(0.5f * (float)pointColor.rgba[0]),
    (uint8_t)(0.5f * (float)pointColor.rgba[1]),
    (uint8_t)(0.5f * (float)pointColor.rgba[2]),
    255
  };
  Coloru8 c2 = {
    (uint8_t)(0.8f * (float)pointColor.rgba[0]),
    (uint8_t)(0.8f * (float)pointColor.rgba[1]),
    (uint8_t)(0.8f * (float)pointColor.rgba[2]),
    255
  };
  
  return Triangle(p + LPoint(-w, -w, 0.0), 
                  p + LPoint(w, -w, 0.0),
                  p + LPoint(0.0, w, 0.0),
                  pointColor,
                  c2,
                  c);
}

void TriangleBatch::draw()
{
  // Update attribute values.
  glVertexAttribPointer(m_attribVertex, 4, GL_FLOAT, 0, 0, &m_vertices[0]);
  glEnableVertexAttribArray(m_attribVertex);
  glVertexAttribPointer(m_attribColor, 4, GL_UNSIGNED_BYTE, 1, 0, &m_colors[0]);
  glEnableVertexAttribArray(m_attribColor);
  glDrawArrays(GL_TRIANGLES, 0, m_vertices.size());
}

Scene::Scene(uint32_t attribVertex, uint32_t attribColor):
m_pointsBatch(attribVertex, attribColor),
m_viewRect(.4f, .4f, attribVertex, attribColor)
{
  m_attribVertex = attribVertex;
  m_attribColor = attribColor;
 // m_sortedPoints = NULL;
}

Scene::~Scene()
{
//  delete m_sortedPoints;
//  m_sortedPoints = NULL;
}

void Scene::draw()
{  
  static float t = 0.0;
  t += 0.009f;
  
  m_viewRect.translateTo(0.2f * cos(t), 0.8f * sin(t));
  m_pointsBatch.draw();
  
  std::vector<LPoint> visiblePoints;
  m_sortedPoints->pointsWithinBounds(m_viewRect.bounds(), visiblePoints);
  TriangleBatch visibleBatch(m_attribVertex, m_attribColor);
  Coloru8 color = { 255, 0, 0, 255};
  for (int i = 0; i < visiblePoints.size(); ++i)
  {
    visibleBatch.addTriangle(triangleFromPoint(visiblePoints[i], color));
  }
  visibleBatch.draw();
  m_viewRect.draw();
}

class PointSetBuilder
{
public:
  virtual void build(std::vector<LPoint> &points) = 0;
};

float randomSignedFloat()
{
  return 2.0f * ((float)rand() / (float)RAND_MAX) - 1.0f;
}

LPoint makeRandomPoint()
{
  return LPoint(randomSignedFloat(), randomSignedFloat(), 0.0f);
}

class RandomPoints : public PointSetBuilder
{
public:
  virtual void build(std::vector<LPoint> &points)
  {
    std::generate(points.begin(), points.end(), makeRandomPoint);
  }
};

bool Scene::init()
{
  srand(time(NULL));

  m_pointsBatch.clear();
  vector<LPoint> points(800);

  PointSetBuilder *builder = new RandomPoints();
  builder->build(points);
  delete builder;
  
  Coloru8 color = {
    0, 0, 55, 255
  };
  for (int i = 0; i < points.size(); ++i) m_pointsBatch.addTriangle(triangleFromPoint(points[i], color));
  //m_sortedPoints = new ListAccelerator();  
  m_sortedPoints = std::tr1::shared_ptr<Accelerator>(new QuadTreeAccelerator());
  m_sortedPoints->init(points);
 // std::cout << m_sortedPoints->description() << std::endl;
  return true;
}
