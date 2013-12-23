/*
 *  QuadTreeAccelerator.cpp
 *  pointview
 *
 *  Created by Daniel Grigg on 6/02/11.
 *  Copyright 2011 Daniel Grigg. All rights reserved.
 *
 */

#include "QuadTreeAccelerator.h"
#include <tr1/functional>
#include <iostream>

bool QuadTreeAccelerator::init(std::vector<LPoint> & points)
{
  Accelerator::init(points);
  return initWithBounds(points, bounds(), 0);
}

bool QuadTreeAccelerator::isLeaf()const
{
  return !(child(QuadTreeAccelerator::kSW) || 
          child(QuadTreeAccelerator::kSE) ||
          child(QuadTreeAccelerator::kNE) ||
          child(QuadTreeAccelerator::kNW));
}

void QuadTreeAccelerator::pointsWithinBounds(const LAABB &vBounds, std::vector<LPoint> &pointsWithin)const
{
#if 1
  if (!bounds().intersects(vBounds)) return;
 
  // Include any points within th\e bounds, even if they don't actually intersect
  if (isLeaf())
  {
    if (vBounds.contains(m_point)) 
      pointsWithin.push_back(m_point);
  }
  else
  {
    if (child(QuadTreeAccelerator::kSW)) child(QuadTreeAccelerator::kSW)->pointsWithinBounds(vBounds, pointsWithin);
    if (child(QuadTreeAccelerator::kSE)) child(QuadTreeAccelerator::kSE)->pointsWithinBounds(vBounds, pointsWithin);
    if (child(QuadTreeAccelerator::kNE)) child(QuadTreeAccelerator::kNE)->pointsWithinBounds(vBounds, pointsWithin);
    if (child(QuadTreeAccelerator::kNW)) child(QuadTreeAccelerator::kNW)->pointsWithinBounds(vBounds, pointsWithin);
  }
   #else
  if (bounds().min().x() > vBounds.max().x() || vBounds.min().x() > bounds().max().x()) return;
  if (bounds().min().y() > vBounds.max().y() || vBounds.min().y() > bounds().max().y()) return;
  if (isLeaf())
  {
    if (vBounds.contains(m_point)) pointsWithin.push_back(m_point);
  }
  else
  {
  if (child(QuadTreeAccelerator::kSW)) child(QuadTreeAccelerator::kSW)->pointsWithinBounds(vBounds, pointsWithin);
  if (child(QuadTreeAccelerator::kSE)) child(QuadTreeAccelerator::kSE)->pointsWithinBounds(vBounds, pointsWithin);
  if (child(QuadTreeAccelerator::kNE)) child(QuadTreeAccelerator::kNE)->pointsWithinBounds(vBounds, pointsWithin);
  if (child(QuadTreeAccelerator::kNW)) child(QuadTreeAccelerator::kNW)->pointsWithinBounds(vBounds, pointsWithin);
  }
#endif
}

void QuadTreeAccelerator::subdivideAABB(const LAABB &box, LAABB boxes[4])
{
  LPoint c = box.centre();
  boxes[kSW].addPoint(box.min()); 
  boxes[kSW].addPoint(LPoint(c.x(), c.y(), box.max().z()));
  boxes[kSE].addPoint(LPoint(c.x(), box.min().y(), box.min().z()));
  boxes[kSE].addPoint(LPoint(box.max().x(), box.max().y(), c.z()));
  boxes[kNE].addPoint(LPoint(c.x(), c.y(), box.min().z()));
  boxes[kNE].addPoint(box.max());
  boxes[kNW].addPoint(LPoint(box.min().x(), box.min().y(), c.z()));
  boxes[kNW].addPoint(LPoint(c.x(), box.max().y(), box.max().z()));
}

QuadTreeAccelerator::QuadTreeAccelerator()
{
  m_depth = 0;
}

QuadTreeAccelerator::Quad QuadTreeAccelerator::quadFromPoint(const LPoint &p)
{
  LVector width = bounds().max() - bounds().min();
  LVector dist = (p - bounds().min());
  int x = LFloatToInt(dist.x() / width.x() + 0.5f);
  int y = LFloatToInt(dist.y() / width.y() + 0.5f);
  return (QuadTreeAccelerator::Quad)((y << 1) + x);
}

bool QuadTreeAccelerator::initWithBounds(std::vector<LPoint> & points, const LAABB &vBounds, int depth)
{
  Accelerator::initWithBounds(vBounds);
  m_depth = depth;
  LAssert(!points.empty());

  if (points.size() > 1)
  {
    std::vector<LPoint> pointsByQuad[kQuadMax];
    for (std::vector<LPoint>::iterator iter = points.begin();
        iter != points.end(); ++iter)
    {
      Quad q = quadFromPoint(*iter);
      LAssert(q < kQuadMax);
      pointsByQuad[q].push_back(*iter);
    }
    std::vector<LPoint>().swap(points);
    LAABB boxes[4];
    subdivideAABB(bounds(), boxes);

    for (int i = 0; i < kQuadMax; ++i)
    {
      if (!pointsByQuad[i].empty())
      {
        m_children[i] = QuadTreeAcceleratorPtr(new QuadTreeAccelerator);
        m_children[i]->initWithBounds(pointsByQuad[i], boxes[i], m_depth + 1);
      }
    }
  }
  else 
  {
    m_point = points[0];
  }
  return true;
}

std::string QuadTreeAccelerator::description()const
{
  std::ostringstream oss;
  for (int i = 0; i < depth(); ++i) oss << "--";
  oss << bounds() << "\n";
  for (int i = 0; i < depth(); ++i) oss << "--";
  oss << ">" << point() << "\n";
  if (child(QuadTreeAccelerator::kSW)) oss << "SW:\n" << child(QuadTreeAccelerator::kSW)->description();
  if (child(QuadTreeAccelerator::kSE)) oss << "SE:\n" << child(QuadTreeAccelerator::kSE)->description();
  if (child(QuadTreeAccelerator::kNE)) oss << "NE:\n" << child(QuadTreeAccelerator::kNE)->description();
  if (child(QuadTreeAccelerator::kNW)) oss << "NW:\n" << child(QuadTreeAccelerator::kNW)->description();
  return oss.str();
}

std::ostream & operator<<(std::ostream &os,  const QuadTreeAccelerator &rhs)
{
  os << rhs.bounds() << "\n";
  for (int i = 0; i < rhs.depth(); ++i) os << ' ';
  os << rhs.point() << "\n";
  if (rhs.child(QuadTreeAccelerator::kSW)) os << *(rhs.child(QuadTreeAccelerator::kSW));
  if (rhs.child(QuadTreeAccelerator::kSE)) os << *(rhs.child(QuadTreeAccelerator::kSE));
  if (rhs.child(QuadTreeAccelerator::kNE)) os << *(rhs.child(QuadTreeAccelerator::kNE));
  if (rhs.child(QuadTreeAccelerator::kNW)) os << *(rhs.child(QuadTreeAccelerator::kNW));
  return os;
}

