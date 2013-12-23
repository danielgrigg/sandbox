/*
 *  ListAccelerator.cpp
 *  pointview
 *
 *  Created by Daniel Grigg on 6/02/11.
 *  Copyright 2011 Daniel Grigg. All rights reserved.
 *
 */

#include "ListAccelerator.h"

bool ListAccelerator::init(std::vector<LPoint> & points)
{
  Accelerator::init(points);
  m_points = points;
  return true;
}

void ListAccelerator::pointsWithinBounds(const LAABB &bounds, std::vector<LPoint> &pointsWithin)const
{
  for (std::vector<LPoint>::const_iterator iter = m_points.begin();
      iter != m_points.end(); ++iter)
  {
    if (bounds.contains(*iter))
    {
      pointsWithin.push_back(*iter);
    }
  }
}

