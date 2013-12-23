/*
 *  Accelerator.cpp
 *  pointview
 *
 *  Created by Daniel Grigg on 6/02/11.
 *  Copyright 2011 Daniel Grigg. All rights reserved.
 *
 */

#include "Accelerator.h"

bool Accelerator::init(std::vector<LPoint> & points)
{
  m_bounds = LAABB(&points[0], points.size());

  // Expand the bounds to avoid precision problems.
  m_bounds = LAABB(m_bounds.min() - LVector(1,1,1),
                   m_bounds.max() + LVector(1,1,1));
  return true;
}

bool Accelerator::initWithBounds(const LAABB &bounds)
{
  m_bounds = bounds;
  return true;
}

const LAABB & Accelerator::bounds()const
{
  return m_bounds;
}
