/*
 *  ListAccelerator.h
 *  pointview
 *
 *  Created by Daniel Grigg on 6/02/11.
 *  Copyright 2011 Daniel Grigg. All rights reserved.
 *
 */

#ifndef LIST_ACCELERATOR_H
#define LIST_ACCELERATOR_H

#include "Accelerator.h"

class ListAccelerator : public Accelerator
{
  public:
    bool init(std::vector<LPoint> & points);

//    void addPoint(const LPoint& p);

    void pointsWithinBounds(const LAABB &bounds, std::vector<LPoint> &pointsWithin)const;

  private:
    std::vector<LPoint> m_points;
};
#endif
