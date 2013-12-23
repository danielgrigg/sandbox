/*
 *  Accelerator.h
 *  pointview
 *
 *  Created by Daniel Grigg on 6/02/11.
 *  Copyright 2011 Daniel Grigg. All rights reserved.
 *
 */

#ifndef ACCELERATOR_H
#define ACCELERATOR_H

#include <lexi/Geometry/LAABB.h>

class Accelerator
{
  public:
    Accelerator(){}
    virtual ~Accelerator(){}
    virtual bool init(std::vector<LPoint> & points);

    const LAABB & bounds()const;

    //    void addPoint(const LPoint& p) = 0;

    virtual void pointsWithinBounds(const LAABB &bounds, std::vector<LPoint> &pointsWithin)const = 0;

    virtual std::string description()const = 0;
  protected:
    virtual bool initWithBounds(const LAABB &bounds);
  private:
    LAABB m_bounds;
};

#endif
