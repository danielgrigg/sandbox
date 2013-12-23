/*
 *  QuadTreeAccelerator.h
 *  pointview
 *
 *  Created by Daniel Grigg on 6/02/11.
 *  Copyright 2011 Daniel Grigg. All rights reserved.
 *
 */

#ifndef QUAD_TREE_ACCELERATOR_H
#define QUAD_TREE_ACCELERATOR_h

#include "Accelerator.h"
#include <iosfwd>
#include <tr1/memory>

class QuadTreeAccelerator;
typedef std::tr1::shared_ptr<QuadTreeAccelerator> QuadTreeAcceleratorPtr;
typedef std::tr1::shared_ptr<const QuadTreeAccelerator> QuadTreeAcceleratorConstPtr;

class QuadTreeAccelerator : public Accelerator
{
  public:
    enum Quad
    {
      kQuadMin,
      kSW = 0,
      kSE,
      kNW,
      kNE,
      kQuadMax
    };

    QuadTreeAccelerator();
    virtual ~QuadTreeAccelerator(){}
    bool init(std::vector<LPoint> & points);
    void pointsWithinBounds(const LAABB &bounds, std::vector<LPoint> &pointsWithin)const;
    LPoint point()const { return m_point; }
    int depth()const { return m_depth; }
    QuadTreeAcceleratorPtr child(Quad q) { return m_children[q]; }
    QuadTreeAcceleratorConstPtr child(Quad q)const { return m_children[q]; }
    virtual std::string description()const;
  private:
  bool isLeaf()const;
    Quad quadFromPoint(const LPoint &p); 
    void subdivideAABB(const LAABB &box, LAABB boxes[4]);
    bool initWithBounds(std::vector<LPoint> & points, const LAABB &bounds, int depth);
    int m_depth;
    LPoint m_point; // Internal nodes have filtered points of leaves.
    QuadTreeAcceleratorPtr m_children[4];
};

std::ostream & operator<<(std::ostream &os, const QuadTreeAccelerator &rhs);

#endif
