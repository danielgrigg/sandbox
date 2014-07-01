#ifndef BIN_ARRAY_H
#define BIN_ARRAY_H

#include "LArray.h"
//#include <Geometry/LPoint.hpp>
#include <vector>
#include "LAABB.h"

class LPoint;

class Bin
{
  public:
    Bin(){}
    void init(const LAABB &bounds);
    void add(const LPoint &item);

    const LAABB &bounds()const { return m_bounds; }
  private:
    LAABB m_bounds;
    std::vector<LPoint> m_items;
};

class BinArray
{
  public:
  BinArray(int numBinsWide, int numBinsHigh, const std::vector<LPoint> &points);
    void init(const std::vector<LPoint> &points);

    float binWidth()const;
    float binHeight()const;

    bool addPoint(const LPoint &item);

    void draw();
  private:
    LAABB m_bounds; 
    LArray2<Bin> m_bins;


    Bin * binForPoint(const LPoint &p);
};

#endif
