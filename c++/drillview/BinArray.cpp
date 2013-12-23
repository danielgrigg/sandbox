#include "BinArray.h"
#include <iostream>
#include <tr1/functional>
#include <boost/bind.hpp>

void Bin::init(const LAABB &bounds)
{
  m_bounds = bounds;
}

void Bin::add(const LPoint &p)
{
  LAssert(m_bounds.contains(p)); 
  m_items.push_back(p);
}

BinArray::BinArray(int xBins, int yBins, const std::vector<LPoint> &points):
  m_bins(xBins, yBins)
{
  LAssert(xBins > 0 && yBins > 0);
  std::for_each(points.begin(), points.end(), 
                boost::bind(&LAABB::addPoint, boost::ref(m_bounds), _1));
}

bool BinArray::addPoint(const LPoint &item)
{
  // TODO Expand the bins?
  if (!m_bounds.contains(item))
  {
    std::cerr << "addPoint: " << item << " failed, outside bounds: " << m_bounds << "\n";
    return false;
  }
  Bin *bin = binForPoint(item);
  if (!bin) return false;
  bin->add(item);
  return true;
}

Bin * BinArray::binForPoint(const LPoint &p)
{
  if (!m_bounds.contains(p)) return NULL;
  int x = LFloatToInt((p.x() - m_bounds.min().x()) / binWidth());
  int y = LFloatToInt((p.y() - m_bounds.min().y()) / binHeight());
  return &m_bins(x,y);
}

float BinArray::binWidth()const 
{
  return m_bins(0,0).bounds().xSize();
}

float BinArray::binHeight()const
{
  return m_bins(0,0).bounds().ySize();
}

void BinArray::draw()
{
}
