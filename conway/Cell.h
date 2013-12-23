/*
 *  Cell.h
 *  conway
 *
 *  Created by Daniel Grigg on 23/08/10.
 *  Copyright 2010 Daniel Grigg. All rights reserved.
 *
 */

#if !defined CONWAY_CELL_H
#define CONWAY_CELL_H

#include <ostream>

namespace conway
{

class Cell
{
public:
  Cell():
  m_x(0),
  m_y(0),
  m_population(0)
  {}
  
  Cell(int x, int y):
  m_x(x),
  m_y(y),
  m_population(0)
  {}
  
  bool isAlive()const { return m_population > 0; }
  int population()const { return m_population; }
  void setPopulation(int p) { m_population = p; }
  
  int x()const { return m_x; }
  int y()const { return m_y; }
  
private:
  int m_population;
  int m_x;
  int m_y;
};
}

inline std::ostream& operator<<(std::ostream &os, const conway::Cell &rhs)
{
  if (rhs.isAlive())
  {
    os << '*';
  }
  else
  {
    os << '.';      
  }
  return os;
}

#endif
