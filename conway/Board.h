/*
 *  Board.h
 *  conway
 *
 *  Created by Daniel Grigg on 23/08/10.
 *  Copyright 2010 Daniel Grigg. All rights reserved.
 *
 */

#if !defined CONWAY_BOARD_H
#define CONWAY_BOARD_H

#include "conway.h"
#include <vector>
#include "Cell.h"
#include <iostream>
#include <ostream>
#include <iterator>
#include <string>

namespace conway 
{

  class Board
  {
  private:
       
  public:
    
    static BoardPtr Create(const std::string pattern, int preferredSize);
    Board(int width);    
 
    
    int getWidth()const { return m_width; }
    
    virtual void update(BoardPtr rhs, KernelPtr kernel );    
    
    Cell & operator()(int x, int y);
    
    Cell operator()(int x, int y)const;
  private:
    
    int m_width;
    std::vector<Cell> m_cells;
  };    
}

std::ostream & operator<<(std::ostream &os, const conway::Board &rhs);

#endif
