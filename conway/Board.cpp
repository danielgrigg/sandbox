/*
 *  Board.cpp
 *  conway
 *
 *  Created by Daniel Grigg on 23/08/10.
 *  Copyright 2010 Daniel Grigg. All rights reserved.
 *
 */

#include "Board.h"
#include "Kernel.h"
#include "Cell.h"
#include <algorithm>
#include <vector>

namespace conway
{
  
  struct CellGenerator
  {
    int m_width;
    int m_current;
    CellGenerator(int width):m_width(width), m_current(0)
    {}
    Cell operator()() 
    {
      int x = m_current % m_width;
      int y = m_current / m_width;
      ++m_current;
      return Cell(x,y);
    }
  };
  
  int RandomBinary(){ return rand() % 2; }
  
  BoardPtr Board::Create(const std::string pattern, int preferredSize)
  {
    
    BoardPtr board;
    std::vector<int> patternCells;
    int width = preferredSize;
    if (pattern == "block")
    {
      int cells[] = 
      {
        0,0,0,0,
        0,1,1,0,
        0,1,1,0,
        0,0,0,0
      };
      width = 4;
      patternCells.assign(cells, cells + sizeof(cells) / sizeof(int));
                        
    }
    else if (pattern == "boat")
    {
      int cells[] = 
      {
        0,0,0,0,0,
        0,1,1,0,0,
        0,1,0,1,0,
        0,0,1,0,0,
        0,0,0,0,0
      };
      width = 5;
      patternCells.assign(cells, cells + sizeof(cells) / sizeof(int));
    }
    else if (pattern == "blinker")
    {
      int cells[] = 
      {
        0,0,0,0,0,
        0,0,0,0,0,
        0,1,1,1,0,
        0,0,0,0,0,
        0,0,0,0,0
      };
      width = 5;
      patternCells.assign(cells, cells + sizeof(cells) / sizeof(int));
    }
    else if (pattern == "toad")
    {
      int cells[] = 
      {
        0,0,0,0,0,0,
        0,0,0,0,0,0,
        0,0,1,1,1,0,
        0,1,1,1,0,0,
        0,0,0,0,0,0,
        0,0,0,0,0,0
      };
      width = 6;
      patternCells.assign(cells, cells + sizeof(cells) / sizeof(int));
    }
    else if (pattern == "pulsar")
    {
      int cells[] = 
      {
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,1,1,1,0,0,0,1,1,1,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,1,0,0,0,0,1,0,1,0,0,0,0,1,0,0,
        0,0,1,0,0,0,0,1,0,1,0,0,0,0,1,0,0,
        0,0,1,0,0,0,0,1,0,1,0,0,0,0,1,0,0,
        0,0,0,0,1,1,1,0,0,0,1,1,1,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,1,1,1,0,0,0,1,1,1,0,0,0,0,
        0,0,1,0,0,0,0,1,0,1,0,0,0,0,1,0,0,
        0,0,1,0,0,0,0,1,0,1,0,0,0,0,1,0,0,
        0,0,1,0,0,0,0,1,0,1,0,0,0,0,1,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,1,1,1,0,0,0,1,1,1,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
      };
      width = 17;
      patternCells.assign(cells, cells + sizeof(cells) / sizeof(int));
    }
    else if (pattern == "random")
    {
      if (width < 0 || width > 100)
      {
        std::cerr << "Preferred size must be between 0 and 100.\n";
        return BoardPtr();
      }
      patternCells.resize(width * width);
      
      generate(patternCells.begin(), patternCells.end(), RandomBinary);
    }
    else
    {
      patternCells.assign(width*width, 0);
    }

    board = BoardPtr(new Board(width));
    for (int i = 0; i < width*width; ++i)
    {
      (*board)(i % width, i / width).setPopulation(patternCells[i]);
    }
    
    return board;
  }
  
  Board::Board(int width):
  m_width(width),
  m_cells(width * width)
  {
    std::generate(m_cells.begin(), m_cells.end(), CellGenerator(width));
  }
  
  void Board::update(BoardPtr rhs, KernelPtr kernel )
  {
    kernel->setBoard(rhs);
    
    //std::foreach(m_cells.begin(), m_cells.end(), std::mem_fun(&Cell::step));
    for (int i = 0; i < m_cells.size(); ++i)
    {
      m_cells[i].setPopulation(kernel->evaluate(rhs->m_cells[i]));
    }
    
  }
  
  Cell & Board::operator()(int x, int y)
  {
    // Transform (x,y) to board space via wrapping over the borders.
    int y_board = y % getWidth();
    if (y_board < 0) y_board += getWidth();
    int x_board = x % getWidth();
    if (x_board < 0) x_board += getWidth();
    return m_cells[y_board * getWidth() + x_board];
  }
  
  Cell Board::operator()(int x, int y)const
  {
    // Transform (x,y) to board space via wrapping over the borders.
    int y_board = y % getWidth();
    int x_board = x % getWidth();
    return m_cells[y_board * getWidth() + x_board];
  }
}

std::ostream & operator<<(std::ostream &os, const conway::Board &rhs)
{
  std::ostream_iterator<char> os_iter(os);
  std::fill_n(os_iter, rhs.getWidth()+2, '-');
  os << "\n";
  /*
   for (std::vector<conway::Cell>::iterator iter = m_cells.begin(); iter != m_cells.end(); iter += getWidth())
   {
   os << "|";
   std::copy(iter, iter + getWidth, os_iter);
   os << "|\n";
   }*/
  for (int y = 0; y < rhs.getWidth(); ++y)
  {
    os << "|";
    for (int x = 0; x < rhs.getWidth(); ++x)
    {
      os << rhs(x,y);
    }
    os << "|\n";
  }
  
  std::fill_n(os_iter, rhs.getWidth()+2, '-');
  return os;
}
