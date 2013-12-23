/*
 *  Conway2D.cpp
 *  conway
 *
 *  Created by Daniel Grigg on 23/08/10.
 *  Copyright 2010 Daniel Grigg. All rights reserved.
 *
 */

#include "Conway2D.h"
#include "Cell.h"
#include "Board.h"

namespace conway 
{
  
  int Conway2DKernel::evaluate(const Cell & cell)const
  {
    return growthRule(cell.isAlive(), sumNeighbourPopulation(cell));
  }
 
  int Conway2DKernel::sumNeighbourPopulation(const Cell &cell)const
  {
    int x = cell.x();
    int y = cell.y();
    int sum = 0;
    sum += cellPopulation(x-1, y-1);
    sum += cellPopulation(x,y-1);
    sum += cellPopulation(x+1, y-1);
    
    sum += cellPopulation(x-1, y);
    sum += cellPopulation(x+1, y);
    
    sum += cellPopulation(x-1, y+1);
    sum += cellPopulation(x, y+1);
    sum += cellPopulation(x+1, y+1);
    return sum;
  }
  
  int Conway2DKernel::cellPopulation(int x, int y)const
  {
    return (*m_board)(x, y).population();
  }
  
  int Conway2DKernel::growthRule(bool cellAlive, int neighbourPopulation)const
  {
    const int n = neighbourPopulation;
    bool stayAlive = cellAlive && n >= 2 && n <= 3;
    bool becomeAlive = !cellAlive && n == 3;
    if (stayAlive || becomeAlive)
    {
      return 1;
    }
    
    return 0;
  }
  
  Conway2DGame::Conway2DGame(BoardPtr board)
  {
    m_kernel = KernelPtr(new Conway2DKernel);
    m_back = board;
    m_front = Board::Create("", board->getWidth());
  }
  
  void Conway2DGame::step()
  {
    m_front->update(m_back, m_kernel);
    std::swap(m_front, m_back);
  }
  
  ConstBoardPtr Conway2DGame::getBoard()const
  {
    return m_back;
  }
}