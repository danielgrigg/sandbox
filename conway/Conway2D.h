/*
 *  Conway2D.h
 *  conway
 *
 *  Created by Daniel Grigg on 23/08/10.
 *  Copyright 2010 Daniel Grigg. All rights reserved.
 *
 */

#if !defined CONWAY_CONWAY2D_KERNEL_H
#define CONWAY_CONWAY2D_KERNEL_H

#include "Kernel.h"
#include "Game.h"

namespace conway 
{
  
  class Conway2DKernel : public Kernel
  {
  public:
    virtual int evaluate(const Cell & cell)const;    
  private:
    
    int sumNeighbourPopulation(const Cell &cell)const;
    int cellPopulation(int x, int y)const;
    int growthRule(bool cellAlive, int neighbourPopulation)const;
  };
  
  class Conway2DGame : public Game
  {
  public:
    
    Conway2DGame(BoardPtr seedBoard);
    
    virtual void step();
    
    virtual ConstBoardPtr getBoard()const;
  private:
    KernelPtr m_kernel;
    BoardPtr m_front;
    BoardPtr m_back;
  };
  
}
#endif