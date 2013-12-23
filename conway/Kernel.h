/*
 *  Kernel.h
 *  conway
 *
 *  Created by Daniel Grigg on 23/08/10.
 *  Copyright 2010 Daniel Grigg. All rights reserved.
 *
 */

#if !defined CONWAY_KERNEL_H
#define CONWAY_KERNEL_H 

#include "conway.h"

namespace conway
{
class Kernel
{
public:
  virtual ~Kernel();
  virtual int evaluate(const Cell & cell)const = 0;
  
  void setBoard(BoardPtr board);
protected:
  BoardPtr m_board;
};

}

#endif
