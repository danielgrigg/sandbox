/*
 *  Kernel.cpp
 *  conway
 *
 *  Created by Daniel Grigg on 23/08/10.
 *  Copyright 2010 Daniel Grigg. All rights reserved.
 *
 */

#include "Kernel.h"
#include "board.h"

namespace conway
{
  
  Kernel::~Kernel()
  {}
  
  
void Kernel::setBoard(BoardPtr board)
{
  m_board = board;
}
  
}