/*
 *  conway.h
 *  conway
 *
 *  Created by Daniel Grigg on 23/08/10.
 *  Copyright 2010 Daniel Grigg. All rights reserved.
 *
 */

#if !defined CONWAY_H
#define CONWAY_H

#include <boost/shared_ptr.hpp>

namespace conway
{
  
  class Board;
  class Kernel;
  class Game;
  class Cell;
  
  typedef boost::shared_ptr<Game> GamePtr;
  typedef boost::shared_ptr<Kernel> KernelPtr;
  typedef boost::shared_ptr<Board> BoardPtr;
  typedef boost::shared_ptr<const Board> ConstBoardPtr;
}

#endif