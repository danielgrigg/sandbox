/*
 *  Game.h
 *  conway
 *
 *  Created by Daniel Grigg on 23/08/10.
 *  Copyright 2010 Daniel Grigg. All rights reserved.
 *
 */

#if !defined CONWAY_GAME_H
#define CONWAY_GAME_H

#include "conway.h"
#include <ostream>

namespace conway 
{
  
class Game
{
public:
  
  static GamePtr create(BoardPtr seedBoard);
  
  virtual ~Game()
  {}
    
  virtual void step() = 0;
  
  virtual ConstBoardPtr getBoard()const = 0;
private:
};
}

std::ostream & operator<<(std::ostream &os, const conway::Game &rhs);

#endif
