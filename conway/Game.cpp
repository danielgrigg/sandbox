/*
 *  Game.cpp
 *  conway
 *
 *  Created by Daniel Grigg on 23/08/10.
 *  Copyright 2010 Daniel Grigg. All rights reserved.
 *
 */

#include "Game.h"
#include "Board.h"

std::ostream & operator<<(std::ostream &os, const conway::Game &rhs)
{
  return os << *(rhs.getBoard()) << "\n";
}