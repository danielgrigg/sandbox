/*
 * \file position.cpp
 * Implement the Position interface.
 *
 * \author Daniel C Grigg
 */

#include "kt.h"
#include "position.h"
#include <iostream>

namespace kt {

  std::ostream& operator<<(std::ostream& os, const Position& p) {
    return os << "(" << p[0] << ", " << p[1] << ")";
  }

  bool Position::valid_position()const {
    return  _xy[0] >= 0 && 
      _xy[0] < KT::BOARD_SIZE &&
      _xy[1] >= 0 && 
      _xy[1] < KT::BOARD_SIZE;
  }

}

