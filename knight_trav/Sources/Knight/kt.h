//
// kt.h
//
// @brief - Contains useful constants and functions for all sources.

#ifndef KT_KT_H
#define KT_KT_H

#include <utility>

namespace kt {

  class KT
  {
    public:
      //! Length of the board side. Fixed to 8 for chess.
      static const int BOARD_SIZE  = 8;

      //! Total number of board vertices.
      static const int NUM_BOARD_VERTICES = BOARD_SIZE * BOARD_SIZE;

      //! Maximum number of valid adjacents for a knight's vertex.
      static const int KNIGHT_MOVE_DEGREE = 8;
  };

  typedef std::pair<int, int> Edge;

}

#endif

