// 
// \file position.h
// An XY coordinate on a chess-board, ranging (0,0) -> (7,7).
//
// \author Daniel C Grigg
//
#ifndef KT_POSITION_H
#define KT_POSITION_H

#include <string>
#include <iosfwd>
#include "kt.h"

namespace kt {

class Position {
  public:
  Position() { 
    _xy[0] = _xy[1] = 0; 
  }

  Position(int x, int y) { 
    _xy[0] = x; 
    _xy[1] = y;
  }

  bool operator==(const Position& rhs)const {
    return _xy[0] == rhs[0] && _xy[1] == rhs[1];
  }

  bool operator!=(const Position& rhs)const {
    return !(*this == rhs);
  }

  int& operator[](int i) { return _xy[i]; }
  int operator[](int i)const { return _xy[i]; }

  //! Convert a Position to its chess_notation, eg, (1,1) -> B2.
  std::string chess_notation()const {
    char rank_file[] = {'A' + _xy[0], '1' + _xy[1], 0};
    return std::string(rank_file);
  }

  //! Occupies a legal chess position.
  bool valid_position()const;

  private:
  int _xy[2];
};

inline Position operator+(const Position& lhs, const Position& rhs) {
  return Position(lhs[0] + rhs[0], lhs[1] + rhs[1]);
}

std::ostream& operator<<(std::ostream& os, const Position& p);
}

#endif
