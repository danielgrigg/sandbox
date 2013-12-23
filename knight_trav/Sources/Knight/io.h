//
// io.h
//
// Input-Output functions for interacting with knight_travail.
//
#ifndef KT_IO_H
#define KT_IO_H

#include "kt.h"
#include "position.h"
#include <string>
#include <vector>
#include <algorithm>
#include <boost/algorithm/string.hpp>
#include <boost/bind.hpp>


namespace kt {

// Parse a chess-notation position to a (rank,file).
bool parse_chess_position(const std::string& s, Position& pos);

// Parse knight_travail input of the form "A3 B4".
bool parse_input(const std::string& line, Position& start_at, Position& end_at);

// Convert a sequence of Positions into a string of chess-positions.
template <typename SequenceT>
inline std::string route_to_string(const SequenceT& r) {
  std::vector<std::string> chess_positions(r.size());
  std::transform(r.begin(), r.end(), chess_positions.begin(), 
      boost::bind(&kt::Position::chess_notation, _1));
  return boost::join(chess_positions, " ");
}


}

#endif
