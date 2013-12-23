#include "io.h"
#include <boost/regex.hpp>

namespace kt {

static const boost::regex position_regex("([a-hA-H])([1-8])");
static const boost::regex input_regex("([a-hA-H][1-8]) ([a-hA-H][1-8])");

// Parse a chess-notation position to a (rank,file).
bool parse_chess_position(const std::string& s, Position& pos) {
  boost::smatch what;
  if (boost::regex_match(s.begin(), s.end(), what, position_regex)) {
    pos[0] = ::tolower(*what[1].first) - 'a';
    pos[1] = *what[2].first - '1';
    return true;
  }
  return false;
}

// Parse knight_travail input of the form "A3 B4".
bool parse_input(const std::string& line, 
    Position& start_at,
    Position& end_at) {

  boost::smatch what;
  if (boost::regex_match(line, what, input_regex) &&
      parse_chess_position(what[1], start_at) && 
      parse_chess_position(what[2], end_at)) {
    return true;
  }
  return false;
}

}

