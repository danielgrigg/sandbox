#include <algorithm>
#include <fstream>
#include <functional>
#include <iomanip>
#include <iostream>
#include <iterator>
#include <map>
#include <numeric>
#include <sstream>
#include <stdint.h>
#include <string>
#include <vector>
#include <boost/range/adaptors.hpp>
#include <boost/range/algorithm.hpp>
#include <boost/bimap.hpp>
#include "boost/multi_array.hpp"
#include <cassert>
#include <regex>

using namespace std;

// Ideally use a bimap here for both turns, but it appears 
// an initializer_list constructor doesn't exist yet in boost :(
const std::map<const char, const char> RIGHT_TURNS {
  {'N', 'E'},
  {'E', 'S'},
  {'S', 'W'},
  {'W', 'N'}};

const std::map<const char, const char> LEFT_TURNS {
  {'N', 'W'},
  {'W', 'S'},
  {'S', 'E'},
  {'E', 'N'}};
  
const std::map<const char, const tuple<int, int> > MOVE_MAP = {
  {'N', std::make_tuple(0, 1)},
  {'E', std::make_tuple(1, 0)},
  {'S', std::make_tuple(0, -1)},
  {'W', std::make_tuple(-1, 0)}
};

template <typename K, typename V>
V constmap_get(const K k, const std::map<const K, V>& m) {
  auto x = m.find(k);
  assert(x != m.end());
  return x->second;
}

enum class Direction {
  North, // 0
  East, // 1
  South, // 2
  West // 3
};

char direction_to_facing(Direction d) {
  const char compass_char[] = {'N', 'E', 'S', 'W'};
  return compass_char[static_cast<int>(d)]; 
}

std::ostream& operator<<(std::ostream& os, Direction d) {
  os << direction_to_facing(d);
  return os;
}

struct Rover {

  tuple<int, int> xy;
  char _facing;

  const char facing()const { return _facing; }

  Rover(int x, int y, char f):
    xy(x, y),
    _facing(f) {
    }

  void left() { 
    _facing = constmap_get(facing(), LEFT_TURNS);
    cout << "turned left to " << facing() << endl;
  }
  void right() { 
    _facing = constmap_get(facing(), RIGHT_TURNS);
    cout << "turned right to " << facing() << endl;
  }
  void move() { 
    int dx, dy;
    std::tie(dx, dy) = constmap_get(facing(), MOVE_MAP);
    cout << "moving (" << dx << ", " << dy << ")\n";
  }
};

std::ostream& operator<<(std::ostream& os, const Rover& rhs) {
  os << std::get<0>(rhs.xy) << " " << std::get<1>(rhs.xy) << 
    " " << rhs.facing();
  return os;
}

typedef std::shared_ptr<Rover> RoverPtr;

RoverPtr make_rover(const std::string& position) {
  std::regex e("(\\d+) (\\d+) ([NESW])");

  std::smatch sm;
  if (std::regex_match(position, sm, e)) {
    cout << "regex matched" << endl;

    int x = std::stoi(sm[1]);
    int y = std::stoi(sm[2]);
    std::string facing_str = sm[3];

    return RoverPtr(new Rover(x, y, facing_str[0]));
  } else { 
    cout << "didn't match" << endl; 
    return RoverPtr();
  }
}

int main () {
  // Create a 3D array that is 3 x 4 x 2
  typedef boost::multi_array<bool, 2> array_type;
  typedef array_type::index index;

  array_type A(boost::extents[3][3]);

  for (int i = 0; i < 3; ++i) {
    for (int j = 0; j < 3; ++j) {
      A[i][j] = false;
    }
  }

  std::map<char, std::function<void(Rover&)>> command_map{
    {'M', &Rover::move},
      {'L', &Rover::left},
      {'R', &Rover::right}};


  std::string command_queue("LMLMRMMRCR");
  std::vector<std::function<void()> > commands;

  std::string rover_str = "23 14 N";

  Rover r(3,3, 'N');

  auto it = boost::find_if(command_queue, 
      [&](char c) { return command_map.find(c) == command_map.end(); });

  //  boost::copy(it,
  //     std::ostream_iterator<char>(cout, "\n"));

  //    [](char c) { std::cout << c << "\n"; });

  auto it2 = 
    find_if(command_queue.begin(), command_queue.end(),
        [&](char c) { return command_map.find(c) == command_map.end(); });

  std::for_each(command_queue.begin(), it2, 
      [&](char c) { command_map[c](r); });


  for (Direction d : { Direction::North, Direction::South, 
                        Direction::East, Direction::West}) {
  std::cout << d << endl;
  }

  cout << "Making rover " << endl;
  RoverPtr p = make_rover("-13 27 E");
  if (p) {
    cout << "rover " << *p << endl;
  }

  return 0;
}
