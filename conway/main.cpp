#include <iostream>
#include <boost/program_options.hpp>
#include <algorithm>

#include "Conway2D.h"
#include "Board.h"

namespace po = boost::program_options;
using std::cout;

namespace conway 
{
GamePtr Game::create(BoardPtr seedBoard)
{
  return GamePtr(new Conway2DGame(seedBoard));
}
}


// A Board consisting of Cells
// A Cell can be Alive or Dead
// A Cell has neighbours
// A Board transitions to a new Board on a tick (Front Board, Back Board)
// A Conway cell transitions by applying a rule to directly adjacent (distance 1) neighbours
// A Conway cell rule counts the living neighbours, n and given it's current Alive state, s0, evaluates s1 = rule(n, s0)

// A simple solver that evalulates all neighbours in one go
// A staged solver that accumulates the horizontal neighbours then evaluates the vertical, accumulated cells.

int main (int argc, char *  argv[]) 
{
  /*
  // Declare the supported options.
  po::options_description desc("Allowed options");
  desc.add_options()
  ("help", "produce help message")
  ("size", po::value<int>(), "set size of the board")
  ;
  
  po::variables_map vm;
  po::store(po::parse_command_line(argc, argv, desc), vm);
  po::notify(vm);    
  
  if (vm.count("help")) 
  {
    cout << desc << "\n";
    return 1;
  }
  
  if (vm.count("size")) 
  {
    cout << "Size level was set to "  << vm["size"].as<int>() << ".\n";
  } 
  else 
  {
    cout << "Size level was not set.\n";
  }
  */
  
  //conway::BoardPtr b = conway::Board::Create("block", 0);
  //conway::BoardPtr b = conway::Board::Create("blank", 0);
  //  conway::BoardPtr b = conway::Board::Create("random", 7);
  //conway::BoardPtr b = conway::Board::Create("boat", 0);
  //conway::BoardPtr b = conway::Board::Create("block", 0);
  
//  conway::BoardPtr b = conway::Board::Create("blinker", 0);
  //conway::BoardPtr b = conway::Board::Create("random", 5);
//  conway::BoardPtr b = conway::Board::Create("toad", 0);
   conway::BoardPtr b = conway::Board::Create("pulsar", 0);
  conway::GamePtr game = conway::Game::create(b);
  
  std::cout << "Game: \n" << *game << std::endl;
  
  int iterations = 10;
  for (int i = 0; i < iterations; ++i)
  {
    game->step();
    std::cout << "Iteration: " << i+1 << "\n" << *game;
  }
  
  return 0;
}
