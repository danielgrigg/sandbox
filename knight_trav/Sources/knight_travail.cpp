/*
 * \file knight_travail.cpp
 * Command-line program to compute the shortest path between two chessboard positions. 
 * Most of the work is delegated to KnightSolver.
 *
 * \author Daniel C Grigg
 */

#include "knight_travail.h"
#include <iostream>
#include <sstream>
#include "config.h"
#include <ctype.h>

#include "io.h"
#include "knight_solver.h"
#include <boost/program_options.hpp>

// Version pulled from cmake
std::string version_string() {
  std::stringstream ss;
  ss << "knight_travail version " << KNIGHT_TRAVAIL_VERSION_MAJOR << 
    "." << KNIGHT_TRAVAIL_VERSION_MINOR << "\n";
  return ss.str();
}

std::string usage_string() {
  std::stringstream ss;
  ss << "Usage\n\n"
    << "  knight_travail [options]\n\n"
    << "Running\n\n"
    << "  Enter a pair of start and end coordinates in chess notation and\n"
    << "  knight_travail will output the shortest path between them.\n"
    << "  For example, entering 'B2 G5' outputs 'D3 E1 F3 G5'.\n"
    << "  Note that a valid chess position is of the form [A-H][1-8].\n";
  return ss.str();
}

int main(int argc, char* argv[]) {
  using namespace kt;
  using std::cout;
  using std::endl;
  namespace po = boost::program_options;

  // We only have one option, but '--help' is universally expected for
  // command-line utils.
  try {
    po::options_description desc("Available options");
    desc.add_options()
      ("help", "display help message");
    po::variables_map vm;
    po::store(po::parse_command_line(argc, argv, desc), vm);
    po::notify(vm);
    if (vm.count("help")) {
      cout << version_string() << "\n" << usage_string() << "\n" << desc 
           << endl;
      return 0;
    }
  } catch (std::exception& e) {
    std::cerr << e.what() << endl;
    return 1;
  } catch (...) {
    std::cerr << "knight_travail has encountered an unknown error " 
      << " and must exit." << std::endl;
    return 1;
  }

  cout << "Start End: ";
  std::string input, output;
  getline(std::cin, input);

  KnightSolver s;
  if (s.knight_travail(input, output)) {
    cout << output << endl;
  }
  
  return 0;
}

