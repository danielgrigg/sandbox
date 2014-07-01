#include <gtest/gtest.h>
#include "knight_solver.h"

using namespace std;
using namespace kt;

TEST(KnightTravailTest, can_output_shortest_path) {
  KnightSolver s;
  std::string input("A5 g5");
  std::string output;
  EXPECT_TRUE(s.knight_travail(input, output));
  EXPECT_EQ("B3 C5 E6 G5", output);
}

