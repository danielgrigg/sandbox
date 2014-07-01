#include "knight_solver.h"
#include "position.h"
#include <gtest/gtest.h>

using namespace std;
using namespace kt;

TEST(KnightSolverTest, fails_invalid_route) {
  Position start_at(7, 7), end_at(8,9);
  vector<Position> route;
  KnightSolver s;
  EXPECT_FALSE(s.shortest_path(start_at, end_at, route));
}

TEST(KnightSolverTest, can_shortest_path_self) {
  Position start_at(3, 3), end_at(3,3);
  vector<Position> route;
  KnightSolver s;
  EXPECT_TRUE(s.shortest_path(start_at, end_at, route));
  EXPECT_EQ(1, route.size());
  if (!route.empty()) EXPECT_EQ(Position(3,3), route[0]);
}

TEST(KnightSolverTest, can_shortest_path_route) {
  vector<Position> route;
  KnightSolver s;
  s.shortest_path(Position(0,0), Position(1,2), route);
  EXPECT_EQ(1, route.size());
  if (!route.empty()) EXPECT_EQ(Position(1,2), route[0]);

  s.shortest_path(Position(0,7), Position(1,6), route);
  EXPECT_EQ(4, route.size());

  s.shortest_path(Position(0,0), Position(6,6), route);
  EXPECT_EQ(4, route.size());
}


