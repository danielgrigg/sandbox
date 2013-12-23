#include "position.h"
#include <gtest/gtest.h>

using namespace std;
using namespace kt;

TEST(PositionTest, can_manipulate) {
  Position p(3, 5);
  EXPECT_EQ(p[0], 3);
  EXPECT_EQ(p[1], 5);
  EXPECT_EQ(p + Position(2,1), Position(5, 6));
}

TEST(PositionTest, can_validate_for_chess) {
  Position a(9,4);
  EXPECT_FALSE(a.valid_position());
  Position b(-1,2);
  EXPECT_FALSE(b.valid_position());
  Position c(3,7);
  EXPECT_TRUE(c.valid_position());
}

TEST(PositionTest, can_write_chess_notation) {
  Position a(2,3);
  EXPECT_EQ("C4", a.chess_notation());
  Position b(7,7);
  EXPECT_EQ("H8", b.chess_notation());
  Position c(0,0);
  EXPECT_EQ("A1", c.chess_notation());
}

