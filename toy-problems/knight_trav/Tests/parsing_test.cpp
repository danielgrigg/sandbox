#include "io.h"
#include <gtest/gtest.h>
#include <string>

using namespace std;
using namespace kt;

TEST(ParsingTest, accepts_valid_chess_position) {
  string a("a1"), b("h8"), c("c4");
  Position p;
  EXPECT_TRUE(parse_chess_position(a, p));
  EXPECT_TRUE(parse_chess_position(b, p));
  EXPECT_TRUE(parse_chess_position(c, p));
}

TEST(ParsingTest, rejects_invalid_chess_position) {
  string a("a0"), b("hh"), c("i4"), d("22"), e("a 1"), f("3b");
  Position p;
  EXPECT_FALSE(parse_chess_position(a, p));
  EXPECT_FALSE(parse_chess_position(b, p));
  EXPECT_FALSE(parse_chess_position(c, p));
}


TEST(ParsingTest, can_parse_chess_position) {
  string a("a1"), b("h8"), c("c4"), d("g5");
  Position pa, pb, pc, pd;
  parse_chess_position(a, pa);
  parse_chess_position(b, pb);
  parse_chess_position(c, pc);
  parse_chess_position(d, pd);

  EXPECT_EQ(pa, Position(0, 0));
  EXPECT_EQ(pb, Position(7, 7));
  EXPECT_EQ(pc, Position(2, 3));
  EXPECT_EQ(pd, Position(6, 4));
}

