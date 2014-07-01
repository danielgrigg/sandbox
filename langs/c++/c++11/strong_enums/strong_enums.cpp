#include <iostream>

using namespace std;

enum class Color {
  Red = 11,
  Green = 22,
  Blue = 33
};

enum class Day : unsigned short {
  Monday,
  Wednesday,
  Friday
};


int main(int argc, char **argv)
{
  Color c = Color::Red;


  // c = Day::Monday; // Strongly typed, won't compile!
  return 0;
}
