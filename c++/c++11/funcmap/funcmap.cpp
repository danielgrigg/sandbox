#include <functional>
#include <iostream>
#include <map>
#include <string>
#include <vector>

using namespace std;

const std::string POSITION = "v";
const std::string NORMAL = "vn";
const std::string UV = "vt";
const std::string GROUP = "g";
const std::string MTL = "mtllib";
const std::string USE_MTL = "usemtl";
const std::string FACE = "f";

void commentfn() { cout << "comment called" << endl; }
void vertexfn() { cout << "vertex called " << endl; }

int main(int argc, char **argv)
{
  map<string, std::function<void(void)>> fn_map =
  { {"#", commentfn} };

  fn_map["v"] = vertexfn;
  fn_map["g"] = []() { cout << "group called" << endl; };

  fn_map["#"]();
  fn_map["g"]();
  return 0;
}
