#include <iostream>
#include <vector>
#include "obj_model.h"

using namespace lap;
using namespace std;

int main(int argc, char **argv)
{
  if (argc < 2) return 1;

  string s = "/Users/daniel/assets/models/test/";
  auto m = obj_model(s + argv[1] + ".obj");
  cout << "Model #positions " << m->_positions.size() << endl;
  
  return 0;
}
