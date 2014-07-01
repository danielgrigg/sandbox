#include <iostream>

namespace long_namespace_name
{
  const int x = 5;
  namespace child_namespace
  {
    const int y = 7;
  }
}

namespace other_namespace
{
  const int x = 9;
}

namespace a = long_namespace_name;
namespace b = a::child_namespace;

int main(int argc, char **argv)
{
  using namespace std;

  //cout << "x: " << x << endl; // not in scope
  cout << "long_namespace_name::x: " << long_namespace_name::x << endl;
  cout << "a::x: " << long_namespace_name::x << endl;


  // cout << "y: " << y << endl; // not in scope
  cout << "y: " << long_namespace_name::child_namespace::y << endl;
  //  cout << "a::y: " << a::y << endl; // not in scope, awkward error message from GCC though.
  cout << "a::b::y " << b::y << endl;

  {
    using other_namespace::x;
    cout << "x: " << x << endl;
  }

  using a::x;
  cout << "x: " << x << endl;
  return 0;
}
