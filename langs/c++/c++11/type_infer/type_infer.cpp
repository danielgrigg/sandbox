#include <iostream>
#include <vector>
#include <boost/bind.hpp>

using namespace std;

int foo(int x, int y, int z) {
  return x * y * z;
}

int main(int argc, char **argv)
{
  auto some_type = boost::bind(foo, _2, _1, 7);
  auto my_var = 88.3f;

  cout << "calling some_type " << some_type(3, 4) << endl;

  decltype(my_var) another_var = 23.32f;

  cout << "another_var " << another_var << endl;

  // Ok initializer lists are pretty cooool
  std::vector<std::pair<int,int>> pair_vector 
    {make_pair(1,2), make_pair(3,5), make_pair(7, 11)};

  for (auto it = pair_vector.begin(); it != pair_vector.end(); ++it) {
    cout << "first " << it->first << ", second " << it->second << endl;
  }

  return 0;
}
