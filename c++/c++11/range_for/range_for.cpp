#include <iostream>
#include <vector>
#include <list>
#include <map>

using namespace std;

int main(int argc, char **argv)
{
  int my_array[5] = {1, 2, 3, 5, 7};
  for (int &x : my_array) {
    x *= 2;
  }

  std::copy(my_array, my_array+5, std::ostream_iterator<int>(cout, "\n"));

  std::list<int> xs {-1, -2, -3, -5, -7};
  for (auto &x : xs) { cout << 3 * x << endl; }

  std::map<string, string> kvs { {"a", "1"} , {"b", "2"} , {"c", "3"}};

  for (auto &kv : kvs) {
    cout << kv.first << ", " << kv.second << endl;
  }

  return 0;
}
