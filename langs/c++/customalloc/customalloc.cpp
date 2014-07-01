#include "customalloc.h"

using namespace std;

template <typename T>
T& ShrinkToFit(T& container)
{
  T(container.begin(), container.end()).swap(container);
  return container;
}

int main(int argc, char **argv)
{
  typedef vector<uint32_t, SharedMemoryAllocator<uint32_t> > SharedUint32Vec;

  {
    std::cout << "starting\n";
    SharedUint32Vec v;
    std::cout << "created container\n";
    v.push_back(2);
    std::cout << "pushed_back\n\n";
    v.push_back(5);
    std::cout << "pushed_back\n\n";
    v.push_back(7);
    std::cout << "pushed_back\n\n";
    v.push_back(9);
    std::cout << "pushed_back\n\n";
    v.push_back(11);
    std::cout << "pushed_back\n\n";
    v.resize(10);
    std::cout << "resized bigger\n\n";
    v.resize(3);
    std::cout << "resized smaller\n\n";
    v.reserve(20);
    std::cout << "reserved larger\n\n";
    std::cout << "done\n";
  }

  int a[] = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
  vector<int> v(a, a+10);
  cout << "v capacity: " << v.capacity() << endl;
  v.reserve(100);
  cout << "v capacity: " << v.capacity() << endl;
  vector<int>(v.begin(), v.end()).swap(v);
  cout << "v capacity: " << v.capacity() << endl;

  v.reserve(50);
  cout << "v capacity: " << v.capacity() << endl;
  ShrinkToFit(v);
  cout << "v capacity: " << v.capacity() << endl;
  return 0;
}
