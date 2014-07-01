#include <algorithm>
#include <functional>
#include <iostream>
#include <vector>
#include <cmath>

using namespace std;

std::function<int(int)> make_scale_f(int x)  {
  return [x](int y) { return y * x; };
}

int main(int argc, char **argv)
{
  auto f1 = [](int x, int y) { return x + y; };
  cout << "f1(3, 5): " << f1(3, 5) << endl;

  auto f2 = [](float x) -> int { return floor(x); };
  cout << "f2(5.7): " << f2(5.7) << endl;

  auto f3 = [](int x) { return x * 2; };
  int (*f_ptr)(int) = f3;
  cout << "f3 as f_ptr(7): " << f_ptr(7) << endl;

  // Using the 'using' syntax for type aliasing.
  using f_ptr_type = int(*)(int);
  f_ptr_type f_ptr2 = f3;
  cout << "f_ptr2(9): " << f_ptr2(9) << endl;

  // capturing vars
  int sum = 0;
  std::vector<int> xs {1,2,3,4,5};
  std::for_each(xs.begin(), xs.end(), [&sum](int x) { sum += x; });
  cout << "sum " << sum << endl;

  // Copy to any used - Error to assign to a copy
//  std::for_each(xs.begin(), xs.end(), [=](int x) { sum += x; });

  int y = 7;
  std::for_each(xs.begin(), xs.end(), [&, y](int x) { sum += x + y; });
  cout << "sum " << sum << endl;

  // Can capture this too but cbf

  // Are closures possible...
  auto scale3_f = make_scale_f(3);
  cout << "scale3_f(9): " << scale3_f(9) << endl;
  // yup!
  //
  cout << "enter digits\n";
  std::transform(istream_iterator<int>(cin),
                  istream_iterator<int>(),
                  ostream_iterator<int>(cout, "\n"),
                  [](int x) { return x * x; });
  
  return 0;
}
