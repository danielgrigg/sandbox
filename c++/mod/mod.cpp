
#include <iostream>
#include <vector>
#include <math.h>

using namespace std;

inline int mod(int a, int b)                                                                  
{
  int n = int(a / b);
  a -= n * b;
  if (a < 0) a += b;
  return a;
}


int main()
{
  for (int i = 0; i < 10; ++i) cout << -i << " % 6 = " << -i % 6 << endl;
  for (int i = 0; i < 10; ++i) cout << -i << " % -6 = " << -i % -6 << endl;
  for (int i = 0; i < 10; ++i) cout << -i << " fmod 6 = " << fmod(-i,6.0) << endl;
  for (int i = 0; i < 10; ++i) cout << -i << " fmod -6 = " << fmod(-i,-6.0) << endl;

  for (int i = 0; i < 10; ++i) cout << -i << " mod 6 = " << mod(i,6) << endl;
  for (int i = 0; i < 10; ++i) cout << -i << " mod 6 = " << mod(i,-6) << endl;
  for (int i = 0; i < 10; ++i) cout << -i << " mod 6 = " << mod(-i,6) << endl;
  for (int i = 0; i < 10; ++i) cout << -i << " mod -6 = " << mod(-i,-6) << endl;

  return 0;
}

