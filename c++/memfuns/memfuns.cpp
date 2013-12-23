#include <algorithm>
#include <fstream>
#include <functional>
#include <iomanip>
#include <iostream>
#include <iterator>
#include <map>
#include <numeric>
#include <sstream>
#include <stdint.h>
#include <string>
#include <vector>

using namespace std;

class A { 
public:
    virtual void foo() { cout << "A::foo\n"; }
};
class B : public A {
public:
    virtual void foo() { cout << "B::foo\n"; }
};

class C {
  public:
  C() { _a = 5; }

  int bar()const { return _a; }

  private:
    int _a;
};

class D : private C 
{
  public:
  void foo() {
    std::cout << "a " << bar() << endl;
  }
};

class E : public D 
{
};

int main()
{
    void (A::*bar)() = &A::foo;
    (A().*bar)();
    (B().*bar)();

    D().foo();
    E().foo();
    E().bar();

    return 0;
}

