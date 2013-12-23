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

namespace t1 {
class A { 
  public:
    void foo() { cout << "A foo\n"; }
    int a;
};

class BB : public A {
  public:
    int bb;
    void foo() { cout << "BB foo\n"; }
};

class CC : public A {
  public:
    int cc;
//    void foo() { cout << "CC foo\n"; }
};

class DDD : public CC, public BB {
  public:
    int ddd;
  //  void foo() { cout << "DDD foo\n"; }
};
} 

namespace t2 {

class A { 
  public:
    int a;
    void foo() { cout << "A foo\n"; }
};

class BB : virtual public A {
  public:
    int bb;
    void foo() { cout << "BB foo\n"; }
};

class CC : virtual public A {
  public:
    int cc;
//    void foo() { cout << "CC foo\n"; }
};

class DDD : public CC, public BB {
  public:
    int ddd;
  //  void foo() { cout << "DDD foo\n"; }
};

}

namespace t3 {

class A { 
  public:
    int a;
    virtual void foo() { cout << "A foo\n"; }
};

class BB : virtual public A {
  public:
    int bb;
    virtual void foo() { cout << "BB foo\n"; }
};

class CC : virtual public A {
  public:
    int cc;
//    void foo() { cout << "CC foo\n"; }
};

class DDD : public CC, public BB {
  public:
    int ddd;
    virtual void foo() { cout << "DDD foo\n"; }
};

}

int main(int argc, char **argv)
{
  {
  using namespace t1;
  cout << "A: "; A().foo();
  cout << "BB: "; BB().foo();
  cout << "CC: "; CC().foo();
  cout << "DDD: "; DDD().BB::foo();
  cout << "sizeof(DDD): " << sizeof(DDD) << "\n";
  }

  {
  using namespace t2;
  cout << "A: "; A().foo();
  cout << "BB: "; BB().foo();
  cout << "CC: "; CC().foo();
  cout << "DDD: "; DDD().BB::foo();

  cout << "sizeof(DDD): " << sizeof(DDD) << "\n";
  }

  {
  using namespace t3;
  cout << "A: "; (new A())->foo();
  cout << "BB: "; (new BB())->foo();
  cout << "CC: "; (new CC())->foo();
  cout << "DDD: "; (new DDD())->BB::foo();

  cout << "sizeof(DDD): " << sizeof(*(new DDD())) << "\n";
  }


  cout << endl;
  return 0;
}
