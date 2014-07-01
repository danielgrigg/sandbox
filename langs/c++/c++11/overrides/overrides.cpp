#include <iostream>

using namespace std;

struct Base {
  virtual void f(float) {}

  virtual void bar(void) final { cout << "x" << endl; }
};

struct Derived : Base {
  //  virtual void f(int) override {} // Wrong!
  virtual void f(float) override {}  // Better!

  // virtual void bar() { cout << "y"; } // Won't compile!
};

struct Base2 final { };

// Won't compile, marked final!
//struct Derived2 : public Base2 {};

int main(int argc, char **argv) {

  int* x = nullptr;

  return 0;
}
