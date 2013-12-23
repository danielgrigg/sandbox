enum class Type { one, two, three };
struct A {
  A(unsigned int a):_a(a) {}
  unsigned int _a;
};

int main() {
  A x = Type::one;
  return 0;
}
