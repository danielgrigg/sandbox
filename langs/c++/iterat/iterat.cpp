// iterator example
#include <iostream>
#include <iterator>
using namespace std;

class myiterator : public iterator<input_iterator_tag, int>
{
  int* p;
public:
  myiterator(int* x) :p(x) {}
  myiterator(const myiterator& mit) : p(mit.p) {}
  myiterator& operator++() {++p;return *this;}
  myiterator operator++(int) {myiterator tmp(*this); operator++(); return tmp;}
  bool operator==(const myiterator& rhs) {return p==rhs.p;}
  bool operator!=(const myiterator& rhs) {return p!=rhs.p;}
  int& operator*() {return *p;}
};

int main () {
  int numbers[]={10,20,30,40,50};
  myiterator beginning(numbers);
  myiterator end(numbers+5);
  for (myiterator it=beginning; it!=end; it++)
    cout << *it << " ";
  cout << endl;

  return 0;
}
