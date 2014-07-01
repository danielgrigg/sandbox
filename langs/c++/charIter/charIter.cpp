#include <iostream>
#include <algorithm>
#include <sstream>
#include <functional>

using namespace std;

void writeChar(char value, ostringstream os)
{
  os << (int)value << " ";
}

struct WriteIntOp : public binary_function<int, ostringstream, void>
{
  void operator()(int value, ostringstream os)
  {
    os << (int)value << " ";
  }
} WriteIntOpObject;

void writeInt(int value, string os)
{
  ostringstream sstr;
  sstr << value;
  os += sstr.str();
}

int main()
{
  //char myChars[]
  int myInts[] = {1, 2, 3, 4, 5};
  random_shuffle(myInts, myInts + 5);

  ostringstream s;
  string myString;
  for_each(myInts, myInts+5, bind2nd(ptr_fun(writeInt), myString));  

  cout << "MyInts: " << s << "\n.";
  return 0;
}
