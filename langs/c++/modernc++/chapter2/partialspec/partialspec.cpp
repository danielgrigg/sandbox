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

class MyController;
class ModalDialog;
template <class T> struct Button {};

template <class Window, class Controller>
class Widget 
{

};

template <class Window>
class Widget<Window, MyController> { 
// my imp..
 };

template <class ButtonArg>
class Widget<Button<ButtonArg> , MyController>
{
};

int main(int argc, char **argv)
{
  cout << "hello world" << endl;
  return 0;
}
