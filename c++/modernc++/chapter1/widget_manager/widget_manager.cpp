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

struct Widget {};

template <class T>
struct OpNewCreator
{
  static T* create() { return new T; }
  protected:
  ~OpNewCreator() {}
};

namespace A {
template <class CreationPolicy>
  class WidgetManager : public CreationPolicy
{

};

typedef WidgetManager<OpNewCreator<Widget> > AWidgetMgr;
}

namespace B {
  template <template <class Created> class CreationPolicy>
    class WidgetManager : public CreationPolicy<Widget>
  {
  };

  typedef WidgetManager<OpNewCreator> BWidgetMgr;
}

namespace C {
  template <template <class Created> class CreationPolicy>
    class WidgetManager : public CreationPolicy<Widget>
  {
    void SwitchPrototype(Widget* newPrototype)
    {
      CreationPolicy<Widget>& myPolicy = *this;
      delete myPolicy.GetPrototype();
      myPolicy.SetPrototype(newPrototype);
    }
  };

  typedef WidgetManager<OpNewCreator> CWidgetMgr;
}



int main(int argc, char **argv)
{
  Widget* a = A::AWidgetMgr::create();

  Widget* b = B::BWidgetMgr::create();

  return 0;
}
