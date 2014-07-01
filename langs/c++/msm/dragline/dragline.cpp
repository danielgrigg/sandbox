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

// back-end
#include <boost/msm/back/state_machine.hpp>

//front-end
#include <boost/msm/front/state_machine_def.hpp>

#include <boost/msm/front/functor_row.hpp>
//#include <boost/msm/front/euml/common.hpp>
// for And_ operator
//#include <boost/msm/front/euml/operator.hpp>
#include "states.h"

namespace msm = boost::msm;
namespace mpl = boost::mpl;
using namespace msm::front;

using namespace std;

// events
struct atDump {};
struct atFill {};
struct filling {};
struct filled {};

// front-end: define the FSM structure 
struct Cycle_ : public msm::front::state_machine_def<Cycle_>
{
  uint32_t _startSwingCounter;
  uint32_t _startReturnCounter;
  uint32_t _startLookCounter;
  uint32_t _startFillCounter;

  Cycle_():
    _startSwingCounter(0),
    _startReturnCounter(0),
    _startLookCounter(0),
    _startFillCounter(0)
  {}

  template <class Event,class FSM>
    void on_entry(Event const& ,FSM&) 
    {
      std::cout << "entering: Cycle" << std::endl;
    }
  template <class Event,class FSM>
    void on_exit(Event const&,FSM& ) 
    {
      std::cout << "leaving: Cycle" << std::endl;
    }

  // the initial state . Must be defined
  typedef Fill initial_state;

  struct StartSwing
  {
    template <typename EVT, typename FSM, typename SourceState, typename TargetState>
      void operator()(EVT const&, FSM& fsm, SourceState&, TargetState&)
      {
        ++fsm._startSwingCounter;
      }
  };

  struct StartReturn
  {
    template <typename EVT, typename FSM, typename SourceState, typename TargetState>
      void operator()(EVT const&, FSM& fsm, SourceState&, TargetState&)
      {
        ++fsm._startReturnCounter;
      }
  };

  struct StartFill
  {
    template <typename EVT, typename FSM, typename SourceState, typename TargetState>
      void operator()(EVT const&, FSM& fsm, SourceState&, TargetState&)
      {
        ++fsm._startFillCounter;
      }
  };
  struct StartLook
  {
    template <typename EVT, typename FSM, typename SourceState, typename TargetState>
      void operator()(EVT const&, FSM& fsm, SourceState&, TargetState&)
      {
        ++fsm._startLookCounter;
      }
  };

  typedef Cycle_ c; // makes transition table cleaner

  struct fill_timedout
  {
    template <class EVT, class FSM, class SourceState, class TargetState>
      bool operator()(EVT const& evt ,FSM& fsm,SourceState& ,TargetState& )
      {
        bool hasTimedOut = (random() % 100) > 80;
        if (hasTimedOut) std::cout << "timed out ..." << std::endl;
        return hasTimedOut;
      }
  };

  struct transition_table : mpl::vector<
    Row<Fill, filled, Return, StartReturn, none>,
    Row<Fill, none, Look, none, fill_timedout>,
    Row<Fill, atDump, Fill, none, none>,
    Row<Fill, atFill, Fill, none, none>,
    Row<Fill, filling, Fill, none, none>,

    Row<Return, atDump, Swing, StartSwing, none>,
    Row<Return, atFill, Return, none, none>,
    Row<Return, filling, Return, none, none>,
    Row<Return, filled, Return, none, none>,
    Row<Return, none, Fill, none, fill_timedout>,

    Row<Swing, atFill, Look, StartLook, none>,
    Row<Swing, filling, Swing, none, none>,
    Row<Swing, filled, Swing, none, none>,
    Row<Swing, atDump, Swing, none, none>,
    Row<Swing, none, Return, none, fill_timedout>,

    Row<Look, filling, Fill, StartFill, none>,
    Row<Look, filled, Look, none, none>,
    Row<Look, atDump, Look, none, none>,
    Row<Look, atFill, Look, none, none> ,
    Row<Look, none, Swing, none, fill_timedout>
      > {};

  // Replaces the default no-transition response.
  /*
  template <class FSM,class Event>
    void no_transition(Event const&, FSM&,int)
    {
      throw "no_transition called!";
    }                
    */
};

// Pick a back-end                                                                                            
typedef msm::back::state_machine<Cycle_> Cycle;

int main(int argc, char **argv)
{
  try
  {
  Cycle _c;
  _c.start();
  for (int i = 0; i < 5; ++i)
  {
    _c.process_event(filled());
    _c.process_event(atDump());
    _c.process_event(atFill());
    _c.process_event(filling());
  }

  }
  catch (const char* c)
  {
    std::cerr << c << std::endl;
  }

  return 0;
}
