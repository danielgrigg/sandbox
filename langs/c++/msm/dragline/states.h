

#include <iostream>
#include <boost/msm/front/state_machine_def.hpp>

namespace msm = boost::msm;

  // The list of FSM states
  struct Look : public msm::front::state<> 
  {
    template <class Event,class FSM>
      void on_entry(Event const&,FSM& ) {std::cout << "entering: Look" << std::endl;}
    template <class Event,class FSM>
      void on_exit(Event const&,FSM& ) {std::cout << "leaving: Look" << std::endl;}
  };

  struct Fill : public msm::front::state<> 
  {
    template <class Event,class FSM>
      void on_entry(Event const&,FSM& ) {std::cout << "entering: Fill" << std::endl;}
    template <class Event,class FSM>
      void on_exit(Event const&,FSM& ) {std::cout << "leaving: Fill" << std::endl;}
  };

  struct Swing : public msm::front::state<> 
  {
    template <class Event,class FSM>
      void on_entry(Event const&,FSM& ) {std::cout << "entering: Swing" << std::endl;}
    template <class Event,class FSM>
      void on_exit(Event const&,FSM& ) {std::cout << "leaving: Swing" << std::endl;}
  };
  struct Return : public msm::front::state<> 
  {
    template <class Event,class FSM>
      void on_entry(Event const&,FSM& ) {std::cout << "entering: Return" << std::endl;}
    template <class Event,class FSM>
      void on_exit(Event const&,FSM& ) {std::cout << "leaving: Return" << std::endl;}
  };



