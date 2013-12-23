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
#include <boost/regex.hpp> 
#include <boost/algorithm/string.hpp>
#include <boost/lambda/lambda.hpp>
#include <boost/lambda/bind.hpp>
#include <tr1/unordered_map>
#include <boost/function.hpp>

using namespace std;
using namespace boost; 
using namespace boost::lambda; 
using namespace std::tr1;

typedef unordered_map<char, string> ProductionMap;
class DOL
{
  public:
    static regex matchProduction;

    DOL(const std::string& source)
    {
      std::vector<std::string> strs;
      boost::split(strs, source, boost::is_any_of("\n"));
      _axiom = strs[0];
      for_each(strs.begin()+1, strs.end(), bind(&DOL::parseLine, this, cref(_1)));
    }

    const ProductionMap& productions()const { return _productions; }
    const string& axiom()const { return _axiom; }

    std::string& step(std::string& str)const
    {
      string result;
      for (std::string::const_iterator i = str.begin(); 
          i != str.end(); ++i)
      {
        ProductionMap::const_iterator p = _productions.find(*i);
        if (p != _productions.end())
        {
          result += p->second;
        }
        else
        {
          result += *i;
        }
      }
      str = result;
      return str;
    }

    private:
    void parseLine(const std::string& line)
    {
      cmatch what;
      if(regex_match(line.c_str(), what, matchProduction)) 
      {
        string succ(what[2]);
        char c = *(what[1].first);
        _productions[c] = succ;
      }
    }
    std::string _axiom;
    ProductionMap _productions;
};

regex DOL::matchProduction("(\\w)\\s*->\\s*(.+)\\s*$");

std::ostream& operator<<(std::ostream& os, const DOL& rhs)
{
  os << "axiom: " << rhs.axiom() << '\n';
  os << "productions:\n";
  for_each(rhs.productions().begin(), rhs.productions().end(),
      os << bind(&ProductionMap::value_type::first, _1) << ": " 
      << bind(&ProductionMap::value_type::second, _1) << '\n');
  return os;
}


class Turtle;
typedef boost::function<void(Turtle*)> Action;
typedef std::tr1::unordered_map<char, Action> ActionMap;
class Turtle
{
  public:
    Turtle()
    {
      _actions['F'] = &Turtle::draw;
      _actions['f'] = &Turtle::forward;
      _actions['+'] = &Turtle::left;
      _actions['-'] = &Turtle::right;
    }

    void render(const std::string& program)
    {
      for_each(program.begin(), program.end(),
        bind(&Turtle::doAction, this, _1));
    }
  private:
    void doAction(const char a) { _actions[a](this); }
    void draw() { cout << "draw "; }
    void forward() { cout << "forward "; }
    void left() { cout << "left "; }
    void right() { cout << "right "; }

    ActionMap _actions; 
};

int main(int argc, char **argv)
{
  string ts = "F-F-F-F\n"
    "F -> F-F+F+FF-F-F+F\n";

  DOL dol(ts);
  cout << "dol: " << dol << endl;

  string q(dol.axiom());
  Turtle turtle;
  for (int i = 0; i < 1; ++i)
  {
    cout << dol.step(q) << endl;
    turtle.render(q);
  }
  return 0;
}

