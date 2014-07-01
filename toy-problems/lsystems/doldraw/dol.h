#ifndef DOL_H
#define DOL_H

#include <string>
#include <boost/regex.hpp>
#include <boost/algorithm/string.hpp>
#include <boost/lambda/lambda.hpp>
#include <boost/lambda/bind.hpp>
#include <tr1/unordered_map>
#include <boost/function.hpp>
#include <boost/lexical_cast.hpp>
#include <tr1/memory>

using namespace boost::lambda;
using namespace std::tr1;
using namespace std;
using boost::regex; using boost::lexical_cast; using boost::cref;
using boost::ref; using boost::cmatch;

class DOL;
typedef std::tr1::unordered_map<char, std::string> ProductionMap;
typedef std::tr1::shared_ptr<DOL> DOLPtr;
class DOL
{

public:
  static regex matchProduction;

  DOL()
  {
  }

  bool init(const std::string& source)
  {
    std::vector<std::string> strs;
    boost::split(strs, source, boost::is_any_of("\n"));
    if (strs.size() < 2) return false;
    _theta = lexical_cast<float>(strs[0]);
    _axiom = strs[1];
    for_each(strs.begin()+1, strs.end(), bind(&DOL::parseLine, this, cref(_1)));
    return true;
  }

  static DOLPtr makeDOL(const string& source)
  {
    DOLPtr dol(new DOL());
    if (!dol->init(source)) return DOLPtr();
    return dol;
  }

  const ProductionMap& productions()const { return _productions; }
  const string& axiom()const { return _axiom; }
  float theta()const { return _theta; }

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
  float _theta;
  std::string _axiom;
  ProductionMap _productions;
};


std::ostream& operator<<(std::ostream& os, const DOL& rhs);




#endif // DOL_H
