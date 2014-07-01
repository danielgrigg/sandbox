#include "dol.h"

regex DOL::matchProduction("(\\w)\\s*->\\s*(.+)\\s*$");

std::ostream& operator<<(std::ostream& os, const DOL& rhs)
{
  os << "theta: " << rhs.theta() << '\n';
  os << "axiom: " << rhs.axiom() << '\n';
  os << "productions:\n";
  for_each(rhs.productions().begin(), rhs.productions().end(),
           os << bind(&ProductionMap::value_type::first, _1) << ": "
           << bind(&ProductionMap::value_type::second, _1) << '\n');
  return os;
}
