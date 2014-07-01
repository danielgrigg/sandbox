
#include <iostream>
#include <boost/algorithm/string/predicate.hpp>
#include <boost/bind.hpp>
#include <vector>
#include <string>

using namespace std;

typedef pair<string,string> file_info;

  bool nocase_compare(const file_info &lhs, const file_info &rhs)
  {
      return boost::algorithm::ilexicographical_compare(lhs.second,rhs.second);
  }

int main()
{
 vector<file_info> file_infos;
 file_infos.push_back(file_info("a1", "c"));
 file_infos.push_back(file_info("b2", "b"));
 file_infos.push_back(file_info("c3", "a"));

 sort(file_infos.begin(), file_infos.end(),                                              
      boost::bind(
        boost::algorithm::ilexicographical_compare<string, string>,
        boost::bind(&file_info::second, _1), 
        boost::bind(&file_info::second, _2)));
/
 sort(file_infos.begin(), file_infos.end(),nocase_compare);

  for (vector<file_info>::const_iterator i = file_infos.begin();
        i != file_infos.end();
        i++)
  {
    cout << i->first << ", " << i->second << " ";
  } 
  cout << endl;
 return 0;
}

