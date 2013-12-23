#include <boost/algorithm/string.hpp>
using namespace std;
using namespace boost;


void doFinder()
{
  string str1("abc-*-ABC-*-aBc");
  // Find all 'abc' substrings (ignoring the case)
  // Create a find_iterator
  typedef find_iterator<string::iterator> string_find_iterator;
  for(string_find_iterator It=
      make_find_iterator(str1, first_finder("abc", is_iequal()));
      It!=string_find_iterator();
      ++It)
  {
    cout << copy_range<std::string>(*It) << endl;
  }

  // Output will be:
  // abc
  // ABC
  // aBC

  typedef split_iterator<string::iterator> string_split_iterator;
  for(string_split_iterator It=
      make_split_iterator(str1, first_finder("-*-", is_iequal()));
      It!=string_split_iterator();
      ++It)
  {
    cout << copy_range<std::string>(*It) << endl;
  }

  // Output will be:
  // abc
  // ABC
  // aBC


}

int main(int argc, char **argv)
{

  string str1(" hello world! ");
  to_upper(str1);  // str1 == " HELLO WORLD! "
  trim(str1);      // str1 == "HELLO WORLD!"

  string str2=
    to_lower_copy(
        ireplace_first_copy(
          str1,"hello","goodbye")); // str2 == "goodbye world!"
  cout << "str2 " << str2 << endl;

  string str3 = "Cubic_20__Grp1";

  str3 = str3.substr(0, str3.rfind("__Grp"));
  cout << "str3 " << str3 << endl;

  string str4 = "the quick brown fox jumped over the lazy dog";

  replace_all(str4, " ", "_");
  cout << str4 << endl;

//  doFinder();

  string opta = "swing_time=123";

  //split_iterator<string::iterator> it = 
  //  make_split_iterator(opta, first_finder("="));
//  cout << "a: " << *it << endl;

  iterator_range<char*> result = find_first("="
  char text[]="hello dolly!";
  iterator_range<char*> result=find_last(text,"ll");

  transform( result.begin(), result.end(), result.begin(), bind2nd(plus<char>(), 1) );
  // text = "hello dommy!"            

  to_upper(result); // text == "hello doMMy!"

  // iterator_range is convertible to bool
  if(find_first(text, "dolly"))
  {
    cout << "Dolly is there" << endl;
  }

}

