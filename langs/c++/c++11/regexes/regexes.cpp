#include <iostream>
#include <string>
#include <vector>
#include <regex>

using namespace std;

int main(int argc, char **argv)
{
  cout << "regexes started" << endl;

  std::string text = "Quick brown fox.";
  // tokenization (non-matched fragments)
  // Note that regex is matched only two times: when the third value is obtained
  // the iterator is a suffix iterator.
  std::regex ws_re("\\s+"); // whitespace
  auto iter_one = std::sregex_token_iterator(text.begin(), text.end(), ws_re, -1);
  ++iter_one; cout << "*iter_one = " << *iter_one << endl;
  ++iter_one; cout << "*iter_one = " << *iter_one << endl;
  ++iter_one; cout << "*iter_one = " << *iter_one << endl;
  ++iter_one; cout << "*iter_one = " << *iter_one << endl;
  ++iter_one; cout << "*iter_one = " << *iter_one << endl;
  ++iter_one; cout << "*iter_one = " << *iter_one << endl;

  return 0;

  std::copy( std::sregex_token_iterator(text.begin(), text.end(), ws_re, -1),
      std::sregex_token_iterator(),
      std::ostream_iterator<std::string>(std::cout, "\n"));

  auto iter = std::sregex_token_iterator(text.begin(), text.end(), ws_re, -1);
  if (iter != std::sregex_token_iterator() ) {
    cout << "Not at end" << endl;
  }

  return 0;
}
