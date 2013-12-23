#include <algorithm>
#include <fstream>
#include <iostream>
#include <iterator>
#include <string>
#include <vector>

using namespace std;

typedef std::pair<std::string, std::string> TestExpect;

// Since I don't seem to have an opportunity to ask questions to disambiguate, I'm 
// making a large number of assumptions. Which I don't like..
// The 'process' must be followed.  That is, each step must be performed in order.
// An 'invalid URL' refers to a non-existent URL, not a badly-formed URL.
// Only http(s) protocol is used or other protocols are also removed, ie ftp:// -> "".
// The test output is correct.  Thus forcing step 4 to actually mean 'remove leading 'www.' strings.
// Big ambiguity on step 6, removing dot segments. The test input contradicts the example,
//    and assuming the output is ok, means either the example is backwards or we need an extra step
//      to remove anything say, adjacent to a '../', or remove up to the end of a dot segment, etc.
//      Removing the previous token is the most 'sensible' choice, since it's redundant, so we'll do that.
//      This implies we drop n 'directory changes' for n dot-segments. Also assume dir-changes end
//      at the semi-querystring (duh).
// 'Trailing /' means immediately prior the query string
// Well we come this far and it seems we 'don't' want to process in the order given,
//   since removing anything query-string and beyond eliminates any fragments....But this 
//   has taken an hour longer to code than I expected beyond the 1h given so I'll just append That
//   deletion and be done.
// Although the directory index requirement specifies a strict set of file extensions,
//   I'll assume we want to remove any extension.  Otherwise our routine would have a regression
//   when we run against some new extension...


// Convert a unreserved octet to a character.  Ascii..no requirement given for wide-chars.
inline char decode_unreserved(unsigned int x) {

  // More efficient approach would be a lookup table but this is much smaller to specify.
  if ((x >= 0x41 && x <= 0x5a) ||
      (x >= 0x61 && x <= 0x7a) ||
      (x >= 0x30 && x <= 0x39) ||
      x == 0x2D ||
      x == 0x2E ||
      x == 0x5F ||
      x == 0x7E) {
    return static_cast<char>(x);
  } else {
    return 0;
  }
}

// The magical function to clean a URL.  This is why regex libraries exist ;)
// Solution in clojure would be nicer as well.
std::string clean_url(const std::string& in) {

  std::string s;

  std::transform(in.begin(), in.end(), std::back_inserter(s), ::tolower);

  // Chop of the entire trailing query-string.
  size_t semi = s.rfind(';');
  if (semi != std::string::npos) {
    s.erase(semi);
  }

  // Pressed for time, so instead of some elegant application of the STL,
  // we'll resort to for loops :O
  for (size_t octet_pos = s.rfind('%'); 
    octet_pos != std::string::npos;
    octet_pos = s.rfind('%', octet_pos - 1)) {

    // Good old sscanf, fastest lexical scanner in the west.
    unsigned int octet_value;
    sscanf(s.data() + octet_pos + 1, "%x", &octet_value);
    char decoded = decode_unreserved(octet_value);
    if (decoded != 0) { 
      s.replace(octet_pos, 3, 1, decoded);
    }

  }

  s.erase(0, s.find("://") + 3);
  
  const std::string www_prefix = "www.";

  // Can't use find_end as we need to keep non-leading matches.
  while (s.substr(0, www_prefix.size()) == www_prefix) {
    s.erase(0, www_prefix.size());
  }

  const std::string default_port = ":80";

  size_t end_at = s.find('/');
  if (end_at == std::string::npos) {
    end_at = s.length();
  }
  if (s.substr(end_at - default_port.size(), default_port.size()) == default_port) {
    s.erase(end_at - default_port.size(), default_port.size());
  }

  const std::string dot_segment_token = "/../";
  for (size_t dot_segment = s.rfind(dot_segment_token);
      dot_segment != std::string::npos;
      dot_segment = s.rfind(dot_segment_token)) {

    size_t start_at = s.rfind('/', dot_segment - 1);
    if (start_at != std::string::npos) {
      size_t n_erase =  dot_segment + dot_segment_token.size() - start_at;
      s.erase(start_at, n_erase);
    }
  }

  if (!s.empty() && s[s.length() - 1] == '/') {
    s.erase(s.length() - 1);
  }

  // Remove directory indices
  size_t dir_index_start = s.rfind('/');
  std::string dir_index = s.substr(dir_index_start + 1);
  if (dir_index.find("index") != std::string::npos ||
      dir_index.find("default") != std::string::npos) {
    s.erase(dir_index_start);
  }

  size_t n = s.rfind("//");
  while (n != std::string::npos) {
    s.erase(n, 1);
    n = s.rfind("//", n - 1);
  }

  return s;
}


/* We 'can' use external libs for testing, but pulling in gtest etc means a lot of 
 * work on the cmake, so do an adhoc test suite.
 */
void run_test(const TestExpect& te) {
  std::cout << "Testing " << te.first << "...";
  std::string result = clean_url(te.first);

  if (te.second == result) {
    std::cout << "OK\n";
  } else {
    std::cout << "FAILED\n\nExpected " << te.second << ", got " << result << "\n";
  }
}

int main(int argc, char **argv)
{
  
  // No requirement for specifying test data file so fix it...
  std::fstream fs;
  fs.open("./test_data.txt");
  if (!fs.is_open()) {
    std::cerr << "Unable to find ./test_data.txt!" << std::endl;
    return 1;
  }

  std::vector<TestExpect > test_expect;
  std::istream_iterator<std::string> eos;

  for (std::istream_iterator<std::string> it(fs); it != eos; ++it) {
    std::string test_str = *it;
    ++it;
    if (it == eos ) { 
      std::cerr << "Input must consist of TEST\tRESULT pairs!" << std::endl;
      return 1;
    }
    std::string expect_str = *it;
    
    test_expect.push_back(std::make_pair(test_str, expect_str));
  }

  for (int i = 0; i < test_expect.size(); ++i ) {
    std::cout << "test: " << test_expect[i].first << "\n";
    std::cout << "expect: " << test_expect[i].second << "\n";
  }

  // No requirement for verifying results of running all tests, so just run them.

  std::for_each(test_expect.begin(), test_expect.end(), run_test);


  return 0;
}
