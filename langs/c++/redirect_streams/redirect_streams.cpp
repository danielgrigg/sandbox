#include <fstream>
#include <iostream>

using namespace std;

// Example of redirecting io through another stream.  Here
// we set cout's buffer to an open fstream's.

void redirect(ostream&);

int main(int argc, char **argv)
{
  cout << "first row " << endl;

  redirect(cout);

  cout << "last row" << endl;
  return 0;
}

void redirect(ostream& s) {
  ofstream file("redirect.txt");
  streambuf* old_buf = s.rdbuf();

  s.rdbuf(file.rdbuf());

  file << "one for file " << endl;
  s << "one for stream " << endl;

  s.rdbuf(old_buf);
}

