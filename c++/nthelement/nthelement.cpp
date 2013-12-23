// nth_element example
#include <iostream>
#include <algorithm>
#include <vector>
#include <utility>
#include <boost/lambda/lambda.hpp>
#include <boost/lambda/bind.hpp>

using namespace std;
using namespace boost::lambda;

bool mycompare(int i, int j, int x) { return i < j; }

bool myfunction (int i,int j) { return (i<j); }

int main () {
  vector<int> myvector;
  vector<int>::iterator it;

  // set some values:
  for (int i=1; i<10; i++) myvector.push_back(i);   // 1 2 3 4 5 6 7 8 9

  random_shuffle (myvector.begin(), myvector.end());

  // using default comparison (operator <):
  nth_element (myvector.begin(), myvector.begin()+5, myvector.end());

  // using function as comp
  nth_element (myvector.begin(), myvector.begin()+5, myvector.end(),myfunction);

  // print out content:
  cout << "myvector contains:";
  for (it=myvector.begin(); it!=myvector.end(); ++it)
    cout << " " << *it;

  cout << endl;

  std::pair<int, int> a = make_pair(3, 11);
  std::pair<int, int> b = make_pair(5, 8);
  std::pair<int, int> c = make_pair(7, 3);
  std::pair<int, int> z = std::min(a, min(b, c));

  cout << z.first << ", " << z.second << endl;

  random_shuffle (myvector.begin(), myvector.end());

  for (it=myvector.begin(); it!=myvector.end(); ++it)
    cout << " " << *it;
  cout << endl;

  nth_element(myvector.begin(), myvector.begin()+5, myvector.end(), 
      bind(mycompare, _1, _2, 4));

  for (it=myvector.begin(); it!=myvector.end(); ++it)
    cout << " " << *it;
  cout << endl;

  return 0;
}

