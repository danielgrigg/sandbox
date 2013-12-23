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

#include <queue>

using namespace std;

struct Message {
  int dispatch_time;
  string contents;
  Message(){}
  Message(int d, string c): dispatch_time(d), contents(c){}

};

bool operator<(const Message& lhs, const Message& rhs) {
  return lhs.dispatch_time < rhs.dispatch_time; 
}

bool operator>(const Message& lhs, const Message& rhs) {
  return lhs.dispatch_time > rhs.dispatch_time; 
}
bool mycompare(const Message& lhs, const Message& rhs) {
  return lhs.dispatch_time < rhs.dispatch_time; 
}

struct Message2 {
  string contents;
};

int myrand() { return random() % 100; }

int main(int argc, char **argv)
{
  int keys[10];
  std::generate(keys, keys+10, myrand);
  std::ostream_iterator<int> osi(cout, " ");
  cout << "keys: "; copy(keys, keys+10, osi); cout << "\n";
  priority_queue<int> first;
  for (int i = 0; i < 10; ++i) first.push(keys[i]);
  cout << "delivered: ";
  for (int i = 0; i < 10; ++i){
    cout << first.top() << " ";
    first.pop();
  }
  cout << endl;

  Message messages[5];
  messages[0] = Message(2200, "quick fox in 2200ms");
  messages[1] = Message(500, "brown fox in 500ms");
  messages[2] = Message(0, "killer fox in 0ms");
  messages[3] = Message(0, "slow fox in 0ms");
  messages[4] = Message(7000, "happy fox in 7000ms");

  priority_queue<Message, vector<Message>, greater<Message> > second;
  for (int i = 0; i < 5; ++i) second.push(messages[i]);
  cout << "delivered: ";
  for (int i = 0; i < 5; ++i){
    cout << "(" << second.top().dispatch_time << "): " << second.top().contents << "\n";
    second.pop();
  }
  cout << endl;

  typedef std::pair<int, string> MessageAt;
  string messages2[5];
  messages2[0] = string("quick fox in 2200ms");
  messages2[1] = string("brown fox in 500ms");
  messages2[2] = string("killer fox in 0ms");
  messages2[3] = string("slow fox in 0ms");
  messages2[4] = string("happy fox in 7000ms");
  int times[5] = {2200, 500, 0, 0, 7000};

  priority_queue<MessageAt, vector<MessageAt>, greater<MessageAt> > third;
  for (int i = 0; i < 5; ++i) third.push(make_pair(times[i], messages2[i]));
  cout << "delivered: ";
  for (int i = 0; i < 5; ++i){
    cout << "(" << third.top().first << "): " << third.top().second << "\n";
    third.pop();
  }
  cout << endl;


  return 0;
}
