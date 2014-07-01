#include <iterator>
#include <algorithm>
#include <iostream>
#include <string>
#include <vector>
#include <thread>
#include <chrono>
#include <atomic>
#include <future>
#include <sstream>
#include <queue>

using namespace std;

// Playing with c++11 threading facilities, including threads,
// condition variables and futures.

std::atomic<bool> g_done;
std::queue<std::string> g_messages;
std::condition_variable cond_var;
std::mutex g_mutex;
bool g_notified = false;

void f(int n) {
  for (int x : {1,2,3,4,5}) {
    std::ostringstream os;
    int rest_time = n * x * 77;
    os << "Thread " << n << " @ " << x << ", waiting " << rest_time << "ms.";
    std::this_thread::sleep_for(std::chrono::milliseconds(rest_time));
    std::unique_lock<std::mutex> lock(g_mutex);
    g_messages.push(os.str());
    g_notified = true;
    cond_var.notify_one();
  }
}

int main(int argc, char **argv) {
  bool done = false;

  std::cout << std::thread::hardware_concurrency() 
    << " hardware threads" << endl;

  std:future<int> long_running_task = 
      std::async([]() -> int {
          std::this_thread::sleep_for(std::chrono::seconds(3));
          std::unique_lock<std::mutex> lock(g_mutex);
          g_notified = true;
          g_messages.push("long_running_task done!");
          cond_var.notify_one();
          return 42;
          });

  std::thread output_thread([&]() {
      std::unique_lock<std::mutex> lock(g_mutex);
      while (!done) {
        while (!g_notified) {
          cond_var.wait(lock); 
        }
        while (!g_messages.empty()) {
          std::cout << g_messages.front() << endl;
          g_messages.pop();
        }
        g_notified = false;
      }
    });

  std::vector<std::thread> pool;
  int n = 0;
  std::generate_n(std::back_inserter(pool), 
      5,
      [&n]() { return std::thread(f, n++); });

  int long_task_value = long_running_task.get();
  for (std::thread &t : pool) {
    t.join();
  }

  done = true;
  g_notified = true;
  cond_var.notify_one();
  output_thread.join();

  cout << "long_running_task returned " << long_task_value << endl;
  return 0;
}

