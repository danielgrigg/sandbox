#include <thread>
#include <chrono>
#include <queue>
#include <iostream>
#include <atomic>

using namespace std;

std::atomic<int> g_counter = ATOMIC_VAR_INIT(0);

class SerialTaskQueue {
  public:

    SerialTaskQueue():
      _notified(false),
      _quit(false)
    {
    }

    ~SerialTaskQueue() {
      {
        std::unique_lock<std::mutex> lock(_mutex);
        _notified = true;
        _quit = true;
        _ready.notify_one();
      }
      _thread.join();
    }

    void start() {
      _thread = std::thread([&]() {
          std::unique_lock<std::mutex> lock(_mutex);
          while (!_quit) {
            while (!_notified) {
              _ready.wait(lock);
              }
              while (!_tasks.empty()) {
                function<void()> f = _tasks.front();
                _tasks.pop();
                lock.unlock();
                f();
                lock.lock();
              }
              _notified = false;
          }
          });
    }

  void dispatch(function<void()> f) {
    std::unique_lock<std::mutex> lock(_mutex);
    _tasks.push(f);
    _notified = true;
    _ready.notify_one();
  }

  private:
    std::queue<std::function<void()> > _tasks;
    std::mutex _mutex;
    std::condition_variable _ready;
    std::thread _thread;
    bool _notified;
    bool _quit;

};

void some_task() {

  std::cout << g_counter << " starting\n";
  std::this_thread::sleep_for(std::chrono::milliseconds( rand() % 500));
  std::cout << g_counter << " done\n";
  std::atomic_fetch_add(&g_counter, 1);
}

void some_task_with_params(int n) {
  std::cout << "doing some funky calcs with " << n << std::endl;
  std::this_thread::sleep_for(std::chrono::milliseconds( rand() % 500));
}

void some_task_lots_data(const std::vector<int>& blob) {
  std::cout << "working a list of data: ";
  for (auto i = blob.begin(); i < blob.end(); ++i) {
    std::cout << *i << " ";
  }
  std::cout << endl;
  std::this_thread::sleep_for(std::chrono::milliseconds( rand() % 500));
}

std::vector<int> range(int start, int end) {
  std::vector<int> v;
  v.reserve(end - start);
  for (int i = start; i < end; ++i) {
    v.push_back(i);
  }
  return v;
}

int main(int argc, char **argv)
{
  std::cout << std::thread::hardware_concurrency() << " hardware threads" << endl;
  const int n = 100;

  SerialTaskQueue queue[n];
  for (int i = 0; i < n; ++i) {
    queue[i].start();
  }
  for (int i = 0; i < 20; ++i) {
    queue[rand() % n].dispatch(
        std::bind(some_task_lots_data, range(0, i)));
//        std::bind(some_task_with_params, i*i));

    std::this_thread::sleep_for(std::chrono::milliseconds( rand() % 50));
  }
  std::cout << "Main thread done!" << endl;
  return 0;
}
