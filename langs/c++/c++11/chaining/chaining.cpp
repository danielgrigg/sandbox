#include <iostream>
#include <vector>
#include <algorithm>
#include <numeric>
#include <future>
#include <unistd.h>
 
using namespace std;

enum class Status {
  Error,
  Ok 
};

template <typename T>
struct Response {
  Status status;
  T value;
};

template <typename RAIter>
int parallel_sum(RAIter beg, RAIter end)
{
  typename RAIter::difference_type len = end-beg;
  if(len < 10000)
    return std::accumulate(beg, end, 0);

  RAIter mid = beg + len/2;
  auto handle = std::async(std::launch::async,
      parallel_sum<RAIter>, mid, end);
  int sum = parallel_sum(beg, mid);
  return sum + handle.get();
}

void response_handler(int x) {
  cout << "got a response " << x << endl;
}

int request_a() {
  printf("request_a start\n");
  sleep(1);
  printf("request_a done\n");
  return 42;
}

template <typename R>
void continue_with(std::function<R()> request, std::function<void(Status)> continue_fn) {
  printf("wrap started\n");
   std::async(std::launch::async, [=]() {
        R result = request();
        std::async(std::launch::async, continue_fn, result);
      });
  printf("wrap end\n");
}

template <typename R>
void continue_with(std::function<Response<R>()> request, 
    std::function<void(R)> success_fn, 
    std::function<void(Status)> error_fn) {
  printf("wrap started\n");
   std::async(std::launch::async, [=]() {
        auto result = request();
        if (result.status == Status::Error) {
          std::async(std::launch::async, error_fn, result.status);
        } else {
          std::async(std::launch::async, success_fn, result.value);
        }
      });
  printf("wrap end\n");
}
Response<int> cool_request() {
  printf("cool requested started...\n");
  sleep(2);
  printf("cool requested ended...\n");
  return { Status::Ok, 23 };
}

Response<int> bad_request() {
  printf("bad requested started...\n");
  sleep(1);
  printf("bad requested ended...\n");
  return { Status::Error, 999 };
}

void err_handler(Status s) {
  printf("Got error\n");
}

template <typename Policy>
int api();


int main()
{
//  continue_with<int>(request_a, response_handler);
  continue_with<int>(cool_request, response_handler, err_handler);
  continue_with<int>(bad_request, response_handler, err_handler);
  for (int i = 0; i < 6; ++i ) {
    printf("main thread...\n");
    sleep(1);
  }
   
  return 0;
}
