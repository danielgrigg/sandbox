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
#include <dispatch/dispatch.h>
#include <boost/lexical_cast.hpp>

using namespace std;
using namespace boost;

void postMessage(void* context)
{
  std::string* message = (string*)context;
  cout << *message << endl;
}

// This snippet from apple's concurrency guide
dispatch_source_t CreateDispatchTimer(uint64_t interval,
              uint64_t leeway,
              dispatch_queue_t queue,
              dispatch_block_t block)
{
   dispatch_source_t timer = dispatch_source_create(DISPATCH_SOURCE_TYPE_TIMER,
                                                     0, 0, queue);
   if (timer)
   {
      dispatch_source_set_timer(timer, dispatch_walltime(NULL, 0), interval, leeway);
      dispatch_source_set_event_handler(timer, block);
      dispatch_resume(timer);
   }
   return timer;
}

void MyPeriodicTask(dispatch_queue_t messageQueue)
{
  cout << time(NULL) << ": ";
  string message = "Periodic task called!";
  dispatch_sync_f(messageQueue, &message, postMessage);
}

void foo(void* context)
{
  dispatch_queue_t* messageQueue = (dispatch_queue_t*)context;
  int end = rand();
  uint64_t sum = 0;

  string startedMessage = "started task: ";
  startedMessage += lexical_cast<string>(end);

  dispatch_sync_f(*messageQueue, &startedMessage, postMessage);
  for (int i = 0; i < end; ++i)
  {
    sum += i;
  }

  string message = "finished task: ";
  message += lexical_cast<string>(end);
  message += ", ";
  message += lexical_cast<string>(sum);

  dispatch_sync_f(*messageQueue, &message, postMessage);
}

int main(int argc, char **argv)
{
  dispatch_queue_t queue = dispatch_get_global_queue(0 /*DISPATCH_QUEUE_PRIORITY_LOW*/,0);
  dispatch_group_t group = dispatch_group_create();
  dispatch_queue_t messageQueue = dispatch_queue_create("com.example.dispatch", DISPATCH_QUEUE_SERIAL);
  dispatch_queue_t eventQueue = dispatch_queue_create("com.example.dispatch.events", DISPATCH_QUEUE_SERIAL);

  dispatch_source_t aTimer = CreateDispatchTimer(1ull * NSEC_PER_SEC,
      0,//1ull * NSEC_PER_SEC,
      eventQueue,
      ^{ MyPeriodicTask(messageQueue); });

  void* context = (void*)&messageQueue;
  for (int i = 0; i < 12; ++i)
  {
    auto f1 = [](void* y) { return 9 * 9 * 9; cout << "foo "; sleep(1); };
    dispatch_group_async_f(group, queue, context, foo);
  }
  long result = dispatch_group_wait(group, DISPATCH_TIME_FOREVER);
  cout << "Finished all tasks: " << result << endl;
  return 0;
}

