#include <algorithm>
#include <functional>
#include <iostream>
#include <vector>
#include <cmath>
#include <dispatch/dispatch.h>

using namespace std;

std::function<int(int)> make_scale_f(int x)  {
  return [x](int y) { return y * x; };
}

void test_lambda() {
  auto f1 = [](int x, int y) { return x + y; };
  cout << "f1(3, 5): " << f1(3, 5) << endl;

  auto f2 = [](float x) -> int { return floor(x); };
  cout << "f2(5.7): " << f2(5.7) << endl;

  int sum = 0;
  std::vector<int> xs {1,2,3,4,5};
  std::for_each(xs.begin(), xs.end(), [&sum](int x) { sum += x; });
  cout << "sum " << sum << endl;
  }

void block_func(void* context) {
      printf("func start\n");
      sleep(1);
      printf("func ending\n");
}

  int g_x = 99;

  struct Foo {
    int a;
    double b;
  };
void test_block() {
  dispatch_queue_t queue = dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT,0);

  auto f = [](void* context) { 
    printf("lambda start\n");
    sleep(1);
    printf("lambda end\n");
  };

  auto b = ^{
      printf("async starting\n");
      sleep(2);
      printf("async ending\n");
      };
  dispatch_async(queue, b);
  dispatch_async_f(queue, NULL, block_func);
  dispatch_async_f(queue, NULL, f);
  dispatch_async_f(queue, NULL, [](void* context) {
      printf("lambda, no capture start\n");
      sleep(1);
      printf("lambda, no capture done\n");
      });

  auto foo = new Foo {77, 3.1459};

  dispatch_async_f(queue, foo, [](void* context) {
      auto p = (Foo*)context;
      printf("lambda, (%d, %f) capture start\n", p->a, p->b);
      sleep(1);
      printf("lambda, (%d, %f) capture end\n", p->a, p->b);
      delete p;
      });


}

int main(int argc, char **argv)
{
  test_lambda();
  test_block();

  sleep(8);
 
  return 0;
}

