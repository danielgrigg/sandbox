#include <iostream>

using namespace std;

int main(int argc, char **argv)
{
  char buf[255];
  time_t ticks = time(NULL);
  snprintf(buf, sizeof(buf), "%.24s\r\n", ctime(&ticks));
  cout << buf;

  printf("first %1.1$\n", 3, 4, 5, 6);
  return 0;
}
