#include <stdio.h>
#include <iostream>
#include <fcntl.h>
#include <termios.h>
#include <errno.h>
#include <algorithm>
#include <iomanip>
#include <vector>

using namespace std;

void printCharAsHex(uint8_t value)
{
  cout << std::hex  << std::setw(2) << std::setfill('0') << (int)value << " ";
}


int main(int argc, char **argv)
{
  std::string port = argc >= 2 ? argv[1] : "/tmp/wd20";
  cout << "Slaving on " << port << endl;
  int handle = open(port.c_str(), O_RDWR|O_NOCTTY);
  if (handle < 0)
  {
    perror("Bad open");
    return 1;
  }
  struct termios tp;
  tcgetattr(handle, &tp);
  cfmakeraw(&tp);
  tcsetattr(handle, TCSANOW, &tp);

  if (handle < 0)
  {
    perror("open err: ");
    return -1;
  }
  vector<uint8_t> buf(256);
  cout << "started..." << endl;
  while (1)
  {
    memset(&buf[0], 0, buf.size());
    int bytesRead = read(handle, &buf[0], buf.size());
    if (bytesRead > 0)
    {
      cout << dec << "RX: (" << bytesRead << "): ";
      for_each(buf.begin(), buf.begin() + bytesRead, ptr_fun(printCharAsHex));
      cout << endl;
    }
  }
  return 0;
}
