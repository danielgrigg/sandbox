#include <stdio.h>
#include <iostream>
#include <fcntl.h>
#include <termios.h>
#include <errno.h>
#include <string>
#include <unistd.h>

using namespace std;

int main()
{
  struct termios tio;
  int pt = open("/dev/ptmx", O_RDWR | O_NOCTTY);

  if (pt < 0)
  {
    perror("open /dev/ptmx");
    return 1;
  }

  if (tcgetattr(pt, &tio) < 0) {
    perror("tcgetattr() failed");
    return 1;
  }
#if 0
  /* disable echo etc */
  tio.c_cc[VEOF]      = 1;
  tio.c_iflag         = BRKINT|ISTRIP|IXON|IXANY|OPOST;
  tio.c_oflag         = 0;
  tio.c_cflag         = 0;
  tio.c_lflag         = 0;
#endif

  cfmakeraw(&tio);

  if (tcsetattr(pt, TCSANOW, &tio) < 0) {
    perror("tcsetattr() failed");
    return 1;
  }

  grantpt(pt);
  unlockpt(pt);

  // Get the Slave side device name of Pseudo-Term
  std::cout << "Pseudo slave: " << ptsname(pt) << std::endl;
  char txBuf[512];
  while (1)
  {
    sleep(1);
    strcpy(txBuf, "foo");
    if (write(pt, txBuf, strlen(txBuf)) <= 0) cout << "Err writing" << endl;
    /*  
        memset(rxBuf, 0, sizeof(rxBuf));
        int bytesRead = read(pt, rxBuf, 256);
        if (bytesRead > 0)
        {
        cout << "RX: " << rxBuf << endl;
        }
        */
  }
  return 0;
}
