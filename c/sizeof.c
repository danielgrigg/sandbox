#include <stdio.h>
#include <stdint.h>

int main() {

  uint8_t my_array[11] = {0};
  int a2[13] = {4};

  printf("sizeof my_array %u\n", sizeof(my_array));
  printf("sizeof a2 %u\n", sizeof(a2));
  return 0;
}
