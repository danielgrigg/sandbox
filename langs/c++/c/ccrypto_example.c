#include <CommonCrypto/CommonCryptor.h>
#include <CommonCrypto/CommonKeyDerivation.h>
#include <stdio.h>

int main () {

  const char* key = "123456789";
  const char* iv = "abcdefghi";
  int i;
  const char* data = "The quick brown fox jumped over the lazy dog.";
  char dataOut[128];
  memset(dataOut, 0, sizeof(dataOut));

  size_t dataOutMoved;

  CCCryptorStatus result = CCCrypt(kCCEncrypt, // operation
      kCCAlgorithmAES128, // Algorithm
      kCCOptionPKCS7Padding, // options
      key, // key
      strlen(key), // keylength
      iv,// iv
      data, // dataIn
      strlen(data), // dataInLength,
      dataOut, // dataOut
      sizeof(dataOut), // dataOutAvailable
      &dataOutMoved); // dataOutMoved

  printf("result %d (0x%x)\n", result, result);
  printf("dataOutMoved %zu\n", dataOutMoved);

  for (i = 0; i < sizeof(dataOut); ++i) {
    printf("%x ", dataOut[i]);
    if (i % 16 == 0 && i > 0) printf("\n");
  }

  return 0;
}
