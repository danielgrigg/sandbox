#include <stdio.h>
#include <string.h>

void test1()
{
  char test[80], blah[80];
  const char *sep = "\\/:;=-";
  char *word, *phrase, *brkt, *brkb;

  strcpy(test, "This;is.a:test:of=the/string\\tokenizer-function.");

  for (word = strtok_r(test, sep, &brkt);
      word;
      word = strtok_r(NULL, sep, &brkt))
  {
    strcpy(blah, "blah:blat:blab:blag");

    for (phrase = strtok_r(blah, sep, &brkb);
        phrase;
        phrase = strtok_r(NULL, sep, &brkb))
    {
      printf("So far we're at %s:%s\n", word, phrase);
    }
  }

}

void test2()
{
  //strtok_r with successive delimeters
  const char *sep = "/";
  char test[80];
  strcpy(test, "1//2/3");
  char* brkt;
  for (char* word = strtok_r(test, sep, &brkt); 
      word; 
      word = strtok_r(NULL, sep, &brkt))
  {
    printf("token: %s\n", word);
  }
}

void test3()
{
  //strtok_r with successive delimeters
  const char *sep = "/";
  char test[80];
  strcpy(test, "1//3");
  char* brkt;
  char* p = test;
  for (char* word = strsep(&p, sep); 
      word; 
      word = strsep(&p, sep))
  {
    printf("token: %s\n", word);
  }
}

void test4()
{
  const char string[] = "words,,separated,by spaces -- and, punctuation!";
  const char delimiters[] = " .,;:!-";
  char *running;
  char *token;

  running = strdup(string);
  token = strsep (&running, delimiters);    /* token => "words" */
  printf("token: %s\n", token);
  token = strsep (&running, delimiters);    /* token => "separated" */
  printf("token: %s\n", token);
  token = strsep (&running, delimiters);    /* token => "by" */
  printf("token: %s\n", token);
}


int main(int argc, char **argv)
{
  //  test1();
  //  test2();
  test3();
  test4();
  return 0;
}
