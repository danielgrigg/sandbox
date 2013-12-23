ruby$target:::function-entry
{
  printf("%s:%s\n", copyinstr(arg0), copyinstr(arg1));
}

ruby$target:::function-return
{
  printf("%s:%s\n", copyinstr(arg0), copyinstr(arg1));
}
