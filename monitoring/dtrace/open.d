syscall::open:entry
/execname == "TextEdit"/
{
  printf("opening %s with flags %x\n", copyinstr(arg0), arg1);
}
