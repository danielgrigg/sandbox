syscall::open*:entry 
/execname == "mdworker"/ 
{ 
	printf ("%s", copyinstr(arg0)); 
}

