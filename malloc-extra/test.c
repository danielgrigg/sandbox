/* Copyright 2011 Toby Smith toby@tismith.id.au */

#include <stdio.h>
#include <stdlib.h>

int main (char argc, char **argv) {
    char *p = NULL;
    printf("starting test\n");
    p = malloc(10);
    printf("p is %p\n", p);
    p = realloc(p, 15);
    printf("p is %p\n", p);
    p = calloc(10, 10);
    printf("p is %p\n", p);
}


