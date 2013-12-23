/* Copyright 2011 Toby Smith toby@tismith.id.au */

#define _GNU_SOURCE
#include <dlfcn.h>
#include <stdlib.h>
#include <stdio.h>

#define ENV_STRING "MALLOC_EXTRA"
#define ENV_DEBUG_STRING "MALLOC_EXTRA_DEBUG"

static size_t extra_malloc = 0;
static int print_debug = 0;
void * (*orig_malloc)(size_t size) = NULL;
void * (*orig_calloc)(size_t nmeb, size_t size) = NULL;
void * (*orig_realloc)(void *ptr, size_t size) = NULL;

void _init(void) {
    char * p = NULL;
    orig_malloc = dlsym(RTLD_NEXT, "malloc");
    orig_calloc = dlsym(RTLD_NEXT, "calloc");
    orig_realloc = dlsym(RTLD_NEXT, "realloc");
    p = getenv(ENV_STRING);
    if (p) {
        extra_malloc = atoi(p);
    }
    p = getenv(ENV_DEBUG_STRING);
    if (p) {
        print_debug = atoi(p);
    }
    if (print_debug) fprintf(stderr, "Loading malloc-extra\n");
}

void * malloc(size_t size) {
    if (print_debug) fprintf(stderr, "malloc of %d\n", size);
    return orig_malloc(size + extra_malloc);
}

void * calloc(size_t nmeb, size_t size) {
    if (print_debug) fprintf(stderr, "calloc of %d of size %d\n", nmeb, size);
    return orig_calloc(nmeb, size + extra_malloc);
}

void * realloc(void *ptr, size_t size) {
    if (print_debug) fprintf(stderr, "realloc of %p of size %d\n", ptr, size);
    return orig_realloc(ptr, size + extra_malloc);
}

