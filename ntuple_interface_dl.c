#include <dlfcn.h>
#include <stdio.h>
#include <stdlib.h>

#define STR1(x) #x
#define STR(x) STR1(x)

static void* lib;

static void*(*f_open_ntuple)(char*);
static void(*f_close_ntuple)(void*);
static void(*f_fill_ntuple)(void*);

#define add_ntuple_branch(TYPE) \
static void(*f_add_ntuple_branch_##TYPE)(void*, char*, TYPE*);

add_ntuple_branch(double)
add_ntuple_branch(float)
add_ntuple_branch(int)

void load_ntuple_lib_(char* file_name) {
  if ((lib = dlopen(file_name,RTLD_LAZY)) == 0) {
    fflush(stdout);
    fprintf(stderr,"\nfailed to load %s\n",file_name);
    exit(1);
  }
  f_open_ntuple = dlsym(lib,"open_ntuple");
  f_close_ntuple = dlsym(lib,"close_ntuple");
  f_fill_ntuple = dlsym(lib,"fill_ntuple");

#undef add_ntuple_branch
#define add_ntuple_branch(TYPE) \
  f_add_ntuple_branch_##TYPE = dlsym(lib,STR(add_ntuple_branch_##TYPE));

  add_ntuple_branch(double)
  add_ntuple_branch(float)
  add_ntuple_branch(int)
}

void* open_ntuple_(char* file_name) {
  printf("opening ntuple file \"%s\"\n",file_name);
  void* ptr = f_open_ntuple(file_name);
  printf("%p\n",ptr);
  return ptr;
}

void close_ntuple_(void** ntuple) {
  printf("closing ntuple\n");
  fflush(stdout);
  return f_close_ntuple(*ntuple);
}

void fill_ntuple_(void** ntuple) {
  return f_fill_ntuple(*ntuple);
}

#undef add_ntuple_branch
#define add_ntuple_branch(TYPE) \
void add_ntuple_branch_##TYPE##_(void** ntuple, char* name, TYPE* ptr) { \
  printf("%p Adding branch %s\n",*ntuple,name); \
  f_add_ntuple_branch_##TYPE(*ntuple,name,ptr); \
}

add_ntuple_branch(double)
add_ntuple_branch(float)
add_ntuple_branch(int)

