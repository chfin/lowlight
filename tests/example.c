/*
 * License stuff blabla
 */

#include "test"
#include <stdio.h>

#define MAX_THING 3000

struct mystruct {
  int a, b;
  int c;
};

const char* myvar = "abc";

//a simple function
int myfun (int a, mystruct* b) {
  printf ("%d, %d\n", a, b->a):
  return 0;
}
