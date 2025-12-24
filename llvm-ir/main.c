#include <stdio.h>
#include <stdlib.h>

struct closure {
  void *anon0;
  void *env[1];
};

struct partial {
  struct closure *add;
  int a;
};

extern int x;
extern struct closure *add;
extern struct partial *addthree;
extern int y;
extern int z;

void caml();

int main(int argc, char *argv[]) {
  caml();
  printf("x = %d\n", x);
  printf("add = %zx\n", (size_t)add);
  printf("add->anon0 = %zx\n", (size_t)add->anon0);
  printf("add->env[0] = %d\n", *(int *)add->env[0]);
  printf("addthree = %zx\n", (size_t)addthree);
  printf("addthree->add = %zx\n", (size_t)addthree->add);
  printf("addthree->a = %d\n", addthree->a);
  printf("y = %d\n", y);
  printf("z = %d\n", z);
}
