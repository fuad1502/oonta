#include <stdio.h>

extern int x;
extern int y;
extern int z;
extern int a;
extern int b;

void ocaml();

int main(int argc, char *argv[]) {
  ocaml();
  printf("x = %d\n", x);
  printf("y = %d\n", y);
  printf("z = %d\n", z);
  printf("a = %d\n", a);
  printf("b = %d\n", b);
}
