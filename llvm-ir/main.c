#include <stdio.h>

extern int x;
extern int y;
extern int z;

void ocaml();

int main(int argc, char *argv[]) {
  ocaml();
  printf("x = %d\n", x);
  printf("y = %d\n", y);
  printf("z = %d\n", z);
}
