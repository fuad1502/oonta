all: main

main: main.o test.o
	clang main.o test.o -o main

main.o: main.c
	clang -c main.c

test.o: test.s
	clang -c test.s

test.s: test.ll
	llc -relocation-model=pic test.ll

clean:
	rm -f main *.o *.s
