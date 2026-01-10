import random
import sys
import subprocess
import time

def generate_random_numbers(n):
    res = []
    for _ in range(n):
        num = random.randint(1, n)
        res.append(num)
    return res

def generate_input_file(numbers):
    with open("benchmark/input.txt", "w") as file:
        file.write(f"{len(numbers)}\n")
        for num in numbers:
            file.write(f"{num}\n")

def generate_output_ref_file(numbers):
    with open("benchmark/output.ref.txt", "w") as file:
        numbers.sort() 
        for num in numbers:
            file.write(f"{num}")

def generate_oonta_benchmark_binary():
    subprocess.run(["cargo", "run", "--", "benchmark/benchmark.ml", "--exec"]) 

def generate_ocamlopt_benchmark_binary():
    subprocess.run(["ocamlopt", "benchmark/benchmark.ml", "-o", "benchmark/ocamlopt.out"]) 

def benchmark(binary):
    with open("benchmark/input.txt", "r") as infile:
        with open("benchmark/output.txt", "w") as outfile:
            start_time = time.perf_counter()
            subprocess.run([binary], stdin=infile, stdout=outfile) 
            end_time = time.perf_counter()
            elapsed_time = end_time - start_time
            print(f"Elapsed time ({binary}): {elapsed_time:.4f} seconds")
    with open("benchmark/output.ref.txt", "r") as reffile:
        with open("benchmark/output.txt", "r") as outfile:
            if reffile.readline() != outfile.readline():
                print(f"{binary} output is incorrect!")


def main():
    if len(sys.argv) != 2:
        print("Usage : python3 benchmark/benchmark.py <num of list elements to sort>")
        print("Tips  : run in the repository root directory")
        sys.exit(1)
    numbers = generate_random_numbers(int(sys.argv[1]))
    generate_input_file(numbers)
    generate_output_ref_file(numbers)
    generate_oonta_benchmark_binary()
    generate_ocamlopt_benchmark_binary()
    benchmark("./benchmark/benchmark.out")
    benchmark("./benchmark/ocamlopt.out")

if __name__ == "__main__":
    main()
