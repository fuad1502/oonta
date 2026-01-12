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

def generate_oonta_benchmark_binary(benchmark_src):
    subprocess.run(["cargo", "run", "--", f"benchmark/{benchmark_src}", "-o", "benchmark/oonta.ll", "--exec"], capture_output=True) 

def generate_ocamlopt_benchmark_binary(benchmark_src):
    subprocess.run(["ocamlopt", "-O3", f"benchmark/{benchmark_src}", "-o", "benchmark/ocamlopt.out"]) 

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
    return elapsed_time

def main():
    if len(sys.argv) != 3:
        print("Usage      : python3 benchmark/benchmark.py <num of list elements to sort> <benchmark idx>")
        print("Benchmarks :\n\t0. Merge sort\n\t1. Insertion sort")
        print("Tips       : run in the repository root directory")
        sys.exit(1)

    numbers = generate_random_numbers(int(sys.argv[1]))
    benchmark_idx = int(sys.argv[2])
    if benchmark_idx == 0:
        benchmark_src = "merge_sort.ml"
    elif benchmark_idx == 1:
        benchmark_src = "insertion_sort.ml"
    else:
        print(f"Invalid benchmark idx ({benchmark_idx})")
        sys.exit(1)
    print(f"Benchmarking: {benchmark_src}")

    generate_input_file(numbers)
    generate_output_ref_file(numbers)
    generate_ocamlopt_benchmark_binary(benchmark_src)
    generate_oonta_benchmark_binary(benchmark_src)
    ref_time = benchmark("./benchmark/ocamlopt.out")
    time = benchmark("./benchmark/oonta.out")
    if time > ref_time:
        print(f"> {time/ref_time:.2f} times slower")
    else:
        print(f"> {ref_time/time:.2f} times faster")

if __name__ == "__main__":
    main()
