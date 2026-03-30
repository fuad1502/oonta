#include "gc.hpp"
#include <cerrno>
#include <cstdio>
#include <cstdlib>
#include <cstring>

// Platform-specific includes
#ifdef _WIN32
#include <windows.h>
#else
#include <sys/mman.h>
#endif

size_t Gc::INITIAL_HEAP_SIZE = 2048;

Gc::Gc() { allocate_new_heap(INITIAL_HEAP_SIZE); }

void *Gc::allocate(size_t size) {
    void *ptr;
    if ((heap_offset + size) >= heap_size) {
        allocate_new_heap(heap_size * 2);
    }
    ptr = (char *)heap + heap_offset;
    heap_offset += size;
    return ptr;
}

void Gc::allocate_new_heap(size_t size) {
    heap_size = size;
    heap_offset = 0;

#ifdef _WIN32
    // Windows implementation using VirtualAlloc
    heap =
        VirtualAlloc(NULL, heap_size, MEM_COMMIT | MEM_RESERVE, PAGE_READWRITE);
    if (heap == NULL) {
        printf("Failed to allocate new heap: error code %lu\n", GetLastError());
        std::exit(-1);
    }
#else
    // Linux/Unix implementation using mmap
    heap = mmap(NULL, heap_size, PROT_READ | PROT_WRITE,
                MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    if (heap == MAP_FAILED) {
        printf("Failed to allocate new heap: %s\n", strerror(errno));
        std::exit(-1);
    }
#endif
}
