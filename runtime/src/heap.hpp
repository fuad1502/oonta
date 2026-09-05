#ifndef HEAP_H
#define HEAP_H

#include <cassert>
#include <cerrno>
#include <cstddef>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <sys/mman.h>

class Heap {
  private:
    void *heap;
    size_t heap_size;
    size_t heap_offset;
    size_t heap_limit;

    static void write_header(void *obj_ptr, size_t *type_info);

  public:
    Heap(size_t size, size_t limit);
    ~Heap();
    void *allocate(size_t *type_info);
    void set_limit(size_t limit);
    void reset();
    size_t limit() const;
    void *start() const;
    void *end() const;
    size_t usage() const;
    bool need_collection() const;

    static bool is_moved(void *obj_addr);
    static void set_moved(void *obj_addr, void *new_addr);
    static size_t get_obj_size(void *obj_addr);
    static size_t *get_type_info(void *obj_addr);
    static void *get_forwarding_ptr(void *obj_addr);
};

inline Heap::Heap(size_t size, size_t limit) {
    assert(limit <= size);
    heap_size = size;
    heap_offset = 0;
    heap_limit = limit;
    heap = mmap(NULL, heap_size, PROT_READ | PROT_WRITE,
                MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    if (heap == MAP_FAILED) {
        printf("Failed to allocate new heap: %s\n", strerror(errno));
        std::exit(-1);
    }
}

inline Heap::~Heap() {
    int rc = munmap(heap, heap_size);
    if (rc == -1) {
        printf("Failed to release heap: %s\n", strerror(errno));
        std::exit(-1);
    }
}

inline void *Heap::allocate(size_t *type_info) {
    size_t size = type_info[0];

    auto total_size = size + 8;

    if (heap_offset + total_size > heap_size) {
        return nullptr;
    }

    auto *obj_ptr = (uint8_t *)heap + heap_offset + 8;

    write_header(obj_ptr, type_info);

    heap_offset += total_size;

    return obj_ptr;
}

inline void Heap::set_limit(size_t limit) {
    assert(limit <= heap_size);
    heap_limit = limit;
}

inline void Heap::reset() { heap_offset = 0; }

inline size_t Heap::limit() const { return heap_limit; }

inline void *Heap::start() const { return heap; }

inline void *Heap::end() const { return (uint8_t *)heap + heap_size; }

inline size_t Heap::usage() const { return heap_offset; }

inline bool Heap::need_collection() const { return heap_offset >= heap_limit; }

inline bool Heap::is_moved(void *obj_addr) {
    size_t *header = (size_t *)obj_addr - 1;

    return (*header & 0b1);
}

inline void Heap::set_moved(void *obj_addr, void *new_addr) {
    size_t *header = (size_t *)obj_addr - 1;

    *header = 0b1;
    *header |= ((size_t)new_addr << 1);
}

inline size_t Heap::get_obj_size(void *obj_addr) {
    size_t *header = (size_t *)obj_addr - 1;
    assert((*header & 0b1) == 0);
    size_t *type_info = (size_t *)(*header >> 1);

    return type_info[0];
}

inline size_t *Heap::get_type_info(void *obj_addr) {
    size_t *header = (size_t *)obj_addr - 1;
    assert((*header & 0b1) == 0);

    return (size_t *)(*header >> 1);
}

inline void *Heap::get_forwarding_ptr(void *obj_addr) {
    size_t *header = (size_t *)obj_addr - 1;
    assert((*header & 0b1) == 1);

    return (void *)(*header >> 1);
}

/*
 * Header format:
 * |xxxxxxxx|xxxxxxxx|xxxxxxxx|xxxxxxxx|xxxxxxxx|xxxxxxxx|xxxxxxxx|xxxxxxxm|obj
 *
 * m = 0 -> not moved, xxx... = type info pointer
 * m = 1 -> moved, xxx... = new location
 */
inline void Heap::write_header(void *obj_ptr, size_t *type_info) {
    size_t *header = (size_t *)obj_ptr - 1;
    *header = (size_t)type_info << 1;
}

#endif // HEAP_H
