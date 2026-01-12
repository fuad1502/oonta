#ifndef GC_H
#define GC_H

#include <cstddef>

class Gc {
  private:
    void *heap;
    size_t heap_size;
    size_t heap_offset;

    static size_t INITIAL_HEAP_SIZE;

    void allocate_new_heap(size_t size);

  public:
    Gc();
    void *allocate(std::size_t size);
};

#endif // GC_H
