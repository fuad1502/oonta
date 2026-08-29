#ifndef GC_H
#define GC_H

#include <unordered_map>

#define UNW_LOCAL_ONLY
#include "libunwind.h"

class Heap {
  private:
    void *heap;
    size_t heap_size;
    size_t heap_offset;

    size_t header_size_from_pointer_field_offs(size_t *pointer_field_offs,
                                               size_t pointer_field_offs_len);
    void write_header(void *obj_ptr, size_t *pointer_field_offs,
                      size_t pointer_field_offs_len);

  public:
    Heap(size_t size);
    void *allocate(size_t size, size_t *pointer_field_offs,
                   size_t pointer_field_offs_len);
};

class Gc {
  private:
    static size_t GEN0_HEAP_SIZE;
    static size_t GEN1_INITIAL_HEAP_SIZE;

    Heap *gen0_heap;
    Heap *gen1_heap;
    std::unordered_map<unw_word_t, struct Safepoint *> *safepoints_map;

  public:
    Gc();
    void *allocate(size_t size, size_t *pointer_field_offs,
                   size_t pointer_field_offs_len);
    void safepoint(unw_cursor_t *cursor);
};

#endif // GC_H
