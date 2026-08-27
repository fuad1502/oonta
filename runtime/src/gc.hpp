#ifndef GC_H
#define GC_H

#define UNW_LOCAL_ONLY
#include "libunwind.h"
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
    void *allocate(size_t size, size_t *pointer_field_offs,
                   size_t pointer_field_offs_len);
    void safepoint(unw_cursor_t *cursor);
};

#endif // GC_H
