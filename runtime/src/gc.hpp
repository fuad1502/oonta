#ifndef GC_H
#define GC_H

#include "safepoints.h"

#include <queue>
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
    void *start();
    void *end();
    void *allocate(size_t size, size_t *pointer_field_offs,
                   size_t pointer_field_offs_len);
    char usage_percentage();
};

class Gc {
  private:
    static size_t GEN0_HEAP_SIZE;
    static size_t GEN1_INITIAL_HEAP_SIZE;
    static char COLLECTION_THRESHOLD_PERCENTAGE;

    Heap *gen0_heap;
    Heap *gen1_heap;
    std::unordered_map<unw_word_t, struct Safepoint *> *safepoints_map;

    unw_cursor_t *cursor;

    bool is_gen0_addr(void *obj_addr);
    void *get_obj_addr(Location *location);
    void relocate(Location *location, void *new_addr);
    void add_pointer_fields_to_work_q(void *obj_addr,
                                      std::queue<Location> work_q);

  public:
    Gc();
    void *allocate(size_t size, size_t *pointer_field_offs,
                   size_t pointer_field_offs_len);
    void safepoint(unw_cursor_t *cursor);
    bool need_collection();
};

#endif // GC_H
