#ifndef GC_H
#define GC_H

#include "heap.hpp"
#include "safepoints.h"

#include <queue>
#include <unordered_map>

#define UNW_LOCAL_ONLY
#include "libunwind.h"

enum class HeapGenerations {
    Zero,
    One,
    Two,
};

class Gc {
  private:
    static size_t GEN0_HEAP_SIZE;
    static size_t GEN1_HEAP_SIZE;
    static size_t GEN2_INITIAL_HEAP_SIZE;
    static char COLLECTION_THRESHOLD_PERCENTAGE;

    Heap *heaps[3];
    std::unordered_map<unw_word_t, struct Safepoint *> *safepoints_map;

    std::queue<Location> work_q;
    unw_cursor_t *cursor;
    HeapGenerations gen_to_collect;
    Heap *next_heap;

    size_t collect();
    void process_work_q();
    void *copy_obj(void *obj_addr);
    void add_pointer_fields_to_work_q(void *obj_addr);
    void relocate(Location *location, void *new_addr);
    Heap *heap_to_collect();
    bool is_addr_in_gen_to_collect(void *obj_addr);
    void *get_obj_addr(Location *location);

  public:
    Gc();
    void *allocate(size_t *type_info);
    void safepoint(unw_cursor_t cursor);
    bool need_collection();
};

#endif // GC_H
