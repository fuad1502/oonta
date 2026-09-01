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

    void write_header(void *obj_ptr, size_t *type_info);

  public:
    Heap(size_t size);
    void *allocate(size_t *type_info);
    void reset();
    void *start();
    void *end();
    size_t usage();
    char usage_percentage();

    static bool is_moved(void *obj_addr);
    static void set_moved(void *obj_addr, void *new_addr);
    static size_t get_obj_size(void *obj_addr);
    static std::vector<size_t> get_pointer_offsets(void *obj_addr);
    static size_t *get_type_info(void *obj_addr);
    static void *get_forwarding_ptr(void *obj_addr);
};

class Gc {
  private:
    static size_t GEN0_HEAP_SIZE;
    static size_t GEN1_INITIAL_HEAP_SIZE;
    static char COLLECTION_THRESHOLD_PERCENTAGE;

    Heap *gen0_heap;
    Heap *gen1_heap;
    std::unordered_map<unw_word_t, struct Safepoint *> *safepoints_map;

    std::queue<Location> work_q;
    unw_cursor_t *cursor;

    void *get_obj_addr(Location *location);
    bool is_gen0_addr(void *obj_addr);
    void process_work_q();
    void relocate(Location *location, void *new_addr);
    void *move_to_gen1(void *obj_addr);
    void add_pointer_fields_to_work_q(void *obj_addr);

  public:
    Gc();
    void *allocate(size_t *type_info);
    void safepoint(unw_cursor_t *cursor);
    bool need_collection();
};

#endif // GC_H
