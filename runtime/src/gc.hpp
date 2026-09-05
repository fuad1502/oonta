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
    static inline size_t GEN0_HEAP_SIZE = 64 * 1024 * 1024;
    static inline size_t GEN1_HEAP_SIZE = 256 * 1024 * 1024;
    static inline size_t GEN2_INITIAL_HEAP_SIZE = 1024 * 1024 * 1024;
    static inline size_t GEN2_PERCENTAGE_INCREMENT = 20;

    Heap *heaps[3];
    size_t gen2_heap_size;
    std::unordered_map<unw_word_t, struct Safepoint *> *safepoints_map;

    std::queue<Location> work_q;
    unw_cursor_t *cursor;
    HeapGenerations gen_to_collect;
    Heap *next_heap;

    size_t collect();
    void process_work_q();
    void add_pointer_fields_to_work_q(void *obj_addr);

    void *copy_obj(void *obj_addr) const;
    bool is_addr_in_gen_to_collect(void *obj_addr) const;
    Heap *heap_to_collect() const;
    void *get_obj_addr(Location *location) const;
    void relocate(Location *location, void *new_addr) const;

  public:
    Gc();
    void *allocate(size_t *type_info) const;
    bool need_collection() const;
    void safepoint(unw_cursor_t cursor);
};

inline void *Gc::allocate(size_t *type_info) const {
    auto *ptr = heaps[0]->allocate(type_info);

    return ptr;
}

inline bool Gc::need_collection() const {
    return heap_to_collect()->need_collection();
}

inline void *Gc::copy_obj(void *obj_addr) const {
    size_t *type_info = Heap::get_type_info(obj_addr);
    size_t size = type_info[0];
    auto *new_addr = next_heap->allocate(type_info);

    if (new_addr == nullptr) {
        printf("Cannot allocate in next generation heap\n");
        exit(-1);
    }

    memcpy(new_addr, obj_addr, size);
    return new_addr;
}

inline bool Gc::is_addr_in_gen_to_collect(void *obj_addr) const {
    return (heap_to_collect()->start() <= obj_addr &&
            obj_addr < heap_to_collect()->end());
}

inline Heap *Gc::heap_to_collect() const {
    switch (gen_to_collect) {
    case HeapGenerations::Zero:
        return heaps[0];
    case HeapGenerations::One:
        return heaps[1];
    case HeapGenerations::Two:
        return heaps[2];
    }
    assert(false);
}

inline void *Gc::get_obj_addr(Location *location) const {
    void *obj_addr;
    switch (location->type) {
    case DIRECT: {
        unw_word_t reg;
        unw_get_reg(cursor, location->reg, &reg);
        obj_addr = (void *)(reg + location->offset);
        break;
    }
    case INDIRECT: {
        unw_word_t reg;
        unw_get_reg(cursor, location->reg, &reg);
        void **ind_addr = (void **)(reg + location->offset);
        obj_addr = *ind_addr;
        break;
    }
    case CONSTANT:
        obj_addr = *((void **)location->constant);
        break;
    }
    return obj_addr;
}

inline void Gc::relocate(Location *location, void *new_addr) const {
    switch (location->type) {
    case DIRECT: {
        unw_word_t reg;
        unw_get_reg(cursor, location->reg, &reg);
        unw_set_reg(cursor, reg, (size_t)new_addr - location->offset);
        break;
    }
    case INDIRECT: {
        unw_word_t reg;
        unw_get_reg(cursor, location->reg, &reg);
        void **ind_addr = (void **)(reg + location->offset);
        *ind_addr = new_addr;
        break;
    }
    case CONSTANT:
        *((void **)location->constant) = new_addr;
        break;
    }
}

#endif // GC_H
