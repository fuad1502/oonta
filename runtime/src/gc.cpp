#include "gc.hpp"
#include <cassert>
#include <cerrno>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <sys/mman.h>
#include <unordered_map>

#include "libunwind.h"
#include "safepoints.h"

size_t Gc::INITIAL_HEAP_SIZE = 2048;

std::unordered_map<unw_word_t, struct Safepoint *> *safepoints_map;

Gc::Gc() {
    allocate_new_heap(INITIAL_HEAP_SIZE);

    // TODO: create safepoints_map at compile-time
    safepoints_map = new std::unordered_map<unw_word_t, struct Safepoint *>();
    for (int i = 0; i < safepoints_len; i++) {
        safepoints_map->insert({(unw_word_t)safepoints[i].ip, &safepoints[i]});
    }
}

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
    heap = mmap(NULL, heap_size, PROT_READ | PROT_WRITE,
                MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    if (heap == MAP_FAILED) {
        printf("Failed to allocate new heap: %s\n", strerror(errno));
        std::exit(-1);
    }
}

void Gc::safepoint(unw_cursor_t *cursor) {
    unw_word_t ip;
    unw_get_reg(cursor, UNW_REG_IP, &ip);

    auto iter = safepoints_map->find(ip);
    assert(iter != safepoints_map->end());
    Safepoint *record = iter->second;

    for (int i = 0; i < record->num_of_locations; i++) {
        auto location = record->obj_locations[i];
        unw_word_t reg;
        unw_get_reg(cursor, location.reg, &reg);

        void *obj_addr;
        switch (location.type) {
        case DIRECT:
            obj_addr = (void *)(reg + location.offset);
            break;
        case INDIRECT:
            void **ind_addr = (void **)(reg + location.offset);
            obj_addr = *ind_addr;
            break;
        }
    }
}
