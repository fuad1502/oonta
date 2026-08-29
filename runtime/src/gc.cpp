#include "gc.hpp"
#include <cassert>
#include <cerrno>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <sys/mman.h>

#include "safepoints.h"

Heap::Heap(size_t size) {
    heap_size = size;
    heap_offset = 0;
    heap = mmap(NULL, heap_size, PROT_READ | PROT_WRITE,
                MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    if (heap == MAP_FAILED) {
        printf("Failed to allocate new heap: %s\n", strerror(errno));
        std::exit(-1);
    }
}

void *Heap::allocate(size_t size, size_t *pointer_field_offs,
                     size_t pointer_field_offs_len) {
    auto header_size = header_size_from_pointer_field_offs(
        pointer_field_offs, pointer_field_offs_len);
    auto total_size = header_size + size;

    if (heap_offset + total_size > heap_size) {
        return nullptr;
    }

    auto *obj_ptr = (char *)heap + heap_offset + header_size;

    write_header(obj_ptr, pointer_field_offs, pointer_field_offs_len);

    heap_offset += total_size;
    return obj_ptr;
}

/*
 * Header format:
 *                      |xxxxxxx0|obj
 *             |xxxxxxxx|00000011|obj
 *    |xxxxxxxx|xxxxxxxx|00000101|obj
 * ...|xxxxxxxx|00000001|11111111|obj
 *
 * x: 0 -> not pointer, 1 -> pointer
 */
void Heap::write_header(void *obj_ptr, size_t *pointer_field_offs,
                        size_t pointer_field_offs_len) {
    auto header_size = header_size_from_pointer_field_offs(
        pointer_field_offs, pointer_field_offs_len);

    if (header_size == 1) {
        int header = 0;
        for (int i = 0; i < pointer_field_offs_len; i++) {
            header |= (1 << (pointer_field_offs[i] + 1));
        }
        *((char *)obj_ptr - 1) = header;
        return;
    }

    memset((char *)obj_ptr - header_size, 0, header_size);
    *((char *)obj_ptr - 1) = 1 + ((header_size - 1) << 1);

    // TODO: Handle writing header with pointer field offsets larger than 7
    printf("pointer field offset larger than 7 is not yet handled\n");
    exit(-1);
}

size_t
Heap::header_size_from_pointer_field_offs(size_t *pointer_field_offs,
                                          size_t pointer_field_offs_len) {
    if (pointer_field_offs == 0)
        return 1;

    auto max_offset = pointer_field_offs[pointer_field_offs_len - 1];

    if (max_offset < 7) {
        return 1;
    }

    if (max_offset < (8 * 0x7f)) {
        return 2 + max_offset / 8;
    }

    // TODO: Handle calculating header size with pointer field offsets larger
    // than (8 * 0x7f)
    printf("pointer field offset larger than %d is not yet handled\n",
           (8 * 0x7f));
    exit(-1);
}

size_t Gc::GEN0_HEAP_SIZE = 2 * 1024;
size_t Gc::GEN1_INITIAL_HEAP_SIZE = 2 * 1024 * 1024;

Gc::Gc() {
    gen0_heap = new Heap(GEN0_HEAP_SIZE);
    gen1_heap = new Heap(GEN1_INITIAL_HEAP_SIZE);

    // TODO: create safepoints_map at compile-time
    safepoints_map = new std::unordered_map<unw_word_t, struct Safepoint *>();
    for (int i = 0; i < safepoints_len; i++) {
        safepoints_map->insert({(unw_word_t)safepoints[i].ip, &safepoints[i]});
    }
}

void *Gc::allocate(size_t size, size_t *pointer_field_offs,
                   size_t pointer_field_offs_len) {
    auto *ptr =
        gen0_heap->allocate(size, pointer_field_offs, pointer_field_offs_len);

    if (ptr == nullptr) {
        printf("Cannot allocate to generation 0 heap\n");
        exit(-1);
    }

    return ptr;
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
