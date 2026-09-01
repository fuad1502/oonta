#include "gc.hpp"
#include <cassert>
#include <cerrno>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <sys/mman.h>

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
    // TODO: Handle object size larger than 255
    assert(size <= 255);

    // TODO: Currently `pointer_field_offs` is assumed to be a multiple of 8
    // bytes.

    auto header_size = header_size_from_pointer_field_offs(
        size, pointer_field_offs, pointer_field_offs_len);
    auto total_size = header_size + size;

    if (heap_offset + total_size > heap_size) {
        return nullptr;
    }

    auto *obj_ptr = (uint8_t *)heap + heap_offset + header_size;

    write_header(obj_ptr, size, pointer_field_offs, pointer_field_offs_len);

    heap_offset += total_size;

    return obj_ptr;
}

void Heap::reset() { heap_offset = 0; }

void *Heap::start() { return heap; }

void *Heap::end() { return (uint8_t *)heap + heap_size; }

size_t Heap::usage() { return heap_offset; }

char Heap::usage_percentage() {
    char usage = (100 * heap_offset) / heap_size;
    return usage;
}

bool Heap::is_moved(void *obj_addr) { return *((uint8_t *)obj_addr - 1) & 0b1; }

void Heap::set_moved(void *obj_addr) { *((uint8_t *)obj_addr - 1) |= 0b1; }

size_t Heap::get_obj_size(void *obj_addr) {
    auto header_offsets_size = get_header_offsets_size(obj_addr);

    return *((uint8_t *)obj_addr - header_offsets_size - 2);
}

std::vector<size_t> Heap::get_pointer_offsets(void *obj_addr) {
    std::vector<size_t> offsets;
    auto header_offsets_size = get_header_offsets_size(obj_addr);

    if (header_offsets_size == 0) {
        uint8_t *header_start = (uint8_t *)obj_addr - 1;
        uint8_t header_offsets = *header_start >> 2;
        for (int i = 0; i < 6; i++) {
            if (header_offsets >> i & 0b1) {
                offsets.push_back(i);
            }
        }
    } else {
        uint8_t *offsets_start = (uint8_t *)obj_addr - 2;

        // TODO: Handle writing header with pointer field offsets larger than 6
        printf("pointer field offset larger than 6 is not yet handled\n");
        exit(-1);
    }

    return offsets;
}

/*
 * Header format:
 *                           |ssssssss|xxxxxx0m|obj
 *                  |ssssssss|xxxxxxxx|0000011m|obj
 *         |ssssssss|xxxxxxxx|xxxxxxxx|0000101m|obj
 * ssssssss|........|xxxxxxxx|00000001|1111111m|obj
 *
 * m: 0 -> not moved, 1 -> moved
 * x: 0 -> not pointer, 1 -> pointer
 * s: size
 */
void Heap::write_header(void *obj_ptr, size_t size, size_t *pointer_field_offs,
                        size_t pointer_field_offs_len) {
    auto header_size = header_size_from_pointer_field_offs(
        size, pointer_field_offs, pointer_field_offs_len);

    if (header_size == 2) {
        uint8_t header = 0;
        for (int i = 0; i < pointer_field_offs_len; i++) {
            header |= (2 << (pointer_field_offs[i] + 1));
        }
        *((uint8_t *)obj_ptr - 1) = header;
        *((uint8_t *)obj_ptr - 2) = size;
        return;
    }

    memset((uint8_t *)obj_ptr - header_size, 0, header_size);
    *((uint8_t *)obj_ptr - 1) = 0b10 | ((header_size - 2) << 2);

    // TODO: Handle writing header with pointer field offsets larger than 6
    printf("pointer field offset larger than 6 is not yet handled\n");
    exit(-1);
}

size_t Heap::header_size_from_pointer_field_offs(
    size_t size, size_t *pointer_field_offs, size_t pointer_field_offs_len) {
    if (pointer_field_offs == 0)
        return 2;

    auto max_offset = pointer_field_offs[pointer_field_offs_len - 1];

    if (max_offset < 6) {
        return 2;
    }

    if (max_offset < (8 * 0x3f)) {
        return 3 + max_offset / 8;
    }

    // TODO: Handle calculating header size with pointer field offsets larger
    // than (8 * 0x3f)
    printf("pointer field offset larger than %d is not yet handled\n",
           (8 * 0x3f));
    exit(-1);
}

size_t Heap::get_header_offsets_size(void *obj_addr) {
    uint8_t *header_start = (uint8_t *)obj_addr - 1;
    uint8_t offsets_size_tag = ((*header_start >> 1) & 0b1);

    if (offsets_size_tag == 0) {
        return 0;
    } else {
        uint8_t header_offsets_size = *header_start >> 2;

        if (header_offsets_size == 0x3f) {
            // TODO: Handle calculating header size with pointer field offsets
            // larger than (8 * 0x3f)
            printf("pointer field offset larger than %d is not yet handled\n",
                   (8 * 0x3f));
            exit(-1);
        }
        return header_offsets_size;
    }
}

size_t Gc::GEN0_HEAP_SIZE = 1024;
size_t Gc::GEN1_INITIAL_HEAP_SIZE = 2 * 1024 * 1024;
char Gc::COLLECTION_THRESHOLD_PERCENTAGE = 80;

Gc::Gc() {
    gen0_heap = new Heap(GEN0_HEAP_SIZE);
    gen1_heap = new Heap(GEN1_INITIAL_HEAP_SIZE);

    // TODO: create safepoints_map at compile-time
    safepoints_map = new std::unordered_map<unw_word_t, struct Safepoint *>();
    for (int i = 0; i < safepoints_len; i++) {
        safepoints_map->insert({(unw_word_t)safepoints[i].ip, &safepoints[i]});
    }

    relocations = std::unordered_map<void *, void *>();
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
    size_t gen0_usage_pre = gen0_heap->usage();
    size_t gen1_usage_pre = gen1_heap->usage();

    while ((size_t)cursor > 0) {
        this->cursor = cursor;

        unw_word_t ip;
        unw_get_reg(cursor, UNW_REG_IP, &ip);

        auto iter = safepoints_map->find(ip);
        if (iter == safepoints_map->end()) {
            break;
        }

        Safepoint *record = iter->second;

        // Populate work queue from stack map record
        for (int i = 0; i < record->num_of_locations; i++) {
            auto location = record->obj_locations[i];
            auto *obj_addr = get_obj_addr(&location);

            if (!is_gen0_addr(obj_addr)) {
                continue;
            }

            work_q.push(location);
        }

        process_work_q();

        unw_step(cursor);
    }

    // Populate work queue from global GC roots
    for (int i = 0; i < global_gcroots_len; i++) {
        auto **glb_addr = (void **)global_gcroots[i];
        auto *obj_addr = *glb_addr;

        if (!is_gen0_addr(obj_addr)) {
            continue;
        }

        Location location = {LocationType::CONSTANT, 0, 0, (size_t)glb_addr};
        work_q.push(location);
    }

    process_work_q();

    gen0_heap->reset();
    relocations.clear();

    size_t gen0_usage_post = gen0_heap->usage();
    size_t gen1_usage_post = gen1_heap->usage();
    size_t collected_garbage =
        gen0_usage_pre - (gen1_usage_post - gen1_usage_pre);
}

bool Gc::need_collection() {
    return gen0_heap->usage_percentage() > COLLECTION_THRESHOLD_PERCENTAGE;
}

void *Gc::get_obj_addr(Location *location) {
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

bool Gc::is_gen0_addr(void *obj_addr) {
    return (gen0_heap->start() <= obj_addr && obj_addr < gen0_heap->end());
}

void Gc::process_work_q() {
    while (!work_q.empty()) {
        auto location = work_q.front();
        auto *obj_addr = get_obj_addr(&location);
        work_q.pop();

        if (Heap::is_moved(obj_addr)) {
            auto *new_addr = relocations.at(obj_addr);
            relocate(&location, new_addr);
        } else if (is_gen0_addr(obj_addr)) {
            auto *new_addr = move_to_gen1(obj_addr);
            relocate(&location, new_addr);
            Heap::set_moved(obj_addr);
            add_pointer_fields_to_work_q(new_addr);
        }
    }
}

void Gc::relocate(Location *location, void *new_addr) {
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

void *Gc::move_to_gen1(void *obj_addr) {
    std::vector<size_t> offsets = Heap::get_pointer_offsets(obj_addr);
    size_t size = Heap::get_obj_size(obj_addr);

    auto *new_addr = gen1_heap->allocate(size, offsets.data(), offsets.size());
    memcpy(new_addr, obj_addr, size);

    relocations.insert({obj_addr, new_addr});

    return new_addr;
}

void Gc::add_pointer_fields_to_work_q(void *obj_addr) {
    auto offsets = Heap::get_pointer_offsets(obj_addr);

    for (auto offset : offsets) {
        Location location = {LocationType::CONSTANT, 0, 0,
                             (size_t)((uint8_t *)obj_addr + 8 * offset)};
        auto *obj_addr = get_obj_addr(&location);

        if (!is_gen0_addr(obj_addr)) {
            continue;
        }

        if (Heap::is_moved(obj_addr)) {
            auto *new_addr = relocations.at(obj_addr);
            relocate(&location, new_addr);
            continue;
        }

        work_q.push(location);
    }
}
