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

void *Heap::start() {
    return heap;
}

void *Heap::end() {
    return (char *)heap + heap_size;
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

char Heap::usage_percentage() {
    char usage = (100 * heap_offset) / heap_size;
    return usage;
}

/*
 * Header format:
 *                      |xxxxxx0m|obj
 *             |xxxxxxxx|0000011m|obj
 *    |xxxxxxxx|xxxxxxxx|0000101m|obj
 * ...|xxxxxxxx|00000001|1111111m|obj
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
            header |= (2 << (pointer_field_offs[i] + 1));
        }
        *((char *)obj_ptr - 1) = header;
        return;
    }

    memset((char *)obj_ptr - header_size, 0, header_size);
    *((char *)obj_ptr - 1) = 0b10 | ((header_size - 1) << 2);

    // TODO: Handle writing header with pointer field offsets larger than 6
    printf("pointer field offset larger than 6 is not yet handled\n");
    exit(-1);
}

size_t
Heap::header_size_from_pointer_field_offs(size_t *pointer_field_offs,
                                          size_t pointer_field_offs_len) {
    if (pointer_field_offs == 0)
        return 1;

    auto max_offset = pointer_field_offs[pointer_field_offs_len - 1];

    if (max_offset < 6) {
        return 1;
    }

    if (max_offset < (8 * 0x3f)) {
        return 2 + max_offset / 8;
    }

    // TODO: Handle calculating header size with pointer field offsets larger
    // than (8 * 0x3f)
    printf("pointer field offset larger than %d is not yet handled\n",
           (8 * 0x3f));
    exit(-1);
}

size_t Gc::GEN0_HEAP_SIZE = 2 * 1024;
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

std::queue<Location> work_q;
std::unordered_map<void *, void *> relocations;

bool is_moved(void *obj_addr) { return *((char *)obj_addr - 1) & 0b1; }
void set_moved(void *obj_addr) { *((char *)obj_addr - 1) |= 0b1; }

std::vector<size_t> get_pointer_offsets(void *obj_addr);

void *move_to_gen1(void *obj_addr);

bool Gc::is_gen0_addr(void *obj_addr) {
    return (gen0_heap->start() <= obj_addr && obj_addr < gen0_heap->end());
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

void *Gc::get_obj_addr(Location *location) {
    switch (location->type) {
    case DIRECT: {
        unw_word_t reg;
        unw_get_reg(cursor, location->reg, &reg);
        return (void *)(reg + location->offset);
    }
    case INDIRECT: {
        unw_word_t reg;
        unw_get_reg(cursor, location->reg, &reg);
        void **ind_addr = (void **)(reg + location->offset);
        return *ind_addr;
    }
    case CONSTANT:
        return *((void **)location->constant);
    }
}

void Gc::add_pointer_fields_to_work_q(void *obj_addr,
                                      std::queue<Location> work_q) {
    auto offsets = get_pointer_offsets(obj_addr);
    for (auto offset : offsets) {
        Location location = {LocationType::CONSTANT, 0, 0,
                             (size_t)((char *)obj_addr + offset)};
        auto *obj_addr = get_obj_addr(&location);

        if (!is_gen0_addr(obj_addr)) {
            continue;
        }

        if (is_moved(obj_addr)) {
            auto *new_addr = relocations.at(obj_addr);
            relocate(&location, new_addr);
            continue;
        }

        work_q.push(location);
    }
}

void Gc::safepoint(unw_cursor_t *cursor) {
    unw_word_t ip;
    unw_get_reg(cursor, UNW_REG_IP, &ip);

    auto iter = safepoints_map->find(ip);
    assert(iter != safepoints_map->end());
    Safepoint *record = iter->second;

    // Populate work queue from stack map
    for (int i = 0; i < record->num_of_locations; i++) {
        auto location = record->obj_locations[i];
        auto *obj_addr = get_obj_addr(&location);

        if (!is_gen0_addr(obj_addr)) {
            continue;
        }

        work_q.push(location);
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

    // Process work queue
    while (!work_q.empty()) {
        auto location = work_q.front();
        auto *obj_addr = get_obj_addr(&location);
        work_q.pop();

        if (is_moved(obj_addr)) {
            auto *new_addr = relocations.at(obj_addr);
            relocate(&location, new_addr);
        } else {
            auto *new_addr = move_to_gen1(obj_addr);
            relocate(&location, new_addr);
            set_moved(obj_addr);
            add_pointer_fields_to_work_q(new_addr, work_q);
        }
    }
}

bool Gc::need_collection() {
    return gen0_heap->usage_percentage() > COLLECTION_THRESHOLD_PERCENTAGE;
}
