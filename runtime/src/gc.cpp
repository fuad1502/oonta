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

void *Heap::allocate(size_t *type_info) {
    size_t size = type_info[0];

    auto total_size = size + 8;

    if (heap_offset + total_size > heap_size) {
        return nullptr;
    }

    auto *obj_ptr = (uint8_t *)heap + heap_offset + 8;

    write_header(obj_ptr, type_info);

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

bool Heap::is_moved(void *obj_addr) {
    size_t *header = (size_t *)obj_addr - 1;

    return (*header & 0b1);
}

void Heap::set_moved(void *obj_addr, void *new_addr) {
    size_t *header = (size_t *)obj_addr - 1;

    *header = 0b1;
    *header |= ((size_t)new_addr << 1);
}

size_t Heap::get_obj_size(void *obj_addr) {
    size_t *header = (size_t *)obj_addr - 1;
    assert((*header & 0b1) == 0);
    size_t *type_info = (size_t *)(*header >> 1);

    return type_info[0];
}

std::vector<size_t> Heap::get_pointer_offsets(void *obj_addr) {
    size_t *header = (size_t *)obj_addr - 1;
    assert((*header & 0b1) == 0);
    size_t *type_info = (size_t *)(*header >> 1);

    std::vector<size_t> offsets;
    for (int i = 0; i < type_info[1]; i++) {
        offsets.push_back(type_info[2 + i]);
    }

    return offsets;
}

size_t *Heap::get_type_info(void *obj_addr) {
    size_t *header = (size_t *)obj_addr - 1;
    assert((*header & 0b1) == 0);

    return (size_t *)(*header >> 1);
}

void *Heap::get_forwarding_ptr(void *obj_addr) {
    size_t *header = (size_t *)obj_addr - 1;
    assert((*header & 0b1) == 1);

    return (void *)(*header >> 1);
}

/*
 * Header format:
 * |xxxxxxxx|xxxxxxxx|xxxxxxxx|xxxxxxxx|xxxxxxxx|xxxxxxxx|xxxxxxxx|xxxxxxxm|obj
 *
 * m = 0 -> not moved, xxx... = type info pointer
 * m = 1 -> moved, xxx... = new location
 */
void Heap::write_header(void *obj_ptr, size_t *type_info) {
    size_t *header = (size_t *)obj_ptr - 1;
    *header = (size_t)type_info << 1;
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
}

void *Gc::allocate(size_t *type_info) {
    auto *ptr = gen0_heap->allocate(type_info);

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
            auto *new_addr = Heap::get_forwarding_ptr(obj_addr);
            relocate(&location, new_addr);
        } else if (is_gen0_addr(obj_addr)) {
            auto *new_addr = move_to_gen1(obj_addr);
            relocate(&location, new_addr);
            Heap::set_moved(obj_addr, new_addr);
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
    size_t *type_info = Heap::get_type_info(obj_addr);
    size_t size = type_info[0];
    auto *new_addr = gen1_heap->allocate(type_info);
    memcpy(new_addr, obj_addr, size);
    return new_addr;
}

void Gc::add_pointer_fields_to_work_q(void *obj_addr) {
    auto offsets = Heap::get_pointer_offsets(obj_addr);

    for (auto offset : offsets) {
        Location location = {LocationType::CONSTANT, 0, 0,
                             (size_t)((uint8_t *)obj_addr + offset)};
        auto *obj_addr = get_obj_addr(&location);

        if (!is_gen0_addr(obj_addr)) {
            continue;
        }

        if (Heap::is_moved(obj_addr)) {
            auto *new_addr = Heap::get_forwarding_ptr(obj_addr);
            relocate(&location, new_addr);
            continue;
        }

        work_q.push(location);
    }
}
