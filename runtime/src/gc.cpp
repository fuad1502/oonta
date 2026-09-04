#include "gc.hpp"
#include "heap.hpp"

#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <sys/mman.h>

size_t Gc::GEN0_HEAP_SIZE = 32 * 1024;
size_t Gc::GEN1_HEAP_SIZE = 2 * 1024 * 1024;
size_t Gc::GEN2_INITIAL_HEAP_SIZE = 64 * 1024 * 1024;
char Gc::COLLECTION_THRESHOLD_PERCENTAGE = 80;
size_t Gc::GEN2_PERCENTAGE_INCREMENT = 20;

Gc::Gc() {
    heaps[0] = new Heap(GEN0_HEAP_SIZE);
    heaps[1] = new Heap(GEN1_HEAP_SIZE);
    heaps[2] = new Heap(GEN2_INITIAL_HEAP_SIZE);
    gen2_heap_size = GEN2_INITIAL_HEAP_SIZE;
    gen_to_collect = HeapGenerations::Zero;
    next_heap = heaps[1];

    // TODO: create safepoints_map at compile-time
    safepoints_map = new std::unordered_map<unw_word_t, struct Safepoint *>();
    for (int i = 0; i < safepoints_len; i++) {
        safepoints_map->insert({(unw_word_t)safepoints[i].ip, &safepoints[i]});
    }
}

void *Gc::allocate(size_t *type_info) {
    auto *ptr = heaps[0]->allocate(type_info);

    return ptr;
}

void Gc::safepoint(unw_cursor_t cursor) {
    size_t collected_garbage;

    // Collect gen 0
    unw_cursor_t saved_cursor = cursor;
    this->cursor = &saved_cursor;
    collected_garbage = collect();

    gen_to_collect = HeapGenerations::One;
    if (need_collection()) {
        // Collect gen 1
        unw_cursor_t saved_cursor = cursor;
        this->cursor = &saved_cursor;
        next_heap = heaps[2];
        collected_garbage = collect();

        gen_to_collect = HeapGenerations::Two;
        if (need_collection()) {
            // Collect gen 2
            unw_cursor_t saved_cursor = cursor;
            this->cursor = &saved_cursor;

            gen2_heap_size += gen2_heap_size / 100 * GEN2_PERCENTAGE_INCREMENT;
            next_heap = new Heap(gen2_heap_size);

            collected_garbage = collect();

            delete heaps[2];
            heaps[2] = next_heap;
        }
    }

    // Reset collection states
    this->cursor = nullptr;
    gen_to_collect = HeapGenerations::Zero;
    next_heap = heaps[1];
}

bool Gc::need_collection() {
    return heap_to_collect()->usage_percentage() >
           COLLECTION_THRESHOLD_PERCENTAGE;
}

size_t Gc::collect() {
    size_t target_heap_usage_before = heap_to_collect()->usage();
    size_t next_heap_usage_before = next_heap->usage();

    while ((size_t)cursor > 0) {
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

            if (!is_addr_in_gen_to_collect(obj_addr)) {
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

        if (!is_addr_in_gen_to_collect(obj_addr)) {
            continue;
        }

        Location location = {LocationType::CONSTANT, 0, 0, (size_t)glb_addr};
        work_q.push(location);
    }

    process_work_q();

    heap_to_collect()->reset();

    size_t next_heap_usage_after = next_heap->usage();
    size_t promoted_obj = (next_heap_usage_after - next_heap_usage_before);
    size_t collected_garbage = target_heap_usage_before - promoted_obj;
    return collected_garbage;
}

void Gc::process_work_q() {
    while (!work_q.empty()) {
        auto location = work_q.front();
        auto *obj_addr = get_obj_addr(&location);
        work_q.pop();

        if (Heap::is_moved(obj_addr)) {
            auto *new_addr = Heap::get_forwarding_ptr(obj_addr);
            relocate(&location, new_addr);
        } else if (is_addr_in_gen_to_collect(obj_addr)) {
            auto *new_addr = copy_obj(obj_addr);
            relocate(&location, new_addr);
            Heap::set_moved(obj_addr, new_addr);
            add_pointer_fields_to_work_q(new_addr);
        }
    }
}

void *Gc::copy_obj(void *obj_addr) {
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

void Gc::add_pointer_fields_to_work_q(void *obj_addr) {
    auto offsets = Heap::get_pointer_offsets(obj_addr);

    for (auto offset : offsets) {
        Location location = {LocationType::CONSTANT, 0, 0,
                             (size_t)((uint8_t *)obj_addr + offset)};
        auto *obj_addr = get_obj_addr(&location);

        if (!is_addr_in_gen_to_collect(obj_addr)) {
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

Heap *Gc::heap_to_collect() {
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

bool Gc::is_addr_in_gen_to_collect(void *obj_addr) {
    return (heap_to_collect()->start() <= obj_addr &&
            obj_addr < heap_to_collect()->end());
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
