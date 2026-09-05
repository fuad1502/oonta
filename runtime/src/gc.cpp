#include "gc.hpp"
#include "heap.hpp"

#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <sys/mman.h>

void Gc::safepoint(unw_cursor_t cursor) {
    size_t collected_garbage;

    // Collect gen 0
    unw_cursor_t saved_cursor = cursor;
    this->cursor = &saved_cursor;
    collected_garbage = collect();

    // Calculate new gen 0 limit
    auto new_limit =
        100 / MAX_SURVIVOR_RATE * (heaps[0]->limit() - collected_garbage);
    new_limit =
        (new_limit < GEN0_INITIAL_LIMIT) ? GEN0_INITIAL_LIMIT : new_limit;
    heaps[0]->set_limit(new_limit);

    gen_to_collect = HeapGenerations::One;
    if (need_collection()) {
        // Collect gen 1
        unw_cursor_t saved_cursor = cursor;
        this->cursor = &saved_cursor;
        next_heap = heaps[2];
        collected_garbage = collect();

        // Calculate new gen 1 limit
        auto new_limit =
            100 / MAX_SURVIVOR_RATE * (heaps[1]->limit() - collected_garbage);
        new_limit =
            (new_limit < GEN1_INITIAL_LIMIT) ? GEN1_INITIAL_LIMIT : new_limit;
        heaps[1]->set_limit(new_limit);

        gen_to_collect = HeapGenerations::Two;
        if (need_collection()) {
            // Collect gen 2
            unw_cursor_t saved_cursor = cursor;
            this->cursor = &saved_cursor;

            // Allocate new heap
            auto new_limit = heaps[2]->limit();
            new_limit += new_limit / 100 * GEN2_PERCENTAGE_INCREMENT;
            next_heap = new Heap(MAX_RESERVED_ADDRESS_SPACE, new_limit);

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

            work_q.push_back(location);
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
        work_q.push_back(location);
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
        auto location = work_q.back();
        auto *obj_addr = get_obj_addr(&location);
        work_q.pop_back();

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

void Gc::add_pointer_fields_to_work_q(void *obj_addr) {
    auto *type_info = Heap::get_type_info(obj_addr);

    for (int i = 0; i < type_info[1]; i++) {
        Location location = {LocationType::CONSTANT, 0, 0,
                             (size_t)((uint8_t *)obj_addr + type_info[2 + i])};
        auto *obj_addr = get_obj_addr(&location);

        if (!is_addr_in_gen_to_collect(obj_addr)) {
            continue;
        }

        if (Heap::is_moved(obj_addr)) {
            auto *new_addr = Heap::get_forwarding_ptr(obj_addr);
            relocate(&location, new_addr);
            continue;
        }

        work_q.push_back(location);
    }
}
