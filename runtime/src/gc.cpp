#include "gc.hpp"
#include "heap.hpp"

#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <sys/mman.h>

#ifdef OONTA_RT_DEBUG_GC
#include <chrono>

static std::chrono::time_point start_time = std::chrono::system_clock::now();
static std::chrono::time_point collection_start_time =
    std::chrono::system_clock::now();
static long accumulated_collection_time_ms = 0;

#define GC_LOG(fmt, args...)                                                   \
    do {                                                                       \
        fprintf(stderr, fmt "\n", ##args);                                     \
    } while (0)

#define GC_START_LOG(gen)                                                      \
    do {                                                                       \
        collection_start_time = std::chrono::system_clock::now();              \
        auto current_time_ms =                                                 \
            std::chrono::duration_cast<std::chrono::milliseconds>(             \
                collection_start_time - start_time)                            \
                .count();                                                      \
                                                                               \
        GC_LOG("=== Collecting gen " #gen " @%ld ms ===", current_time_ms);    \
    } while (0)

#define GC_COLLECT_STAT_LOG(collected, promoted)                               \
    do {                                                                       \
        auto collection_finish_time = std::chrono::system_clock::now();        \
        auto collection_duration_ms =                                          \
            std::chrono::duration_cast<std::chrono::milliseconds>(             \
                collection_finish_time - collection_start_time)                \
                .count();                                                      \
        accumulated_collection_time_ms += collection_duration_ms;              \
                                                                               \
        GC_LOG("# Collected %.2f MB, Promoted %.2f MB (%.2f %%), in %ld ms",   \
               (float)collected / 1024 / 1024, (float)promoted / 1024 / 1024,  \
               (float)promoted / (promoted + collected) * 100,                 \
               collection_duration_ms);                                        \
        GC_LOG("# Accumulated collection time = %ld ms",                       \
               accumulated_collection_time_ms);                                \
    } while (0)

#define GC_LIMIT_CHANGE_LOG(gen, new_limit)                                    \
    do {                                                                       \
        GC_LOG("# Changed gen " #gen " limit: %.2f MB -> %.2f MB",             \
               (float)heaps[gen]->limit() / 1024 / 1024,                       \
               (float)new_limit / 1024 / 1024);                                \
    } while (0)

#define GC_STAT()                                                              \
    do {                                                                       \
        GC_LOG("# Heap statistics:");                                          \
        GC_LOG("> Gen 0 (%.2f MB / %.2f MB)\n> Gen 1 (%.2f MB / %.2f MB)\n> "  \
               "Gen 2 "                                                        \
               "(%.2f "                                                        \
               "MB / %.2f MB)\n",                                              \
               (float)heaps[0]->usage() / 1024 / 1024,                         \
               (float)heaps[0]->limit() / 1024 / 1024,                         \
               (float)heaps[1]->usage() / 1024 / 1024,                         \
               (float)heaps[1]->limit() / 1024 / 1024,                         \
               (float)heaps[2]->usage() / 1024 / 1024,                         \
               (float)heaps[2]->limit() / 1024 / 1024);                        \
    } while (0)
#else
#define GC_LOG(fmt, args...)                                                   \
    do {                                                                       \
    } while (0)

#define GC_START_LOG(gen)                                                      \
    do {                                                                       \
    } while (0)

#define GC_COLLECT_STAT_LOG(collected, promoted)                               \
    do {                                                                       \
    } while (0)

#define GC_LIMIT_CHANGE_LOG(gen, new_limit)                                    \
    do {                                                                       \
    } while (0)

#define GC_STAT()                                                              \
    do {                                                                       \
    } while (0)
#endif

void Gc::safepoint(unw_cursor_t cursor) {
    size_t collected, promoted;

    unw_cursor_t saved_cursor = cursor;
    this->cursor = &saved_cursor;

    // Collect gen 0
    GC_START_LOG(0);
    std::tie(collected, promoted) = collect();
    GC_COLLECT_STAT_LOG(collected, promoted);

    // Calculate new gen 0 limit
    auto new_limit = 100 / MAX_SURVIVOR_RATE * promoted;
    new_limit =
        (new_limit < GEN0_INITIAL_LIMIT) ? GEN0_INITIAL_LIMIT : new_limit;
    GC_LIMIT_CHANGE_LOG(0, new_limit);
    heaps[0]->set_limit(new_limit);

    GC_STAT();

    gen_to_collect = HeapGenerations::One;
    if (need_collection()) {
        unw_cursor_t saved_cursor = cursor;
        this->cursor = &saved_cursor;
        next_heap = heaps[2];

        // Collect gen 1
        GC_START_LOG(1);
        std::tie(collected, promoted) = collect();
        GC_COLLECT_STAT_LOG(collected, promoted);

        // Calculate new gen 1 limit
        auto new_limit = 100 / MAX_SURVIVOR_RATE * promoted;
        new_limit =
            (new_limit < GEN1_INITIAL_LIMIT) ? GEN1_INITIAL_LIMIT : new_limit;
        GC_LIMIT_CHANGE_LOG(1, new_limit);
        heaps[1]->set_limit(new_limit);

        GC_STAT();

        gen_to_collect = HeapGenerations::Two;
        if (need_collection()) {
            unw_cursor_t saved_cursor = cursor;
            this->cursor = &saved_cursor;

            // Allocate new heap
            auto new_limit = heaps[2]->limit();
            new_limit += new_limit / 100 * GEN2_PERCENTAGE_INCREMENT;
            next_heap = new Heap(MAX_RESERVED_ADDRESS_SPACE, new_limit);

            // Collect gen 2
            GC_START_LOG(2);
            std::tie(collected, promoted) = collect();
            GC_COLLECT_STAT_LOG(collected, promoted);
            GC_LIMIT_CHANGE_LOG(2, new_limit);

            delete heaps[2];
            heaps[2] = next_heap;

            GC_STAT();
        }
    }

    // Reset collection states
    this->cursor = nullptr;
    gen_to_collect = HeapGenerations::Zero;
    next_heap = heaps[1];
}

std::pair<size_t, size_t> Gc::collect() {
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
    size_t promoted_bytes = (next_heap_usage_after - next_heap_usage_before);
    size_t collected_garbage_bytes = target_heap_usage_before - promoted_bytes;
    return {collected_garbage_bytes, promoted_bytes};
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
