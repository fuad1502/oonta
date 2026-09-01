#include "gc.hpp"
#include <cstdio>
#include <stdlib.h>

static Gc gc{};

char gcneedcollection = 0;

extern "C" void *gcmalloc(size_t size, size_t *type_info) {
    void *ptr = gc.allocate(type_info);

    if (gc.need_collection()) {
        gcneedcollection = 1;
    }

    return ptr;
}

extern "C" void gcsafepoint() {
    unw_cursor_t cursor;
    unw_context_t uc;

    unw_getcontext(&uc);
    unw_init_local(&cursor, &uc);
    unw_step(&cursor);

    gc.safepoint(&cursor);

    gcneedcollection = 0;
}
