#include "gc.hpp"
#include <cstdio>
#include <stdlib.h>

#define UNW_LOCAL_ONLY
#include "libunwind.h"

static Gc gc{};

extern "C" void *gcmalloc(size_t size) { return gc.allocate(size); }

extern "C" void gcsafepoint() {
    unw_cursor_t cursor;
    unw_context_t uc;

    unw_getcontext(&uc);
    unw_init_local(&cursor, &uc);
    unw_step(&cursor);

    gc.safepoint(&cursor);
}
