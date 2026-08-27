#include "gc.hpp"
#include <cstdio>
#include <stdlib.h>

static Gc gc{};

extern "C" void *gcmalloc(size_t size, size_t *pointer_field_offs,
                          size_t pointer_field_offs_len) {
    return gc.allocate(size, pointer_field_offs, pointer_field_offs_len);
}

extern "C" void gcsafepoint() {
    unw_cursor_t cursor;
    unw_context_t uc;

    unw_getcontext(&uc);
    unw_init_local(&cursor, &uc);
    unw_step(&cursor);

    gc.safepoint(&cursor);
}
