#include "gc.hpp"
#include <stdlib.h>

static Gc gc;

extern "C" void *gcmalloc(size_t size) { return gc.allocate(size); }
