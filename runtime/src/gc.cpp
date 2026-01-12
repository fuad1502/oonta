#include "gc.hpp"
#include <stdlib.h>

void *Gc::allocate(size_t size) { return malloc(size); }
