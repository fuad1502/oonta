#ifndef GC_H
#define GC_H

#include <cstddef>

class Gc {
public:
  void *allocate(std::size_t size);
};

#endif // GC_H
