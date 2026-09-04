#ifndef HEAP_H
#define HEAP_H

#include <cstddef>

class Heap {
  private:
    static size_t LIMIT_PERCENTAGE;

    void *heap;
    size_t heap_size;
    size_t heap_offset;
    size_t heap_limit;

    static void write_header(void *obj_ptr, size_t *type_info);

  public:
    Heap(size_t size);
    ~Heap();
    void *allocate(size_t *type_info);
    void reset();
    void *start() const;
    void *end() const;
    size_t usage() const;
    bool need_collection() const;

    static bool is_moved(void *obj_addr);
    static void set_moved(void *obj_addr, void *new_addr);
    static size_t get_obj_size(void *obj_addr);
    static size_t *get_type_info(void *obj_addr);
    static void *get_forwarding_ptr(void *obj_addr);
};

#endif // HEAP_H
