#ifndef HEAP_H
#define HEAP_H

#include <cstddef>
#include <vector>

class Heap {
  private:
    void *heap;
    size_t heap_size;
    size_t heap_offset;

    void write_header(void *obj_ptr, size_t *type_info);

  public:
    Heap(size_t size);
    ~Heap();
    void *allocate(size_t *type_info);
    void reset();
    void *start();
    void *end();
    size_t usage();
    char usage_percentage();

    static bool is_moved(void *obj_addr);
    static void set_moved(void *obj_addr, void *new_addr);
    static size_t get_obj_size(void *obj_addr);
    static std::vector<size_t> get_pointer_offsets(void *obj_addr);
    static size_t *get_type_info(void *obj_addr);
    static void *get_forwarding_ptr(void *obj_addr);
};

#endif // HEAP_H
