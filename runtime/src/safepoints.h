/*
 * This file is copied from https://github.com/fuad1502/llvm-stackmap-parser/src/safepoints.h 
 */

#ifndef __SAFEPOINTS_H
#define __SAFEPOINTS_H

#include <stddef.h>
#include <stdint.h>

enum LocationType { DIRECT, INDIRECT, CONSTANT };

struct Location {
  enum LocationType type;
  uint16_t reg;
  int32_t offset;
  size_t constant;
};

struct Safepoint {
  void *ip;
  uint64_t stack_size;
  uint32_t num_of_locations;
  struct Location *obj_locations;
};

extern struct Safepoint safepoints[];

extern int safepoints_len;

extern void *global_gcroots[];

extern int global_gcroots_len;

#endif // __SAFEPOINTS_H
