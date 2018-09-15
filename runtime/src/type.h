#pragma once

#include <stdint.h>

typedef struct IeoTerm IeoTerm;

typedef struct IeoType
{
  /// Size of value data in bytes.
  uint8_t value_size;

  /// A pointer to function that frees memory after terms of this type.
  void (*deleter)(IeoTerm *);
} IeoType;
