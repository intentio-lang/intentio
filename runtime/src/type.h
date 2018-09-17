#pragma once

#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>

#include "util.h"

typedef struct IeoTerm IeoTerm;
typedef struct IeoString IeoString;

typedef struct IeoType
{
  /// Type name. Preferably the term should be static.
  IEO_NOTNULL IeoString *type_name;

  /// Size of term data, including header, in bytes.
  size_t term_size;

  /// A pointer to function that frees memory after terms of this type.
  IEO_NOTNULL void (*deleter)(IeoTerm *);
} IeoType;

/**
 * @brief Iterate over all registered types.
 *
 * @param opaque a pointer where the iterator function  will store its state.
 *               Must point to NULL to start the iteration.
 * @return IeoType* the next registered type or NULL when the iteration is finished.
 */
IEO_NULLABLE IeoType *
ieo_type_iterate(IEO_NOTNULL void **opaque);
