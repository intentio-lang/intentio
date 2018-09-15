#pragma once

#include <stdlib.h>

#include "attrs.h"

IEO_MALLOC IEO_WARN_UNUSED_RESULT IEO_ALLOC_SIZE(1) inline void *ieo_malloc(size_t size)
{
  return malloc(size);
}

IEO_MALLOC IEO_WARN_UNUSED_RESULT IEO_ALLOC_SIZE(1) inline void *ieo_mallocz(size_t size)
{
  return calloc(1, size);
}

IEO_MALLOC IEO_WARN_UNUSED_RESULT IEO_ALLOC_SIZE(1, 2) inline void *ieo_calloc(size_t num,
                                                                               size_t size)
{
  return calloc(num, size);
}

IEO_WARN_UNUSED_RESULT IEO_ALLOC_SIZE(2) inline void *ieo_realloc(void *ptr, size_t size)
{
  return realloc(ptr, size);
}

inline void
ieo_free(void *ptr)
{
  free(ptr);
}

inline void
ieo_freep(void **ptr)
{
  ieo_free(*ptr);
  *ptr = NULL;
}
