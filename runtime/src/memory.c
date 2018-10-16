#include "memory.h"

extern inline void *
ieo_malloc(size_t size);

extern inline void *
ieo_mallocz(size_t size);

extern inline void *
ieo_calloc(size_t num, size_t size);

extern inline void *
ieo_realloc(void *ptr, size_t size);

extern inline void
ieo_free(void *ptr);

extern inline void
ieo_freep(void **ptr);
