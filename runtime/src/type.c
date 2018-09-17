#include "type.h"

#include <assert.h>

#include "alltypes.h"

IEO_NULLABLE IeoType *
ieo_type_iterate(IEO_NOTNULL void **opaque)
{
  assert(opaque != NULL);

  size_t i = (size_t)*opaque;
  IeoType *ty = ieo_type_all[i];

  if (ty) {
    *opaque = (void *)(i + 1);
  }

  return ty;
}
