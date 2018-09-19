#include "type.h"

#include <stdbool.h>

#include "alltypes.h"

IEO_NULLABLE IeoType *
ieo_type_iterate(IEO_NOTNULL void **opaque)
{
  ieo_assert(opaque);

  size_t i = (size_t)*opaque;
  IeoType *ty = ieo_type_all[i];

  if (ty) {
    *opaque = (void *)(i + 1);
  }

  return ty;
}

IEO_PURE int
ieo_type_compare(IEO_NOTNULL const IeoType *lhs, IEO_NOTNULL const IeoType *rhs)
{
  ieo_assert(lhs);
  ieo_assert(rhs);

  if (lhs == rhs) {
    return 0;
  }

  for (int i = 0; ieo_type_all[i] != NULL; i++) {
    const IeoType *ty = ieo_type_all[i];

    if (ty == lhs) {
      return -1;
    } else if (ty == rhs) {
      return 1;
    }
  }

  ieo_assert(false);
}
