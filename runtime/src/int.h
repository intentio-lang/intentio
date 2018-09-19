#pragma once

#include "term.h"

typedef struct IeoInt
{
  IeoTermHeader head;
  int64_t val;
} IeoInt;

IeoResult
ieo_int_new(int64_t val);

IEO_PURE IeoResult
ieo_is_int(IEO_NOTNULL const IeoTerm *term);

inline IEO_PURE int64_t
ieo_int_value(IEO_NOTNULL const IeoTerm *p)
{
  ieo_assert(p);
  ieo_assert(IEO_OK(ieo_is_int(p)));
  return ((IeoInt *)p)->val;
}
