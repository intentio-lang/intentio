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
ieo_is_int(IEO_NOTNULL IeoTerm *term);

inline IEO_PURE int64_t
ieo_int_value(IEO_NOTNULL IeoTerm *p)
{
  IEO_ASSERT(p);
  IEO_ASSERT(IEO_OK(ieo_is_int(p)));
  return ((IeoInt *)p)->val;
}

IeoResult
ieo_int(IEO_NOTNULL IeoTerm *x);
