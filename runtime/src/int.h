#pragma once

#include <stdint.h>

#include "term.h"

typedef int64_t ieo_int_t;
#define IEO_INT_MIN INT64_MIN
#define IEO_INT_MAX INT64_MAX
#define PRIdIEO PRId64

typedef struct IeoInt
{
  IeoTermHeader head;
  ieo_int_t val;
} IeoInt;

IeoResult
ieo_int_new(ieo_int_t val);

IEO_PURE IeoResult
ieo_is_int(IEO_NOTNULL IeoTerm *term);

inline IEO_PURE ieo_int_t
ieo_int_value(IEO_NOTNULL IeoTerm *p)
{
  IEO_ASSERT(p);
  IEO_ASSERT(IEO_OK(ieo_is_int(p)));
  return ((IeoInt *)p)->val;
}

IeoResult
ieo_int(IEO_NOTNULL IeoTerm *x);
