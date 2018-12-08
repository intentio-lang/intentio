#pragma once

#include "term.h"

typedef long double ieo_float_t;
#define PRIfIEO "Lf"

typedef struct IeoFloat
{
  IeoTermHeader head;
  ieo_float_t val;
} IeoFloat;

IeoResult
ieo_float_new(ieo_float_t val);

IEO_PURE IeoResult
ieo_is_float(IEO_NOTNULL IeoTerm *term);

inline IEO_PURE ieo_float_t
ieo_float_value(IEO_NOTNULL IeoTerm *p)
{
  IEO_ASSERT(p);
  IEO_ASSERT(IEO_OK(ieo_is_float(p)));
  return ((IeoFloat *)p)->val;
}

IeoResult
ieo_float(IEO_NOTNULL IeoTerm *x);
