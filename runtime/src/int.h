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
