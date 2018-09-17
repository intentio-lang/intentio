#pragma once

#include "term.h"

#define IEO_BOOL(VALUE) IEO_SSET(VALUE, ieo_none())

typedef struct IeoNone
{
  IeoTermHeader head;
} IeoNone;

IEO_PURE IeoResult
ieo_none(void);

IEO_PURE IeoResult
ieo_is_none(IEO_NOTNULL IeoTerm *term);
