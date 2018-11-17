#pragma once

#include "term.h"

#define IEO_BOOL(VALUE) IEO_SSET(VALUE, ieo_none())

typedef struct IeoNone
{
  IeoTermHeader head;
} IeoNone;

IEO_CONST IeoResult
ieo_none(void);

IEO_CONST IeoResult
ieo_none_fail(void);

IEO_CONST IeoTerm *
ieo_none_unwrap(void);

IEO_PURE IeoResult
ieo_is_none(IEO_NOTNULL const IeoTerm *term);
