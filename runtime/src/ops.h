#pragma once

#include "term.h"

IeoResult
ieo_neg(IEO_NOTNULL IeoTerm *self);

IeoResult
ieo_add(IEO_NOTNULL IeoTerm *lhs, IEO_NOTNULL IeoTerm *rhs);

IeoResult
ieo_div(IEO_NOTNULL IeoTerm *lhs, IEO_NOTNULL IeoTerm *rhs);

IeoResult
ieo_mul(IEO_NOTNULL IeoTerm *lhs, IEO_NOTNULL IeoTerm *rhs);

IeoResult
ieo_sub(IEO_NOTNULL IeoTerm *lhs, IEO_NOTNULL IeoTerm *rhs);

IeoResult
ieo_eq(IEO_NOTNULL IeoTerm *lhs, IEO_NOTNULL IeoTerm *rhs);

IeoResult
ieo_gt(IEO_NOTNULL IeoTerm *lhs, IEO_NOTNULL IeoTerm *rhs);

IeoResult
ieo_gteq(IEO_NOTNULL IeoTerm *lhs, IEO_NOTNULL IeoTerm *rhs);

IeoResult
ieo_lt(IEO_NOTNULL IeoTerm *lhs, IEO_NOTNULL IeoTerm *rhs);

IeoResult
ieo_lteq(IEO_NOTNULL IeoTerm *lhs, IEO_NOTNULL IeoTerm *rhs);

IeoResult
ieo_neq(IEO_NOTNULL IeoTerm *lhs, IEO_NOTNULL IeoTerm *rhs);

IeoResult
ieo_compare(IEO_NOTNULL IeoTerm *lhs, IEO_NOTNULL IeoTerm *rhs);

IeoResult
ieo_seq(IEO_NOTNULL IeoTerm *lhs, IEO_NOTNULL IeoTerm *rhs);

IeoResult
ieo_sneq(IEO_NOTNULL IeoTerm *lhs, IEO_NOTNULL IeoTerm *rhs);
