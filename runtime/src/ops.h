#pragma once

#include "term.h"

IeoResult
ieo_neg(IEO_NOTNULL const IeoTerm *self);

IeoResult
ieo_add(IEO_NOTNULL const IeoTerm *lhs, IEO_NOTNULL const IeoTerm *rhs);

IeoResult
ieo_div(IEO_NOTNULL const IeoTerm *lhs, IEO_NOTNULL const IeoTerm *rhs);

IeoResult
ieo_mul(IEO_NOTNULL const IeoTerm *lhs, IEO_NOTNULL const IeoTerm *rhs);

IeoResult
ieo_sub(IEO_NOTNULL const IeoTerm *lhs, IEO_NOTNULL const IeoTerm *rhs);

IeoResult
ieo_eq(IEO_NOTNULL const IeoTerm *lhs, IEO_NOTNULL const IeoTerm *rhs);

IeoResult
ieo_gt(IEO_NOTNULL const IeoTerm *lhs, IEO_NOTNULL const IeoTerm *rhs);

IeoResult
ieo_gteq(IEO_NOTNULL const IeoTerm *lhs, IEO_NOTNULL const IeoTerm *rhs);

IeoResult
ieo_lt(IEO_NOTNULL const IeoTerm *lhs, IEO_NOTNULL const IeoTerm *rhs);

IeoResult
ieo_lteq(IEO_NOTNULL const IeoTerm *lhs, IEO_NOTNULL const IeoTerm *rhs);

IeoResult
ieo_neq(IEO_NOTNULL const IeoTerm *lhs, IEO_NOTNULL const IeoTerm *rhs);

IeoResult
ieo_compare(IEO_NOTNULL const IeoTerm *lhs, IEO_NOTNULL const IeoTerm *rhs);

IeoResult
ieo_seq(IEO_NOTNULL const IeoTerm *lhs, IEO_NOTNULL const IeoTerm *rhs);

IeoResult
ieo_sneq(IEO_NOTNULL const IeoTerm *lhs, IEO_NOTNULL const IeoTerm *rhs);
