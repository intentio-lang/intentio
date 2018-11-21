#pragma once

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

//----------------------------------------------------------------------------
// Core API Data Structures

typedef struct IeoTerm IeoTerm;

struct IeoResult
{
  bool succ;
  IeoTerm *term;
};
typedef struct IeoResult IeoResult;

//----------------------------------------------------------------------------
// Primitive values constructors

IeoResult
ieo_none(void);

IeoResult
ieo_none_fail(void);

IeoResult
ieo_int_new(int64_t val);

IeoResult
ieo_float_new(long double val);

IeoResult
ieo_string_new(const char *str, size_t strsz);

IeoResult
ieo_regex_new(const char *val);

//----------------------------------------------------------------------------
// Operators

IeoResult
ieo_neg(IeoTerm *self);

IeoResult
ieo_add(IeoTerm *lhs, IeoTerm *rhs);

IeoResult
ieo_div(IeoTerm *lhs, IeoTerm *rhs);

IeoResult
ieo_mul(IeoTerm *lhs, IeoTerm *rhs);

IeoResult
ieo_sub(IeoTerm *lhs, IeoTerm *rhs);

IeoResult
ieo_eq(IeoTerm *lhs, IeoTerm *rhs);

IeoResult
ieo_gt(IeoTerm *lhs, IeoTerm *rhs);

IeoResult
ieo_gteq(IeoTerm *lhs, IeoTerm *rhs);

IeoResult
ieo_lt(IeoTerm *lhs, IeoTerm *rhs);

IeoResult
ieo_lteq(IeoTerm *lhs, IeoTerm *rhs);

IeoResult
ieo_neq(IeoTerm *lhs, IeoTerm *rhs);

IeoResult
ieo_compare(IeoTerm *lhs, IeoTerm *rhs);

IeoResult
ieo_seq(IeoTerm *lhs, IeoTerm *rhs);

IeoResult
ieo_sneq(IeoTerm *lhs, IeoTerm *rhs);

//----------------------------------------------------------------------------
// Type conversions

IeoResult
ieo_float(IeoTerm *x);

IeoResult
ieo_int(IeoTerm *x);

IeoResult
ieo_str(IeoTerm *x);

//----------------------------------------------------------------------------
// I/O functions

IeoResult
ieo_println(IeoTerm *x);

IeoResult
ieo_scanln();

//----------------------------------------------------------------------------
// Runtime metadata functions

IeoResult
ieo_rt_info(void);

IeoResult
ieo_rt_version_major(void);

IeoResult
ieo_rt_version_minor(void);

IeoResult
ieo_rt_version_patch(void);

IeoResult
ieo_rt_version_str(void);
