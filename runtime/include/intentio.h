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
ieo_int_new(int64_t val);

IeoResult
ieo_float_new(long double val);

IeoResult
ieo_char_new(char val);

IeoResult
ieo_string_new(const char *str, size_t strsz);

IeoResult
ieo_regex_new(const char *val);

//----------------------------------------------------------------------------
// Operators

IeoResult
ieo_neg(const IeoTerm *self);

IeoResult
ieo_add(const IeoTerm *lhs, const IeoTerm *rhs);

IeoResult
ieo_div(const IeoTerm *lhs, const IeoTerm *rhs);

IeoResult
ieo_mul(const IeoTerm *lhs, const IeoTerm *rhs);

IeoResult
ieo_sub(const IeoTerm *lhs, const IeoTerm *rhs);

IeoResult
ieo_eq(const IeoTerm *lhs, const IeoTerm *rhs);

IeoResult
ieo_gt(const IeoTerm *lhs, const IeoTerm *rhs);

IeoResult
ieo_gteq(const IeoTerm *lhs, const IeoTerm *rhs);

IeoResult
ieo_lt(const IeoTerm *lhs, const IeoTerm *rhs);

IeoResult
ieo_lteq(const IeoTerm *lhs, const IeoTerm *rhs);

IeoResult
ieo_neq(const IeoTerm *lhs, const IeoTerm *rhs);

IeoResult
ieo_compare(const IeoTerm *lhs, const IeoTerm *rhs);

IeoResult
ieo_seq(const IeoTerm *lhs, const IeoTerm *rhs);

IeoResult
ieo_sneq(const IeoTerm *lhs, const IeoTerm *rhs);

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
