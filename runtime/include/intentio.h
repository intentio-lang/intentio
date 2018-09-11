#pragma once

#include <stdbool.h>

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
ieo_none();

IeoResult
ieo_int_new(long long int val);

IeoResult
ieo_float_new(long double val);

IeoResult
ieo_char_new(char val);

IeoResult
ieo_string_new(const char *val);

IeoResult
ieo_regex_new(const char *val);

//----------------------------------------------------------------------------
// Runtime metadata functions

IeoResult
ieo_rt_info();

IeoResult
ieo_rt_version_major();

IeoResult
ieo_rt_version_minor();

IeoResult
ieo_rt_version_patch();

IeoResult
ieo_rt_version_str();
