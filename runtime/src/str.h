#pragma once

#include "term.h"

#define IEO_STATIC_STRING(NAME, STR) static IeoString NAME = IEO_STATIC_STRING_DECL(STR)

#define IEO_STATIC_STRING_DECL(STR)                                                                \
  {                                                                                                \
    .head =                                                                                        \
      {                                                                                            \
        .ty = &ieo_std_type_string,                                                                \
        .refcount = 0,                                                                             \
        .flags =                                                                                   \
          {                                                                                        \
            .is_static = true,                                                                     \
          },                                                                                       \
      },                                                                                           \
    .size = (sizeof(STR) / sizeof(char) - 1), .data = (const char *)(STR)                          \
  }

#define IEO_STRING_ALLOC(STR) ieo_string_new((STR), sizeof(STR) / sizeof(char) - 1)

typedef struct IeoString
{
  IeoTermHeader head;
  size_t size;
  const char *data;
} IeoString;

extern IeoType ieo_std_type_string;

IeoResult
ieo_string_new(const char *str, size_t strsz);

IEO_PURE IeoResult
ieo_is_string(IEO_NOTNULL IeoTerm *term);

IEO_PURE IeoResult
ieo_string_equal(IEO_NOTNULL IeoTerm *lhs, IEO_NOTNULL IeoTerm *rhs);

IEO_PURE IeoResult
ieo_string_compare(IEO_NOTNULL IeoTerm *lhs, IEO_NOTNULL IeoTerm *rhs);