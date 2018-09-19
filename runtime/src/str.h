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
    .value = {                                                                                     \
      .size = (sizeof(STR) / sizeof(char) - 1),                                                    \
      .data = (const char *)(STR)                                                                  \
    }                                                                                              \
  }

#define IEO_STRING_ALLOC(STR) ieo_string_new((STR), sizeof(STR) / sizeof(char) - 1)

typedef struct IeoStringValue
{
  size_t size;
  const char *data;
} IeoStringValue;

/**
 * @brief A string term implementation, conforming to Intentio reference.
 *
 * Strings in Intentio are immutable.
 *
 * @warning NEVER TOUCH FIELDS OF THIS STRUCTURE DIRECTLY, USE ACCESSOR FUNCTIONS!
 */
typedef struct IeoString
{
  IeoTermHeader head;
  IeoStringValue value;
} IeoString;

extern IeoType ieo_std_type_string;

IeoResult
ieo_string_new(const char *str, size_t strsz);

IEO_PURE IeoResult
ieo_is_string(IEO_NOTNULL const IeoTerm *term);

inline IEO_PURE size_t
ieo_string_size(IEO_NOTNULL const IeoTerm *p)
{
  ieo_assert(p);
  ieo_assert(IEO_OK(ieo_is_string(p)));
  return ((IeoString *)p)->value.size;
}

inline IEO_PURE const char *
ieo_string_data(IEO_NOTNULL const IeoTerm *p)
{
  ieo_assert(p);
  ieo_assert(IEO_OK(ieo_is_string(p)));
  return ((IeoString *)p)->value.data;
}

IEO_PURE IeoResult
ieo_string_equal(IEO_NOTNULL const IeoTerm *lhs, IEO_NOTNULL const IeoTerm *rhs);

IEO_PURE IeoResult
ieo_string_compare(IEO_NOTNULL const IeoTerm *lhs, IEO_NOTNULL const IeoTerm *rhs);
