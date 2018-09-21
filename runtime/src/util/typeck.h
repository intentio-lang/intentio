#pragma once

#include "attrs.h"

#ifdef NDEBUG

#define IEO_TYPECK(TERM_EXPR, TYPE_NAME)                                       \
  (IEO_OK(ieo_is_##TYPE_NAME((TERM_EXPR)))                                     \
     ? ((void)0)                                                               \
     : ieo_typeck_fail_impl_(#TERM_EXPR, #TYPE_NAME, NULL, 0, NULL))

#else

#define IEO_TYPECK(TERM_EXPR, TYPE_NAME)                                       \
  (IEO_OK(ieo_is_##TYPE_NAME((TERM_EXPR)))                                     \
     ? ((void)0)                                                               \
     : ieo_typeck_fail_impl_(                                                  \
         #TERM_EXPR, #TYPE_NAME, __FILE__, __LINE__, __func__))

#endif

void
ieo_typeck_fail_impl_(const char *expr,
                      const char *type_name,
                      const char *file,
                      int line,
                      const char *func) IEO_NORETURN;
