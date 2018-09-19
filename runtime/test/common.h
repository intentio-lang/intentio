#include <setjmp.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <runtime/src/term.h>
#include <runtime/src/util.h>

#include <cmocka.h>

#define assert_ieo_ok(EXPR) assert_ieo_ok_cond(EXPR, 1);
#define assert_ieo_err(EXPR) assert_ieo_err_cond(EXPR, 1);

#define assert_ieo_ok_cond(EXPR, COND) assert_ieo_X_cond_impl(EXPR, 1, COND);
#define assert_ieo_err_cond(EXPR, COND) assert_ieo_X_cond_impl(EXPR, 0, COND);

#define assert_ieo_X_cond_impl(EXPR, SUCC, COND)                                                   \
  do {                                                                                             \
    IeoResult tmp__ = (EXPR);                                                                      \
    if ((SUCC)) {                                                                                  \
      if (!IEO_OK(tmp__)) {                                                                        \
        fail_msg("expected SUCC result");                                                          \
      }                                                                                            \
    } else {                                                                                       \
      if (!IEO_ERR(tmp__)) {                                                                       \
        fail_msg("expected FAIL result");                                                          \
      }                                                                                            \
    }                                                                                              \
    do {                                                                                           \
      IEO_UNUSED IeoTerm *it = (IeoTerm *)tmp__.term;                                              \
      if (!(COND)) {                                                                               \
        fail_msg("the condition `" IEO_STRINGIZE(COND) "` failed");                                \
        return;                                                                                    \
      }                                                                                            \
    } while (0);                                                                                   \
  } while (0)

#define TRY_UNWRAP(TERM_VAR, EXPR)                                                                 \
  do {                                                                                             \
    IeoResult tmp__ = (EXPR);                                                                      \
    if (IEO_OK(tmp__)) {                                                                           \
      (TERM_VAR) = tmp__.term;                                                                     \
    } else {                                                                                       \
      fail_msg("TRY_UNWRAP: expected SUCC result");                                                \
      return;                                                                                      \
    }                                                                                              \
  } while (0)
