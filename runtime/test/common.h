#include <setjmp.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <runtime/src/str.h>
#include <runtime/src/term.h>
#include <runtime/src/util.h>

#include <cmocka.h>

#define assert_ieo_ok(EXPR) assert_ieo_ok_cond(EXPR, 1)
#define assert_ieo_err(EXPR) assert_ieo_err_cond(EXPR, 1)

#define assert_ieo_ok_cond(EXPR, COND) assert_ieo_X_cond(EXPR, 1, COND)
#define assert_ieo_err_cond(EXPR, COND) assert_ieo_X_cond(EXPR, 0, COND)

#define assert_ieo_X(EXPR, SUCC) assert_ieo_X_cond(EXPR, SUCC, 1)

#define assert_ieo_X_cond(EXPR, SUCC, COND)                                    \
  do {                                                                         \
    IeoResult tmp__ = (EXPR);                                                  \
    if ((SUCC)) {                                                              \
      if (!IEO_OK(tmp__)) {                                                    \
        assert_fancy_fail("expected SUCC result", tmp__.term);                 \
      }                                                                        \
    } else {                                                                   \
      if (!IEO_ERR(tmp__)) {                                                   \
        assert_fancy_fail("expected FAIL result", tmp__.term);                 \
      }                                                                        \
    }                                                                          \
    do {                                                                       \
      IEO_UNUSED IeoTerm *it = (IeoTerm *)tmp__.term;                          \
      if (!(COND)) {                                                           \
        fail_msg("the condition `" IEO_STRINGIZE(COND) "` failed");            \
        return;                                                                \
      }                                                                        \
    } while (0);                                                               \
  } while (0)

#define assert_fancy_fail(MSG, TERM)                                           \
  do {                                                                         \
    if ((TERM) == NULL) {                                                      \
      fail_msg(MSG ": GOT NULL!");                                             \
    } else if (IEO_OK(ieo_is_string((TERM)))) {                                \
      fail_msg(MSG ": got string `%s`", ieo_string_c_str((TERM)));             \
    } else {                                                                   \
      fail_msg(MSG ": got term of type %s",                                    \
               ieo_string_c_str(ieo_term_ty((TERM))->type_name));              \
    }                                                                          \
  } while (0);

#define TRY_UNWRAP(TERM_VAR, EXPR)                                             \
  do {                                                                         \
    IeoResult tmp__ = (EXPR);                                                  \
    if (IEO_OK(tmp__)) {                                                       \
      (TERM_VAR) = tmp__.term;                                                 \
    } else {                                                                   \
      fail_msg("TRY_UNWRAP: expected SUCC result");                            \
      return;                                                                  \
    }                                                                          \
  } while (0)
