#include "common.h"

#include <runtime/src/none.h>
#include <runtime/src/ops.h>
#include <runtime/src/str.h>

static void
ieo_none_always_succeeds_and_is_none(IEO_UNUSED void **state)
{
  IeoTerm *t;
  TRY_UNWRAP(t, ieo_none());
  assert_ieo_ok(ieo_is_none(t));
}

#define TEST_MATH_OP(OP)                                                       \
  static void OP##_with_none(IEO_UNUSED void **state)                          \
  {                                                                            \
    IeoTerm *none;                                                             \
    TRY_UNWRAP(none, ieo_none());                                              \
    assert_ieo_ok_cond(ieo_##OP(none, none), IEO_OK(ieo_is_none(it)));         \
  }                                                                            \
                                                                               \
  static void OP##_with_other(IEO_UNUSED void **state)                         \
  {                                                                            \
    IeoTerm *none, *other;                                                     \
    TRY_UNWRAP(none, ieo_none());                                              \
    TRY_UNWRAP(other, IEO_STRING_ALLOC("hello"));                              \
    assert_ieo_err(ieo_##OP(none, other));                                     \
  }

TEST_MATH_OP(add)
TEST_MATH_OP(div)
TEST_MATH_OP(mul)
TEST_MATH_OP(sub)

#define TEST_COMPARISON_OP(OP, STATE_WITH_NONE, STATE_WITH_OTHER)              \
  static void OP##_with_none(IEO_UNUSED void **state)                          \
  {                                                                            \
    IeoTerm *none;                                                             \
    TRY_UNWRAP(none, ieo_none());                                              \
    assert_ieo_X(ieo_##OP(none, none), STATE_WITH_NONE);                       \
  }                                                                            \
                                                                               \
  static void OP##_with_other(IEO_UNUSED void **state)                         \
  {                                                                            \
    IeoTerm *none, *other;                                                     \
    TRY_UNWRAP(none, ieo_none());                                              \
    TRY_UNWRAP(other, IEO_STRING_ALLOC("hello"));                              \
    assert_ieo_X(ieo_##OP(none, other), STATE_WITH_OTHER);                     \
  }

TEST_COMPARISON_OP(eq, 1, 0)
TEST_COMPARISON_OP(neq, 0, 1)
TEST_COMPARISON_OP(gt, 0, 0)
TEST_COMPARISON_OP(gteq, 1, 0)
TEST_COMPARISON_OP(lt, 0, 1)
TEST_COMPARISON_OP(lteq, 1, 1)

int
main(void)
{
  const struct CMUnitTest tests[] = {
    cmocka_unit_test(ieo_none_always_succeeds_and_is_none),

    cmocka_unit_test(add_with_none),
    cmocka_unit_test(add_with_other),
    cmocka_unit_test(div_with_none),
    cmocka_unit_test(div_with_other),
    cmocka_unit_test(mul_with_none),
    cmocka_unit_test(mul_with_other),
    cmocka_unit_test(sub_with_none),
    cmocka_unit_test(sub_with_other),

    cmocka_unit_test(eq_with_none),
    cmocka_unit_test(eq_with_other),
    cmocka_unit_test(neq_with_none),
    cmocka_unit_test(neq_with_other),
    cmocka_unit_test(gt_with_none),
    cmocka_unit_test(gt_with_other),
    cmocka_unit_test(gteq_with_none),
    cmocka_unit_test(gteq_with_other),
    cmocka_unit_test(lt_with_none),
    cmocka_unit_test(lt_with_other),
    cmocka_unit_test(lteq_with_none),
    cmocka_unit_test(lteq_with_other),
  };

  return cmocka_run_group_tests(tests, NULL, NULL);
}
