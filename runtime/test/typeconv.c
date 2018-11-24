#include "common.h"

#include <runtime/src/int.h>
#include <runtime/src/none.h>
#include <runtime/src/str.h>
#include <runtime/src/util.h>

#define INT_TO_STR_SUCC_TEST(NAME, INT_VAL, STR_VAL)                           \
  static void NAME(IEO_UNUSED void **state)                                    \
  {                                                                            \
    IeoTerm *x;                                                                \
    TRY_UNWRAP(x, ieo_int_new((INT_VAL)));                                     \
    IeoResult r = ieo_str(x);                                                  \
    assert_ieo_ok(r);                                                          \
    assert_ieo_ok(ieo_is_string(r.term));                                      \
    assert_string_equal(ieo_string_c_str(r.term), (STR_VAL));                  \
  }

INT_TO_STR_SUCC_TEST(int_to_str_zero, 0, "0")
INT_TO_STR_SUCC_TEST(int_to_str_one, 1, "1")
INT_TO_STR_SUCC_TEST(int_to_str_minus_one, -1, "-1")
INT_TO_STR_SUCC_TEST(int_to_str_min, IEO_INT_MIN, "-9223372036854775808")
INT_TO_STR_SUCC_TEST(int_to_str_max, IEO_INT_MAX, "9223372036854775807")

#define STR_TO_INT_SUCC_TEST(NAME, STR_VAL, INT_VAL)                           \
  static void NAME(IEO_UNUSED void **state)                                    \
  {                                                                            \
    IEO_STATIC_STRING(s, (STR_VAL));                                           \
    IeoResult r = ieo_int(IEO_STP(s));                                         \
    assert_ieo_ok(r);                                                          \
    assert_ieo_ok(ieo_is_int(r.term));                                         \
    assert_int_equal(ieo_int_value(r.term), (INT_VAL));                        \
  }

#define STR_TO_INT_FAIL_TEST(NAME, STR_VAL)                                    \
  static void NAME(IEO_UNUSED void **state)                                    \
  {                                                                            \
    IEO_STATIC_STRING(s, (STR_VAL));                                           \
    IeoResult r = ieo_int(IEO_STP(s));                                         \
    assert_ieo_err(r);                                                         \
  }

STR_TO_INT_SUCC_TEST(str_to_int_zero, "0", 0)
STR_TO_INT_SUCC_TEST(str_to_int_zero_ws, "  0  ", 0)
STR_TO_INT_SUCC_TEST(str_to_int_one, "1", 1)
STR_TO_INT_SUCC_TEST(str_to_int_minus_one, "-1", -1)
// STR_TO_INT_SUCC_TEST(str_to_int_min, "-9223372036854775808", IEO_INT_MIN)
STR_TO_INT_SUCC_TEST(str_to_int_max, "9223372036854775807", IEO_INT_MAX)

STR_TO_INT_FAIL_TEST(str_to_int_empty, "")
STR_TO_INT_FAIL_TEST(str_to_only_ws, "    ")
STR_TO_INT_FAIL_TEST(str_to_alpha, "abc")
STR_TO_INT_FAIL_TEST(str_to_alphanum, "1a0")

int
main(void)
{
  const struct CMUnitTest tests[] = {
    cmocka_unit_test(int_to_str_zero),
    cmocka_unit_test(int_to_str_one),
    cmocka_unit_test(int_to_str_minus_one),
    cmocka_unit_test(int_to_str_min),
    cmocka_unit_test(int_to_str_max),
    cmocka_unit_test(str_to_int_zero),
    cmocka_unit_test(str_to_int_zero_ws),
    cmocka_unit_test(str_to_int_one),
    cmocka_unit_test(str_to_int_minus_one),
    // cmocka_unit_test(str_to_int_min),
    cmocka_unit_test(str_to_int_max),
    cmocka_unit_test(str_to_int_empty),
    cmocka_unit_test(str_to_only_ws),
    cmocka_unit_test(str_to_alpha),
    cmocka_unit_test(str_to_alphanum),
  };

  return cmocka_run_group_tests(tests, NULL, NULL);
}
