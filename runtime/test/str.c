#include "common.h"

#include <runtime/src/int.h>
#include <runtime/src/none.h>
#include <runtime/src/ops.h>
#include <runtime/src/str.h>

static void
static_empty_string(IEO_UNUSED void **state)
{
  IEO_STATIC_STRING_P(str, "");
  assert_int_equal(ieo_string_size(str), 0);
  assert_ieo_ok(ieo_is_string(str));
}

static void
static_string_size_is_computed_properly(IEO_UNUSED void **state)
{
  IEO_STATIC_STRING_P(str, "foobar");
  assert_int_equal(ieo_string_size(str), strlen("foobar"));
}

static void
allocate_empty_string(IEO_UNUSED void **state)
{
  IeoTerm *str;
  TRY_UNWRAP(str, IEO_STRING_ALLOC(""));
  assert_int_equal(ieo_string_size(str), 0);
  assert_ieo_ok(ieo_is_string((IeoTerm *)str));
}

static void
allocate_nonempty_string(IEO_UNUSED void **state)
{
  IeoTerm *str;
  TRY_UNWRAP(str, IEO_STRING_ALLOC("foobar"));
  assert_int_equal(ieo_string_size(str), strlen("foobar"));
  assert_memory_equal(ieo_string_data(str), "foobar", strlen("foobar"));
}

static void
string_equal(IEO_UNUSED void **state)
{
  IEO_STATIC_STRING_P(empty_static, "");
  IEO_STATIC_STRING_P(foobar_static, "foobar");
  IeoTerm *empty_alloc;
  TRY_UNWRAP(empty_alloc, IEO_STRING_ALLOC(""));
  IeoTerm *foobar_alloc;
  TRY_UNWRAP(foobar_alloc, IEO_STRING_ALLOC("foobar"));

  assert_ieo_ok(ieo_eq(empty_static, empty_static));
  assert_ieo_ok(ieo_eq(foobar_static, foobar_static));
  assert_ieo_ok(ieo_eq(empty_alloc, empty_alloc));
  assert_ieo_ok(ieo_eq(foobar_alloc, foobar_alloc));

  assert_ieo_err(ieo_eq(empty_static, foobar_static));
  assert_ieo_err(ieo_eq(empty_alloc, foobar_alloc));

  assert_ieo_err(ieo_eq(ieo_none_unwrap(), foobar_static));
  assert_ieo_err(ieo_eq(empty_alloc, ieo_none_unwrap()));
}

static void
string_compare(IEO_UNUSED void **state)
{
  IEO_STATIC_STRING_P(empty, "");
  IEO_STATIC_STRING_P(foobar, "foobar");
  IEO_STATIC_STRING_P(foobbr, "foobbr");

  assert_ieo_ok_cond(ieo_compare(empty, empty), ieo_int_value(it) == 0);
  assert_ieo_ok_cond(ieo_compare(foobar, foobar), ieo_int_value(it) == 0);
  assert_ieo_ok_cond(ieo_compare(foobar, foobbr), ieo_int_value(it) < 0);
  assert_ieo_ok_cond(ieo_compare(foobbr, foobar), ieo_int_value(it) > 0);
  assert_ieo_ok_cond(ieo_compare(empty, foobar), ieo_int_value(it) < 0);
  assert_ieo_ok_cond(ieo_compare(foobar, empty), ieo_int_value(it) > 0);

  assert_ieo_ok_cond(ieo_compare(empty, ieo_none_unwrap()),
                     ieo_int_value(it) > 0);

  assert_ieo_ok(ieo_lt(foobar, foobbr));
  assert_ieo_ok(ieo_lteq(foobar, foobbr));
  assert_ieo_ok(ieo_lteq(foobar, foobar));
  assert_ieo_err(ieo_gt(foobar, foobbr));
  assert_ieo_err(ieo_gteq(foobar, foobbr));
  assert_ieo_ok(ieo_gteq(foobar, foobar));
}

int
main(void)
{
  const struct CMUnitTest tests[] = {
    cmocka_unit_test(static_empty_string),
    cmocka_unit_test(static_string_size_is_computed_properly),
    cmocka_unit_test(allocate_empty_string),
    cmocka_unit_test(allocate_nonempty_string),
    cmocka_unit_test(string_equal),
    cmocka_unit_test(string_compare),
  };

  return cmocka_run_group_tests(tests, NULL, NULL);
}
