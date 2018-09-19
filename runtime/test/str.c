#include "common.h"

#include <runtime/src/int.h>
#include <runtime/src/none.h>
#include <runtime/src/str.h>

static void
static_empty_string(IEO_UNUSED void **state)
{
  IEO_STATIC_STRING(str, "");
  assert_int_equal(ieo_string_size((IeoTerm *)&str), 0);
  assert_ieo_ok(ieo_is_string((IeoTerm *)&str));
}

static void
static_string_size_is_computed_properly(IEO_UNUSED void **state)
{
  IEO_STATIC_STRING(str, "foobar");
  assert_int_equal(ieo_string_size((IeoTerm *)&str), strlen("foobar"));
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
  IEO_STATIC_STRING(empty_static, "");
  IEO_STATIC_STRING(foobar_static, "foobar");
  IeoTerm *empty_alloc;
  TRY_UNWRAP(empty_alloc, IEO_STRING_ALLOC(""));
  IeoTerm *foobar_alloc;
  TRY_UNWRAP(foobar_alloc, IEO_STRING_ALLOC("foobar"));

  assert_ieo_ok(
    ieo_string_equal((IeoTerm *)&empty_static, (IeoTerm *)&empty_static));
  assert_ieo_ok(
    ieo_string_equal((IeoTerm *)&foobar_static, (IeoTerm *)&foobar_static));
  assert_ieo_ok(ieo_string_equal(empty_alloc, empty_alloc));
  assert_ieo_ok(ieo_string_equal(foobar_alloc, foobar_alloc));

  assert_ieo_err(
    ieo_string_equal((IeoTerm *)&empty_static, (IeoTerm *)&foobar_static));
  assert_ieo_err(ieo_string_equal(empty_alloc, foobar_alloc));

  assert_ieo_err(ieo_string_equal(ieo_none().term, (IeoTerm *)&foobar_static));
  assert_ieo_err(ieo_string_equal(empty_alloc, ieo_none().term));
}

static void
string_compare(IEO_UNUSED void **state)
{
  IeoTerm *empty;
  TRY_UNWRAP(empty, IEO_STRING_ALLOC(""));
  IeoTerm *foobar;
  TRY_UNWRAP(foobar, IEO_STRING_ALLOC("foobar"));
  IeoTerm *foobbr;
  TRY_UNWRAP(foobbr, IEO_STRING_ALLOC("foobbr"));

  assert_ieo_ok_cond(ieo_string_compare(empty, empty), ieo_int_value(it) == 0);
  assert_ieo_ok_cond(ieo_string_compare(foobar, foobar),
                     ieo_int_value(it) == 0);
  assert_ieo_ok_cond(ieo_string_compare(foobar, foobbr), ieo_int_value(it) < 0);
  assert_ieo_ok_cond(ieo_string_compare(foobbr, foobar), ieo_int_value(it) > 0);
  assert_ieo_ok_cond(ieo_string_compare(empty, foobar), ieo_int_value(it) < 0);
  assert_ieo_ok_cond(ieo_string_compare(foobar, empty), ieo_int_value(it) > 0);

  assert_ieo_err(ieo_string_compare(empty, ieo_none().term));
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
