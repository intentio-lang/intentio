#include "common.h"

#include <runtime/src/str.h>

void
__wrap_ieo_term_deleter(IeoTerm *p)
{
  check_expected(p);
  if (!p)
    return;
  ieo_free(p);
}

static void
ieo_term_free_calls_deleter_and_sets_pointer_to_null(IEO_UNUSED void **state)
{
  IeoTerm *term;
  TRY_UNWRAP(term, IEO_STRING_ALLOC("foo"));
  assert_non_null(term);
  expect_value(__wrap_ieo_term_deleter, p, term);
  ieo_term_free(&term);
  assert_null(term);
}

static void
ieo_term_refcount_increments_and_decrements(IEO_UNUSED void **state)
{
  IeoTerm *term;
  TRY_UNWRAP(term, IEO_STRING_ALLOC("foo"));
  assert_int_equal(1, ieo_term_refcount(term));
  ieo_term_ref(term);
  assert_int_equal(2, ieo_term_refcount(term));
  ieo_term_ref(term);
  assert_int_equal(3, ieo_term_refcount(term));
  ieo_term_unref(&term);
  assert_int_equal(2, ieo_term_refcount(term));
  ieo_term_unref(&term);
  assert_int_equal(1, ieo_term_refcount(term));
  expect_value(__wrap_ieo_term_deleter, p, term);
  ieo_term_unref(&term);
  assert_null(term);
}

static void
ieo_term_free_on_static_is_noop(IEO_UNUSED void **state)
{
  IEO_STATIC_STRING_P(str, "foo");
  ieo_term_free(&str);
  assert_non_null(str);
}

static void
ieo_term_refcount_on_static_is_noop(IEO_UNUSED void **state)
{
  IEO_STATIC_STRING_P(str, "foo");
  assert_int_equal(0, ieo_term_refcount(str));
  ieo_term_ref(str);
  assert_int_equal(0, ieo_term_refcount(str));
  ieo_term_unref(&str);
  assert_int_equal(0, ieo_term_refcount(str));
  assert_non_null(str);
}

int
main(void)
{
  const struct CMUnitTest tests[] = {
    cmocka_unit_test(ieo_term_free_calls_deleter_and_sets_pointer_to_null),
    cmocka_unit_test(ieo_term_refcount_increments_and_decrements),
    cmocka_unit_test(ieo_term_free_on_static_is_noop),
    cmocka_unit_test(ieo_term_refcount_on_static_is_noop),
  };

  return cmocka_run_group_tests(tests, NULL, NULL);
}
