#include "common.h"

#include <runtime/src/none.h>

static void
ieo_none_always_succeeds_and_is_none(IEO_UNUSED void **state)
{
  IeoTerm *t;
  TRY_UNWRAP(t, ieo_none());
  assert_ieo_ok(ieo_is_none(t));
}

int
main(void)
{
  const struct CMUnitTest tests[] = {
    cmocka_unit_test(ieo_none_always_succeeds_and_is_none),
  };

  return cmocka_run_group_tests(tests, NULL, NULL);
}
