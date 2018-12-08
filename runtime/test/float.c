#include "common.h"

#include <runtime/src/float.h>
#include <runtime/src/none.h>

static void
ieo_float_new_succeeds_and_is_float(IEO_UNUSED void **state)
{
  IeoTerm *t;
  TRY_UNWRAP(t, ieo_float_new(42));
  assert_ieo_ok(ieo_is_float(t));
}

int
main(void)
{
  const struct CMUnitTest tests[] = {
    cmocka_unit_test(ieo_float_new_succeeds_and_is_float),
  };

  return cmocka_run_group_tests(tests, NULL, NULL);
}
