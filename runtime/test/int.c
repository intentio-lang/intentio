#include "common.h"

#include <runtime/src/int.h>
#include <runtime/src/none.h>

static void
ieo_int_new_succeeds_and_is_int(IEO_UNUSED void **state)
{
  IeoTerm *t;
  TRY_UNWRAP(t, ieo_int_new(42));
  assert_ieo_ok(ieo_is_int(t));
}

int
main(void)
{
  const struct CMUnitTest tests[] = {
    cmocka_unit_test(ieo_int_new_succeeds_and_is_int),
  };

  return cmocka_run_group_tests(tests, NULL, NULL);
}
