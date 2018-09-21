#include "common.h"

#include <runtime/src/none.h>
#include <runtime/src/type.h>

static void
iteration(IEO_UNUSED void **state)
{
  int count = 0;
  bool found_none = false;

  void *opaque = 0;
  IeoType *ty;
  while ((ty = ieo_type_iterate(&opaque))) {
    count++;
    found_none = found_none || ty == ieo_none_unwrap()->head.ty;
  }

  assert_null(ty);
  assert_true(count > 1);
  assert_true(found_none);
}

int
main(void)
{
  const struct CMUnitTest tests[] = {
    cmocka_unit_test(iteration),
  };

  return cmocka_run_group_tests(tests, NULL, NULL);
}
