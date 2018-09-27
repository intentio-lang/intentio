#include "typeck.h"

#include <stdio.h>
#include <stdlib.h>

void
ieo_typeck_fail_impl_(const char *expr,
                      const char *type_name,
                      const char *file,
                      int line,
                      const char *func)
{
  fprintf(stderr, "Intentio Type Error: %s is not a %s.\n", expr, type_name);
  if (file) {
    fprintf(stderr, "... in file %s:%d\n", file, line);
  }
  if (func) {
    fprintf(stderr, "... in function %s\n", func);
  }
  fprintf(stderr, "\n");
  fflush(stderr);
  abort();
}
