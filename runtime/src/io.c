// For getline
#ifdef __STDC_ALLOC_LIB__
#define __STDC_WANT_LIB_EXT2__ 1
#else
#define _POSIX_C_SOURCE 200809L
#endif

#include "io.h"

#include <stdio.h>

#include "none.h"
#include "str.h"
#include "util.h"

IeoResult
ieo_println(IEO_NOTNULL IeoTerm *s)
{
  IeoResult e = IEO_SUCCT(s);

  if (IEO_ERR(ieo_is_string(s))) {
    IEO_TRY(e, ieo_str(s));
    return ieo_println(e.term);
  }

  const char *cstr = ieo_string_c_str(s);
  IEO_ASSERT(cstr != NULL);

  printf("%s\n", cstr);

  return e;
}

IeoResult
ieo_scanln()
{
  IEO_STATIC_STRING(FERR, "Error reading from standard input.");

  char *data = NULL;
  size_t len = 0;
  ssize_t nread = getline(&data, &len, stdin);

  if (nread < 0) {
    free(data);
    return IEO_FAILT(&FERR);
  }

  // Trim trailing \n
  if (nread > 0) {
    nread--;
  }

  IeoResult e = ieo_string_new(data, (size_t)nread);
  free(data);
  return e;
}
