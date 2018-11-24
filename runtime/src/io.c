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
  IEO_ASSERT(false);
  IeoResult e;
  return e;
}
