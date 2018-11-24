#include "int.h"

#include <inttypes.h>
#include <stdio.h>
#include <string.h>

#include "none.h"
#include "str.h"

IEO_STATIC_STRING(ieo_int_type_name, "Int");

extern IeoType ieo_std_type_int;

IeoResult
ieo_int_new(ieo_int_t val)
{
  IeoInt *p = (IeoInt *)ieo_term_alloc(&ieo_std_type_int);
  if (!p)
    return IEO_FAIL(ieo_none());

  p->val = val;

  return IEO_SUCCT(p);
}

IEO_PURE IeoResult
ieo_is_int(IEO_NOTNULL IeoTerm *term)
{
  IEO_ASSERT(term);
  IEO_ASSERT(term->head.ty);
  return IEO_BOOL(term->head.ty == &ieo_std_type_int);
}

extern inline IEO_PURE ieo_int_t
ieo_int_value(IEO_NOTNULL IeoTerm *p);

IeoResult
ieo_int(IEO_NOTNULL IeoTerm *x)
{
  IEO_STATIC_STRING(not_implemented,
                    "Method __int__ is not implemented for this term.");
  IeoOpUnary *f = ieo_term_ty(x)->to_int_func;
  return f ? f(x) : IEO_FAILT(&not_implemented);
}

#define INT_TO_STR_MAX_LEN 32

static IeoResult
to_str_func(IEO_NOTNULL IeoTerm *x)
{
  char buff[INT_TO_STR_MAX_LEN];
  snprintf(buff, INT_TO_STR_MAX_LEN, "%" PRIdIEO, ieo_int_value(x));
  return ieo_string_new(buff, strlen(buff));
}

IeoType ieo_std_type_int = {
  .type_name = IEO_STP(ieo_int_type_name),
  .term_size = sizeof(IeoInt),
  .deleter = ieo_term_deleter,
  .to_int_func = &ieo_succ,
  .to_str_func = &to_str_func,
};
