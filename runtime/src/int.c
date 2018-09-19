#include "int.h"

#include "none.h"
#include "str.h"

IEO_STATIC_STRING(ieo_int_type_name, "Int");

IeoType ieo_std_type_int = {
  .type_name = &ieo_int_type_name,
  .term_size = sizeof(IeoInt),
  .deleter = ieo_term_deleter,
};

IeoResult
ieo_int_new(int64_t val)
{
  IeoInt *p = (IeoInt *)ieo_term_alloc(&ieo_std_type_int);
  if (!p)
    return IEO_FAIL(ieo_none());

  p->val = val;

  return IEO_SUCCT(p);
}

IEO_PURE IeoResult
ieo_is_int(IEO_NOTNULL const IeoTerm *term)
{
  ieo_assert(term);
  ieo_assert(term->head.ty);
  return IEO_BOOL(term->head.ty == &ieo_std_type_int);
}

extern inline IEO_PURE int64_t
ieo_int_value(IEO_NOTNULL const IeoTerm *p);
