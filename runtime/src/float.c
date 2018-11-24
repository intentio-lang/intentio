#include "float.h"

#include "str.h"

IeoResult
ieo_float_new(IEO_UNUSED long double val)
{
  IEO_ASSERT(false);
  IeoResult result;
  return result;
}

IeoResult
ieo_float(IEO_NOTNULL IeoTerm *x)
{
  IEO_STATIC_STRING(not_implemented,
                    "Method __float__ is not implemented for this term.");
  IeoOpUnary *f = ieo_term_ty(x)->to_float_func;
  return f ? f(x) : IEO_FAILT(&not_implemented);
}
