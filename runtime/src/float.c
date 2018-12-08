#include "float.h"

#include <stdio.h>
#include <string.h>

#include "int.h"
#include "none.h"
#include "str.h"

IEO_STATIC_STRING(ieo_float_type_name, "Float");

extern IeoType ieo_std_type_float;

IeoResult
ieo_float_new(ieo_float_t val)
{
  IeoFloat *p = (IeoFloat *)ieo_term_alloc(&ieo_std_type_float);
  if (!p)
    return IEO_FAIL(ieo_none());

  p->val = val;

  return IEO_SUCCT(p);
}

IEO_PURE IeoResult
ieo_is_float(IEO_NOTNULL IeoTerm *term)
{
  IEO_ASSERT(term);
  IEO_ASSERT(term->head.ty);
  return IEO_BOOL(term->head.ty == &ieo_std_type_float);
}

extern inline IEO_PURE ieo_float_t
ieo_float_value(IEO_NOTNULL IeoTerm *p);

IeoResult
ieo_float(IEO_NOTNULL IeoTerm *self)
{
  IEO_STATIC_STRING(not_implemented,
                    "Method __float__ is not implemented for this term.");
  IeoOpUnary *f = ieo_term_ty(self)->to_float_func;
  return f ? f(self) : IEO_FAILT(&not_implemented);
}

static IeoResult
neg_func(IEO_NOTNULL IeoTerm *self)
{
  return ieo_float_new(-ieo_float_value(self));
}

IEO_STATIC_STRING(
  ERR_ARITH_NOT_NUMERIC,
  "Right hand side of the arithmetic operation is not convertible to float.");

#define ARITH_CONVERT_TO_FLOAT(Z, V)                                           \
  do {                                                                         \
    if (IEO_OK(ieo_is_float((V)))) {                                           \
      (Z) = ieo_float_value((V));                                              \
    } else if (IEO_OK(ieo_is_int((V)))) {                                      \
      (Z) = (ieo_float_t)ieo_int_value((V));                                   \
    } else {                                                                   \
      return IEO_FAILT(&ERR_ARITH_NOT_NUMERIC);                                \
    }                                                                          \
  } while (0)

static IeoResult
add_func(IEO_NOTNULL IeoTerm *self, IEO_NOTNULL IeoTerm *other)
{
  ieo_float_t a = ieo_float_value(self);
  ieo_float_t b;
  ARITH_CONVERT_TO_FLOAT(b, other);
  return ieo_float_new(a + b);
}

static IeoResult
div_func(IEO_NOTNULL IeoTerm *self, IEO_NOTNULL IeoTerm *other)
{
  ieo_float_t a = ieo_float_value(self);
  ieo_float_t b;
  ARITH_CONVERT_TO_FLOAT(b, other);
  return ieo_float_new(a / b);
}

static IeoResult
mul_func(IEO_NOTNULL IeoTerm *self, IEO_NOTNULL IeoTerm *other)
{
  ieo_float_t a = ieo_float_value(self);
  ieo_float_t b;
  ARITH_CONVERT_TO_FLOAT(b, other);
  return ieo_float_new(a * b);
}

static IeoResult
sub_func(IEO_NOTNULL IeoTerm *self, IEO_NOTNULL IeoTerm *other)
{
  ieo_float_t a = ieo_float_value(self);
  ieo_float_t b;
  ARITH_CONVERT_TO_FLOAT(b, other);
  return ieo_float_new(a - b);
}

static IeoResult
compare_func(IEO_NOTNULL IeoTerm *self, IEO_NOTNULL IeoTerm *other)
{
  ieo_float_t a = ieo_float_value(self);
  ieo_float_t b = ieo_float_value(other);
  if (a < b) {
    return ieo_int_new(-1);
  } else if (a > b) {
    return ieo_int_new(1);
  } else {
    return ieo_int_new(0);
  }
}

static IeoResult
to_str_func(IEO_NOTNULL IeoTerm *x)
{
  char buff[32];
  snprintf(buff, IEO_COUNT_OF(buff), "%" PRIfIEO, ieo_float_value(x));
  return ieo_string_new(buff, strlen(buff));
}

IeoType ieo_std_type_float = {
  .type_name = IEO_STP(ieo_float_type_name),
  .term_size = sizeof(IeoFloat),
  .deleter = &ieo_term_deleter,
  .neg_func = &neg_func,
  .add_func = &add_func,
  .div_func = &div_func,
  .mul_func = &mul_func,
  .sub_func = &sub_func,
  .compare_func = &compare_func,
  .to_float_func = &ieo_succ,
  .to_str_func = &to_str_func,
};
