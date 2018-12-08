#include "int.h"

#include <inttypes.h>
#include <stdio.h>
#include <string.h>

#include "float.h"
#include "none.h"
#include "ops.h"
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
ieo_int(IEO_NOTNULL IeoTerm *self)
{
  IEO_STATIC_STRING(not_implemented,
                    "Method __int__ is not implemented for this term.");
  IeoOpUnary *f = ieo_term_ty(self)->to_int_func;
  return f ? f(self) : IEO_FAILT(&not_implemented);
}

static IeoResult
neg_func(IEO_NOTNULL IeoTerm *self)
{
  return ieo_int_new(-ieo_int_value(self));
}

IEO_STATIC_STRING(ERR_ARITH_TYPE_ERROR,
                  "Type error on arithmetic operation with integer.");

#define ARITH_PROMOTE_FLOAT(SELF, V, OP)                                       \
  do {                                                                         \
    if (IEO_OK(ieo_is_float((V)))) {                                           \
      return ieo_##OP((V), (SELF));                                            \
    }                                                                          \
  } while (0)

#define ARITH_FAIL_OTHERWISE(V)                                                \
  do {                                                                         \
    if (IEO_ERR(ieo_is_int((V)))) {                                            \
      return IEO_FAILT(&ERR_ARITH_TYPE_ERROR);                                 \
    }                                                                          \
  } while (0)

static IeoResult
add_func(IEO_NOTNULL IeoTerm *self, IEO_NOTNULL IeoTerm *other)
{
  ARITH_PROMOTE_FLOAT(self, other, add);
  ARITH_FAIL_OTHERWISE(other);
  ieo_int_t a = ieo_int_value(self);
  ieo_int_t b = ieo_int_value(other);
  return ieo_int_new(a + b);
}

static IeoResult
div_func(IEO_NOTNULL IeoTerm *self, IEO_NOTNULL IeoTerm *other)
{
  ARITH_PROMOTE_FLOAT(self, other, div);
  ARITH_FAIL_OTHERWISE(other);
  ieo_int_t a = ieo_int_value(self);
  ieo_int_t b = ieo_int_value(other);
  return ieo_int_new(a / b);
}

static IeoResult
mul_func(IEO_NOTNULL IeoTerm *self, IEO_NOTNULL IeoTerm *other)
{
  ARITH_PROMOTE_FLOAT(self, other, mul);
  ARITH_FAIL_OTHERWISE(other);
  ieo_int_t a = ieo_int_value(self);
  ieo_int_t b = ieo_int_value(other);
  return ieo_int_new(a * b);
}

static IeoResult
sub_func(IEO_NOTNULL IeoTerm *self, IEO_NOTNULL IeoTerm *other)
{
  ARITH_PROMOTE_FLOAT(self, other, sub);
  ARITH_FAIL_OTHERWISE(other);
  ieo_int_t a = ieo_int_value(self);
  ieo_int_t b = ieo_int_value(other);
  return ieo_int_new(a - b);
}

static IeoResult
to_str_func(IEO_NOTNULL IeoTerm *x)
{
  char buff[32];
  snprintf(buff, IEO_COUNT_OF(buff), "%" PRIdIEO, ieo_int_value(x));
  return ieo_string_new(buff, strlen(buff));
}

IeoType ieo_std_type_int = {
  .type_name = IEO_STP(ieo_int_type_name),
  .term_size = sizeof(IeoInt),
  .deleter = ieo_term_deleter,
  .neg_func = &neg_func,
  .add_func = &add_func,
  .div_func = &div_func,
  .mul_func = &mul_func,
  .sub_func = &sub_func,
  .compare_func = &sub_func,
  .to_int_func = &ieo_succ,
  .to_str_func = &to_str_func,
};
