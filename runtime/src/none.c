#include "none.h"

#include "int.h"
#include "str.h"

extern IeoType ieo_std_type_none;

IEO_STATIC_STRING(ieo_none_type_name, "None");

static IeoNone ieo_none_singleton = { .head = {
                                        .ty = &ieo_std_type_none,
                                        .refcount = 0,
                                        .flags =
                                          {
                                            .is_static = true,
                                          },
                                      } };

IEO_CONST IeoResult
ieo_none(void)
{
  return IEO_SUCCT(&ieo_none_singleton);
}

IEO_PURE IeoResult
ieo_is_none(IEO_NOTNULL const IeoTerm *term)
{
  ieo_assert(term != NULL);
  ieo_assert(term->head.ty != NULL);
  return IEO_BOOL(term->head.ty == &ieo_std_type_none);
}

IEO_PURE IeoResult
ieo_none_neg(IEO_NOTNULL const IeoTerm *self)
{
  ieo_assert(IEO_OK(ieo_is_none(self)));
  return ieo_none();
}

IEO_PURE IeoResult
ieo_none_add(IEO_NOTNULL const IeoTerm *self, IEO_NOTNULL const IeoTerm *other)
{
  ieo_assert(IEO_OK(ieo_is_none(self)));
  IEO_TRY_(ieo_is_none(other));
  return ieo_none();
}

IEO_PURE IeoResult
ieo_none_div(IEO_NOTNULL const IeoTerm *self, IEO_NOTNULL const IeoTerm *other)
{
  ieo_assert(IEO_OK(ieo_is_none(self)));
  IEO_TRY_(ieo_is_none(other));
  return ieo_none();
}

IEO_PURE IeoResult
ieo_none_mul(IEO_NOTNULL const IeoTerm *self, IEO_NOTNULL const IeoTerm *other)
{
  ieo_assert(IEO_OK(ieo_is_none(self)));
  IEO_TRY_(ieo_is_none(other));
  return ieo_none();
}

IEO_PURE IeoResult
ieo_none_sub(IEO_NOTNULL const IeoTerm *self, IEO_NOTNULL const IeoTerm *other)
{
  ieo_assert(IEO_OK(ieo_is_none(self)));
  IEO_TRY_(ieo_is_none(other));
  return ieo_none();
}

IEO_PURE IeoResult
ieo_none_eq(IEO_NOTNULL const IeoTerm *self, IEO_NOTNULL const IeoTerm *other)
{
  return IEO_BOOL(IEO_OK(ieo_is_none(self)) && IEO_OK(ieo_is_none(other)));
}

IEO_PURE IeoResult
ieo_none_neq(IEO_NOTNULL const IeoTerm *self, IEO_NOTNULL const IeoTerm *other)
{
  return IEO_BOOL(IEO_ERR(ieo_is_none(self)) || IEO_ERR(ieo_is_none(other)));
}

IEO_PURE IeoResult
ieo_none_compare(IEO_NOTNULL const IeoTerm *self,
                 IEO_NOTNULL const IeoTerm *other)
{
  ieo_assert(IEO_OK(ieo_is_none(self)));
  ieo_assert(IEO_OK(ieo_is_none(other)));
  return ieo_int_new(0);
}

IeoType ieo_std_type_none = {
  .type_name = &ieo_none_type_name,
  .term_size = sizeof(IeoNone),
  .deleter = ieo_term_deleter,
  .neg_func = ieo_none_neg,
  .add_func = ieo_none_add,
  .div_func = ieo_none_div,
  .mul_func = ieo_none_mul,
  .sub_func = ieo_none_sub,
  .eq_func = ieo_none_eq,
  .neq_func = ieo_none_neq,
  .compare_func = ieo_none_compare,
};
