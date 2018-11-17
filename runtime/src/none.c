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
  return IEO_SUCCT(IEO_STP(ieo_none_singleton));
}

IEO_CONST IeoResult
ieo_none_fail(void)
{
  return IEO_FAILT(IEO_STP(ieo_none_singleton));
}

IEO_CONST IeoTerm *
ieo_none_unwrap(void)
{
  return IEO_STP(ieo_none_singleton);
}

IEO_PURE IeoResult
ieo_is_none(IEO_NOTNULL const IeoTerm *term)
{
  IEO_ASSERT(term != NULL);
  IEO_ASSERT(term->head.ty != NULL);
  return IEO_BOOL(term->head.ty == &ieo_std_type_none);
}

static IEO_PURE IeoResult
unary_math_func(const IeoTerm *self)
{
  IEO_ASSERT(IEO_OK(ieo_is_none(self)));
  return ieo_none();
}

static IEO_PURE IeoResult
binary_math_func(const IeoTerm *self, const IeoTerm *other)
{
  IEO_ASSERT(IEO_OK(ieo_is_none(self)));
  IEO_TYPECK(other, none);
  return ieo_none();
}

static IEO_PURE IeoResult
eq_func(const IeoTerm *self, const IeoTerm *other)
{
  return IEO_BOOL(IEO_OK(ieo_is_none(self)) && IEO_OK(ieo_is_none(other)));
}

static IEO_PURE IeoResult
neq_func(const IeoTerm *self, const IeoTerm *other)
{
  return IEO_BOOL(IEO_ERR(ieo_is_none(self)) || IEO_ERR(ieo_is_none(other)));
}

static IEO_PURE IeoResult
compare_func(const IeoTerm *self, const IeoTerm *other)
{
  IEO_ASSERT(IEO_OK(ieo_is_none(self)));
  IEO_ASSERT(IEO_OK(ieo_is_none(other)));
  return ieo_int_new(0);
}

IeoType ieo_std_type_none = {
  .type_name = &ieo_none_type_name,
  .term_size = sizeof(IeoNone),
  .deleter = ieo_term_deleter,
  .neg_func = unary_math_func,
  .add_func = binary_math_func,
  .div_func = binary_math_func,
  .mul_func = binary_math_func,
  .sub_func = binary_math_func,
  .eq_func = eq_func,
  .neq_func = neq_func,
  .compare_func = compare_func,
};
