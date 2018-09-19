#include "none.h"

#include "str.h"

IEO_STATIC_STRING(ieo_none_type_name, "None");

IeoType ieo_std_type_none = {
  .type_name = &ieo_none_type_name,
  .term_size = sizeof(IeoNone),
  .deleter = ieo_term_deleter,
};

static IeoNone ieo_none_singleton = { .head = {
                                        .ty = &ieo_std_type_none,
                                        .refcount = 0,
                                        .flags =
                                          {
                                            .is_static = true,
                                          },
                                      } };

IEO_PURE IeoResult
ieo_none(void)
{
  return IEO_SUCCT(&ieo_none_singleton);
}

IEO_PURE IeoResult
ieo_is_none(IEO_NOTNULL IeoTerm *term)
{
  ieo_assert(term != NULL);
  ieo_assert(term->head.ty != NULL);
  return IEO_BOOL(term->head.ty == &ieo_std_type_none);
}
