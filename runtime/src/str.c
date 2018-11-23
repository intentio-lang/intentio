#include "str.h"

#include <ctype.h>
#include <errno.h>
#include <inttypes.h>
#include <string.h>

#include "int.h"
#include "none.h"
#include "util.h"

IEO_STATIC_STRING(ieo_str_type_name, "String");

/**
 * Special structure of allocated strings.
 *
 * Allocated strings contain character data right next to the term, the usage of
 * the `value` fields ensures correct alignment. Freeing allocated string also
 * frees its data, as the memory is allocated in one chunk. This type is
 * internal to string allocator, as string data is normally accessible via
 * `data` pointer.
 */
typedef struct IeoStringAllocated
{
  IeoString str;
  const char value[];
} IeoStringAllocated;

extern IeoType ieo_std_type_string;

IeoResult
ieo_string_new(const char *str, size_t strsz)
{
  // Allocate base storage for string term + storage for string data + one byte
  // for NULL character
  IeoStringAllocated *p =
    (IeoStringAllocated *)ieo_term_alloc_s(&ieo_std_type_string, strsz + 1);
  if (!p)
    return IEO_FAIL(ieo_none());

  p->str.value.size = strsz;
  p->str.value.data = p->value;

  memcpy((void *)p->value, str, strsz);

  return IEO_SUCCT(IEO_TP(p));
}

IEO_PURE IeoResult
ieo_is_string(IEO_NOTNULL IeoTerm *term)
{
  IEO_ASSERT(term);
  IEO_ASSERT(term->head.ty);
  return IEO_BOOL(term->head.ty == &ieo_std_type_string);
}

extern inline IEO_PURE size_t
ieo_string_size(IEO_NOTNULL IeoTerm *p);

extern inline IEO_PURE const char *
ieo_string_data(IEO_NOTNULL IeoTerm *p);

extern inline IEO_PURE const char *
ieo_string_c_str(IEO_NOTNULL IeoTerm *p);

IeoResult
ieo_str(IEO_NOTNULL IeoTerm *x)
{
  IEO_STATIC_STRING(not_implemented,
                    "Method __str__ is not implemented for this term.");
  IeoOpUnary *f = ieo_term_ty(x)->to_str_func;
  return f ? f(x) : IEO_FAILT(&not_implemented);
}

static IEO_PURE IeoResult
add_func(IeoTerm *self, IeoTerm *other)
{
  IEO_ASSERT(IEO_OK(ieo_is_string(self)));
  IEO_TYPECK(other, string);

  const IeoStringValue *lhs = ieo_term_value(self);
  const IeoStringValue *rhs = ieo_term_value(other);

  IeoTerm *result;
  IEO_TRY_UNWRAP(result, ieo_string_new("", lhs->size + rhs->size));

  const IeoStringValue *res = ieo_term_value(result);
  memcpy((void *)res->data, lhs->data, lhs->size);
  memcpy((void *)(res->data + lhs->size), rhs->data, rhs->size);

  return IEO_SUCCT(result);
}

static IEO_PURE IeoResult
eq_impl(IEO_NOTNULL IeoTerm *lhs, IEO_NOTNULL IeoTerm *rhs)
{
  IEO_ASSERT(IEO_OK(ieo_is_string(lhs)));
  IEO_ASSERT(IEO_OK(ieo_is_string(rhs)));

  if (lhs == rhs) {
    return ieo_none();
  }

  const IeoStringValue *lhs_val = ieo_term_value(lhs);
  const IeoStringValue *rhs_val = ieo_term_value(rhs);

  if (lhs_val->size != rhs_val->size) {
    return IEO_FAIL(ieo_none());
  }

  if (strncmp(lhs_val->data, rhs_val->data, lhs_val->size) != 0) {
    return IEO_FAIL(ieo_none());
  }

  return ieo_none();
}

static IEO_PURE IeoResult
eq_func(IeoTerm *self, IeoTerm *rhs)
{
  IEO_ASSERT(IEO_OK(ieo_is_string(self)));
  return !IEO_OK(ieo_is_string(rhs)) ? IEO_BOOL(false) : eq_impl(self, rhs);
}

static IEO_PURE IeoResult
neq_func(IeoTerm *self, IeoTerm *rhs)
{
  IEO_ASSERT(IEO_OK(ieo_is_string(self)));
  return IEO_OK(ieo_is_string(rhs)) ? IEO_NOT(eq_impl(self, rhs))
                                    : IEO_BOOL(true);
}

static IEO_PURE IeoResult
compare_func(IEO_NOTNULL IeoTerm *lhs, IEO_NOTNULL IeoTerm *rhs)
{
  IEO_ASSERT(IEO_OK(ieo_is_string(lhs)));
  IEO_ASSERT(IEO_OK(ieo_is_string(rhs)));

  if (lhs == rhs) {
    return ieo_int_new(0);
  }

  const IeoStringValue *lhs_val = ieo_term_value(lhs);
  const IeoStringValue *rhs_val = ieo_term_value(rhs);

  size_t common_size = MIN(lhs_val->size, rhs_val->size);
  int cmp = strncmp(lhs_val->data, rhs_val->data, common_size);

  if (cmp == 0) {
    if (lhs_val->size < rhs_val->size) {
      cmp = -1;
    } else if (lhs_val->size > rhs_val->size) {
      cmp = 1;
    }
  }

  return ieo_int_new(cmp);
}

static IeoResult
to_int_func(IEO_NOTNULL IeoTerm *x)
{
  IEO_STATIC_STRING(
    ERR_UNDERFLOW,
    "Cannot convert integer to string because it is out of range (underflow).");
  IEO_STATIC_STRING(
    ERR_OVERFLOW,
    "Cannot convert integer to string because it is out of range (overflow).");
  IEO_STATIC_STRING(ERR_INCONVERTIBLE,
                    "Cannot convert integer to string because it does not "
                    "represent numeric value.");

  const char *data = ieo_string_c_str(x);
  char *end = (char *)data;
  ieo_int_t n = strtoimax(data, &end, 10);

  if (errno = ERANGE && n == INTMAX_MIN) {
    return IEO_FAILT(&ERR_UNDERFLOW);
  } else if (errno = ERANGE && n == INTMAX_MIN) {
    return IEO_FAILT(&ERR_OVERFLOW);
  } else if (n == 0 && end == data) {
    return IEO_FAILT(&ERR_INCONVERTIBLE);
  } else if (*end != '\0') {
    while (*end != '\0') {
      if (!isspace(*end)) {
        return IEO_FAILT(&ERR_INCONVERTIBLE);
      }
      end++;
    }

    return ieo_int_new(n);
  } else {
    return ieo_int_new(n);
  }
}

IeoType ieo_std_type_string = {
  .type_name = IEO_STP(ieo_str_type_name),
  .term_size = sizeof(IeoStringAllocated),
  .deleter = ieo_term_deleter,
  .add_func = add_func,
  .eq_func = eq_func,
  .neq_func = neq_func,
  .compare_func = compare_func,
  .to_int_func = &to_int_func,
  .to_str_func = &ieo_succ,
};
