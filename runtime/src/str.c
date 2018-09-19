#include "str.h"

#include <string.h>

#include "int.h"
#include "none.h"
#include "util.h"

IEO_STATIC_STRING(ieo_str_type_name, "String");

/**
 * @brief Special structure of allocated strings.
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

IeoType ieo_std_type_string = {
  .type_name = &ieo_str_type_name,
  .term_size = sizeof(IeoStringAllocated),
  .deleter = ieo_term_deleter,
};

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

  return IEO_SUCCT(p);
}

IEO_PURE IeoResult
ieo_is_string(IEO_NOTNULL const IeoTerm *term)
{
  ieo_assert(term);
  ieo_assert(term->head.ty);
  return IEO_BOOL(term->head.ty == &ieo_std_type_string);
}

extern inline IEO_PURE size_t
ieo_string_size(IEO_NOTNULL const IeoTerm *p);

extern inline IEO_PURE const char *
ieo_string_data(IEO_NOTNULL const IeoTerm *p);

IEO_PURE IeoResult
ieo_string_equal(IEO_NOTNULL const IeoTerm *lhs, IEO_NOTNULL const IeoTerm *rhs)
{
  IEO_TRY_(ieo_is_string(lhs));
  IEO_TRY_(ieo_is_string(rhs));

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

IEO_PURE IeoResult
ieo_string_compare(IEO_NOTNULL const IeoTerm *lhs,
                   IEO_NOTNULL const IeoTerm *rhs)
{
  IEO_TRY_(ieo_is_string(lhs));
  IEO_TRY_(ieo_is_string(rhs));

  if (lhs == rhs) {
    return ieo_int_new(0);
  }

  const IeoStringValue *lhs_val = ieo_term_value(lhs);
  const IeoStringValue *rhs_val = ieo_term_value(rhs);

  int cmp =
    strncmp(lhs_val->data, rhs_val->data, MIN(lhs_val->size, rhs_val->size));

  if (cmp == 0) {
    if (lhs_val->size < rhs_val->size) {
      cmp = -1;
    } else if (lhs_val->size > rhs_val->size) {
      cmp = 1;
    }
  }

  return ieo_int_new(cmp);
}
