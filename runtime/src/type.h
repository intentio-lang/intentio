#pragma once

#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>

#include "util.h"

typedef struct IeoTerm IeoTerm;
typedef struct IeoResult IeoResult;
typedef struct IeoString IeoString;

typedef IeoResult(IeoOpUnary)(IEO_NOTNULL IeoTerm *self);

typedef IeoResult(IeoOpBinary)(IEO_NOTNULL IeoTerm *self,
                               IEO_NOTNULL IeoTerm *rhs);

typedef struct IeoType
{
  /**
   * Type name. Preferably the term should be static if declared by
   * native code.
   */
  IEO_NOTNULL IeoString *type_name;

  /**
   * Size of term data, including header, in bytes.
   *
   * This is the size used by ::ieo_term_alloc & family functions.
   */
  size_t term_size;

  /**
   * Free term memory after itself.
   *
   * In 90% cases, this would point to the ::ieo_term_deleter function.
   * Overriding it is useful if the term contains owned memory (to free it) or
   * other terms (to unref them). This method should either be or call directly
   * or indirectly the mentioned ::ieo_term_deleter function, which calls
   * `ieo_free()` on the term itself and also may apply some debug or
   * performance hooks.
   */
  IEO_NOTNULL void (*deleter)(IEO_NULLABLE IeoTerm *self);

  /**
   * @defgroup IeoTypeMethodPointers Method pointers
   * @{
   */

  /**
   * @defgroup IeoTypeMathMethods Mathematical operators implementations
   * @{
   */

  /**
   * Perform `-self` operation.
   */
  IeoOpUnary *neg_func;

  /**
   * Perform `self + rhs` operation.
   */
  IeoOpBinary *add_func;

  /**
   * Perform `self / rhs` operation.
   */
  IeoOpBinary *div_func;

  /**
   * Perform `self * rhs` operation.
   */
  IeoOpBinary *mul_func;

  /**
   * Perform `self - rhs` operation.
   */
  IeoOpBinary *sub_func;

  /** @} */

  /**
   * @defgroup IeoTypeCompareMethods Comparison operators implementations
   * @{
   */

  /**
   * Perform `self == rhs` operation.
   */
  IeoOpBinary *eq_func;

  /**
   * Perform `self > rhs` operation.
   *
   * Comparison is always performed between terms of same type. If both hand
   * sides have different type, the Term Ordering by Type rule dictates the
   * result of the operation.
   */
  IeoOpBinary *gt_func;

  /**
   * Perform `self >= rhs` operation.
   *
   * Comparison is always performed between terms of same type. If both hand
   * sides have different type, the Term Ordering by Type rule dictates the
   * result of the operation.
   */
  IeoOpBinary *gteq_func;

  /**
   * Perform `self < rhs` operation.
   *
   * Comparison is always performed between terms of same type. If both hand
   * sides have different type, the Term Ordering by Type rule dictates the
   * result of the operation.
   */
  IeoOpBinary *lt_func;

  /**
   * Perform `self <= rhs` operation.
   *
   * Comparison is always performed between terms of same type. If both hand
   * sides have different type, the Term Ordering by Type rule dictates the
   * result of the operation.
   */
  IeoOpBinary *lteq_func;

  /**
   * Perform `self != rhs` operation.
   */
  IeoOpBinary *neq_func;

  /**
   * Compare `self` to `rhs`.
   *
   * Comparison is always performed between terms of same type. If both hand
   * sides have different type, the Term Ordering by Type rule dictates the
   * result of the operation.
   *
   * Returns:
   * - Negative value if lhs appears before rhs.
   * - Zero if lhs and rhs compare equal.
   * - Positive value if lhs appears after rhs.
   *
   * @return IeoInt*
   */
  IeoOpBinary *compare_func;

  /** @} */

  /** @} */
} IeoType;

/**
 * Iterate over all registered types.
 *
 * @param opaque a pointer where the iterator function  will store its state.
 *               Must point to NULL to start the iteration.
 * @return IeoType* the next registered type or NULL when the iteration is
 * finished.
 */
IEO_NULLABLE IeoType *
ieo_type_iterate(IEO_NOTNULL void **opaque);

/**
 * Compare two types in terms of type sorting order.
 *
 * Returns:
 * - Negative value if lhs appears before rhs.
 * - Zero if lhs and rhs compare equal.
 * - Positive value if lhs appears after rhs.
 *
 * @param IeoType* lhs left hand side of comparison
 * @param IeoType* rhs right hand side of comparison
 * @return IeoInt
 */
IEO_PURE int
ieo_type_compare(IEO_NOTNULL const IeoType *lhs,
                 IEO_NOTNULL const IeoType *rhs);
