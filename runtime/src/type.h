#pragma once

#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>

#include "util.h"

typedef struct IeoTerm IeoTerm;
typedef struct IeoResult IeoResult;
typedef struct IeoString IeoString;

typedef IeoResult(IeoOpUnary)(IEO_NOTNULL const IeoTerm *self);

typedef IeoResult(IeoOpBinary)(IEO_NOTNULL const IeoTerm *self,
                               IEO_NOTNULL const IeoTerm *rhs);

typedef struct IeoType
{
  /**
   * @brief Type name. Preferably the term should be static if declared by
   * native code.
   */
  IEO_NOTNULL IeoString *type_name;

  /**
   * @brief Size of term data, including header, in bytes.
   *
   * This is the size used by ::ieo_term_alloc & family functions.
   */
  size_t term_size;

  /**
   * @brief Free term memory after itself.
   *
   * In 90% cases, this would point to the ::ieo_term_deleter function.
   * Overriding it is useful if the term contains owned memory. This method
   * should either be or call directly or indirectly the mentioned
   * ::ieo_term_deleter function, which calls `ieo_free()` on the term itself
   * and also may apply some debug or performance hooks.
   */
  IEO_NOTNULL void (*deleter)(IEO_NULLABLE IeoTerm *self);

  /**
   * @brief Perform `-self` operation.
   */
  IeoOpUnary *neg_func;

  /**
   * @brief Perform `self + rhs` operation.
   */
  IeoOpBinary *add_func;

  /**
   * @brief Perform `self / rhs` operation.
   */
  IeoOpBinary *div_func;

  /**
   * @brief Perform `self == rhs` operation.
   *
   * If this method is not defined, the IeoType::compare_func method is tried.
   */
  IeoOpBinary *eq_func;

  /**
   * @brief Perform `self > rhs` operation.
   *
   * If this method is not defined, the IeoType::compare_func method is tried.
   */
  IeoOpBinary *gt_func;

  /**
   * @brief Perform `self >= rhs` operation.
   *
   * If this method is not defined, the IeoType::compare_func method is tried.
   */
  IeoOpBinary *gteq_func;

  /**
   * @brief Perform `self < rhs` operation.
   *
   * If this method is not defined, the IeoType::compare_func method is tried.
   */
  IeoOpBinary *lt_func;

  /**
   * @brief Perform `self <= rhs` operation.
   *
   * If this method is not defined, the IeoType::compare_func method is tried.
   */
  IeoOpBinary *lteq_func;

  /**
   * @brief Perform `self * rhs` operation.
   *
   * If this method is not defined, the IeoType::compare_func method is tried.
   */
  IeoOpBinary *mul_func;

  /**
   * @brief Perform `self != rhs` operation.
   *
   * If this method is not defined, the IeoType::compare_func method is tried.
   */
  IeoOpBinary *neq_func;

  /**
   * @brief Perform `self - rhs` operation.
   *
   * If this method is not defined, the IeoType::compare_func method is tried.
   */
  IeoOpBinary *sub_func;

  /**
   * @brief Compare `self` to `rhs`.
   *
   * Returns:
   * - Negative value if lhs appears before rhs.
   * - Zero if lhs and rhs compare equal.
   * - Positive value if lhs appears after rhs.
   *
   * @return IeoInt*
   */
  IeoOpBinary *compare_func;
} IeoType;

/**
 * @brief Iterate over all registered types.
 *
 * @param opaque a pointer where the iterator function  will store its state.
 *               Must point to NULL to start the iteration.
 * @return IeoType* the next registered type or NULL when the iteration is
 * finished.
 */
IEO_NULLABLE IeoType *
ieo_type_iterate(IEO_NOTNULL void **opaque);

/**
 * @brief Compare two types in terms of type sorting order.
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
