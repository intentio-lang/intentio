#pragma once

#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>

#include "util.h"

typedef struct IeoTerm IeoTerm;
typedef struct IeoString IeoString;

typedef struct IeoType
{
  /**
   * @brief Type name. Preferably the term should be static if declared by native code.
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
   * In 90% cases, this would point to the ::ieo_term_deleter function. Overriding it is useful if
   * the term contains owned memory. This method should either be or call directly or
   * indirectly the mentioned ::ieo_term_deleter function, which calls `ieo_free()` on the term
   * itself and also may apply some debug or performance hooks.
   */
  IEO_NOTNULL void (*deleter)(IEO_NULLABLE IeoTerm *self);

  /**
   * @brief Perform `self + rhs` operation.
   */
  IEO_NOTNULL IeoTerm *(*add_func)(IEO_NOTNULL IeoTerm *self, IEO_NOTNULL IeoTerm *rhs);

  /**
   * @brief Perform `self / rhs` operation.
   */
  IEO_NOTNULL IeoTerm *(*div_func)(IEO_NOTNULL IeoTerm *self, IEO_NOTNULL IeoTerm *rhs);

  /**
   * @brief Perform `self == rhs` operation.
   *
   * If this method is not defined, the IeoType::compare_func method is tried.
   */
  IEO_NOTNULL IeoTerm *(*eq_func)(IEO_NOTNULL IeoTerm *self, IEO_NOTNULL IeoTerm *rhs);

  /**
   * @brief Perform `self > rhs` operation.
   *
   * If this method is not defined, the IeoType::compare_func method is tried.
   */
  IEO_NOTNULL IeoTerm *(*gt_func)(IEO_NOTNULL IeoTerm *self, IEO_NOTNULL IeoTerm *rhs);

  /**
   * @brief Perform `self >= rhs` operation.
   *
   * If this method is not defined, the IeoType::compare_func method is tried.
   */
  IEO_NOTNULL IeoTerm *(*gteq_func)(IEO_NOTNULL IeoTerm *self, IEO_NOTNULL IeoTerm *rhs);

  /**
   * @brief Perform `self < rhs` operation.
   *
   * If this method is not defined, the IeoType::compare_func method is tried.
   */
  IEO_NOTNULL IeoTerm *(*lt_func)(IEO_NOTNULL IeoTerm *self, IEO_NOTNULL IeoTerm *rhs);

  /**
   * @brief Perform `self <= rhs` operation.
   *
   * If this method is not defined, the IeoType::compare_func method is tried.
   */
  IEO_NOTNULL IeoTerm *(*lteq_func)(IEO_NOTNULL IeoTerm *self, IEO_NOTNULL IeoTerm *rhs);

  /**
   * @brief Perform `self * rhs` operation.
   *
   * If this method is not defined, the IeoType::compare_func method is tried.
   */
  IEO_NOTNULL IeoTerm *(*mul_func)(IEO_NOTNULL IeoTerm *self, IEO_NOTNULL IeoTerm *rhs);

  /**
   * @brief Perform `self != rhs` operation.
   *
   * If this method is not defined, the IeoType::compare_func method is tried.
   */
  IEO_NOTNULL IeoTerm *(*neq_func)(IEO_NOTNULL IeoTerm *self, IEO_NOTNULL IeoTerm *rhs);

  /**
   * @brief Perform `self - rhs` operation.
   *
   * If this method is not defined, the IeoType::compare_func method is tried.
   */
  IEO_NOTNULL IeoTerm *(*sub_func)(IEO_NOTNULL IeoTerm *self, IEO_NOTNULL IeoTerm *rhs);

  /**
   * @brief Compare `self` to `rhs`.
   *
   */
  IEO_NOTNULL IeoTerm *(*compare_func)(IEO_NOTNULL IeoTerm *self, IEO_NOTNULL IeoTerm *other);
} IeoType;

/**
 * @brief Iterate over all registered types.
 *
 * @param opaque a pointer where the iterator function  will store its state.
 *               Must point to NULL to start the iteration.
 * @return IeoType* the next registered type or NULL when the iteration is finished.
 */
IEO_NULLABLE IeoType *
ieo_type_iterate(IEO_NOTNULL void **opaque);
