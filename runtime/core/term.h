#pragma once

#include <assert.h>
#include <stdatomic.h>
#include <stdbool.h>
#include <stdint.h>

#include "attrs.h"
#include "memory.h"
#include "type.h"

typedef uint_fast32_t IeoRefCount;

/**
 * @brief Represents Intentio term objects.
 * @warning NEVER TOUCH FIELDS OF THIS STRUCTURE DIRECTLY, USE ACCESSOR FUNCTIONS!
 */
struct IeoTerm
{
  IeoType *ty;
  _Atomic IeoRefCount refcount;
  uint8_t value[];
};
typedef struct IeoTerm IeoTerm;

/**
 * @brief Get pointer to term type.
 */
inline const IeoType *
ieo_term_ty(const IeoTerm *p)
{
  assert(p);
  assert(p->ty);
  return p->ty;
}

/**
 * @brief Get pointer to term type with mutable access.
 * @warning Use this only when needed.
 */
inline IeoType *
ieo_term_ty_mut(const IeoTerm *p)
{
  assert(p);
  assert(p->ty);
  return p->ty;
}

/**
 * @brief Get current reference count of given term object.
 */
inline IeoRefCount
ieo_term_refcount(const IeoTerm *p)
{
  assert(p);
  return atomic_load(&p->refcount);
}

/**
 * @brief Get pointer to value memory of given term.
 */
inline void *
ieo_term_value(IeoTerm *p)
{
  assert(p);
  return (void *)p->value;
}

/**
 * @brief Allocate and initialize a new term object.
 *
 * The object starts with reference count equal to 1.
 *
 * @param ty a pointer to term type definition, must not be null
 * @return IEO_MALLOC* ieo_term_alloc
 */
IEO_MALLOC IEO_WARN_UNUSED_RESULT IeoTerm *
ieo_term_alloc(IeoType *ty);

/**
 * @brief Increment reference counter of given term object.
 *
 * @param p a pointer to term object
 * @return IeoTerm* a pointer to same term object
 */
IeoTerm *
ieo_term_ref(IeoTerm *p);

/**
 * @brief Decrement reference counter of given term object, and free it if it reaches zero.
 *
 * @param p a pointer to term object, set to null if term is freed
 */
void
ieo_term_unref(IeoTerm **p);

/**
 * @brief Force free term from memory and set remaining pointer to null.
 *
 * @warning This is dangerous, avoid this function as much as possible and use reference counting
 * instead.
 *
 * @param p a pointer to term object
 */
void
ieo_term_free(IeoTerm **p);

/**
 * @brief Default deleter of term objects.
 */
void
ieo_term_deleter(IeoTerm *p);
