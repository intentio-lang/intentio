#pragma once

#include <stdatomic.h>
#include <stdbool.h>
#include <stdint.h>

#include "memory.h"
#include "type.h"
#include "util.h"

#define IEO_SUCCT(TERM_PTR)                                                    \
  ((IeoResult){ .succ = true, .term = (IeoTerm *)(TERM_PTR) })
#define IEO_FAILT(TERM_PTR)                                                    \
  ((IeoResult){ .succ = false, .term = (IeoTerm *)(TERM_PTR) })

#define IEO_SUCC(RESULT) ((IeoResult){ .succ = true, .term = (RESULT).term })
#define IEO_FAIL(RESULT) ((IeoResult){ .succ = false, .term = (RESULT).term })
#define IEO_SSET(STATE, RESULT)                                                \
  ((IeoResult){ .succ = (STATE), .term = (RESULT).term })

#define IEO_OK(COND) ((COND).succ)
#define IEO_ERR(COND) (!(COND).succ)

#define IEO_TRY(RESULT_VAR, EXPR)                                              \
  do {                                                                         \
    (RESULT_VAR) = (EXPR);                                                     \
    if (IEO_ERR(RESULT_VAR)) {                                                 \
      return (RESULT_VAR);                                                     \
    }                                                                          \
  } while (0)

#define IEO_TRY_(EXPR)                                                         \
  do {                                                                         \
    IeoResult tmp__;                                                           \
    IEO_TRY(tmp__, EXPR);                                                      \
  } while (0);

typedef uint_fast32_t IeoRefCount;
typedef atomic_uint_fast32_t AtomicIeoRefCount;

typedef struct IeoTermFlags
{
  /// This term is stored in static memory: it is not refcounted and cannot be
  /// freed.
  bool is_static : 1;
} IeoTermFlags;
static_assert(sizeof(IeoTermFlags) <= sizeof(uint8_t),
              "IeoTermFlags must occupy 1 byte");

typedef struct IeoTermHeader
{
  IeoType *ty;
  AtomicIeoRefCount refcount;
  IeoTermFlags flags;
} IeoTermHeader;

/**
 * @brief Represents Intentio term objects.
 * @warning NEVER TOUCH FIELDS OF THIS STRUCTURE DIRECTLY, USE ACCESSOR
 * FUNCTIONS!
 */
typedef struct IeoTerm
{
  IeoTermHeader head;
  uint8_t value[];
} IeoTerm;

typedef struct IeoResult
{
  bool succ;
  IeoTerm *term;
} IeoResult;

/**
 * @brief Get pointer to term type.
 */
inline IEO_PURE const IeoType *
ieo_term_ty(IEO_NOTNULL const IeoTerm *p)
{
  ieo_assert(p);
  ieo_assert(p->head.ty);
  return p->head.ty;
}

/**
 * @brief Get pointer to term type with mutable access.
 * @warning Use this only when needed.
 */
inline IEO_PURE IeoType *
ieo_term_ty_mut(IEO_NOTNULL const IeoTerm *p)
{
  ieo_assert(p);
  ieo_assert(p->head.ty);
  return p->head.ty;
}

/**
 * @brief Get current reference count of given term object.
 */
inline IeoRefCount
ieo_term_refcount(IEO_NOTNULL const IeoTerm *p)
{
  ieo_assert(p);
  return atomic_load(&p->head.refcount);
}

/**
 * @brief Get term flags.
 */
inline IEO_PURE IeoTermFlags
ieo_term_flags(IEO_NOTNULL const IeoTerm *p)
{
  ieo_assert(p);
  return p->head.flags;
}

/**
 * @brief Get pointer to value memory of given term.
 */
inline IEO_PURE IEO_NOTNULL const void *
ieo_term_value(IEO_NOTNULL const IeoTerm *p)
{
  ieo_assert(p);
  return (void *)p->value;
}

/**
 * @brief Get pointer to mutable value memory of given term.
 */
inline IEO_PURE IEO_NOTNULL void *
ieo_term_value_mut(IEO_NOTNULL IeoTerm *p)
{
  ieo_assert(p);
  return (void *)p->value;
}

/**
 * @brief Allocate and initialize a new term object.
 *
 * The object starts with reference count equal to 1.
 *
 * @param ty a pointer to term type definition, must not be null
 * @return IeoTerm* ieo_term_alloc
 */
IEO_MALLOC IEO_WARN_UNUSED_RESULT IeoTerm *
ieo_term_alloc(IeoType *ty);

/**
 * @brief Allocate and initialize a new term object with additional `size` bytes
 * of data after term header.
 *
 * The object starts with reference count equal to 1.
 *
 * @param ty a pointer to term type definition, must not be null
 * @return IeoTerm* ieo_term_alloc
 */
IEO_MALLOC IEO_WARN_UNUSED_RESULT IeoTerm *
ieo_term_alloc_s(IeoType *ty, size_t size);

/**
 * @brief Increment reference counter of given term object.
 *
 * @param p a pointer to term object
 * @return IeoTerm* a pointer to same term object
 */
IeoTerm *
ieo_term_ref(IeoTerm *p);

/**
 * @brief Decrement reference counter of given term object, and free it if it
 * reaches zero.
 *
 * @param p a pointer to term object, set to null if term is freed
 */
void
ieo_term_unref(IeoTerm **p);

/**
 * @brief Force free term from memory and set remaining pointer to null.
 *
 * @warning This is dangerous, avoid this function as much as possible and use
 * reference counting instead.
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
