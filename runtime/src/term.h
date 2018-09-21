#pragma once

#include <stdatomic.h>
#include <stdbool.h>
#include <stdint.h>

#include "memory.h"
#include "type.h"
#include "util.h"

/**
 * Cast pointer to term pointer.
 */
#define IEO_TP(P) ((IeoTerm *)(P))

/**
 * Produce term pointer to static term value.
 */
#define IEO_STP(VAL) (IEO_TP(&(VAL)))

#define IEO_SUCCT(TERM_PTR)                                                    \
  ((IeoResult){ .succ = true, .term = IEO_TP(TERM_PTR) })
#define IEO_FAILT(TERM_PTR)                                                    \
  ((IeoResult){ .succ = false, .term = IEO_TP(TERM_PTR) })

#define IEO_SUCC(RESULT) ((IeoResult){ .succ = true, .term = (RESULT).term })
#define IEO_FAIL(RESULT) ((IeoResult){ .succ = false, .term = (RESULT).term })
#define IEO_NOT(RESULT) (ieo_not_impl_((RESULT)))
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
    IeoResult tmp_;                                                            \
    IEO_TRY(tmp_, EXPR);                                                       \
  } while (0);

#define IEO_TRY_ERR(RESULT_VAR, EXPR, ERR)                                     \
  do {                                                                         \
    (RESULT_VAR) = (EXPR);                                                     \
    if (IEO_ERR(RESULT_VAR)) {                                                 \
      return IEO_FAILT((ERR));                                                 \
    }                                                                          \
  } while (0)

#define IEO_TRY_ERR_(EXPR, ERR)                                                \
  do {                                                                         \
    IeoResult tmp_;                                                            \
    IEO_TRY_ERR(tmp_, EXPR, ERR);                                              \
  } while (0);

#define IEO_TRY_UNWRAP(TERM_VAR, EXPR)                                         \
  do {                                                                         \
    IeoResult tmp_ = (EXPR);                                                   \
    if (IEO_ERR(tmp_)) {                                                       \
      return (tmp_);                                                           \
    } else {                                                                   \
      (TERM_VAR) = tmp_.term;                                                  \
    }                                                                          \
  } while (0)

#define IEO_TRY_UNWRAP_ERR(TERM_VAR, EXPR, ERR)                                \
  do {                                                                         \
    IeoResult tmp_ = (EXPR);                                                   \
    if (IEO_ERR(tmp_)) {                                                       \
      return IEO_FAILT((ERR));                                                 \
    } else {                                                                   \
      (TERM_VAR) = tmp_.term;                                                  \
    }                                                                          \
  } while (0)

typedef uint_fast32_t IeoRefCount;
typedef atomic_uint_fast32_t AtomicIeoRefCount;

typedef struct IeoTermFlags
{
  /**
   * This term is stored in static memory: it is not reference counted and
   * freeing it is no-op.
   */
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
 * Represents Intentio term objects.
 * @warning NEVER TOUCH FIELDS OF THIS STRUCTURE DIRECTLY, USE
 * ACCESSOR FUNCTIONS!
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
 * @private
 */
inline IEO_CONST IeoResult
ieo_not_impl_(IeoResult r)
{
  return (IeoResult){ .succ = !r.succ, .term = r.term };
}

/**
 * Get pointer to term type.
 */
inline IEO_PURE const IeoType *
ieo_term_ty(IEO_NOTNULL const IeoTerm *p)
{
  IEO_ASSERT(p);
  IEO_ASSERT(p->head.ty);
  return p->head.ty;
}

/**
 * Get pointer to term type with mutable access.
 * @warning Use this only when needed.
 */
inline IEO_PURE IeoType *
ieo_term_ty_mut(IEO_NOTNULL const IeoTerm *p)
{
  IEO_ASSERT(p);
  IEO_ASSERT(p->head.ty);
  return p->head.ty;
}

/**
 * Get current reference count of given term object.
 */
inline IeoRefCount
ieo_term_refcount(IEO_NOTNULL const IeoTerm *p)
{
  IEO_ASSERT(p);
  return atomic_load(&p->head.refcount);
}

/**
 * Get term flags.
 */
inline IEO_PURE IeoTermFlags
ieo_term_flags(IEO_NOTNULL const IeoTerm *p)
{
  IEO_ASSERT(p);
  return p->head.flags;
}

/**
 * Get pointer to value memory of given term.
 */
inline IEO_PURE IEO_NOTNULL const void *
ieo_term_value(IEO_NOTNULL const IeoTerm *p)
{
  IEO_ASSERT(p);
  return (void *)p->value;
}

/**
 * Get pointer to mutable value memory of given term.
 */
inline IEO_PURE IEO_NOTNULL void *
ieo_term_value_mut(IEO_NOTNULL IeoTerm *p)
{
  IEO_ASSERT(p);
  return (void *)p->value;
}

/**
 * Allocate and initialize a new term object.
 *
 * The object starts with reference count equal to 1.
 *
 * @param ty a pointer to term type definition, must not be null
 * @return IeoTerm* ieo_term_alloc
 */
IEO_MALLOC IEO_WARN_UNUSED_RESULT IeoTerm *
ieo_term_alloc(IeoType *ty);

/**
 * Allocate and initialize a new term object with additional `size`
 * bytes of data after term header.
 *
 * The object starts with reference count equal to 1.
 *
 * @param ty a pointer to term type definition, must not be null
 * @return IeoTerm* ieo_term_alloc
 */
IEO_MALLOC IEO_WARN_UNUSED_RESULT IeoTerm *
ieo_term_alloc_s(IeoType *ty, size_t size);

/**
 * Increment reference counter of given term object.
 *
 * @param p a pointer to term object
 * @return IeoTerm* a pointer to same term object
 */
IeoTerm *
ieo_term_ref(IeoTerm *p);

/**
 * Decrement reference counter of given term object, and free it if
 * it reaches zero.
 *
 * @param p a pointer to term object, set to null if term is freed
 */
void
ieo_term_unref(IeoTerm **p);

/**
 * Force free term from memory and set remaining pointer to null.
 *
 * @warning This is dangerous, avoid this function as much as
 * possible and use reference counting instead.
 *
 * @param p a pointer to term object
 */
void
ieo_term_free(IeoTerm **p);

/**
 * Default deleter of term objects.
 */
void
ieo_term_deleter(IeoTerm *p);
