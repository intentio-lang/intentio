#include "term.h"

extern inline IEO_PURE const IeoType *
ieo_term_ty(IEO_NOTNULL const IeoTerm *p);

extern inline IEO_PURE IeoType *
ieo_term_ty_mut(IEO_NOTNULL const IeoTerm *p);

extern inline IEO_PURE IeoRefCount
ieo_term_refcount(IEO_NOTNULL const IeoTerm *p);

extern inline IEO_PURE const void *
ieo_term_value(IEO_NOTNULL const IeoTerm *p);

extern inline IEO_PURE void *
ieo_term_value_mut(IEO_NOTNULL IeoTerm *p);

extern inline IEO_PURE IeoTermFlags
ieo_term_flags(IEO_NOTNULL const IeoTerm *p);

extern inline IEO_CONST IeoResult
ieo_not_impl_(IeoResult r);

IeoTerm *
ieo_term_alloc(IeoType *ty)
{
  return ieo_term_alloc_s(ty, 0);
}

IEO_MALLOC IEO_WARN_UNUSED_RESULT IeoTerm *
ieo_term_alloc_s(IeoType *ty, size_t size)
{
  ieo_assert(ty);
  ieo_assert(ty->term_size >= sizeof(IeoTermHeader));

  IeoTerm *p = (IeoTerm *)ieo_mallocz(ty->term_size + size);
  if (!p)
    return NULL;

  p->head.ty = ty;
  atomic_init(&p->head.refcount, 1);

  return p;
}

IeoTerm *
ieo_term_ref(IeoTerm *p)
{
  if (!p)
    return NULL;

  if (p->head.flags.is_static)
    return p;

  atomic_fetch_add_explicit(&p->head.refcount, 1, memory_order_relaxed);
  return p;
}

void
ieo_term_unref(IeoTerm **p)
{
  if (!p || !*p)
    return;

  if ((*p)->head.flags.is_static)
    return;

  if (atomic_fetch_sub_explicit(
        &(*p)->head.refcount, 1, memory_order_acq_rel) == 1) {
    ieo_term_free(p);
  }
}

void
ieo_term_free(IeoTerm **p)
{
  if (!p || !*p)
    return;

  if ((*p)->head.flags.is_static)
    return;

  ieo_assert(ieo_term_ty(*p)->deleter);
  ieo_term_ty(*p)->deleter(*p);

  *p = NULL;
}

void
ieo_term_deleter(IeoTerm *p)
{
  if (!p)
    return;
  ieo_free(p);
}
