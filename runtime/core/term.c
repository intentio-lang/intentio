#include "term.h"

extern inline const IeoType *
ieo_term_ty(const IeoTerm *p);

extern inline IeoType *
ieo_term_ty_mut(const IeoTerm *p);

extern inline IeoRefCount
ieo_term_refcount(const IeoTerm *p);

extern inline void *
ieo_term_value(IeoTerm *p);

IeoTerm *
ieo_term_alloc(IeoType *ty)
{
  assert(ty);

  IeoTerm *p = (IeoTerm *)ieo_mallocz(sizeof(IeoTerm) + ty->value_size * sizeof(uint8_t));
  if (!p)
    return NULL;

  p->ty = ty;
  p->refcount = 1;

  return p;
}

IeoTerm *
ieo_term_ref(IeoTerm *p)
{
  if (!p)
    return NULL;

  atomic_fetch_add_explicit(&p->refcount, 1, memory_order_relaxed);
  return p;
}

void
ieo_term_unref(IeoTerm **p)
{
  if (!p || !*p)
    return;

  if (atomic_fetch_sub_explicit(&(*p)->refcount, 1, memory_order_acq_rel) == 1) {
    ieo_term_free(p);
  }
}

void
ieo_term_free(IeoTerm **p)
{
  if (!p || !*p)
    return;
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
