#include "ops.h"

#include "int.h"
#include "none.h"
#include "str.h"

#define FNAME(METHOD, SELF) f_##METHOD##_##SELF

#define SAME_TYPE(LHS, RHS) (ieo_term_ty((LHS)) == ieo_term_ty((RHS)))

#define FIND_UNARY(METHOD, SELF) FIND_IMPL(Unary, METHOD, SELF)
#define FIND_BINARY(METHOD, SELF) FIND_IMPL(Binary, METHOD, SELF)
#define FIND_IMPL(KIND, METHOD, SELF)                                          \
  IeoOp##KIND *FNAME(METHOD, SELF) = ieo_term_ty((SELF))->METHOD##_func;       \
  if (FNAME(METHOD, SELF))

#define CALL_UNARY(METHOD, SELF) (FNAME(METHOD, SELF)(SELF))
#define CALL_BINARY(METHOD, SELF, OTHER) (FNAME(METHOD, SELF)(SELF, OTHER))

#define DELEGATE_UNARY(METHOD, SELF) return CALL_UNARY(METHOD, SELF);
#define DELEGATE_BINARY(METHOD, SELF, OTHER)                                   \
  return CALL_BINARY(METHOD, SELF, OTHER);

#define FIND_DELEGATE_UNARY(METHOD, SELF)                                      \
  FIND_UNARY(METHOD, SELF) { DELEGATE_UNARY(METHOD, SELF); }

#define FIND_DELEGATE_BINARY(METHOD, SELF, OTHER)                              \
  FIND_BINARY(METHOD, SELF) { DELEGATE_BINARY(METHOD, SELF, OTHER); }

#define FAIL(METHOD)                                                           \
  return IEO_FAIL(IEO_STRING_ALLOC(#METHOD " is not available"));

#define COMMUTATIVE(LHS, RHS, BLOCK)                                           \
  do {                                                                         \
    IeoTerm *lhs_ = (LHS);                                                     \
    IeoTerm *rhs_ = (RHS);                                                     \
    do {                                                                       \
      (LHS) = lhs_;                                                            \
      (RHS) = rhs_;                                                            \
      {                                                                        \
        BLOCK;                                                                 \
      }                                                                        \
    } while (0);                                                               \
    do {                                                                       \
      (LHS) = rhs_;                                                            \
      (RHS) = lhs_;                                                            \
      {                                                                        \
        BLOCK;                                                                 \
      }                                                                        \
    } while (0);                                                               \
  } while (0)

#define CALL_COMPARE(LHS, RHS, COND)                                           \
  do {                                                                         \
    FIND_BINARY(compare, LHS)                                                  \
    {                                                                          \
      IeoTerm *cmp_;                                                           \
      IEO_TRY_UNWRAP(cmp_, CALL_BINARY(compare, LHS, RHS));                    \
      IEO_TRY_ERR_(ieo_is_int(cmp_), IEO_STP(COMPARE_RETURNED_NON_INT));       \
      ieo_int_t it = ieo_int_value(cmp_);                                      \
      bool r_ = (COND);                                                        \
      return IEO_BOOL(r_);                                                     \
    }                                                                          \
    FIND_BINARY(compare, RHS)                                                  \
    {                                                                          \
      IeoTerm *cmp_;                                                           \
      IEO_TRY_UNWRAP(cmp_, CALL_BINARY(compare, RHS, LHS));                    \
      IEO_TRY_ERR_(ieo_is_int(cmp_), IEO_STP(COMPARE_RETURNED_NON_INT));       \
      ieo_int_t it = -ieo_int_value(cmp_);                                     \
      bool r_ = (COND);                                                        \
      return IEO_BOOL(r_);                                                     \
    }                                                                          \
  } while (0);

IEO_STATIC_STRING(COMPARE_RETURNED_NON_INT,
                  "expected __compare__ to return an Int");

IeoResult
ieo_neg(IEO_NOTNULL IeoTerm *self)
{
  IEO_ASSERT(self);

  FIND_DELEGATE_UNARY(neg, self);
  FAIL(neg);
}

IeoResult
ieo_add(IEO_NOTNULL IeoTerm *lhs, IEO_NOTNULL IeoTerm *rhs)
{
  IEO_ASSERT(lhs);
  IEO_ASSERT(rhs);

  COMMUTATIVE(lhs, rhs, FIND_DELEGATE_BINARY(add, lhs, rhs));
  FAIL(add);
}

IeoResult
ieo_div(IEO_NOTNULL IeoTerm *lhs, IEO_NOTNULL IeoTerm *rhs)
{
  IEO_ASSERT(lhs);
  IEO_ASSERT(rhs);

  FIND_DELEGATE_BINARY(div, lhs, rhs);
  FAIL(div);
}

IeoResult
ieo_mul(IEO_NOTNULL IeoTerm *lhs, IEO_NOTNULL IeoTerm *rhs)
{
  IEO_ASSERT(lhs);
  IEO_ASSERT(rhs);

  COMMUTATIVE(lhs, rhs, FIND_DELEGATE_BINARY(mul, lhs, rhs));
  FAIL(mul);
}

IeoResult
ieo_sub(IEO_NOTNULL IeoTerm *lhs, IEO_NOTNULL IeoTerm *rhs)
{
  IEO_ASSERT(lhs);
  IEO_ASSERT(rhs);

  FIND_DELEGATE_BINARY(sub, lhs, rhs);
  FAIL(sub);
}

IeoResult
ieo_eq(IEO_NOTNULL IeoTerm *lhs, IEO_NOTNULL IeoTerm *rhs)
{
  IEO_ASSERT(lhs);
  IEO_ASSERT(rhs);

  COMMUTATIVE(lhs, rhs, {
    FIND_DELEGATE_BINARY(eq, lhs, rhs);
    FIND_BINARY(neq, lhs) { return IEO_NOT(CALL_BINARY(eq, lhs, rhs)); }
  });
  CALL_COMPARE(lhs, rhs, it == 0);
  FAIL(eq);
}

IeoResult
ieo_gt(IEO_NOTNULL IeoTerm *lhs, IEO_NOTNULL IeoTerm *rhs)
{
  IEO_ASSERT(lhs);
  IEO_ASSERT(rhs);

  if (!SAME_TYPE(lhs, rhs)) {
    return IEO_BOOL(ieo_type_compare(ieo_term_ty(lhs), ieo_term_ty(rhs)) > 0);
  }

  FIND_DELEGATE_BINARY(gt, lhs, rhs);
  CALL_COMPARE(lhs, rhs, it > 0);
  FAIL(gt);
}

IeoResult
ieo_gteq(IEO_NOTNULL IeoTerm *lhs, IEO_NOTNULL IeoTerm *rhs)
{
  IEO_ASSERT(lhs);
  IEO_ASSERT(rhs);

  if (!SAME_TYPE(lhs, rhs)) {
    return IEO_BOOL(ieo_type_compare(ieo_term_ty(lhs), ieo_term_ty(rhs)) >= 0);
  }

  FIND_DELEGATE_BINARY(gteq, lhs, rhs);
  FIND_DELEGATE_BINARY(lteq, rhs, lhs);
  CALL_COMPARE(lhs, rhs, it >= 0);
  FAIL(gteq);
}

IeoResult
ieo_lt(IEO_NOTNULL IeoTerm *lhs, IEO_NOTNULL IeoTerm *rhs)
{
  IEO_ASSERT(lhs);
  IEO_ASSERT(rhs);

  if (!SAME_TYPE(lhs, rhs)) {
    return IEO_BOOL(ieo_type_compare(ieo_term_ty(lhs), ieo_term_ty(rhs)) < 0);
  }

  FIND_DELEGATE_BINARY(lt, lhs, rhs);
  CALL_COMPARE(lhs, rhs, it < 0);
  FAIL(lt);
}

IeoResult
ieo_lteq(IEO_NOTNULL IeoTerm *lhs, IEO_NOTNULL IeoTerm *rhs)
{
  IEO_ASSERT(lhs);
  IEO_ASSERT(rhs);

  if (!SAME_TYPE(lhs, rhs)) {
    return IEO_BOOL(ieo_type_compare(ieo_term_ty(lhs), ieo_term_ty(rhs)) <= 0);
  }

  FIND_DELEGATE_BINARY(lteq, lhs, rhs);
  FIND_DELEGATE_BINARY(gteq, rhs, lhs);
  CALL_COMPARE(lhs, rhs, it <= 0);
  FAIL(lteq);
}

IeoResult
ieo_neq(IEO_NOTNULL IeoTerm *lhs, IEO_NOTNULL IeoTerm *rhs)
{
  IEO_ASSERT(lhs);
  IEO_ASSERT(rhs);

  COMMUTATIVE(lhs, rhs, {
    FIND_DELEGATE_BINARY(neq, lhs, rhs);
    FIND_BINARY(eq, lhs) { return IEO_NOT(CALL_BINARY(eq, lhs, rhs)); }
  });
  CALL_COMPARE(lhs, rhs, it != 0);
  FAIL(neq);
}

IeoResult
ieo_seq(IEO_NOTNULL IeoTerm *lhs, IEO_NOTNULL IeoTerm *rhs)
{
  return SAME_TYPE(lhs, rhs) ? ieo_eq(lhs, rhs) : IEO_BOOL(false);
}

IeoResult
ieo_sneq(IEO_NOTNULL IeoTerm *lhs, IEO_NOTNULL IeoTerm *rhs)
{
  return SAME_TYPE(lhs, rhs) ? IEO_BOOL(true) : ieo_neq(lhs, rhs);
}

IeoResult
ieo_compare(IEO_NOTNULL IeoTerm *lhs, IEO_NOTNULL IeoTerm *rhs)
{
  IEO_ASSERT(lhs);
  IEO_ASSERT(rhs);

  if (!SAME_TYPE(lhs, rhs)) {
    return ieo_int_new(ieo_type_compare(ieo_term_ty(lhs), ieo_term_ty(rhs)));
  }

  FIND_DELEGATE_BINARY(compare, lhs, rhs);
  FIND_BINARY(compare, rhs)
  {
    IeoTerm *cmp;
    IEO_TRY_UNWRAP(cmp, CALL_BINARY(compare, rhs, lhs));
    return ieo_neg(cmp);
  }
  FAIL(compare);
}
