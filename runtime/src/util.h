#pragma once

#ifdef __GNUC__
#define IEO_GCC_GTEQ(x, y) (__GNUC__ > (x) || __GNUC__ == (x) && __GNUC_MINOR__ >= (y))
#define IEO_GCC_LTEQ(x, y) (__GNUC__ < (x) || __GNUC__ == (x) && __GNUC_MINOR__ <= (y))
#else
#define IEO_GCC_GTEQ(x, y) 0
#define IEO_GCC_LTEQ(x, y) 0
#endif

/**
 * @def IEO_ALWAYS_INLINE
 * Generally, functions are not inlined unless optimization is specified. For functions declared
 * inline, this attribute inlines the function independent of any restrictions that otherwise apply
 * to inlining. Failure to inline such a function is diagnosed as an error. Note that if such a
 * function is called indirectly the compiler may or may not inline it depending on optimization
 * level and a failure to inline an indirect call may or may not be diagnosed.
 */
#if IEO_GCC_GTEQ(3, 1)
#define IEO_ALWAYS_INLINE __attribute__((always_inline)) inline
#elif defined(_MSC_VER)
#define IEO_ALWAYS_INLINE __forceinline
#else
#define IEO_ALWAYS_INLINE inline
#endif

/**
 * @def IEO_WARN_UNUSED_RESULT
 * The warn_unused_result attribute causes a warning to be emitted if a caller of the function with
 * this attribute does not use its return value. This is useful for functions where not checking the
 * result is either a security problem or always a bug, such as realloc.
 */
#if IEO_GCC_GTEQ(3, 4)
#define IEO_WARN_UNUSED_RESULT __attribute__((warn_unused_result))
#else
#define IEO_WARN_UNUSED_RESULT
#endif

/**
 * @def IEO_NOINLINE
 * This function attribute prevents a function from being considered for inlining. If the function
 * does not have side effects, there are optimizations other than inlining that cause function calls
 * to be optimized away, although the function call is live.
 */
#if IEO_GCC_GTEQ(3, 1)
#define IEO_NOINLINE __attribute__((noinline))
#elif defined(_MSC_VER)
#define IEO_NOINLINE __declspec(noinline)
#else
#define IEO_NOINLINE
#endif

/**
 * @def IEO_PURE
 * Many functions have no effects except the return value and their return value depends only on the
 * parameters and/or global variables. Calls to such functions can be subject to common
 * subexpression elimination and loop optimization just as an arithmetic operator would be. These
 * functions should be declared with the attribute pure.
 *
 * Some common examples of pure functions are strlen or memcmp. Interesting non-pure functions are
 * functions with infinite loops or those depending on volatile memory or other system resource,
 * that may change between two consecutive calls (such as feof in a multithreading environment).
 *
 * The pure attribute imposes similar but looser restrictions on a function’s defintion than the
 * const attribute: it allows the function to read global variables. Decorating the same function
 * with both the pure and the const attribute is diagnosed. Because a pure function cannot have any
 * side effects it does not make sense for such a function to return void. Declaring such a function
 * is diagnosed.
 */
#if IEO_GCC_GTEQ(3, 1) || defined(__clang__)
#define IEO_PURE __attribute__((pure))
#else
#define IEO_PURE
#endif

/**
 * @def IEO_CONST
 * Many functions do not examine any values except their arguments, and have no effects except to
 * return a value. Calls to such functions lend themselves to optimization such as common
 * subexpression elimination. The const attribute imposes greater restrictions on a function’s
 * definition than the similar pure attribute below because it prohibits the function from reading
 * global variables. Consequently, the presence of the attribute on a function declaration allows
 * GCC to emit more efficient code for some calls to the function. Decorating the same function with
 * both the const and the pure attribute is diagnosed.
 *
 * Note that a function that has pointer arguments and examines the data pointed to must not be
 * declared const. Likewise, a function that calls a non-const function usually must not be const.
 * Because a const function cannot have any side effects it does not make sense for such a function
 * to return void. Declaring such a function is diagnosed.
 */
#if IEO_GCC_GTEQ(2, 6) || defined(__clang__)
#define IEO_CONST __attribute__((const))
#else
#define IEO_CONST
#endif

/**
 * @def IEO_HOT
 * The hot attribute on a function is used to inform the compiler that the function is a hot spot of
 * the compiled program. The function is optimized more aggressively and on many targets it is
 * placed into a special subsection of the text section so all hot functions appear close together,
 * improving locality.
 */
#if IEO_GCC_GTEQ(4, 3) || defined(__clang__)
#define IEO_HOT __attribute__((hot))
#else
#define IEO_HOT
#endif

/**
 * @def IEO_COLD
 * The cold attribute on functions is used to inform the compiler that the function is unlikely to
 * be executed. The function is optimized for size rather than speed and on many targets it is
 * placed into a special subsection of the text section so all cold functions appear close together,
 * improving code locality of non-cold parts of program. The paths leading to calls of cold
 * functions within code are marked as unlikely by the branch prediction mechanism. It is thus
 * useful to mark functions used to handle unlikely conditions, such as perror, as cold to improve
 * optimization of hot functions that do call marked functions in rare occasions.
 */
#if IEO_GCC_GTEQ(4, 3) || defined(__clang__)
#define IEO_COLD __attribute__((cold))
#else
#define IEO_COLD
#endif

/**
 * @def IEO_FLATTEN
 * Generally, inlining into a function is limited. For a function marked with this attribute, every
 * call inside this function is inlined, if possible. Whether the function itself is considered for
 * inlining depends on its size and the current inlining parameters.
 */
#if IEO_GCC_GTEQ(4, 1) && !defined(__llvm__)
#define IEO_FLATTEN __attribute__((flatten))
#else
#define IEO_FLATTEN
#endif

/**
 * @def IEO_DEPRECATED
 * The deprecated attribute results in a warning if the item is used anywhere in the source file.
 * This is useful when identifying items that are expected to be removed in a future version of a
 * program. The warning also includes the location of the declaration of the deprecated item, to
 * enable users to easily find further information about why the item is deprecated, or what they
 * should do instead.
 */

#if IEO_GCC_GTEQ(3, 1)
#define IEO_DEPRECATED __attribute__((deprecated))
#elif defined(_MSC_VER)
#define IEO_DEPRECATED __declspec(deprecated)
#else
#define IEO_DEPRECATED
#endif

/**
 * @def IEO_UNUSED
 * This attribute, attached to a function, means that the function is meant to be possibly unused.
 * GCC does not produce a warning for this function.
 */
#if defined(__GNUC__) || defined(__clang__)
#define IEO_UNUSED __attribute__((unused))
#else
#define IEO_UNUSED
#endif

/**
 * @def IEO_MALLOC
 * This tells the compiler that a function is malloc-like, i.e., that the pointer P returned by the
 * function cannot alias any other pointer valid when the function returns, and moreover no pointers
 * to valid objects occur in any storage addressed by P.
 *
 * Using this attribute can improve optimization. Compiler predicts that a function with the
 * attribute returns non-null in most cases. Functions like malloc and calloc have this property
 * because they return a pointer to uninitialized or zeroed-out storage. However, functions like
 * realloc do not have this property, as they can return a pointer to storage containing pointers.
 */
#if defined(__GNUC__) || defined(__clang__)
#define IEO_MALLOC __attribute__((malloc))
#else
#define IEO_MALLOC
#endif

/**
 * @def IEO_ALLOC_SIZE
 * The alloc_size attribute is used to tell the compiler that the function return value points to
 * memory, where the size is given by one or two of the functions parameters. GCC uses this
 * information to improve the correctness of
 * __builtin_object_size.
 *
 * The function parameter(s) denoting the allocated size are specified by one or two integer
 * arguments supplied to the attribute. The allocated size is either the value of the single
 * function argument specified or the product of the two function arguments specified. Argument
 * numbering starts at one.
 */
#if IEO_GCC_GTEQ(4, 3)
#define IEO_ALLOC_SIZE(...) __attribute__((alloc_size(__VA_ARGS__)))
#else
#define IEO_ALLOC_SIZE(...)
#endif

/**
 * @def IEO_NOTNULL
 * The variable/parameter may never be NULL, or the function never returns NULL.
 */
#define IEO_NOTNULL

/**
 * @def IEO_NULLABLE
 * The variable/parameter may be initialized with NULL, or the function may return NULL.
 */
#define IEO_NULLABLE

/**
 * @def IEO_NULL
 * The variable/parameter must always be initialized with NULL (this can change during function
 * evaluation later), or function always return NULL.
 */
#define IEO_NULL

#define IEO_STRINGIZE(X) IEO_STRINGIZE_IMPL(X)
#define IEO_STRINGIZE_IMPL(X) #X

#define IEO_JOIN(X, Y) IEO_JOIN_IMPL(X, Y)
#define IEO_JOIN_IMPL(X, Y) IEO_JOIN_IMPL2(X, Y)
#define IEO_JOIN_IMPL2(X, Y) X##Y

inline int
ieo_min_i(int const x, int const y)
{
  return y < x ? y : x;
}

inline unsigned
ieo_min_u(unsigned const x, unsigned const y)
{
  return y < x ? y : x;
}

inline long
ieo_min_l(long const x, long const y)
{
  return y < x ? y : x;
}

inline unsigned long
ieo_min_ul(unsigned long const x, unsigned long const y)
{
  return y < x ? y : x;
}

inline long long
ieo_min_ll(long long const x, long long const y)
{
  return y < x ? y : x;
}

inline unsigned long long
ieo_min_ull(unsigned long long const x, unsigned long long const y)
{
  return y < x ? y : x;
}

inline float
ieo_min_f(float const x, float const y)
{
  return y < x ? y : x;
}

inline double
ieo_min_d(double const x, double const y)
{
  return y < x ? y : x;
}

inline long double
ieo_min_ld(long double const x, long double const y)
{
  return y < x ? y : x;
}

inline int
ieo_max_i(int const x, int const y)
{
  return x < y ? y : x;
}

inline unsigned
ieo_max_u(unsigned const x, unsigned const y)
{
  return x < y ? y : x;
}

inline long
ieo_max_l(long const x, long const y)
{
  return x < y ? y : x;
}

inline unsigned long
ieo_max_ul(unsigned long const x, unsigned long const y)
{
  return x < y ? y : x;
}

inline long long
ieo_max_ll(long long const x, long long const y)
{
  return x < y ? y : x;
}

inline unsigned long long
ieo_max_ull(unsigned long long const x, unsigned long long const y)
{
  return x < y ? y : x;
}

inline float
ieo_max_f(float const x, float const y)
{
  return x < y ? y : x;
}

inline double
ieo_max_d(double const x, double const y)
{
  return x < y ? y : x;
}

inline long double
ieo_max_ld(long double const x, long double const y)
{
  return x < y ? y : x;
}

#define MIN(X, Y)                                                                                  \
  (_Generic((X) + (Y), int                                                                         \
            : ieo_min_i, unsigned                                                                  \
            : ieo_min_u, long                                                                      \
            : ieo_min_l, unsigned long                                                             \
            : ieo_min_ul, long long                                                                \
            : ieo_min_ll, unsigned long long                                                       \
            : ieo_min_ull, float                                                                   \
            : ieo_min_f, double                                                                    \
            : ieo_min_d, long double                                                               \
            : ieo_min_ld)((X), (Y)))

#define MAX(X, Y)                                                                                  \
  (_Generic((X) + (Y), int                                                                         \
            : ieo_max_i, unsigned                                                                  \
            : ieo_max_u, long                                                                      \
            : ieo_max_l, unsigned long                                                             \
            : ieo_max_ul, long long                                                                \
            : ieo_max_ll, unsigned long long                                                       \
            : ieo_max_ull, float                                                                   \
            : ieo_max_f, double                                                                    \
            : ieo_max_d, long double                                                               \
            : ieo_max_ld)((X), (Y)))
