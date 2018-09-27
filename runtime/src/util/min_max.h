#pragma once

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

#define MIN(X, Y)                                                              \
  (_Generic((X) + (Y), int                                                     \
            : ieo_min_i, unsigned                                              \
            : ieo_min_u, long                                                  \
            : ieo_min_l, unsigned long                                         \
            : ieo_min_ul, long long                                            \
            : ieo_min_ll, unsigned long long                                   \
            : ieo_min_ull, float                                               \
            : ieo_min_f, double                                                \
            : ieo_min_d, long double                                           \
            : ieo_min_ld)((X), (Y)))

#define MAX(X, Y)                                                              \
  (_Generic((X) + (Y), int                                                     \
            : ieo_max_i, unsigned                                              \
            : ieo_max_u, long                                                  \
            : ieo_max_l, unsigned long                                         \
            : ieo_max_ul, long long                                            \
            : ieo_max_ll, unsigned long long                                   \
            : ieo_max_ull, float                                               \
            : ieo_max_f, double                                                \
            : ieo_max_d, long double                                           \
            : ieo_max_ld)((X), (Y)))
