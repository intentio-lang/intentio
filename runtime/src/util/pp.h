#pragma once

#define IEO_STRINGIZE(X) IEO_STRINGIZE_IMPL(X)
#define IEO_STRINGIZE_IMPL(X) #X

#define IEO_JOIN(X, Y) IEO_JOIN_IMPL(X, Y)
#define IEO_JOIN_IMPL(X, Y) IEO_JOIN_IMPL2(X, Y)
#define IEO_JOIN_IMPL2(X, Y) X##Y

#define IEO_COUNT_OF(ARRAY) (sizeof(ARRAY) / sizeof(ARRAY[0]))
