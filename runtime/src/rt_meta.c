#include "rt_meta.h"

#include "int.h"
#include "str.h"

#include <_intentio_config.h>

IeoResult
ieo_rt_info(void)
{
  IEO_STATIC_STRING(s, "Intentio Runtime v" INTENTIO_VERSION_STRING);
  return IEO_SUCCT(&s);
}

IeoResult
ieo_rt_version_major(void)
{
  return ieo_int_new(INTENTIO_VERSION_MAJOR);
}

IeoResult
ieo_rt_version_minor(void)
{
  return ieo_int_new(INTENTIO_VERSION_MINOR);
}

IeoResult
ieo_rt_version_patch(void)
{
  return ieo_int_new(INTENTIO_VERSION_PATCH);
}

IeoResult
ieo_rt_version_str(void)
{
  IEO_STATIC_STRING(s, INTENTIO_VERSION_STRING);
  return IEO_SUCCT(&s);
}
