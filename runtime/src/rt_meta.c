#include "rt_meta.h"

#include "num.h"
#include "str.h"

#include <_intentio_config.h>

IeoResult
ieo_rt_info(void)
{
  return ieo_string_new("Intentio Runtime v" INTENTIO_VERSION_STRING);
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
  return ieo_string_new(INTENTIO_VERSION_STRING);
}
