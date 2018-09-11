#include <intentio.h>

#include <_intentio_config.h>

IeoResult
ieo_rt_info()
{
  return ieo_string_new("Intentio Runtime v" INTENTIO_VERSION_STRING);
}

IeoResult
ieo_rt_version_major()
{
  return ieo_int_new(INTENTIO_VERSION_MAJOR);
}

IeoResult
ieo_rt_version_minor()
{
  return ieo_int_new(INTENTIO_VERSION_MINOR);
}

IeoResult
ieo_rt_version_patch()
{
  return ieo_int_new(INTENTIO_VERSION_PATCH);
}

IeoResult
ieo_rt_version_str()
{
  return ieo_string_new(INTENTIO_VERSION_STRING);
}
