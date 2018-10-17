#include "alltypes.h"

extern IeoType ieo_std_type_int;
extern IeoType ieo_std_type_none;
extern IeoType ieo_std_type_string;

IeoType *ieo_type_all[] = {
  &ieo_std_type_none,
  &ieo_std_type_int,
  &ieo_std_type_string,
  NULL,
};
