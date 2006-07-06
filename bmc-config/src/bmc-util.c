
#include "bmc-sections.h"
#include "bmc-types.h"

int is_yesno (const struct arguments *args,
	      const struct section *sect,
	      const char *value)
{
  return value && (same (value, "yes") || same (value, "no"));
}
