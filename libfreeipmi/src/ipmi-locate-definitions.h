#ifndef _IPMI_LOCATE_DEFINITIONS_H
#define _IPMI_LOCATE_DEFINITIONS_H

#include "freeipmi/ipmi-locate.h"

#define IPMI_LOCATE_CTX_MAGIC 0x1bbaadd1

struct ipmi_locate_ctx {
  uint32_t magic;
  int32_t errnum;
};

#endif /* _IPMI_LOCATE_DEFINITIONS_H */
