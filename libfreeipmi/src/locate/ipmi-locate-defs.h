
#ifndef _IPMI_LOCATE_DEFS_H
#define _IPMI_LOCATE_DEFS_H

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include <stdint.h>

#include "freeipmi/locate/ipmi-locate.h"

#define IPMI_LOCATE_CTX_MAGIC  0xF1CD9376

struct ipmi_locate_ctx {
  uint32_t magic;
  int errnum;
};

#endif /* _IPMI_LOCATE_DEFS_H */
