#if HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif
#include <sys/types.h>

#include "ipmi-common.h"

/* From David Wheeler's Secure Programming Guide */
void *guaranteed_memset(void *s, int c, size_t n)
{
  volatile char *p = s;

  if (!s || !n)
    return NULL;

  while (n--)
    *p++=c;

  return s;
}


