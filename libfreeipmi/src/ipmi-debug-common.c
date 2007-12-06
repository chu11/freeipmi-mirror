/*
  Copyright (C) 2003, 2004, 2005 FreeIPMI Core Team

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2, or (at your option)
  any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software Foundation,
  Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA.
*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#ifdef STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#if HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */
#include <assert.h>
#include <errno.h>

#include "ipmi-debug-common.h"

#include "ipmi-common.h"
#include "ipmi-err-wrappers.h"
#include "ipmi-fiid-wrappers.h"

#include "freeipmi-portability.h"

#define IPMI_DEBUG_MAX_BUF_LEN        65536
#define IPMI_DEBUG_MAX_PKT_LEN        65536
#define IPMI_DEBUG_CHAR_PER_LINE          8
#define IPMI_DEBUG_DEFAULT_FD STDERR_FILENO

#define FREEIPMI_DPRINTF(args) \
        do { \
          ERR (!((freeipmi_dprintf args) < 0)); \
        } while(0) 

#define FREEIPMI_DPRINTF_CLEANUP(args) \
        do { \
          ERR_CLEANUP (!((freeipmi_dprintf args) < 0)); \
        } while(0) 

int8_t
ipmi_debug_set_prefix(char *buf, unsigned int buflen, char *prefix)
{
  assert(buf && buflen > 3);

  memset(buf, '\0', buflen);
  if (prefix)
    {
      strncpy(buf, prefix, buflen);
      buf[buflen - 1] = '\0'; /* strncpy may not null terminate */
      buf[buflen - 2] = '\0'; /* guaranteed space for ' ' */
      buf[buflen - 3] = '\0'; /* guaranteed space for ':' */
      strcat(buf, ": ");
    }

  return (0);
}

int8_t
ipmi_debug_output_str(int fd, char *prefix, char *str)
{
  /* achu: Yeah, I know this is slow.  Figure out something better
   * later.
   */
  if (str)
    {
      char *ptr = str;

      if (prefix)
        FREEIPMI_DPRINTF((fd, "%s", prefix));
      while (*ptr != '\0')
        {
          if (*ptr == '\n')
            {
              FREEIPMI_DPRINTF((fd, "%c", *ptr++));
              if (prefix)
                FREEIPMI_DPRINTF((fd, "%s", prefix));
            }
          else
            FREEIPMI_DPRINTF((fd, "%c", *ptr++));
        }
      FREEIPMI_DPRINTF((fd, "\n"));
    }

  return 0;
} 

int8_t
ipmi_debug_output_byte_array(int fd, char *prefix, uint8_t *buf, uint32_t buf_len)
{
  uint32_t count = 0;

  assert(buf);

  while (count < buf_len)
    {
      int i = 0;
      if (prefix)
        FREEIPMI_DPRINTF ((fd, "%s", prefix));
      FREEIPMI_DPRINTF ((fd, "[ "));
      while (count < buf_len && i < IPMI_DEBUG_CHAR_PER_LINE)
	{
	  FREEIPMI_DPRINTF ((fd, "%02Xh ", buf[count++]));
	  i++;
	}
      FREEIPMI_DPRINTF ((fd, "]\n"));
    }

  return 0;
}
