/*****************************************************************************\
 *  $Id: ipmipower_workarounds.c,v 1.1 2007-03-29 16:36:03 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2003 The Regents of the University of California.
 *  Produced at Lawrence Livermore National Laboratory (cf, DISCLAIMER).
 *  Written by Albert Chu <chu11@llnl.gov>
 *  UCRL-CODE-155698
 *  
 *  This file is part of Ipmipower, a remote power control utility.
 *  For details, see http://www.llnl.gov/linux/.
 *  
 *  Ipmipower is free software; you can redistribute it and/or modify 
 *  it under the terms of the GNU General Public License as published by the 
 *  Free Software Foundation; either version 2 of the License, or (at your 
 *  option) any later version.
 *  
 *  Ipmipower is distributed in the hope that it will be useful, but 
 *  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY 
 *  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License 
 *  for more details.
 *  
 *  You should have received a copy of the GNU General Public License along
 *  with Ipmipower; if not, write to the Free Software Foundation, Inc.,
 *  51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA.
\*****************************************************************************/

#if HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <assert.h>

#include "ipmipower.h"
#include "ipmipower_workarounds.h"

#include "wrappers.h"

extern struct ipmipower_config *conf;

#define IPMIPOWER_WORKAROUNDS_BUFLEN  4096

/* we're single threaded, so we are being lazy */
static char workarounds_buffer[IPMIPOWER_WORKAROUNDS_BUFLEN];

#define WORKAROUND_FLAG_FORCE_PERMSG_AUTHENTICATION_STR "forcepermsg"
#define WORKAROUND_FLAG_ACCEPT_SESSION_ID_ZERO_STR      "idzero"
#define WORKAROUND_FLAG_CHECK_UNEXPECTED_AUTHCODE_STR   "unexpectedauth"
#define WORKAROUND_FLAG_INTEL_2_0_SESSION_STR           "intel20"
#define WORKAROUND_FLAG_SUPERMICRO_2_0_SESSION_STR      "supermicro20"

int
ipmipower_workarounds_parse(char *str, uint32_t *workaround_flags)
{
  uint32_t flags = 0;
  char *s, *tok;
  int ret = -1;

  assert(str);
  assert(workaround_flags);

  s = Strdup(str);
  tok = strtok(s, ",");
  while (tok)
    {
      if (!strcasecmp(tok, WORKAROUND_FLAG_FORCE_PERMSG_AUTHENTICATION_STR))
        flags |= WORKAROUND_FLAG_FORCE_PERMSG_AUTHENTICATION;
      else if (!strcasecmp(tok, WORKAROUND_FLAG_ACCEPT_SESSION_ID_ZERO_STR))
        flags |= WORKAROUND_FLAG_ACCEPT_SESSION_ID_ZERO;
      else if (!strcasecmp(tok, WORKAROUND_FLAG_CHECK_UNEXPECTED_AUTHCODE_STR))
        flags |= WORKAROUND_FLAG_CHECK_UNEXPECTED_AUTHCODE;
      else if (!strcasecmp(tok, WORKAROUND_FLAG_INTEL_2_0_SESSION_STR))
        flags |= WORKAROUND_FLAG_INTEL_2_0_SESSION;
      else if (!strcasecmp(tok, WORKAROUND_FLAG_SUPERMICRO_2_0_SESSION_STR))
        flags |= WORKAROUND_FLAG_SUPERMICRO_2_0_SESSION;
      else
        goto cleanup;
      tok = strtok(NULL, ",");
    }

  *workaround_flags = flags;
  ret = 0;

 cleanup:
  Free(s);
  return ret;
}

char *
ipmipower_workarounds_string(uint32_t workaround_flags)
{
  int not_first = 0;

  memset(workarounds_buffer, '\0', IPMIPOWER_WORKAROUNDS_BUFLEN);
  
  printf("workaround_flags = %x\n", workaround_flags);

  if (workaround_flags & WORKAROUND_FLAG_FORCE_PERMSG_AUTHENTICATION)
    {
      if (not_first)
        strcat(workarounds_buffer, ",");
      strcat(workarounds_buffer, WORKAROUND_FLAG_FORCE_PERMSG_AUTHENTICATION_STR);
      not_first++;
    }
  if (workaround_flags & WORKAROUND_FLAG_ACCEPT_SESSION_ID_ZERO)
    {
      if (not_first)
        strcat(workarounds_buffer, ",");
      strcat(workarounds_buffer, WORKAROUND_FLAG_ACCEPT_SESSION_ID_ZERO_STR);
      not_first++;
    }
  if (workaround_flags & WORKAROUND_FLAG_CHECK_UNEXPECTED_AUTHCODE)
    {
      if (not_first)
        strcat(workarounds_buffer, ",");
      strcat(workarounds_buffer, WORKAROUND_FLAG_CHECK_UNEXPECTED_AUTHCODE_STR);
      not_first++;
    }
  if (workaround_flags & WORKAROUND_FLAG_INTEL_2_0_SESSION)
    {
      if (not_first)
        strcat(workarounds_buffer, ",");
      strcat(workarounds_buffer, WORKAROUND_FLAG_INTEL_2_0_SESSION_STR);
      not_first++;
    }
  if (workaround_flags & WORKAROUND_FLAG_SUPERMICRO_2_0_SESSION)
    {
      if (not_first)
        strcat(workarounds_buffer, ",");
      strcat(workarounds_buffer, WORKAROUND_FLAG_SUPERMICRO_2_0_SESSION_STR);
      not_first++;
    }

  return workarounds_buffer;
}

char *
ipmipower_workarounds_list(void) 
{
  memset(workarounds_buffer, '\0', IPMIPOWER_WORKAROUNDS_BUFLEN);
  snprintf(workarounds_buffer,
           IPMIPOWER_WORKAROUNDS_BUFLEN,
           "%s,%s,%s,%s,%s",
           WORKAROUND_FLAG_FORCE_PERMSG_AUTHENTICATION_STR,
           WORKAROUND_FLAG_ACCEPT_SESSION_ID_ZERO_STR,
           WORKAROUND_FLAG_CHECK_UNEXPECTED_AUTHCODE_STR,
           WORKAROUND_FLAG_INTEL_2_0_SESSION_STR,
           WORKAROUND_FLAG_SUPERMICRO_2_0_SESSION_STR);
  return workarounds_buffer;
}
