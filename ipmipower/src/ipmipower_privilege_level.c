/*****************************************************************************\
 *  $Id: ipmipower_privilege_level.c,v 1.7 2007-12-14 19:16:26 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2007 Lawrence Livermore National Security, LLC.
 *  Copyright (C) 2003-2007 The Regents of the University of California.
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
 *  with Ipmipower.  If not, see <http://www.gnu.org/licenses/>.
\*****************************************************************************/

#if HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#if HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */
#include <assert.h>

#include "ipmipower_privilege_level.h"
#include "ipmipower_wrappers.h"

#include "tool-cmdline-common.h"

#define IPMIPOWER_PRIVILEGE_BUFLEN  4096

/* we're single threaded, so we are being lazy */
static char privilege_buffer[IPMIPOWER_PRIVILEGE_BUFLEN];

privilege_level_t 
ipmipower_privilege_level_index(char *str) 
{
  int tmp;

  assert(str != NULL);

  if (!strcasecmp(str, "auto"))
    return PRIVILEGE_LEVEL_AUTO;

  tmp = parse_privilege_level(str);
  if (tmp == IPMI_PRIVILEGE_LEVEL_USER)
    return PRIVILEGE_LEVEL_USER;
  else if (tmp == IPMI_PRIVILEGE_LEVEL_OPERATOR)
    return PRIVILEGE_LEVEL_OPERATOR;
  else if (tmp == IPMI_PRIVILEGE_LEVEL_ADMIN)
    return PRIVILEGE_LEVEL_ADMIN;
  else
    return PRIVILEGE_LEVEL_INVALID;
}

char *
ipmipower_privilege_level_string(privilege_level_t priv) 
{
  assert(PRIVILEGE_LEVEL_VALID_OR_AUTO(priv));

  switch(priv) 
    {
    case PRIVILEGE_LEVEL_AUTO:
      return "auto";
      break;
    case PRIVILEGE_LEVEL_USER:
      return IPMI_PRIVILEGE_LEVEL_USER_STR;
      break;
    case PRIVILEGE_LEVEL_OPERATOR:
      return IPMI_PRIVILEGE_LEVEL_OPERATOR_STR;
      break;
    case PRIVILEGE_LEVEL_ADMIN:
      return IPMI_PRIVILEGE_LEVEL_ADMIN_STR;
      break;
    default:
      err_exit("ipmipower_privilege_string: Invalid Privilege Type: %d\n", priv);
    }
  
  return NULL;                  /* NOT_REACHED */
}

char *
ipmipower_privilege_level_list(void) 
{
  memset(privilege_buffer, '\0', IPMIPOWER_PRIVILEGE_BUFLEN);

  snprintf(privilege_buffer,
           IPMIPOWER_PRIVILEGE_BUFLEN,
           "auto, %s, %s, %s",
           IPMI_PRIVILEGE_LEVEL_USER_STR,
           IPMI_PRIVILEGE_LEVEL_OPERATOR_STR,
           IPMI_PRIVILEGE_LEVEL_ADMIN_STR);
  return privilege_buffer;
}

uint8_t
ipmipower_ipmi_privilege_level(privilege_level_t priv)
{
  assert(PRIVILEGE_LEVEL_VALID(priv));

  switch(priv) 
    {
    case PRIVILEGE_LEVEL_USER:
      return IPMI_PRIVILEGE_LEVEL_USER;
      break;
    case PRIVILEGE_LEVEL_OPERATOR:
      return IPMI_PRIVILEGE_LEVEL_OPERATOR;
      break;
    case PRIVILEGE_LEVEL_ADMIN:
      return IPMI_PRIVILEGE_LEVEL_ADMIN;
      break;
    default:
      err_exit("ipmipower_ipmi_privilege_level: Invalid Privilege Type: %d\n", priv);
    }
  
  return 0;                  /* NOT_REACHED */
}
