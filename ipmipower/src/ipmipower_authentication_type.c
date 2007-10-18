/*****************************************************************************\
 *  $Id: ipmipower_authentication_type.c,v 1.6 2007-10-18 16:18:51 chu11 Exp $
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

#include "ipmipower_authentication_type.h"
#include "ipmipower_wrappers.h"

#include "cmdline-parse-common.h"

#define IPMIPOWER_AUTHENTICATION_BUFLEN  4096

/* we're single threaded, so we are being lazy */
static char authentication_buffer[IPMIPOWER_AUTHENTICATION_BUFLEN];

authentication_type_t 
ipmipower_authentication_type_index(char *str) 
{
  int tmp;

  assert(str != NULL);

  if (!strcasecmp(str, "auto"))
    return AUTHENTICATION_TYPE_AUTO;

  tmp = parse_authentication_type(str);
  if (tmp == IPMI_AUTHENTICATION_TYPE_NONE)
    return AUTHENTICATION_TYPE_NONE;
  else if (tmp == IPMI_AUTHENTICATION_TYPE_STRAIGHT_PASSWORD_KEY)
    return AUTHENTICATION_TYPE_STRAIGHT_PASSWORD_KEY;
  else if (tmp == IPMI_AUTHENTICATION_TYPE_MD2)
    return AUTHENTICATION_TYPE_MD2;
  else if (tmp == IPMI_AUTHENTICATION_TYPE_MD5)
    return AUTHENTICATION_TYPE_MD5;
  else 
    return AUTHENTICATION_TYPE_INVALID;
}

char *
ipmipower_authentication_type_string(authentication_type_t at) 
{
  assert(AUTHENTICATION_TYPE_VALID_OR_AUTO(at));

  switch(at) 
    {
    case AUTHENTICATION_TYPE_AUTO:
      return "auto";
      break;
    case AUTHENTICATION_TYPE_NONE:
      return IPMI_AUTHENTICATION_TYPE_NONE_STR;
      break;
    case AUTHENTICATION_TYPE_STRAIGHT_PASSWORD_KEY:
      return IPMI_AUTHENTICATION_TYPE_STRAIGHT_PASSWORD_KEY_STR;
      break;
    case AUTHENTICATION_TYPE_MD2:
      return IPMI_AUTHENTICATION_TYPE_MD2_STR;
      break;
    case AUTHENTICATION_TYPE_MD5:
      return IPMI_AUTHENTICATION_TYPE_MD5_STR;
      break;
    default:
      err_exit("ipmipower_authentication_type_string: Invalid Authentication Type: %d\n", at);
    }
  
  return NULL;                  /* NOT_REACHED */
}

char *
ipmipower_authentication_type_list(void) 
{
  memset(authentication_buffer, '\0', IPMIPOWER_AUTHENTICATION_BUFLEN);

  snprintf(authentication_buffer, 
           IPMIPOWER_AUTHENTICATION_BUFLEN,
           "auto, %s, %s, %s, %s",
           IPMI_AUTHENTICATION_TYPE_NONE_STR,
           IPMI_AUTHENTICATION_TYPE_STRAIGHT_PASSWORD_KEY_STR,
           IPMI_AUTHENTICATION_TYPE_MD2_STR,
           IPMI_AUTHENTICATION_TYPE_MD5_STR);
  return authentication_buffer;
}

uint8_t
ipmipower_ipmi_authentication_type(authentication_type_t at)
{
  assert(AUTHENTICATION_TYPE_VALID(at));

  switch(at) 
    {
    case AUTHENTICATION_TYPE_NONE:
      return IPMI_AUTHENTICATION_TYPE_NONE;
      break;
    case AUTHENTICATION_TYPE_STRAIGHT_PASSWORD_KEY:
      return IPMI_AUTHENTICATION_TYPE_STRAIGHT_PASSWORD_KEY;
      break;
    case AUTHENTICATION_TYPE_MD2:
      return IPMI_AUTHENTICATION_TYPE_MD2;
      break;
    case AUTHENTICATION_TYPE_MD5:
      return IPMI_AUTHENTICATION_TYPE_MD5;
      break;
    default:
      err_exit("ipmipower_ipmi_authentication_type: Invalid Authentication Type: %d\n", at);
    }
  
  return 0;                  /* NOT_REACHED */
}
