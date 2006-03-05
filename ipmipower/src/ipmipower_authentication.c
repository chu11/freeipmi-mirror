/*****************************************************************************\
 *  $Id: ipmipower_authentication.c,v 1.1 2006-03-05 19:18:38 chu11 Exp $
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
#endif

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif
#if HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <assert.h>

#include "ipmipower_authentication.h"
#include "ipmipower_wrappers.h"

authentication_type_t 
ipmipower_authentication_type_index(char *str) 
{
  assert(str != NULL);

  if (!strcasecmp(str, "auto"))
    return AUTHENTICATION_TYPE_AUTO;
  else if (!strcasecmp(str, "none"))
    return AUTHENTICATION_TYPE_NONE;
  else if (!strcasecmp(str, "straight_password_key"))
    return AUTHENTICATION_TYPE_STRAIGHT_PASSWORD_KEY;
  else if (!strcasecmp(str, "md2"))
    return AUTHENTICATION_TYPE_MD2;
  else if (!strcasecmp(str, "md5"))
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
      return "none";
      break;
    case AUTHENTICATION_TYPE_STRAIGHT_PASSWORD_KEY:
      return "straight_password_key";
      break;
    case AUTHENTICATION_TYPE_MD2:
      return "md2";
      break;
    case AUTHENTICATION_TYPE_MD5:
      return "md5";
      break;
    default:
      err_exit("ipmipower_auth_string: Invalid Authentication Type: %d\n", at);
    }
  
  return NULL;                  /* NOT_REACHED */
}

char *
ipmipower_authentication_type_list(void) 
{
  return "auto, none, straight_password_key, md2, md5";
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
