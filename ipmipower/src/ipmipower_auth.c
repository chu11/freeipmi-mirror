/*****************************************************************************\
 *  $Id: ipmipower_auth.c,v 1.2 2004-11-16 01:28:12 chu11 Exp $
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
 *  59 Temple Place, Suite 330, Boston, MA  02111-1307  USA.
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

#include "ipmipower_auth.h"
#include "ipmipower_wrappers.h"

auth_type_t 
ipmipower_auth_index(char *str) 
{
  assert(str != NULL);

  if (!strcasecmp(str, "auto"))
    return AUTH_TYPE_AUTO;
  else if (!strcasecmp(str, "none"))
    return AUTH_TYPE_NONE;
  else if (!strcasecmp(str, "straight_passwd_key"))
    return AUTH_TYPE_STRAIGHT_PASSWD_KEY;
  else if (!strcasecmp(str, "md2"))
    return AUTH_TYPE_MD2;
  else if (!strcasecmp(str, "md5"))
    return AUTH_TYPE_MD5;
  else 
    return AUTH_TYPE_INVALID;
}

char *
ipmipower_auth_string(auth_type_t at) 
{
  assert(AUTH_TYPE_VALID(at));

  switch(at) 
    {
    case AUTH_TYPE_AUTO:
      return "auto";
      break;
    case AUTH_TYPE_NONE:
      return "none";
      break;
    case AUTH_TYPE_STRAIGHT_PASSWD_KEY:
      return "straight_passwd_key";
      break;
    case AUTH_TYPE_MD2:
      return "md2";
      break;
    case AUTH_TYPE_MD5:
      return "md5";
      break;
    default:
      err_exit("ipmipower_auth_string: Invalid Auth Type: %d\n", at);
    }
  
  return NULL;                  /* NOT_REACHED */
}

char *
ipmipower_auth_list(void) 
{
  return "auto, none, straight_passwd_key, md2, md5";
}

u_int8_t
ipmipower_ipmi_auth_type(auth_type_t at)
{
  assert(AUTH_TYPE_VALID(at));

  switch(at) 
    {
    case AUTH_TYPE_NONE:
      return IPMI_SESSION_AUTH_TYPE_NONE;
      break;
    case AUTH_TYPE_STRAIGHT_PASSWD_KEY:
      return IPMI_SESSION_AUTH_TYPE_STRAIGHT_PASSWD_KEY;
      break;
    case AUTH_TYPE_MD2:
      return IPMI_SESSION_AUTH_TYPE_MD2;
      break;
    case AUTH_TYPE_MD5:
      return IPMI_SESSION_AUTH_TYPE_MD5;
      break;
    default:
      err_exit("ipmipower_ipmi_auth-type: Invalid Auth Type: %d\n", at);
    }
  
  return 0;                  /* NOT_REACHED */
}
