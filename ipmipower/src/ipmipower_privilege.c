/*****************************************************************************\
 *  $Id: ipmipower_privilege.c,v 1.4.2.3 2006-02-15 05:05:56 chu11 Exp $
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

#include "ipmipower_privilege.h"
#include "ipmipower_wrappers.h"

privilege_type_t 
ipmipower_privilege_index(char *str) 
{
  assert(str != NULL);

  if (!strcasecmp(str, "auto"))
    return PRIVILEGE_TYPE_AUTO;
  else if (!strcasecmp(str, "user"))
    return PRIVILEGE_TYPE_USER;
  else if (!strcasecmp(str, "operator"))
    return PRIVILEGE_TYPE_OPERATOR;
  else if (!strcasecmp(str, "admin"))
    return PRIVILEGE_TYPE_ADMIN;
  else 
    return PRIVILEGE_TYPE_INVALID;
}

char *
ipmipower_privilege_string(privilege_type_t priv) 
{
  assert(PRIVILEGE_TYPE_VALID_OR_AUTO(priv));

  switch(priv) 
    {
    case PRIVILEGE_TYPE_AUTO:
      return "auto";
      break;
    case PRIVILEGE_TYPE_USER:
      return "user";
      break;
    case PRIVILEGE_TYPE_OPERATOR:
      return "operator";
      break;
    case PRIVILEGE_TYPE_ADMIN:
      return "admin";
      break;
    default:
      err_exit("ipmipower_privilege_string: Invalid Privilege Type: %d\n", priv);
    }
  
  return NULL;                  /* NOT_REACHED */
}

char *
ipmipower_privilege_list(void) 
{
  return "auto, user, operator, admin";
}

uint8_t
ipmipower_ipmi_privilege_type(privilege_type_t priv)
{
  assert(PRIVILEGE_TYPE_VALID(priv));

  switch(priv) 
    {
    case PRIVILEGE_TYPE_USER:
      return IPMI_PRIVILEGE_LEVEL_USER;
      break;
    case PRIVILEGE_TYPE_OPERATOR:
      return IPMI_PRIVILEGE_LEVEL_OPERATOR;
      break;
    case PRIVILEGE_TYPE_ADMIN:
      return IPMI_PRIVILEGE_LEVEL_ADMIN;
      break;
    default:
      err_exit("ipmipower_ipmi_privilege_type: Invalid Privilege Type: %d\n", priv);
    }
  
  return 0;                  /* NOT_REACHED */
}
