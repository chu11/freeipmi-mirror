/*****************************************************************************\
 *  $Id: ipmipower_driver_type.c,v 1.1 2008-05-12 23:46:50 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2007-2008 Lawrence Livermore National Security, LLC.
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

#include "ipmipower_driver_type.h"
#include "ipmipower_wrappers.h"

#include "tool-cmdline-common.h"

driver_type_t 
ipmipower_driver_type_index(char *str) 
{
  assert(str != NULL);

  /* accept 1.5 or 2.0 strings for backwards compatability */

  if (!strcasecmp(str, "1.5"))
    return DRIVER_TYPE_LAN;
  /* support "lanplus" for those that might be used to ipmitool.
   * support typo variants to ease.
   */
  else if (!strcasecmp(str, "2.0"))
    return DRIVER_TYPE_LAN_2_0;
  else 
    {
      int driver_type;
      
      driver_type = parse_outofband_driver_type(str);
      
      if (driver_type == IPMI_DEVICE_LAN)
        return DRIVER_TYPE_LAN;
      else if (driver_type == IPMI_DEVICE_LAN_2_0)
        return DRIVER_TYPE_LAN_2_0;
      return DRIVER_TYPE_INVALID;
    }
}

char *
ipmipower_driver_type_string(driver_type_t driver_type) 
{
  assert(DRIVER_TYPE_VALID(driver_type));

  switch(driver_type) 
    {
    case DRIVER_TYPE_LAN:
      return "lan";
      break;
    case DRIVER_TYPE_LAN_2_0:
      return "lan_2_0";
      break;
    default:
      ierr_exit("ipmipower_driver_type_string: Invalid Ipmi Version Type: %d\n", driver_type);
    }
  
  return NULL;                  /* NOT_REACHED */
}

char *
ipmipower_driver_type_list(void) 
{
  return "lan, lan_2_0";
}
