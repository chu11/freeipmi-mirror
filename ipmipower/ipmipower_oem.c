/*****************************************************************************\
 *  $Id: ipmipower_util.c,v 1.37 2010-02-08 22:02:31 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2007-2014 Lawrence Livermore National Security, LLC.
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
 *  Free Software Foundation; either version 3 of the License, or (at your
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
#include <stdint.h>
#if STDC_HEADERS
#include <string.h>
#include <stdarg.h>
#endif /* STDC_HEADERS */
#if HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */
#include <assert.h>
#include <errno.h>

#include "ipmipower_oem.h"
#include "ipmipower_error.h"
#include "ipmipower_util.h"

#include "freeipmi-portability.h"
#include "cbuf.h"

extern struct ipmipower_arguments cmd_args;

struct oem_power_type_data oem_power_type_data[] =
  { 
    /*
     * IPMIPOWER_OEM_POWER_TYPE_NONE
     */
    {
      "none",
      IPMIPOWER_OEM_POWER_TYPE_SUPPORT_ALL,
    },

    /*
     * IPMIPOWER_OEM_POWER_TYPE_C410X - supports off, on, status
     */
    {
      "C410x",
      IPMIPOWER_OEM_POWER_TYPE_SUPPORT_OFF | IPMIPOWER_OEM_POWER_TYPE_SUPPORT_ON | IPMIPOWER_OEM_POWER_TYPE_SUPPORT_STATUS,
    },

    {
      NULL,
      0,
    },
  };

/* OEM fiid templates */

/* Dell Poweredge OEM
 *
 * From Dell Provided Docs
 *
 * Slot Power Control Request
 *
 * 0x30 - OEM network function
 * 0xF0 - OEM cmd
 * 0x?? - bit 0 - slot 1
 *      - bit 1 - slot 2
 *      - ...
 *      - bit 7 - slot 8
 * 0x?? - bit 0 - slot 9
 *      - bit 1 - slot 10
 *      - ...
 *      - bit 7 - slot 16
 *
 * only should do one slot at a time
 *
 * Slot Power Control Response
 *
 * 0xF0 - OEM cmd
 * 0x?? - Completion Code
 */

fiid_template_t tmpl_cmd_c410x_slot_power_control_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "slot_number_bitmask", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_c410x_slot_power_control_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 0, "", 0}
  };

static int
_ipmipower_power_cmd_to_oem_power_type_support (ipmipower_power_cmd_t cmd)
{
  assert (IPMIPOWER_POWER_CMD_VALID (cmd));

  switch (cmd)
    {
    case IPMIPOWER_POWER_CMD_POWER_OFF:
      return (IPMIPOWER_OEM_POWER_TYPE_SUPPORT_OFF);
    case IPMIPOWER_POWER_CMD_POWER_ON:
      return (IPMIPOWER_OEM_POWER_TYPE_SUPPORT_ON);
    case IPMIPOWER_POWER_CMD_POWER_CYCLE:
      return (IPMIPOWER_OEM_POWER_TYPE_SUPPORT_CYCLE);
    case IPMIPOWER_POWER_CMD_POWER_RESET:
      return (IPMIPOWER_OEM_POWER_TYPE_SUPPORT_RESET);
    case IPMIPOWER_POWER_CMD_POWER_STATUS:
      return (IPMIPOWER_OEM_POWER_TYPE_SUPPORT_STATUS);
    case IPMIPOWER_POWER_CMD_PULSE_DIAGNOSTIC_INTERRUPT:
      return (IPMIPOWER_OEM_POWER_TYPE_SUPPORT_DIAGNOSTIC_INTERRUPT);
    case IPMIPOWER_POWER_CMD_SOFT_SHUTDOWN_OS:
      return (IPMIPOWER_OEM_POWER_TYPE_SUPPORT_SOFT_SHUTDOWN_OS);
    case IPMIPOWER_POWER_CMD_IDENTIFY_ON:
      return (IPMIPOWER_OEM_POWER_TYPE_SUPPORT_IDENTIFY_ON);
    case IPMIPOWER_POWER_CMD_IDENTIFY_OFF:
      return (IPMIPOWER_OEM_POWER_TYPE_SUPPORT_IDENTIFY_OFF);
    case IPMIPOWER_POWER_CMD_IDENTIFY_STATUS:
      return (IPMIPOWER_OEM_POWER_TYPE_SUPPORT_IDENTIFY_STATUS);
    default:
      IPMIPOWER_ERROR (("_ipmipower_power_cmd_to_oem_power_type_support: invalid power cmd: %d", cmd));
      exit (EXIT_FAILURE);
    }

  return (-1);			/* NOT REACHED */
}

int
ipmipower_oem_power_cmd_check_support_and_privilege (ipmipower_power_cmd_t cmd,
						     char *errbuf,
						     unsigned int errbuflen)
{
  unsigned int oem_power_type_support_mask;
  char *power_cmd_str;
  int rv = -1;
  
  assert (IPMIPOWER_POWER_CMD_VALID (cmd));
  /* errbuf & errbuflen can be NULL/0 if doing an assert check */
  assert (cmd_args.oem_power_type != IPMIPOWER_OEM_POWER_TYPE_NONE);

  oem_power_type_support_mask = _ipmipower_power_cmd_to_oem_power_type_support (cmd);
  
  power_cmd_str = ipmipower_power_cmd_to_string (cmd);
  
  if (!(oem_power_type_data[cmd_args.oem_power_type].supported_operations & oem_power_type_support_mask))
    {
      if (errbuf && errbuflen)
	snprintf (errbuf,
		  errbuflen,
		  "'%s' operation not supported by oem power type '%s'",
		  power_cmd_str,
		  oem_power_type_data[cmd_args.oem_power_type].name);
      rv = 0;
      goto cleanup;
    }
  
  if (cmd_args.oem_power_type == IPMIPOWER_OEM_POWER_TYPE_C410X)
    {
      /* XXX - I'm pretty sure */
      if ((cmd == IPMIPOWER_POWER_CMD_POWER_OFF
	   || cmd == IPMIPOWER_POWER_CMD_POWER_ON)
	  && (cmd_args.common_args.privilege_level == IPMI_PRIVILEGE_LEVEL_USER
	      || cmd_args.common_args.privilege_level == IPMI_PRIVILEGE_LEVEL_OPERATOR))
	{
	  if (errbuf && errbuflen)
	    snprintf (errbuf,
		      errbuflen,
		      "'%s' requires admin privilege for oem power type '%s'",
		      power_cmd_str,
		      oem_power_type_data[cmd_args.oem_power_type].name);
	  rv = 0;
	  goto cleanup;
	} 
    }

  rv = 1;
 cleanup:
  return (rv);
}

int
ipmipower_oem_power_cmd_check_extra_arg (const char *extra_arg,
					 char *errbuf,
					 unsigned int errbuflen)
{
  int rv = -1;

  /* extra_arg can be NULL, user didn't input one */
  /* errbuf & errbuflen can be NULL/0 if doing an assert check */
  assert (cmd_args.oem_power_type != IPMIPOWER_OEM_POWER_TYPE_NONE);
  
  if (cmd_args.oem_power_type == IPMIPOWER_OEM_POWER_TYPE_C410X)
    {
      char *endptr;
      unsigned int tmp;
      
      if (!extra_arg)
	{
	  if (errbuf && errbuflen)
	    snprintf (errbuf,
		      errbuflen,
		      "slot number must be specified for oem power type '%s'",
		      oem_power_type_data[cmd_args.oem_power_type].name);
	  rv = 0;
	  goto cleanup;
	}
      
      errno = 0; 
      tmp = strtol (extra_arg, &endptr, 0);
      if (errno
          || endptr[0] != '\0')
	{
	  if (errbuf && errbuflen)
	    snprintf (errbuf,
		      errbuflen,
		      "slot number '%s' for oem power type '%s' invalid",
		      extra_arg,
		      oem_power_type_data[cmd_args.oem_power_type].name);
	  rv = 0;
	  goto cleanup;
	}
      
      if (tmp < IPMI_OEM_DELL_SLOT_POWER_CONTROL_SLOT_NUMBER_MIN
	  || tmp > IPMI_OEM_DELL_SLOT_POWER_CONTROL_SLOT_NUMBER_MAX)
	{
	  if (errbuf && errbuflen)
	    snprintf (errbuf,
		      errbuflen,
		      "slot number '%s' for oem power type '%s' out of range",
		      extra_arg,
		      oem_power_type_data[cmd_args.oem_power_type].name);
	  rv = 0;
	  goto cleanup;
	}
    }
  
  rv = 1;
 cleanup:
  return (rv);
}
