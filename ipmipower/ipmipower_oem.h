/*****************************************************************************\
 *  $Id: ipmipower_util.h,v 1.23 2010-02-08 22:02:31 chu11 Exp $
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

#ifndef IPMIPOWER_OEM_H
#define IPMIPOWER_OEM_H

#include "ipmipower.h"

/*
 * OEM C410x
 */

extern fiid_template_t tmpl_cmd_c410x_slot_power_control_rq;
extern fiid_template_t tmpl_cmd_c410x_slot_power_control_rs;

/* ipmipower_oem_power_cmd_check_support_and_privilege
 * - check if power cmd supported for OEM type and privilege level ok
 * - returns 1 if ok, 0 if not, -1 on error
 * - if returns 0 or -1, error message in buf 
 */
int ipmipower_oem_power_cmd_check_support_and_privilege (ipmipower_power_cmd_t cmd,
							 char *errbuf,
							 unsigned int errbuflen);

/* ipmipower_oem_power_cmd_check_extra_arg
 * - check if extra arg is valid
 * - returns 1 if ok, 0 if not, -1 on error
 * - if returns 0 or -1, error message in buf
 */
int ipmipower_oem_power_cmd_check_extra_arg (const char *extra_arg,
					     char *errbuf,
					     unsigned int errbuflen);

#endif /* IPMIPOWER_OEM_H */
