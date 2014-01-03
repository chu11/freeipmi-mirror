/*****************************************************************************\
 *  $Id: ipmiseld.h,v 1.11 2010-02-08 22:02:30 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2012-2014 Lawrence Livermore National Security, LLC.
 *  Produced at Lawrence Livermore National Laboratory (cf, DISCLAIMER).
 *  Written by Albert Chu <chu11@llnl.gov>
 *  LLNL-CODE-559172
 *
 *  This file is part of Ipmiseld, an IPMI SEL syslog logging daemon.
 *  For details, see http://www.llnl.gov/linux/.
 *
 *  Ipmiseld is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by the
 *  Free Software Foundation; either version 3 of the License, or (at your
 *  option) any later version.
 *
 *  Ipmiseld is distributed in the hope that it will be useful, but
 *  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 *  for more details.
 *
 *  You should have received a copy of the GNU General Public License along
 *  with Ipmiseld.  If not, see <http://www.gnu.org/licenses/>.
\*****************************************************************************/

#ifndef IPMISELD_COMMON_H
#define IPMISELD_COMMON_H

#include "ipmiseld.h"

#define IPMISELD_NOMINAL_FILTER  0x01
#define IPMISELD_WARNING_FILTER  0x02
#define IPMISELD_CRITICAL_FILTER 0x04
#define IPMISELD_NA_FILTER       0x08

int ipmiseld_event_state_filter_parse (const char *str);

int ipmiseld_log_facility_parse (const char *str);

int ipmiseld_log_priority_parse (const char *str);

void ipmiseld_syslog (ipmiseld_host_data_t *host_data,
		      const char *message,
		      ...);

void ipmiseld_syslog_host (ipmiseld_host_data_t *host_data,
			   const char *message,
			   ...);

void ipmiseld_err_output (ipmiseld_host_data_t *host_data,
			  const char *message,
			  ...);

#endif /* IPMISELD_COMMON_H */
