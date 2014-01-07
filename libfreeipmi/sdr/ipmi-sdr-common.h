/*****************************************************************************\
 *  $Id: ipmi-sdr-common.h,v 1.8 2010-02-08 22:09:40 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2007-2014 Lawrence Livermore National Security, LLC.
 *  Copyright (C) 2006-2007 The Regents of the University of California.
 *  Produced at Lawrence Livermore National Laboratory (cf, DISCLAIMER).
 *  Written by Albert Chu <chu11@llnl.gov>
 *  UCRL-CODE-222073
 *
 *  This file is part of Ipmimonitoring, an IPMI sensor monitoring
 *  library.  For details, see http://www.llnl.gov/linux/.
 *
 *  Ipmimonitoring is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by the
 *  Free Software Foundation; either version 3 of the License, or (at your
 *  option) any later version.
 *
 *  Ipmimonitoring is distributed in the hope that it will be useful, but
 *  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 *  for more details.
 *
 *  You should have received a copy of the GNU General Public License along
 *  with Ipmimonitoring.  If not, see <http://www.gnu.org/licenses/>.
\*****************************************************************************/

#ifndef IPMI_SDR_COMMON_H
#define IPMI_SDR_COMMON_H

#include <stdint.h>

#include "freeipmi/sdr/ipmi-sdr.h"

#include "ipmi-sdr-defs.h"

#define IPMI_SDR_CACHE_DEBUG_BUFLEN 256

void sdr_init_ctx (ipmi_sdr_ctx_t ctx);

int sdr_info (ipmi_sdr_ctx_t ctx,
	      ipmi_ctx_t ipmi_ctx,
	      uint8_t *sdr_version,
	      uint16_t *record_count,
	      uint32_t *most_recent_addition_timestamp,
	      uint32_t *most_recent_erase_timestamp);

const char *sdr_record_type_str (ipmi_sdr_ctx_t ctx,
				 uint8_t *sdr_record,
				 unsigned int sdr_record_len);

void sdr_check_read_status (ipmi_sdr_ctx_t ctx);

#endif /* IPMI_SDR_COMMON_H */
