/*****************************************************************************\
 *  $Id: ipmi-fru-output.h,v 1.5 2010-02-08 22:20:58 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2007-2014 Lawrence Livermore National Security, LLC.
 *  Copyright (C) 2007 The Regents of the University of California.
 *  Produced at Lawrence Livermore National Laboratory (cf, DISCLAIMER).
 *  Written by Albert Chu <chu11@llnl.gov>
 *  UCRL-CODE-232183
 *
 *  This file is part of Ipmi-fru, a tool used for retrieving
 *  motherboard field replaceable unit (FRU) information. For details,
 *  see http://www.llnl.gov/linux/.
 *
 *  Ipmi-fru is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by the
 *  Free Software Foundation; either version 3 of the License, or (at your
 *  option) any later version.
 *
 *  Ipmi-fru is distributed in the hope that it will be useful, but
 *  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 *  for more details.
 *
 *  You should have received a copy of the GNU General Public License along
 *  with Ipmi-fru.  If not, see <http://www.gnu.org/licenses/>.
\*****************************************************************************/

#ifndef IPMI_FRU_OEM_WISTRON_H
#define IPMI_FRU_OEM_WISTRON_H

#include <freeipmi/freeipmi.h>

#include "ipmi-fru_.h"

/* Returns 1 on interpretation, 0 if not, -1 on error */
int ipmi_fru_oem_wistron_oem_record (ipmi_fru_state_data_t *state_data,
				     uint8_t record_type_id,
				     uint32_t manufacturer_id,
				     uint8_t *oem_data,
				     unsigned int oem_data_len);

#endif /* IPMI_FRU_OEM_WISTRON_H */
