/*****************************************************************************\
 *  $Id: ipmi-fru-output.c,v 1.8 2010-03-02 21:09:07 chu11 Exp $
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

#if HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <assert.h>

#include <freeipmi/freeipmi.h>

#include "ipmi-fru_.h"
#include "ipmi-fru-oem-wistron.h"

#include "freeipmi-portability.h"

int
ipmi_fru_oem_wistron_oem_record (ipmi_fru_state_data_t *state_data,
				 uint8_t record_type_id,
				 uint32_t manufacturer_id,
				 uint8_t *oem_data,
				 unsigned int oem_data_len)
{
  assert (state_data);
  assert (state_data->oem_data.manufacturer_id == IPMI_IANA_ENTERPRISE_ID_WISTRON);
  assert (oem_data);

  if (state_data->oem_data.product_id == IPMI_WISTRON_PRODUCT_ID_C6220)
    {
      if (record_type_id == IPMI_FRU_OEM_WISTRON_PROPRIETARY_STRING
	  && oem_data_len)
	{
	  char string[IPMI_OEM_WISTRON_PROPRIETARY_STRING_MAX + 1];
	  unsigned int len;
	  
	  memset (string, '\0', IPMI_OEM_WISTRON_PROPRIETARY_STRING_MAX + 1);
	  
	  len = oem_data_len;
	  if (len > IPMI_OEM_WISTRON_PROPRIETARY_STRING_MAX)
	    len = IPMI_OEM_WISTRON_PROPRIETARY_STRING_MAX;
	  
	  memcpy (string,
		  oem_data,
		  len);
	  
	  pstdout_printf (state_data->pstate,
			  "  FRU Proprietary String: %s\n",
			  string);
	  
	  return (1);
	}
    }
  
  return (0);
}

