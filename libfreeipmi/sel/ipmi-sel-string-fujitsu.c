/*
  Copyright (C) 2003-2015 FreeIPMI Core Team

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2, or (at your option)
  any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software Foundation,
  Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA.
*/

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <assert.h>
#include <errno.h>

#include "freeipmi/sel/ipmi-sel.h"

#include "freeipmi/cmds/ipmi-device-global-cmds.h"
#include "freeipmi/cmds/ipmi-sel-cmds.h"
#include "freeipmi/spec/ipmi-netfn-spec.h"
#include "freeipmi/spec/ipmi-comp-code-spec.h"
#include "freeipmi/record-format/ipmi-sdr-record-format.h"
#include "freeipmi/record-format/ipmi-sel-record-format.h"
#include "freeipmi/spec/ipmi-event-reading-type-code-spec.h"
#include "freeipmi/spec/ipmi-iana-enterprise-numbers-spec.h"
#include "freeipmi/spec/ipmi-product-id-spec.h"
#include "freeipmi/spec/ipmi-sensor-and-event-code-tables-spec.h"
#include "freeipmi/spec/ipmi-sensor-types-spec.h"
#include "freeipmi/spec/ipmi-slave-address-spec.h"
#include "freeipmi/spec/oem/ipmi-cmd-oem-fujitsu-spec.h"
#include "freeipmi/spec/oem/ipmi-oem-fujitsu-spec.h"
#include "freeipmi/spec/oem/ipmi-sensor-and-event-code-tables-oem-fujitsu-spec.h"
#include "freeipmi/spec/oem/ipmi-sensor-types-oem-fujitsu-spec.h"
#include "freeipmi/util/ipmi-iana-enterprise-numbers-util.h"
#include "freeipmi/util/ipmi-sensor-and-event-code-tables-util.h"

#include "ipmi-sel-common.h"
#include "ipmi-sel-defs.h"
#include "ipmi-sel-string.h"
#include "ipmi-sel-string-fujitsu.h"
#include "ipmi-sel-string-fujitsu-irmc-common.h"
#include "ipmi-sel-trace.h"
#include "ipmi-sel-util.h"

#include "freeipmi-portability.h"

/* return (0) - no OEM match
 * return (1) - OEM match
 * return (-1) - error, cleanup and return error
 */
int
sel_string_output_fujitsu_event_data1_class_sensor_specific_discrete (ipmi_sel_ctx_t ctx,
								      struct ipmi_sel_entry *sel_entry,
								      uint8_t sel_record_type,
								      char *tmpbuf,
								      unsigned int tmpbuflen,
								      unsigned int flags,
								      unsigned int *wlen,
								      struct ipmi_sel_system_event_record_data *system_event_record_data) 
{
  int ret;

  assert (ctx);
  assert (ctx->magic == IPMI_SEL_CTX_MAGIC);
  assert (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_FUJITSU);
  assert (sel_entry);
  assert (tmpbuf);
  assert (tmpbuflen);
  assert (!(flags & ~IPMI_SEL_STRING_FLAGS_MASK));
  assert (flags & IPMI_SEL_STRING_FLAGS_INTERPRET_OEM_DATA);
  assert (wlen);
  assert (system_event_record_data);
  assert (system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_SENSOR_SPECIFIC);

  /* 
   * Fujitsu iRMC / iRMC S2
   */
  if (ctx->product_id >= IPMI_FUJITSU_PRODUCT_ID_MIN
       && ctx->product_id <= IPMI_FUJITSU_PRODUCT_ID_MAX)
    {
      if ((ret = sel_string_output_fujitsu_event_data1_class_sensor_specific_discrete (ctx,
										       sel_entry,
										       sel_record_type,
										       tmpbuf,
										       tmpbuflen,
										       flags,
										       wlen,
										       system_event_record_data)) < 0)
	return (-1);

      if (ret)
	return (1);
    }
  
  return (0);
}

/* return (0) - no OEM match
 * return (1) - OEM match
 * return (-1) - error, cleanup and return error
 *
 * in oem_rv, return
 * 0 - continue on
 * 1 - buffer full, return full buffer to user
 */
int
sel_string_output_fujitsu_event_data2_event_data3 (ipmi_sel_ctx_t ctx,
						   struct ipmi_sel_entry *sel_entry,
						   uint8_t sel_record_type,
						   char *buf,
						   unsigned int buflen,
						   unsigned int flags,
						   unsigned int *wlen,
						   struct ipmi_sel_system_event_record_data *system_event_record_data,
						   int *oem_rv)
{
  int ret;

  assert (ctx);
  assert (ctx->magic == IPMI_SEL_CTX_MAGIC);
  assert (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_FUJITSU);
  assert (sel_entry);
  assert (buf);
  assert (buflen);
  assert (!(flags & ~IPMI_SEL_STRING_FLAGS_MASK));
  assert (flags & IPMI_SEL_STRING_FLAGS_INTERPRET_OEM_DATA);
  assert (wlen);
  assert (system_event_record_data);
  assert (oem_rv);

  /* 
   * Fujitsu iRMC / iRMC S2
   */
  if (ctx->product_id >= IPMI_FUJITSU_PRODUCT_ID_MIN
       && ctx->product_id <= IPMI_FUJITSU_PRODUCT_ID_MAX)
    {
      if ((ret = sel_string_output_fujitsu_irmc_common_event_data2_event_data3 (ctx,
										sel_entry,
										sel_record_type,
										buf,
										buflen,
										flags,
										wlen,
										system_event_record_data,
										oem_rv)) < 0)
	return (-1);
      
      if (ret)
	return (1);
    }

  return (0);
}

/* return (0) - no OEM match
 * return (1) - OEM match
 * return (-1) - error, cleanup and return error
 *
 * in oem_rv, return
 * 0 - continue on
 * 1 - buffer full, return full buffer to user
 */
int
sel_string_output_fujitsu_oem_record_data (ipmi_sel_ctx_t ctx,
					   struct ipmi_sel_entry *sel_entry,
					   uint8_t sel_record_type,
					   char *buf,
					   unsigned int buflen,
					   unsigned int flags,
					   unsigned int *wlen,
					   int *oem_rv)
{
  int ret;

  assert (ctx);
  assert (ctx->magic == IPMI_SEL_CTX_MAGIC);
  assert (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_FUJITSU);
  assert (ipmi_sel_record_type_class (sel_record_type) == IPMI_SEL_RECORD_TYPE_CLASS_TIMESTAMPED_OEM_RECORD
          || ipmi_sel_record_type_class (sel_record_type) == IPMI_SEL_RECORD_TYPE_CLASS_NON_TIMESTAMPED_OEM_RECORD);
  assert (sel_entry);
  assert (buf);
  assert (buflen);
  assert (!(flags & ~IPMI_SEL_STRING_FLAGS_MASK));
  assert (flags & IPMI_SEL_STRING_FLAGS_INTERPRET_OEM_DATA);
  assert (wlen);
  assert (oem_rv);

  /* 
   * Fujitsu iRMC / iRMC S2
   */
  if (ctx->product_id >= IPMI_FUJITSU_PRODUCT_ID_MIN
       && ctx->product_id <= IPMI_FUJITSU_PRODUCT_ID_MAX)
    {
      if ((ret = sel_string_output_fujitsu_irmc_common_oem_record_data (ctx,
									sel_entry,
									sel_record_type,
									buf,
									buflen,
									flags,
									wlen,
									oem_rv)) < 0)
	return (-1);

      if (ret)
	return (1);
}

  return (0);
}

/* return (0) - no OEM match
 * return (1) - OEM match
 * return (-1) - error, cleanup and return error
 *
 * in oem_rv, return
 * 0 - continue on
 * 1 - buffer full, return full buffer to user
 */
int
sel_string_output_fujitsu_oem_string (ipmi_sel_ctx_t ctx,
				      struct ipmi_sel_entry *sel_entry,
				      uint8_t sel_record_type,
				      char *buf,
				      unsigned int buflen,
				      unsigned int flags,
				      unsigned int *wlen,
				      int *oem_rv)
{
  int ret;

  assert (ctx);
  assert (ctx->magic == IPMI_SEL_CTX_MAGIC);
  assert (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_FUJITSU);
  assert (sel_entry);
  assert (buf);
  assert (buflen);
  assert (!(flags & ~IPMI_SEL_STRING_FLAGS_MASK));
  assert (wlen);
  assert (oem_rv);
  
  /* 
   * Fujitsu iRMC / iRMC S2
   */
  if (ctx->product_id >= IPMI_FUJITSU_PRODUCT_ID_MIN
      && ctx->product_id <= IPMI_FUJITSU_PRODUCT_ID_MAX)
    {
      if ((ret = sel_string_output_fujitsu_irmc_common_oem_string (ctx,
								   sel_entry,
								   sel_record_type,
								   buf,
								   buflen,
								   flags,
								   wlen,
								   oem_rv)) < 0)
	return (-1);

      if (ret)
	return (1);
    }

  return (0);
}
