/*
 * Copyright (C) 2003-2014 FreeIPMI Core Team
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 * 
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#include <stdarg.h>
#endif /* STDC_HEADERS */
#if TIME_WITH_SYS_TIME
#include <sys/time.h>
#include <time.h>
#else /* !TIME_WITH_SYS_TIME */
#if HAVE_SYS_TIME_H
#include <sys/time.h>
#else /* !HAVE_SYS_TIME_H */
#include <time.h>
#endif /* !HAVE_SYS_TIME_H */
#endif /* !TIME_WITH_SYS_TIME */
#include <assert.h>
#include <errno.h>

#include "freeipmi/sel/ipmi-sel.h"

#include "freeipmi/cmds/ipmi-sel-cmds.h"
#include "freeipmi/record-format/ipmi-sdr-record-format.h"
#include "freeipmi/record-format/ipmi-sel-record-format.h"
#include "freeipmi/spec/ipmi-event-reading-type-code-spec.h"
#include "freeipmi/spec/ipmi-event-reading-type-code-oem-spec.h"
#include "freeipmi/spec/ipmi-iana-enterprise-numbers-spec.h"
#include "freeipmi/spec/ipmi-product-id-spec.h"
#include "freeipmi/spec/ipmi-sensor-and-event-code-tables-spec.h"
#include "freeipmi/spec/ipmi-sensor-and-event-code-tables-oem-spec.h"
#include "freeipmi/spec/ipmi-sensor-numbers-oem-spec.h"
#include "freeipmi/spec/ipmi-sensor-types-spec.h"
#include "freeipmi/spec/ipmi-sensor-types-oem-spec.h"
#include "freeipmi/spec/ipmi-sensor-units-spec.h"
#include "freeipmi/spec/ipmi-slave-address-spec.h"
#include "freeipmi/spec/ipmi-slave-address-oem-spec.h"
#include "freeipmi/util/ipmi-iana-enterprise-numbers-util.h"
#include "freeipmi/util/ipmi-sensor-and-event-code-tables-util.h"

#include "ipmi-sel-common.h"
#include "ipmi-sel-defs.h"
#include "ipmi-sel-string.h"
#include "ipmi-sel-string-supermicro.h"
#include "ipmi-sel-trace.h"
#include "ipmi-sel-util.h"

#include "freeipmi-portability.h"

/* return (0) - no OEM match
 * return (1) - OEM match
 * return (-1) - error, cleanup and return error
 */
int
sel_string_output_supermicro_event_data1_class_oem (ipmi_sel_ctx_t ctx,
						    struct ipmi_sel_entry *sel_entry,
						    uint8_t sel_record_type,
						    char *tmpbuf,
						    unsigned int tmpbuflen,
						    unsigned int flags,
						    unsigned int *wlen,
						    struct ipmi_sel_system_event_record_data *system_event_record_data)
{
  assert (ctx);
  assert (ctx->magic == IPMI_SEL_CTX_MAGIC);
  assert (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_SUPERMICRO
	  || ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_SUPERMICRO_WORKAROUND
	  || ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_PEPPERCON
	  || ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_MAGNUM_TECHNOLOGIES);
  assert (sel_entry);
  assert (tmpbuf);
  assert (tmpbuflen);
  assert (!(flags & ~IPMI_SEL_STRING_FLAGS_MASK));
  assert (flags & IPMI_SEL_STRING_FLAGS_INTERPRET_OEM_DATA);
  assert (wlen);
  assert (system_event_record_data);

  /* OEM Interpretation
   *
   * 
   * Supermicro X7DBR-3 (X7DBR_3)     
   * Supermicro X7DB8       
   * Supermicro X8DTN       
   * Supermicro X7SBI-LN4 (X7SBI_LN4) 
   * Supermicro X8DTH       
   * Supermicro X8DTG       
   * Supermicro X8DTU       
   * Supermicro X8DT3-LN4F (X8DT3_LN4F)         
   * Supermicro X8DTU-6+ (X8DTU_6PLUS)
   * Supermicro X8DTL       
   * Supermicro X8DTL-3F (X8DTL_3F)   
   * Supermicro X8SIL-F  (X8SIL_F)    
   * Supermicro X9SCL       
   * Supermicro X9SCM       
   * Supermicro X8DTN+-F (X8DTNPLUS_F)
   * Supermicro X8SIE       
   * Supermicro X9SCA-F-O (X9SCA_F_O) 
   * Supermicro H8DGU-F (H8DGU_F)
   * Supermicro X9DRi-F (X9DRI_F)
   * Supermicro X9DRI-LN4F+ (X9DRI_LN4F_PLUS)
   * Supermicro X9SPU-F-O (X9SPU_F_O) 
   * Supermicro X9SCM-iiF (X9SCM_IIF)
   *    
   * Note: Early Supermicro motherboards used the "Peppercon" Manufacturer ID 
   * Note: Some Supermicro motherboards are rebranded with random manufacturer IDs    
   */

  /* achu: Via reverse engineering, see 
   *
   * "Supermicro X8DTG-QF System Event Log" thread in late
   * January/early February 2012.
   */

  if (((ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_PEPPERCON
	&& (ctx->product_id == IPMI_SUPERMICRO_PRODUCT_ID_X7DBR_3
	    || ctx->product_id == IPMI_SUPERMICRO_PRODUCT_ID_X7DB8
	    || ctx->product_id == IPMI_SUPERMICRO_PRODUCT_ID_X8DTN
	    || ctx->product_id == IPMI_SUPERMICRO_PRODUCT_ID_X7SBI_LN4))
       || ((ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_SUPERMICRO
	    || ctx->manufacturer_id ==  IPMI_IANA_ENTERPRISE_ID_SUPERMICRO_WORKAROUND)
	   && (ctx->product_id == IPMI_SUPERMICRO_PRODUCT_ID_X8DTH
	       || ctx->product_id == IPMI_SUPERMICRO_PRODUCT_ID_X8DTG
	       || ctx->product_id == IPMI_SUPERMICRO_PRODUCT_ID_X8DTU
	       || ctx->product_id == IPMI_SUPERMICRO_PRODUCT_ID_X8DT3_LN4F
	       || ctx->product_id == IPMI_SUPERMICRO_PRODUCT_ID_X8DTU_6PLUS
	       || ctx->product_id == IPMI_SUPERMICRO_PRODUCT_ID_X8DTL
	       || ctx->product_id == IPMI_SUPERMICRO_PRODUCT_ID_X8DTL_3F
	       || ctx->product_id == IPMI_SUPERMICRO_PRODUCT_ID_X8SIL_F
	       || ctx->product_id == IPMI_SUPERMICRO_PRODUCT_ID_X9SCL
	       || ctx->product_id == IPMI_SUPERMICRO_PRODUCT_ID_X9SCM
	       || ctx->product_id == IPMI_SUPERMICRO_PRODUCT_ID_X8DTNPLUS_F
	       || ctx->product_id == IPMI_SUPERMICRO_PRODUCT_ID_X8SIE
	       || ctx->product_id == IPMI_SUPERMICRO_PRODUCT_ID_X9SCA_F_O
	       || ctx->product_id == IPMI_SUPERMICRO_PRODUCT_ID_H8DGU_F
	       || ctx->product_id == IPMI_SUPERMICRO_PRODUCT_ID_H8DGU
	       || ctx->product_id == IPMI_SUPERMICRO_PRODUCT_ID_H8DG6
	       || ctx->product_id == IPMI_SUPERMICRO_PRODUCT_ID_X9DRI_F
	       || ctx->product_id == IPMI_SUPERMICRO_PRODUCT_ID_X9DRI_LN4F_PLUS
	       || ctx->product_id == IPMI_SUPERMICRO_PRODUCT_ID_X9SPU_F_O
	       || ctx->product_id == IPMI_SUPERMICRO_PRODUCT_ID_X9SCM_IIF))
       || (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_MAGNUM_TECHNOLOGIES
	   && ctx->product_id == IPMI_SUPERMICRO_PRODUCT_ID_X8DTL))
      && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_SUPERMICRO_GENERIC
      && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_OEM_SUPERMICRO_CPU_TEMP
      && system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_OEM_SUPERMICRO_CPU_TEMP_SEL_OVERHEAT)
    {
      snprintf (tmpbuf,
		tmpbuflen,
		"Overheat");

      return (1);
    }

  return (0);
}

