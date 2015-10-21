/*
 * Copyright (C) 2003-2015 FreeIPMI Core Team
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
#include "ipmi-sel-string-supermicro-peppercon-x7dbr-3.h"
#include "ipmi-sel-string-supermicro-peppercon-x7db8.h"
#include "ipmi-sel-string-supermicro-peppercon-x8dtn.h"
#include "ipmi-sel-string-supermicro-peppercon-x7sbi-ln4.h"
#include "ipmi-sel-string-supermicro-x8dth.h"
#include "ipmi-sel-string-supermicro-x8dtg.h"
#include "ipmi-sel-string-supermicro-x8dtu.h"
#include "ipmi-sel-string-supermicro-x8dt3-ln4f.h"
#include "ipmi-sel-string-supermicro-x8dtu-6plus.h"
#include "ipmi-sel-string-supermicro-x8dtl.h"
#include "ipmi-sel-string-supermicro-x8dtl-3f.h"
#include "ipmi-sel-string-supermicro-x8sil-f.h"
#include "ipmi-sel-string-supermicro-x9scl.h"
#include "ipmi-sel-string-supermicro-x9scm.h"
#include "ipmi-sel-string-supermicro-x8dtnplus-f.h"
#include "ipmi-sel-string-supermicro-x8sie.h"
#include "ipmi-sel-string-supermicro-x9sca-f-o.h"
#include "ipmi-sel-string-supermicro-h8dgu-f.h"
#include "ipmi-sel-string-supermicro-h8dgu.h"                              
#include "ipmi-sel-string-supermicro-h8dg6.h"
#include "ipmi-sel-string-supermicro-x9dri-f.h"
#include "ipmi-sel-string-supermicro-x9dri-ln4f-plus.h"
#include "ipmi-sel-string-supermicro-x9spu-f-o.h"
#include "ipmi-sel-string-supermicro-x9scm-iif.h"
#include "ipmi-sel-string-supermicro-magnum-technologies-x8dtl.h"
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
  int ret;

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

  /* 
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

  if (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_PEPPERCON)
    {
      if (ctx->product_id == IPMI_SUPERMICRO_PRODUCT_ID_PEPPERCON_X7DBR_3)
	{
	  if ((ret = sel_string_output_supermicro_peppercon_x7dbr_3_event_data1_class_oem (ctx,
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

      if (ctx->product_id == IPMI_SUPERMICRO_PRODUCT_ID_PEPPERCON_X7DB8)
	{
	  if ((ret = sel_string_output_supermicro_peppercon_x7db8_event_data1_class_oem (ctx,
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

      if (ctx->product_id == IPMI_SUPERMICRO_PRODUCT_ID_PEPPERCON_X8DTN)
	{
	  if ((ret = sel_string_output_supermicro_peppercon_x8dtn_event_data1_class_oem (ctx,
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

      if (ctx->product_id == IPMI_SUPERMICRO_PRODUCT_ID_PEPPERCON_X7SBI_LN4)
	{
	  if ((ret = sel_string_output_supermicro_peppercon_x7sbi_ln4_event_data1_class_oem (ctx,
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
    }

  if (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_SUPERMICRO
      || ctx->manufacturer_id ==  IPMI_IANA_ENTERPRISE_ID_SUPERMICRO_WORKAROUND)
    {
      if (ctx->product_id == IPMI_SUPERMICRO_PRODUCT_ID_X8DTH)
	{
	  if ((ret = sel_string_output_supermicro_x8dth_event_data1_class_oem (ctx,
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

      if (ctx->product_id == IPMI_SUPERMICRO_PRODUCT_ID_X8DTG)
	{
	  if ((ret = sel_string_output_supermicro_x8dtg_event_data1_class_oem (ctx,
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

      if (ctx->product_id == IPMI_SUPERMICRO_PRODUCT_ID_X8DTU)
	{
	  if ((ret = sel_string_output_supermicro_x8dtu_event_data1_class_oem (ctx,
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

      if (ctx->product_id == IPMI_SUPERMICRO_PRODUCT_ID_X8DT3_LN4F)
	{
	  if ((ret = sel_string_output_supermicro_x8dt3_ln4f_event_data1_class_oem (ctx,
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

      if (ctx->product_id == IPMI_SUPERMICRO_PRODUCT_ID_X8DTU_6PLUS)
	{
	  if ((ret = sel_string_output_supermicro_x8dtu_6plus_event_data1_class_oem (ctx,
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

      if (ctx->product_id == IPMI_SUPERMICRO_PRODUCT_ID_X8DTL)
	{
	  if ((ret = sel_string_output_supermicro_x8dtl_event_data1_class_oem (ctx,
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

      if (ctx->product_id == IPMI_SUPERMICRO_PRODUCT_ID_X8DTL_3F)
	{
	  if ((ret = sel_string_output_supermicro_x8dtl_3f_event_data1_class_oem (ctx,
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

      if (ctx->product_id == IPMI_SUPERMICRO_PRODUCT_ID_X8SIL_F)
	{
	  if ((ret = sel_string_output_supermicro_x8sil_f_event_data1_class_oem (ctx,
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

      if (ctx->product_id == IPMI_SUPERMICRO_PRODUCT_ID_X9SCL)
	{
	  if ((ret = sel_string_output_supermicro_x9scl_event_data1_class_oem (ctx,
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

      if (ctx->product_id == IPMI_SUPERMICRO_PRODUCT_ID_X9SCM)
	{
	  if ((ret = sel_string_output_supermicro_x9scm_event_data1_class_oem (ctx,
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

      if (ctx->product_id == IPMI_SUPERMICRO_PRODUCT_ID_X8DTNPLUS_F)
	{
	  if ((ret = sel_string_output_supermicro_x8dtnplus_f_event_data1_class_oem (ctx,
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

      if (ctx->product_id == IPMI_SUPERMICRO_PRODUCT_ID_X8SIE)
	{
	  if ((ret = sel_string_output_supermicro_x8sie_event_data1_class_oem (ctx,
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

      if (ctx->product_id == IPMI_SUPERMICRO_PRODUCT_ID_X9SCA_F_O)
	{
	  if ((ret = sel_string_output_supermicro_x9sca_f_o_event_data1_class_oem (ctx,
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

      if (ctx->product_id == IPMI_SUPERMICRO_PRODUCT_ID_H8DGU_F)
	{
	  if ((ret = sel_string_output_supermicro_h8dgu_f_event_data1_class_oem (ctx,
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

      if (ctx->product_id == IPMI_SUPERMICRO_PRODUCT_ID_H8DGU)
	{
	  if ((ret = sel_string_output_supermicro_h8dgu_event_data1_class_oem (ctx,
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

      if (ctx->product_id == IPMI_SUPERMICRO_PRODUCT_ID_H8DG6)
	{
	  if ((ret = sel_string_output_supermicro_h8dg6_event_data1_class_oem (ctx,
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

      if (ctx->product_id == IPMI_SUPERMICRO_PRODUCT_ID_X9DRI_F)
	{
	  if ((ret = sel_string_output_supermicro_x9dri_f_event_data1_class_oem (ctx,
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

      if (ctx->product_id == IPMI_SUPERMICRO_PRODUCT_ID_X9DRI_LN4F_PLUS)
	{
	  if ((ret = sel_string_output_supermicro_x9dri_ln4f_plus_event_data1_class_oem (ctx,
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

      if (ctx->product_id == IPMI_SUPERMICRO_PRODUCT_ID_X9SPU_F_O)
	{
	  if ((ret = sel_string_output_supermicro_x9spu_f_o_event_data1_class_oem (ctx,
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

      if (ctx->product_id == IPMI_SUPERMICRO_PRODUCT_ID_X9SCM_IIF)
	{
	  if ((ret = sel_string_output_supermicro_x9scm_iif_event_data1_class_oem (ctx,
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
    }

  if (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_MAGNUM_TECHNOLOGIES)
    {
      if (ctx->product_id == IPMI_SUPERMICRO_PRODUCT_ID_MAGNUM_TECHNOLOGIES_X8DTL)
	{
	  if ((ret = sel_string_output_supermicro_magnum_technologies_x8dtl_event_data1_class_oem (ctx,
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
    }

  return (0);
}

