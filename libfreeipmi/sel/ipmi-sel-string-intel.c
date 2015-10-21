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
#include "freeipmi/record-format/ipmi-sel-oem-record-format.h"
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
#include "ipmi-sel-string-intel.h"
#include "ipmi-sel-string-intel-node-manager.h"
#include "ipmi-sel-string-intel-s5500wb.h"
#include "ipmi-sel-string-intel-quanta-qssc-s4r.h"
#include "ipmi-sel-string-intel-s2600jf.h"
#include "ipmi-sel-string-intel-windmill.h"
#include "ipmi-sel-string-intel-s2600kp.h"
#include "ipmi-sel-string-intel-s2600wtt.h"
#include "ipmi-sel-string-intel-s2600wt2.h"
#include "ipmi-sel-trace.h"
#include "ipmi-sel-util.h"

#include "freeipmi-portability.h"

int
sel_string_output_intel_sensor_name (ipmi_sel_ctx_t ctx,
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
  assert (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_INTEL);
  assert (sel_entry);
  assert (buf);
  assert (buflen);
  assert (!(flags & ~IPMI_SEL_STRING_FLAGS_MASK));
  assert (flags & IPMI_SEL_STRING_FLAGS_INTERPRET_OEM_DATA);
  assert (wlen);
  assert (system_event_record_data);
  assert (oem_rv);
  
  /* 
   * Intel S5500WB/Penguin Computing Relion 700
   */
  if (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S5500WB)
    {
      if ((ret = sel_string_output_intel_s5500wb_sensor_name (ctx,
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

  /*
   * Quanta QSSC-S4R/Appro GB812X-CN
   * (Quanta motherboard contains Intel manufacturer ID)
   */
  if (ctx->product_id == IPMI_INTEL_PRODUCT_ID_QUANTA_QSSC_S4R)
    {
      if ((ret = sel_string_output_intel_quanta_qssc_s4r_sensor_name (ctx,
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

  /* 
   * Intel S2600JF/Appro 512X
   */
  if (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600JF)
    {
      if ((ret = sel_string_output_intel_s2600jf_sensor_name (ctx,
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

  /*
   * Intel S2600KP
   */
  if (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600KP)
    {
      if ((ret = sel_string_output_intel_s2600kp_sensor_name (ctx,
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

  /*
   * Intel S2600WT2
   */
  if (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600WT2)
    {
      if ((ret = sel_string_output_intel_s2600wt2_sensor_name (ctx,
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

  /*
   * Intel S2600WTT
   */
  if (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600WTT)
    {
      if ((ret = sel_string_output_intel_s2600wtt_sensor_name (ctx,
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
 */
int
sel_string_output_intel_event_data1_class_sensor_specific_discrete (ipmi_sel_ctx_t ctx,
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
  assert (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_INTEL);
  assert (sel_entry);
  assert (tmpbuf);
  assert (tmpbuflen);
  assert (!(flags & ~IPMI_SEL_STRING_FLAGS_MASK));
  assert (flags & IPMI_SEL_STRING_FLAGS_INTERPRET_OEM_DATA);
  assert (wlen);
  assert (system_event_record_data);
  assert (system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_SENSOR_SPECIFIC);

  /* 
   * Intel Windmill
   * (Quanta Winterfell)
   * (Wiwynn Windmill)
   */
  if (ctx->product_id == IPMI_INTEL_PRODUCT_ID_WINDMILL)
    {
      if ((ret = sel_string_output_intel_windmill_event_data1_class_sensor_specific_discrete (ctx,
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
 */
int
sel_string_output_intel_event_data1_class_oem (ipmi_sel_ctx_t ctx,
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
  assert (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_INTEL);
  assert (sel_entry);
  assert (tmpbuf);
  assert (tmpbuflen);
  assert (!(flags & ~IPMI_SEL_STRING_FLAGS_MASK));
  assert (flags & IPMI_SEL_STRING_FLAGS_INTERPRET_OEM_DATA);
  assert (wlen);
  assert (system_event_record_data);

  /* 
   * Intel S5500WB/Penguin Computing Relion 700
   */
  if (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S5500WB)
    {
      if ((ret = sel_string_output_intel_s5500wb_event_data1_class_oem (ctx,
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

  /* Quanta QSSC-S4R/Appro GB812X-CN
   * (Quanta motherboard contains Intel manufacturer ID)
   */
  if (ctx->product_id == IPMI_INTEL_PRODUCT_ID_QUANTA_QSSC_S4R)
    {
      if ((ret = sel_string_output_intel_quanta_qssc_s4r_event_data1_class_oem (ctx,
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

  /* 
   * Intel S2600JF/Appro 512X
   */
  if (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600JF)
    {
      if ((ret = sel_string_output_intel_s2600jf_event_data1_class_oem (ctx,
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

  /* 
   * Intel S2600JF/Appro 512X
   */
  if (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600JF)
    {
      if ((ret = sel_string_output_intel_s2600jf_event_data1_class_oem (ctx,
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

  /*
   * Intel S2600KP
   */
  if (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600KP)
    {
      if ((ret = sel_string_output_intel_s2600kp_event_data1_class_oem (ctx,
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

  /*
   * Intel S2600WT2
   */
  if (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600WT2)
    {
      if ((ret = sel_string_output_intel_s2600wt2_event_data1_class_oem (ctx,
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

  /*
   * Intel S2600WTT
   */
  if (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600WTT)
    {
      if ((ret = sel_string_output_intel_s2600wtt_event_data1_class_oem (ctx,
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
 */
int
sel_string_output_intel_event_data2_discrete_oem (ipmi_sel_ctx_t ctx,
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
  assert (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_INTEL);
  assert (sel_entry);
  assert (tmpbuf);
  assert (tmpbuflen);
  assert (!(flags & ~IPMI_SEL_STRING_FLAGS_MASK));
  assert (flags & IPMI_SEL_STRING_FLAGS_INTERPRET_OEM_DATA);
  assert (wlen);
  assert (system_event_record_data);
  assert (system_event_record_data->event_data2_flag == IPMI_SEL_EVENT_DATA_OEM_CODE);

  /* 
   * Intel S5500WB/Penguin Computing Relion 700
   */
  if (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S5500WB)
    {
      if ((ret = sel_string_output_intel_s5500wb_event_data2_discrete_oem (ctx,
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
  
  /* 
   * Quanta QSSC-S4R/Appro GB812X-CN
   * (Quanta motherboard contains Intel manufacturer ID) 
   */
  if (ctx->product_id == IPMI_INTEL_PRODUCT_ID_QUANTA_QSSC_S4R)
    {
      if ((ret = sel_string_output_intel_quanta_qssc_s4r_event_data2_discrete_oem (ctx,
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

  /* 
   * Intel S2600JF/Appro 512X
   */
  if (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600JF)
    {
      if ((ret = sel_string_output_intel_s2600jf_event_data2_discrete_oem (ctx,
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
  
  /* 
   * Intel Windmill
   * (Quanta Winterfell)
   * (Wiwynn Windmill)
   */
  if (ctx->product_id == IPMI_INTEL_PRODUCT_ID_WINDMILL)
    {
      if ((ret = sel_string_output_intel_windmill_event_data2_discrete_oem (ctx,
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

  /*
   * Intel S2600KP
   */
  if (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600KP)
    {
      if ((ret = sel_string_output_intel_s2600kp_event_data2_discrete_oem (ctx,
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

  /*
   * Intel S2600WT2
   */
  if (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600WT2)
    {
      if ((ret = sel_string_output_intel_s2600wt2_event_data2_discrete_oem (ctx,
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

  /*
   * Intel S2600WTT
   */
  if (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600WTT)
    {
      if ((ret = sel_string_output_intel_s2600wtt_event_data2_discrete_oem (ctx,
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
 */
int
sel_string_output_intel_event_data2_class_oem (ipmi_sel_ctx_t ctx,
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
  assert (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_INTEL);
  assert (sel_entry);
  assert (tmpbuf);
  assert (tmpbuflen);
  assert (!(flags & ~IPMI_SEL_STRING_FLAGS_MASK));
  assert (flags & IPMI_SEL_STRING_FLAGS_INTERPRET_OEM_DATA);
  assert (wlen);
  assert (system_event_record_data);

  /* 
   * Intel S5500WB/Penguin Computing Relion 700
   */
  if (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S5500WB)
    {
      if ((ret = sel_string_output_intel_s5500wb_event_data2_class_oem (ctx,
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

  /* 
   * Quanta QSSC-S4R/Appro GB812X-CN
   * (Quanta motherboard contains Intel manufacturer ID)
   */
  if (ctx->product_id == IPMI_INTEL_PRODUCT_ID_QUANTA_QSSC_S4R)
    {
      if ((ret = sel_string_output_intel_quanta_qssc_s4r_event_data2_class_oem (ctx,
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

  /* 
   * Intel S2600JF/Appro 512X
   */
  if (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600JF)
    {
      if ((ret = sel_string_output_intel_s2600jf_event_data2_class_oem (ctx,
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

  /* 
   * Intel Windmill
   * (Quanta Winterfell)
   * (Wiwynn Windmill)
   */
  if (ctx->product_id == IPMI_INTEL_PRODUCT_ID_WINDMILL)
    {
      if ((ret = sel_string_output_intel_windmill_event_data2_class_oem (ctx,
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

  /*
   * Intel S2600KP
   */
  if (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600KP)
    {
      if ((ret = sel_string_output_intel_s2600kp_event_data2_class_oem (ctx,
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

  /*
   * Intel S2600WT2
   */
  if (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600WT2)
    {
      if ((ret = sel_string_output_intel_s2600wt2_event_data2_class_oem (ctx,
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

  /*
   * Intel S2600WTT
   */
  if (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600WTT)
    {
      if ((ret = sel_string_output_intel_s2600wtt_event_data2_class_oem (ctx,
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
 */
int
sel_string_output_intel_event_data3_discrete_oem (ipmi_sel_ctx_t ctx,
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
  assert (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_INTEL);
  assert (sel_entry);
  assert (tmpbuf);
  assert (tmpbuflen);
  assert (!(flags & ~IPMI_SEL_STRING_FLAGS_MASK));
  assert (flags & IPMI_SEL_STRING_FLAGS_INTERPRET_OEM_DATA);
  assert (wlen);
  assert (system_event_record_data);
  assert (system_event_record_data->event_data3_flag == IPMI_SEL_EVENT_DATA_OEM_CODE);

  /* 
   * Intel S5500WB/Penguin Computing Relion 700
   */
  if (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S5500WB)
    {
      if ((ret = sel_string_output_intel_s5500wb_event_data3_discrete_oem (ctx,
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

  /* 
   * Quanta QSSC-S4R/Appro GB812X-CN
   * (Quanta motherboard contains Intel manufacturer ID) 
   */
  if (ctx->product_id == IPMI_INTEL_PRODUCT_ID_QUANTA_QSSC_S4R)
    {
      if ((ret = sel_string_output_intel_quanta_qssc_s4r_event_data3_discrete_oem (ctx,
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

  /* 
   * Intel S2600JF/Appro 512X
   */
  if (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600JF)
    {
      if ((ret = sel_string_output_intel_s2600jf_event_data3_discrete_oem (ctx,
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

  /* 
   * Intel Windmill
   * (Quanta Winterfell)
   * (Wiwynn Windmill)
   */
  if (ctx->product_id == IPMI_INTEL_PRODUCT_ID_WINDMILL)
    {
      if ((ret = sel_string_output_intel_windmill_event_data3_discrete_oem (ctx,
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
  
  /*
   * Intel S2600KP
   */
  if (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600KP)
    {
      if ((ret = sel_string_output_intel_s2600kp_event_data3_discrete_oem (ctx,
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

  /*
   * Intel S2600WT2
   */
  if (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600WT2)
    {
      if ((ret = sel_string_output_intel_s2600wt2_event_data3_discrete_oem (ctx,
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

  /*
   * Intel S2600WTT
   */
  if (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600WTT)
    {
      if ((ret = sel_string_output_intel_s2600wtt_event_data3_discrete_oem (ctx,
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
 */
int
sel_string_output_intel_event_data3_class_oem (ipmi_sel_ctx_t ctx,
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
  assert (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_INTEL);
  assert (sel_entry);
  assert (tmpbuf);
  assert (tmpbuflen);
  assert (!(flags & ~IPMI_SEL_STRING_FLAGS_MASK));
  assert (flags & IPMI_SEL_STRING_FLAGS_INTERPRET_OEM_DATA);
  assert (wlen);
  assert (system_event_record_data);

  /* 
   * Intel S5500WB/Penguin Computing Relion 700
   */
  if (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S5500WB)
    {
      if ((ret = sel_string_output_intel_s5500wb_event_data3_class_oem (ctx,
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

  /* 
   * Quanta QSSC-S4R/Appro GB812X-CN
   * (Quanta motherboard contains Intel manufacturer ID)
   */
  if (ctx->product_id == IPMI_INTEL_PRODUCT_ID_QUANTA_QSSC_S4R)
    {
      if ((ret = sel_string_output_intel_quanta_qssc_s4r_event_data3_class_oem (ctx,
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

  /* 
   * Intel S2600JF/Appro 512X
   */
  if (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600JF)
    {
      if ((ret = sel_string_output_intel_s2600jf_event_data3_class_oem (ctx,
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

  /* 
   * Intel Windmill
   * (Quanta Winterfell)
   * (Wiwynn Windmill)
   */
  if (ctx->product_id == IPMI_INTEL_PRODUCT_ID_WINDMILL)
    {
      if ((ret = sel_string_output_intel_windmill_event_data3_class_oem (ctx,
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

  /*
   * Intel S2600KP
   */
  if (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600KP)
    {
      if ((ret = sel_string_output_intel_s2600kp_event_data3_class_oem (ctx,
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

  /*
   * Intel S2600WT2
   */
  if (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600WT2)
    {
      if ((ret = sel_string_output_intel_s2600wt2_event_data3_class_oem (ctx,
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

  /*
   * Intel S2600WTT
   */
  if (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600WTT)
    {
      if ((ret = sel_string_output_intel_s2600wtt_event_data3_class_oem (ctx,
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
sel_string_output_intel_event_data2_event_data3 (ipmi_sel_ctx_t ctx,
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
  assert (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_INTEL);
  assert (sel_entry);
  assert (buf);
  assert (buflen);
  assert (!(flags & ~IPMI_SEL_STRING_FLAGS_MASK));
  assert (flags & IPMI_SEL_STRING_FLAGS_INTERPRET_OEM_DATA);
  assert (wlen);
  assert (system_event_record_data);
  assert (oem_rv);

  /* 
   * Intel S5500WB/Penguin Computing Relion 700
   */
  if (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S5500WB)
    {
      if ((ret = sel_string_output_intel_s5500wb_event_data2_event_data3 (ctx,
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

  /* 
   * Quanta QSSC-S4R/Appro GB812X-CN
   * (Quanta motherboard contains Intel manufacturer ID) 
   */
  if (ctx->product_id == IPMI_INTEL_PRODUCT_ID_QUANTA_QSSC_S4R)
    {
      if ((ret = sel_string_output_intel_quanta_qssc_s4r_event_data2_event_data3 (ctx,
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

  /* 
   * Intel S2600JF/Appro 512X
   */
  if (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600JF)
    {
      if ((ret = sel_string_output_intel_s2600jf_event_data2_event_data3 (ctx,
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

  /* 
   * Intel Windmill
   * (Quanta Winterfell)
   * (Wiwynn Windmill)
   */
  if (ctx->product_id == IPMI_INTEL_PRODUCT_ID_WINDMILL)
    {
      if ((ret = sel_string_output_intel_windmill_event_data2_event_data3 (ctx,
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

  /*
   * Intel S2600KP
   */
  if (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600KP)
    {
      if ((ret = sel_string_output_intel_s2600kp_event_data2_event_data3 (ctx,
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

  /*
   * Intel S2600WT2
   */
  if (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600WT2)
    {
      if ((ret = sel_string_output_intel_s2600wt2_event_data2_event_data3 (ctx,
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

  /*
   * Intel S2600WTT
   */
  if (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600WTT)
    {
      if ((ret = sel_string_output_intel_s2600wtt_event_data2_event_data3 (ctx,
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
sel_string_output_intel_oem_record_data (ipmi_sel_ctx_t ctx,
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
  assert (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_INTEL);
  assert (ipmi_sel_record_type_class (sel_record_type) == IPMI_SEL_RECORD_TYPE_CLASS_TIMESTAMPED_OEM_RECORD
          || ipmi_sel_record_type_class (sel_record_type) == IPMI_SEL_RECORD_TYPE_CLASS_NON_TIMESTAMPED_OEM_RECORD);
  assert (sel_entry);
  assert (buf);
  assert (buflen);
  assert (!(flags & ~IPMI_SEL_STRING_FLAGS_MASK));
  assert (flags & IPMI_SEL_STRING_FLAGS_INTERPRET_OEM_DATA);
  assert (wlen);
  assert (oem_rv);

  /* OEM Interpretation
   *
   * Intel Windmill
   * (Quanta Winterfell)
   * (Wiwynn Windmill)
   */
  if (ctx->product_id == IPMI_INTEL_PRODUCT_ID_WINDMILL)
    {
      if ((ret = sel_string_output_intel_windmill_oem_record_data (ctx,
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
