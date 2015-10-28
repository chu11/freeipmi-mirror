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
#endif /* STDC_HEADERS */
#include <assert.h>
#include <errno.h>

#include "freeipmi/sel/ipmi-sel.h"

#include "freeipmi/record-format/ipmi-sel-record-format.h"
#include "freeipmi/spec/ipmi-event-reading-type-code-spec.h"
#include "freeipmi/spec/ipmi-iana-enterprise-numbers-spec.h"
#include "freeipmi/spec/ipmi-product-id-spec.h"

#include "ipmi-sel-common.h"
#include "ipmi-sel-defs.h"
#include "ipmi-sel-string.h"
#include "ipmi-sel-string-dell.h"
#include "ipmi-sel-string-dell-2900.h"
#include "ipmi-sel-string-dell-2950.h"
#include "ipmi-sel-string-dell-r610.h"
#include "ipmi-sel-string-dell-r710.h"
#include "ipmi-sel-string-dell-r720.h"
#include "ipmi-sel-trace.h"
#include "ipmi-sel-util.h"

#include "freeipmi-portability.h"

#define DELL_EVENT_BUFFER_LENGTH 4096

/* return (0) - no OEM match
 * return (1) - OEM match
 * return (-1) - error, cleanup and return error
 */
int
sel_string_output_dell_event_data1_class_sensor_specific_discrete (ipmi_sel_ctx_t ctx,
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
  assert (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_DELL);
  assert (sel_entry);
  assert (tmpbuf);
  assert (tmpbuflen);
  assert (!(flags & ~IPMI_SEL_STRING_FLAGS_MASK));
  assert (flags & IPMI_SEL_STRING_FLAGS_INTERPRET_OEM_DATA);
  assert (wlen);
  assert (system_event_record_data);
  assert (system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_SENSOR_SPECIFIC);

  /*
   * Dell Poweredge R610
   */
  if (ctx->product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_R610)
    {
      if ((ret = sel_string_output_dell_r610_event_data1_class_sensor_specific_discrete (ctx,
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
   * Dell Poweredge R710
   */
  if (ctx->product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_R710)
    {
      if ((ret = sel_string_output_dell_r710_event_data1_class_sensor_specific_discrete (ctx,
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
   * Dell Poweredge R720
   */
  if (ctx->product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_R720)
    {
      if ((ret = sel_string_output_dell_r720_event_data1_class_sensor_specific_discrete (ctx,
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
sel_string_output_dell_event_data1_class_oem (ipmi_sel_ctx_t ctx,
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
  assert (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_DELL);
  assert (sel_entry);
  assert (tmpbuf);
  assert (tmpbuflen);
  assert (!(flags & ~IPMI_SEL_STRING_FLAGS_MASK));
  assert (flags & IPMI_SEL_STRING_FLAGS_INTERPRET_OEM_DATA);
  assert (wlen);
  assert (system_event_record_data);

  /* 
   * Dell Poweredge 2900
   */
  if (ctx->product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_2900)
    {
      if ((ret = sel_string_output_dell_2900_event_data1_class_oem (ctx,
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
   * Dell Poweredge 2950
   */
  if (ctx->product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_2950)
    {
      if ((ret = sel_string_output_dell_2950_event_data1_class_oem (ctx,
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
   * Dell Poweredge R610
   */
  if (ctx->product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_R610)
    {
      if ((ret = sel_string_output_dell_r610_event_data1_class_oem (ctx,
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
   * Dell Poweredge R710
   */
  if (ctx->product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_R710)
    {
      if ((ret = sel_string_output_dell_r710_event_data1_class_oem (ctx,
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
   * Dell Poweredge R720
   */
  if (ctx->product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_R720)
    {
      if ((ret = sel_string_output_dell_r720_event_data1_class_oem (ctx,
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
sel_string_output_dell_event_data2_discrete_oem (ipmi_sel_ctx_t ctx,
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
  assert (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_DELL);
  assert (sel_entry);
  assert (tmpbuf);
  assert (tmpbuflen);
  assert (!(flags & ~IPMI_SEL_STRING_FLAGS_MASK));
  assert (flags & IPMI_SEL_STRING_FLAGS_INTERPRET_OEM_DATA);
  assert (wlen);
  assert (system_event_record_data);
  assert (system_event_record_data->event_data2_flag == IPMI_SEL_EVENT_DATA_OEM_CODE);

  /*
   * Dell Poweredge R610
   */
  if (ctx->product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_R610)
    {
      if ((ret = sel_string_output_dell_r610_event_data2_discrete_oem (ctx,
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
   * Dell Poweredge R710
   */
  if (ctx->product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_R710)
    {
      if ((ret = sel_string_output_dell_r710_event_data2_discrete_oem (ctx,
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
   * Dell Poweredge R720
   */
  if (ctx->product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_R720)
    {
      if ((ret = sel_string_output_dell_r720_event_data2_discrete_oem (ctx,
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

  /* achu: I don't know what motherboards this applies to, probably very old ones */
#if 0
  /* 
   * From Dell Spec and Dell Code
   */
  if (system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_SENSOR_SPECIFIC
      && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_MEMORY
      && (system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_MEMORY_PRESENCE_DETECTED
          || system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_MEMORY_CONFIGURATION_ERROR)
      && ctx->ipmi_version_major == IPMI_1_5_MAJOR_VERSION
      && ctx->ipmi_version_minor == IPMI_1_5_MINOR_VERSION)
    {
      uint8_t memory_card;
      uint8_t bank_number;
          
      memory_card = (system_event_record_data->event_data2 & IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_DELL_MEMORY_CARD_BITMASK);
      memory_card >>= IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_DELL_MEMORY_CARD_SHIFT;

      bank_number = (system_event_record_data->event_data2 & IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_DELL_BANK_NUMBER_BITMASK);
      bank_number >>= IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_DELL_BANK_NUMBER_SHIFT;

      if (IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_DELL_MEMORY_CARD_VALID (memory_card)
          && IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_DELL_BANK_NUMBER_VALID (bank_number))
        {
          snprintf (tmpbuf,
                    tmpbuflen,
                    "Memory Card %c, Bank %u",
                    'A' + memory_card,
                    bank_number);
          return (1);
        }
      else if (IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_DELL_MEMORY_CARD_VALID (memory_card))
        {
          snprintf (tmpbuf,
                    tmpbuflen,
                    "Memory Card %c",
                    'A' + memory_card);
          return (1);
        }
      else if (IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_DELL_BANK_NUMBER_VALID (bank_number))
        {
          snprintf (tmpbuf,
                    tmpbuflen,
                    "Bank %u",
                    bank_number);
          return (1);
        }
    }
#endif

  return (0);
}

/* return (0) - no OEM match
 * return (1) - OEM match
 * return (-1) - error, cleanup and return error
 */
int
sel_string_output_dell_event_data2_class_oem (ipmi_sel_ctx_t ctx,
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
  assert (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_DELL);
  assert (sel_entry);
  assert (tmpbuf);
  assert (tmpbuflen);
  assert (!(flags & ~IPMI_SEL_STRING_FLAGS_MASK));
  assert (flags & IPMI_SEL_STRING_FLAGS_INTERPRET_OEM_DATA);
  assert (wlen);
  assert (system_event_record_data);

  /* 
   * Dell Poweredge 2900
   */
  if (ctx->product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_2900)
    {
      if ((ret = sel_string_output_dell_2900_event_data2_class_oem (ctx,
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
   * Dell Poweredge 2950
   */
  if (ctx->product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_2950)
    {
      if ((ret = sel_string_output_dell_2950_event_data2_class_oem (ctx,
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
   * Dell Poweredge R610
   */
  if (ctx->product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_R610)
    {
      if ((ret = sel_string_output_dell_r610_event_data2_class_oem (ctx,
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
   * Dell Poweredge R710
   */
  if (ctx->product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_R710)
    {
      if ((ret = sel_string_output_dell_r710_event_data2_class_oem (ctx,
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
   * Dell Poweredge R720
   */
  if (ctx->product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_R720)
    {
      if ((ret = sel_string_output_dell_r720_event_data2_class_oem (ctx,
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
sel_string_output_dell_event_data3_discrete_oem (ipmi_sel_ctx_t ctx,
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
  assert (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_DELL);
  assert (sel_entry);
  assert (tmpbuf);
  assert (tmpbuflen);
  assert (!(flags & ~IPMI_SEL_STRING_FLAGS_MASK));
  assert (flags & IPMI_SEL_STRING_FLAGS_INTERPRET_OEM_DATA);
  assert (wlen);
  assert (system_event_record_data);
  assert (system_event_record_data->event_data3_flag == IPMI_SEL_EVENT_DATA_OEM_CODE);

  /*
   * Dell Poweredge R610
   */
  if (ctx->product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_R610)
    {
      if ((ret = sel_string_output_dell_r610_event_data3_discrete_oem (ctx,
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
   * Dell Poweredge R710
   */
  if (ctx->product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_R710)
    {
      if ((ret = sel_string_output_dell_r710_event_data3_discrete_oem (ctx,
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
   * Dell Poweredge R720
   */
  if (ctx->product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_R720)
    {
      if ((ret = sel_string_output_dell_r720_event_data3_discrete_oem (ctx,
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

  /* achu: I don't know what motherboards this applies to */
#if 0
  /* 
   * From Dell Spec and Dell Code
   */
  if (system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_SENSOR_SPECIFIC
      && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_MEMORY
      && (system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_MEMORY_PRESENCE_DETECTED
          || system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_MEMORY_CONFIGURATION_ERROR)
      && ctx->ipmi_version_major == IPMI_1_5_MAJOR_VERSION
      && ctx->ipmi_version_minor == IPMI_1_5_MINOR_VERSION)
    {
      if (IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_DELL_DIMM_NUMBER_VALID (system_event_record_data->event_data3))
        {
          snprintf (tmpbuf,
                    tmpbuflen,
                    "DIMM %c",
                    'A' + system_event_record_data->event_data3);
          
          return (1);
        }
    }
#endif

  return (0);
}

/* return (0) - no OEM match
 * return (1) - OEM match
 * return (-1) - error, cleanup and return error
 */
int
sel_string_output_dell_event_data3_class_oem (ipmi_sel_ctx_t ctx,
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
  assert (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_DELL);
  assert (sel_entry);
  assert (tmpbuf);
  assert (tmpbuflen);
  assert (!(flags & ~IPMI_SEL_STRING_FLAGS_MASK));
  assert (flags & IPMI_SEL_STRING_FLAGS_INTERPRET_OEM_DATA);
  assert (wlen);
  assert (system_event_record_data);

  /* 
   * Dell Poweredge 2900
   */
  if (ctx->product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_2900)
    {
      if ((ret = sel_string_output_dell_2900_event_data3_class_oem (ctx,
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
   * Dell Poweredge 2950
   */
  if (ctx->product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_2950)
    {
      if ((ret = sel_string_output_dell_2950_event_data3_class_oem (ctx,
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
   * Dell Poweredge R610
   */
  if (ctx->product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_R610)
    {
      if ((ret = sel_string_output_dell_r610_event_data3_class_oem (ctx,
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
   * Dell Poweredge R710
   */
  if (ctx->product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_R710)
    {
      if ((ret = sel_string_output_dell_r710_event_data3_class_oem (ctx,
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
   * Dell Poweredge R720
   */
  if (ctx->product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_R720)
    {
      if ((ret = sel_string_output_dell_r720_event_data3_class_oem (ctx,
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

#if 0
static char *
_dell_version_change_entity_string (uint8_t data_entity)
{
  if (data_entity == 0)
    return "BIOS";
  else if (data_entity == 1)
    return "BMC";
  else if (data_entity == 2)
    return "iDRAC";
  else if (data_entity == 3)
    return "CMC";
  else if (data_entity == 4)
    return "NIC";
  else
    return "Unrecognized Entity";
}
#endif

/* return (0) - no OEM match
 * return (1) - OEM match
 * return (-1) - error, cleanup and return error
 *
 * in oem_rv, return
 * 0 - continue on
 * 1 - buffer full, return full buffer to user
 */
int
sel_string_output_dell_event_data2_event_data3 (ipmi_sel_ctx_t ctx,
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
  assert (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_DELL);
  assert (sel_entry);
  assert (buf);
  assert (buflen);
  assert (!(flags & ~IPMI_SEL_STRING_FLAGS_MASK));
  assert (flags & IPMI_SEL_STRING_FLAGS_INTERPRET_OEM_DATA);
  assert (wlen);
  assert (system_event_record_data);
  assert (oem_rv);

  /*
   * Dell Poweredge R610
   */
  if (ctx->product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_R610)
    {
      if ((ret = sel_string_output_dell_r610_event_data2_event_data3 (ctx,
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
   * Dell Poweredge R710
   */
  if (ctx->product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_R710)
    {
      if ((ret = sel_string_output_dell_r710_event_data2_event_data3 (ctx,
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
   * Dell Poweredge R720
   */
  if (ctx->product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_R720)
    {
      if ((ret = sel_string_output_dell_r720_event_data2_event_data3 (ctx,
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

  /* achu: I don't know what motherboards this applies to */
#if 0

  /* 
   * From Dell Provided Source Code
   *
   * Specifically for Version Change Sensors with an event offset
   * IPMI_SENSOR_TYPE_VERSION_CHANGE_FIRMWARE_OR_SOFTWARE_INCOMPATABILITY_DETECTED_WITH_ASSOCIATED_ENTITY
   *
   * achu: XXX: dataX & 0x1F != 1F ???  The bitmasks below have never
   * been verified by Dell.
   */

#define IPMI_SENSOR_TYPE_MEMORY_OEM_DELL_VERSION_CHANGE_ENTITY_BITMASK 0xE0
#define IPMI_SENSOR_TYPE_MEMORY_OEM_DELL_VERSION_CHANGE_ENTITY_SHIFT      5

#define IPMI_SENSOR_TYPE_MEMORY_OEM_DELL_VERSION_CHANGE_NUMBER_BITMASK 0x1F
#define IPMI_SENSOR_TYPE_MEMORY_OEM_DELL_VERSION_CHANGE_NUMBER_SHIFT      0

#define IPMI_SENSOR_TYPE_MEMORY_OEM_DELL_VERSION_CHANGE_NUMBER_INVALID 0x1F

  if (ctx->ipmi_version_major == IPMI_2_0_MAJOR_VERSION
      && ctx->ipmi_version_minor == IPMI_2_0_MINOR_VERSION
      && system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_SENSOR_SPECIFIC
      && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_VERSION_CHANGE
      && system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_VERSION_CHANGE_FIRMWARE_OR_SOFTWARE_INCOMPATABILITY_DETECTED_WITH_ASSOCIATED_ENTITY
      && system_event_record_data->event_data2_flag == IPMI_SEL_EVENT_DATA_OEM_CODE
      && system_event_record_data->event_data3_flag == IPMI_SEL_EVENT_DATA_OEM_CODE)
    {
      uint8_t data2_entity, data3_entity;
      uint8_t data2_number, data3_number;
      char *data2_entity_str = NULL;
      char *data3_entity_str = NULL;
      char data2_number_str[DELL_EVENT_BUFFER_LENGTH];
      char data3_number_str[DELL_EVENT_BUFFER_LENGTH];

      data2_entity = (system_event_record_data->event_data2 & IPMI_SENSOR_TYPE_MEMORY_OEM_DELL_VERSION_CHANGE_ENTITY_BITMASK);
      data2_entity >>= IPMI_SENSOR_TYPE_MEMORY_OEM_DELL_VERSION_CHANGE_ENTITY_SHIFT;

      data2_number = (system_event_record_data->event_data2 & IPMI_SENSOR_TYPE_MEMORY_OEM_DELL_VERSION_CHANGE_NUMBER_BITMASK);
      data2_number >>= IPMI_SENSOR_TYPE_MEMORY_OEM_DELL_VERSION_CHANGE_NUMBER_SHIFT;

      data3_entity = (system_event_record_data->event_data3 & IPMI_SENSOR_TYPE_MEMORY_OEM_DELL_VERSION_CHANGE_ENTITY_BITMASK);
      data3_entity >>= IPMI_SENSOR_TYPE_MEMORY_OEM_DELL_VERSION_CHANGE_ENTITY_SHIFT;

      data3_number = (system_event_record_data->event_data3 & IPMI_SENSOR_TYPE_MEMORY_OEM_DELL_VERSION_CHANGE_NUMBER_BITMASK);
      data3_number >>= IPMI_SENSOR_TYPE_MEMORY_OEM_DELL_VERSION_CHANGE_NUMBER_SHIFT;

      data2_entity_str = _dell_version_change_entity_string (data2_entity);
      data3_entity_str = _dell_version_change_entity_string (data3_entity);

      memset (data2_number_str, '\0', DELL_EVENT_BUFFER_LENGTH);
      memset (data3_number_str, '\0', DELL_EVENT_BUFFER_LENGTH);

      if (data2_number != IPMI_SENSOR_TYPE_MEMORY_OEM_DELL_VERSION_CHANGE_NUMBER_INVALID)
        snprintf (data2_number_str,
                  DELL_EVENT_BUFFER_LENGTH,
                  "%u",
                  data2_number);

      if (data3_number != IPMI_SENSOR_TYPE_MEMORY_OEM_DELL_VERSION_CHANGE_NUMBER_INVALID)
        snprintf (data3_number_str,
                  DELL_EVENT_BUFFER_LENGTH,
                  "%u",
                  data3_number);

      if (sel_string_snprintf (buf,
			       buflen,
			       wlen,
			       "%s%s%s with %s%s%s",
			       data2_entity_str,
			       strlen (data2_number_str) ? " " : "",
			       data2_number_str,
			       data3_entity_str,
			       strlen (data3_number_str) ? " " : "",
			       data3_number_str))
        (*oem_rv) = 1;
      else
        (*oem_rv) = 0;
      
      return (1);     
    }
#endif

  return (0);
}
