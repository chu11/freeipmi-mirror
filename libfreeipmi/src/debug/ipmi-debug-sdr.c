/* 
   Copyright (C) 2003, 2004, 2005 FreeIPMI Core Team

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
#include <config.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#ifdef STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <assert.h>
#include <errno.h>

#include "freeipmi/debug/ipmi-debug.h"
#include "freeipmi/record-format/ipmi-sdr-record-format.h"

#include "ipmi-debug-common.h"

#include "libcommon/ipmi-err-wrappers.h"
#include "libcommon/ipmi-fiid-wrappers.h"

#include "freeipmi-portability.h"

int32_t
ipmi_dump_sdr_record (int fd, char *prefix, uint8_t *sdr_record, uint32_t sdr_record_len)
{
  char prefix_buf[IPMI_DEBUG_MAX_PREFIX_LEN];
  char *sdr_record_header_hdr = 
    "================================================\n"
    "SDR Record Header\n"
    "================================================";
  char *sdr_full_sensor_record_hdr = 
    "================================================\n"
    "SDR Full Sensor Record\n"
    "================================================";
  char *sdr_compact_sensor_record_hdr = 
    "================================================\n"
    "SDR Compact Sensor Record\n"
    "================================================";
  char *sdr_event_only_record_hdr = 
    "================================================\n"
    "SDR Event Only Record\n"
    "================================================";
  char *sdr_entity_association_record_hdr = 
    "================================================\n"
    "SDR Entity Association Record\n"
    "================================================";
  char *sdr_device_relative_entity_association_record_hdr = 
    "================================================\n"
    "SDR Device Relative Entity Association Record\n"
    "================================================";
  char *sdr_generic_device_locator_record_hdr = 
    "================================================\n"
    "SDR Generic Device Locator Record\n"
    "================================================";
  char *sdr_fru_device_locator_record_hdr = 
    "================================================\n"
    "SDR FRU Device Locator Record\n"
    "================================================";
  char *sdr_management_controller_device_locator_record_hdr = 
    "================================================\n"
    "SDR Management Controller Device Locator Record\n"
    "================================================";
  char *sdr_management_controller_confirmation_record_hdr = 
    "================================================\n"
    "SDR Management Controller Confirmation Record\n"
    "================================================";
  char *sdr_bmc_message_channel_info_record_hdr = 
    "================================================\n"
    "SDR Message Channel Info Record\n"
    "================================================";
  char *sdr_oem_record_hdr = 
    "================================================\n"
    "SDR OEM Record\n"
    "================================================";
  char *sdr_record_header_hdr_ptr;
  fiid_obj_t obj_sdr_record_header = NULL;
  fiid_obj_t obj_sdr_record = NULL;
  uint64_t val;
  uint8_t record_type;
  int32_t sdr_record_header_len;
  int8_t rv = -1;

  ERR_EINVAL (sdr_record);

  ERR(!(ipmi_debug_set_prefix (prefix_buf, IPMI_DEBUG_MAX_PREFIX_LEN, prefix) < 0));

  FIID_TEMPLATE_LEN_BYTES(sdr_record_header_len, tmpl_sdr_record_header);

  FIID_OBJ_CREATE(obj_sdr_record_header, tmpl_sdr_record_header);
  
  FIID_OBJ_SET_ALL(obj_sdr_record_header,
                   sdr_record,
                   sdr_record_len);
  
  /* Not enough for a real SDR record, just dump whatever we have */
  if (sdr_record_len <= sdr_record_header_len)
    {
      ERR_CLEANUP (!(ipmi_obj_dump_perror(fd, 
                                          prefix_buf, 
                                          sdr_record_header_hdr, 
                                          NULL, 
                                          obj_sdr_record_header) < 0));
      rv = 0;
      goto cleanup;
    }
  
  FIID_OBJ_GET(obj_sdr_record_header, "record_type", &val);
  record_type = val;

  if (record_type == IPMI_SDR_FORMAT_FULL_RECORD)
    {
      FIID_OBJ_CREATE(obj_sdr_record, tmpl_sdr_full_sensor_record);
      sdr_record_header_hdr_ptr = sdr_full_sensor_record_hdr;
    }
  else if (record_type == IPMI_SDR_FORMAT_COMPACT_RECORD)
    {
      FIID_OBJ_CREATE(obj_sdr_record, tmpl_sdr_compact_sensor_record);
      sdr_record_header_hdr_ptr = sdr_compact_sensor_record_hdr;
    }
  else if (record_type == IPMI_SDR_FORMAT_EVENT_ONLY_RECORD)
    {
      FIID_OBJ_CREATE(obj_sdr_record, tmpl_sdr_event_only_record);
      sdr_record_header_hdr_ptr = sdr_event_only_record_hdr;
    }
  else if (record_type == IPMI_SDR_FORMAT_ENTITY_ASSOCIATION_RECORD)
    {
      FIID_OBJ_CREATE(obj_sdr_record, tmpl_sdr_entity_association_record);
      sdr_record_header_hdr_ptr = sdr_entity_association_record_hdr;
    }
  else if (record_type == IPMI_SDR_FORMAT_DEVICE_RELATIVE_ENTITY_ASSOCIATION_RECORD)
    {
      FIID_OBJ_CREATE(obj_sdr_record, tmpl_sdr_device_relative_entity_association_record);
      sdr_record_header_hdr_ptr = sdr_device_relative_entity_association_record_hdr;
    }
  else if (record_type == IPMI_SDR_FORMAT_GENERIC_DEVICE_LOCATOR_RECORD)
    {
      FIID_OBJ_CREATE(obj_sdr_record, tmpl_sdr_generic_device_locator_record);
      sdr_record_header_hdr_ptr = sdr_generic_device_locator_record_hdr;
    }
  else if (record_type == IPMI_SDR_FORMAT_FRU_DEVICE_LOCATOR_RECORD)
    {
      FIID_OBJ_CREATE(obj_sdr_record, tmpl_sdr_fru_device_locator_record);
      sdr_record_header_hdr_ptr = sdr_fru_device_locator_record_hdr;
    }
  else if (record_type == IPMI_SDR_FORMAT_MANAGEMENT_CONTROLLER_DEVICE_LOCATOR_RECORD)
    {
      FIID_OBJ_CREATE(obj_sdr_record, tmpl_sdr_management_controller_device_locator_record);
      sdr_record_header_hdr_ptr = sdr_management_controller_device_locator_record_hdr;
    }
  else if (record_type == IPMI_SDR_FORMAT_MANAGEMENT_CONTROLLER_CONFIRMATION_RECORD)
    {
      FIID_OBJ_CREATE(obj_sdr_record, tmpl_sdr_management_controller_confirmation_record);
      sdr_record_header_hdr_ptr = sdr_management_controller_confirmation_record_hdr;
    }
  else if (record_type == IPMI_SDR_FORMAT_BMC_MESSAGE_CHANNEL_INFO_RECORD)
    {
      FIID_OBJ_CREATE(obj_sdr_record, tmpl_sdr_bmc_message_channel_info_record);
      sdr_record_header_hdr_ptr = sdr_bmc_message_channel_info_record_hdr;
    }
  else if (record_type == IPMI_SDR_FORMAT_OEM_RECORD)
    {
      FIID_OBJ_CREATE(obj_sdr_record, tmpl_sdr_oem_record);
      sdr_record_header_hdr_ptr = sdr_oem_record_hdr;
    }
  else
    {
      /* I don't know this record_type, maybe its in a newer spec?? */
      FIID_OBJ_CREATE(obj_sdr_record, tmpl_sdr_record_header);
      sdr_record_header_hdr_ptr = sdr_record_header_hdr;
    }
 
  FIID_OBJ_SET_ALL(obj_sdr_record,
                   sdr_record,
                   sdr_record_len);

  ERR_CLEANUP (!(ipmi_obj_dump_perror(fd, 
                                      prefix_buf, 
                                      sdr_record_header_hdr_ptr, 
                                      NULL, 
                                      obj_sdr_record) < 0));
  
  rv = 0;
 cleanup:
  FIID_OBJ_DESTROY(obj_sdr_record_header);
  FIID_OBJ_DESTROY(obj_sdr_record);
  return (rv);
}

