/* 
   ipmi-sensor-api.c - IPMI sensor commands API

   Copyright (C) 2005 FreeIPMI Core Team

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
   Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.  

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#define _GNU_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#ifdef STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <errno.h>
#if defined (IPMI_SYSLOG)
#include <syslog.h>
#endif /* IPMI_SYSLOG */

#include "ipmi-sensor-api.h"

#include "freeipmi/fiid.h"
#include "freeipmi/ipmi-sdr-record-types.h"
#include "freeipmi/ipmi-sdr-repository-cmds.h"
#include "freeipmi/ipmi-sensor-cmds.h"
#include "freeipmi/ipmi-sensor-event-messages.h"
#include "freeipmi/ipmi-sensor-types-spec.h"
#include "freeipmi/ipmi-sensor-utils.h"
#include "freeipmi/udm/ipmi-sdr-repository-cmds-udm.h"
#include "freeipmi/udm/ipmi-sensor-cmds-udm.h"

#include "bit-ops.h"
#include "err-wrappers.h"
#include "fiid-wrappers.h"
#include "freeipmi-portability.h"

enum system_software_type
  {
    IPMI_BIOS,
    IPMI_SMI_HANDLER,
    IPMI_SYSTEM_MANAGEMENT_SOFTWARE,
    IPMI_OEM,
    IPMI_REMOTE_CONSOLE_SOFTWARE,
    IPMI_TERMINAL_MODE_REMOTE_CONSOLE_SOFTWARE,
    IPMI_SYS_SOFT_ID_RESERVED
  };

const char *system_software_type_desc[] =
  {
    "BIOS",
    "SMI Handler",
    "System Management Software",
    "OEM",
    "Remote Console software",
    "Terminal Mode Remote Console software",
    "Reserved",
    NULL
  };

static int
get_system_software_type (uint8_t system_software_id)
{
  /* To avoid "warning: comparison is always true due to limited range of data type" */
  if ((system_software_id + 1) >= 1 && system_software_id <= 0x0F)
    return IPMI_BIOS;
  if (system_software_id >= 0x10 && system_software_id <= 0x1F)
    return IPMI_SMI_HANDLER;
  if (system_software_id >= 0x20 && system_software_id <= 0x2F)
    return IPMI_SYSTEM_MANAGEMENT_SOFTWARE;
  if (system_software_id >= 0x30 && system_software_id <= 0x3F)
    return IPMI_OEM;
  if (system_software_id >= 0x40 && system_software_id <= 0x46)
    return IPMI_REMOTE_CONSOLE_SOFTWARE;
  if (system_software_id == 0x47)
    return IPMI_TERMINAL_MODE_REMOTE_CONSOLE_SOFTWARE;

  return IPMI_SYS_SOFT_ID_RESERVED;
}

int
ipmi_sensor_get_decode_parameters (uint8_t *sensor_record, 
				   uint32_t sensor_record_len,
				   uint8_t *analog_data_format, 
				   char *r_exponent, 
				   char *b_exponent, 
				   char *linear, 
				   short *b, 
				   short *m)
{
  uint64_t val;
  
  uint64_t m_ls;
  uint64_t m_ms;
  
  uint64_t b_ls;
  uint64_t b_ms;
  
  fiid_obj_t obj = NULL;

  ERR_EINVAL (sensor_record 
	      && analog_data_format
	      && r_exponent
	      && b_exponent
	      && linear
	      && b
	      && m);

  FIID_OBJ_CREATE_CLEANUP (obj, tmpl_sdr_full_sensor_record);
  
  FIID_OBJ_SET_ALL_CLEANUP (obj, sensor_record, sensor_record_len);

  FIID_OBJ_GET_CLEANUP (obj, "r_exponent", &val);
  *r_exponent = (char) val;
  if (*r_exponent & 0x08)
    *r_exponent |= 0xF0;
  
  FIID_OBJ_GET_CLEANUP (obj, "b_exponent", &val);
  *b_exponent = (char) val;
  if (*b_exponent & 0x08)
    *b_exponent |= 0xF0;
  
  FIID_OBJ_GET_CLEANUP (obj, "m_ls", &m_ls);
  FIID_OBJ_GET_CLEANUP (obj, "m_ms", &m_ms);
  ERR_CLEANUP (!(bits_merge (m_ls, 8, 10, m_ms, &val) < 0));
  *m = (short) val;
  if (*m & 0x200)
    *m |= 0xFE00;
  
  FIID_OBJ_GET_CLEANUP (obj, "b_ls", &b_ls);
  FIID_OBJ_GET_CLEANUP (obj, "b_ms", &b_ms);
  ERR_CLEANUP (!(bits_merge (b_ls, 8, 10, b_ms, &val) < 0));
  *b = (short) val;
  if (*b & 0x200)
    *b |= 0xFE00;
  
  FIID_OBJ_GET_CLEANUP (obj, "sensor_unit1.analog_data_format", &val);
  *analog_data_format = (uint8_t) val;

  return (0);

 cleanup:
  FIID_OBJ_DESTROY_NO_RETURN(obj);
  return (-1);
}

void 
get_sdr_full_record (uint8_t *sdr_record_data, 
		     uint32_t sdr_record_data_len,
		     sdr_full_record_t *sdr_full_record)
{
  uint64_t val;
  
  short b;
  short m;
  char r_exponent;
  char b_exponent;
  uint8_t linear;
  uint8_t analog_data_format;  
  fiid_obj_t obj = NULL;

  ERR_EINVAL_VOID_RETURN(sdr_record_data && sdr_full_record);

  FIID_OBJ_CREATE_CLEANUP(obj, tmpl_sdr_full_sensor_record);

  FIID_OBJ_SET_ALL_CLEANUP (obj,sdr_record_data, sdr_record_data_len);
 
  ERR_CLEANUP (!(ipmi_sensor_get_decode_parameters (sdr_record_data, 
						    sdr_record_data_len,
						    &analog_data_format,
						    &r_exponent,
						    &b_exponent, 
						    (char *)&linear, 
						    &b, 
						    &m)) < 0);
  sdr_full_record->b = b;
  sdr_full_record->m = m;
  sdr_full_record->r_exponent = r_exponent;
  sdr_full_record->b_exponent = b_exponent;
  sdr_full_record->linear = linear;
  sdr_full_record->analog_data_format = analog_data_format;
  
  FIID_OBJ_GET_CLEANUP (obj, "sensor_owner_id.id", &val);
  sdr_full_record->sensor_owner_id = val;
  
  FIID_OBJ_GET_CLEANUP (obj, "sensor_number", &val);
  sdr_full_record->sensor_number = val;
  
  FIID_OBJ_GET_CLEANUP (obj, "sensor_type", &val);
  sdr_full_record->sensor_type = val;
  
  FIID_OBJ_GET_CLEANUP (obj, "event_reading_type_code", &val);
  sdr_full_record->event_reading_type_code = val;
  
  FIID_OBJ_GET_CLEANUP (obj, "sensor_unit2.base_unit", &val);
  sdr_full_record->sensor_unit = val;
  
  FIID_OBJ_GET_CLEANUP (obj, "nominal_reading", &val);
  
  ERR_CLEANUP (!(ipmi_sensor_decode_value (r_exponent,
					   b_exponent,
					   m,
					   b,
					   linear,
					   analog_data_format,
					   val,
					   &(sdr_full_record->nominal_reading)) < 0));
  
  FIID_OBJ_GET_CLEANUP (obj, "normal_minimum", &val);

  ERR_CLEANUP (!(ipmi_sensor_decode_value (r_exponent,
					   b_exponent,
					   m,
					   b,
					   linear,
					   analog_data_format,
					   val,
					   &(sdr_full_record->normal_minimum)) < 0));
  
  FIID_OBJ_GET_CLEANUP (obj, "normal_maximum", &val);

  ERR_CLEANUP (!(ipmi_sensor_decode_value (r_exponent,
					   b_exponent,
					   m,
					   b,
					   linear,
					   analog_data_format,
					   val,
					   &(sdr_full_record->normal_maximum)) < 0));
  
  FIID_OBJ_GET_CLEANUP (obj, "sensor_minimum_reading", &val);
  
  ERR_CLEANUP (!(ipmi_sensor_decode_value (r_exponent,
					   b_exponent,
					   m,
					   b,
					   linear,
					   analog_data_format,
					   val,
					   &(sdr_full_record->sensor_minimum_reading)) < 0));
  
  FIID_OBJ_GET_CLEANUP (obj, "sensor_maximum_reading", &val);

  ERR_CLEANUP (!(ipmi_sensor_decode_value (r_exponent,
					   b_exponent,
					   m,
					   b,
					   linear,
					   analog_data_format,
					   val,
					   &(sdr_full_record->sensor_maximum_reading)) < 0));
  
  FIID_OBJ_GET_CLEANUP (obj, "negative_going_threshold_hysteresis", &val);
  sdr_full_record->negative_going_threshold_hysteresis = val;
  
  FIID_OBJ_GET_CLEANUP (obj, "positive_going_threshold_hysteresis", &val);
  sdr_full_record->positive_going_threshold_hysteresis = val;
  
  FIID_OBJ_GET_CLEANUP (obj, "lower_non_recoverable_threshold", &val);

  ERR_CLEANUP (!(ipmi_sensor_decode_value (r_exponent,
					   b_exponent,
					   m,
					   b,
					   linear,
					   analog_data_format,
					   val,
					   &(sdr_full_record->lower_non_recoverable_threshold)) < 0));
  
  FIID_OBJ_GET_CLEANUP (obj, "upper_non_recoverable_threshold", &val);

  ERR_CLEANUP (!(ipmi_sensor_decode_value (r_exponent,
					   b_exponent,
					   m,
					   b,
					   linear,
					   analog_data_format,
					   val,
					   &(sdr_full_record->upper_non_recoverable_threshold)) < 0));
  
  FIID_OBJ_GET_CLEANUP (obj, "lower_critical_threshold", &val);

  ERR_CLEANUP (!(ipmi_sensor_decode_value (r_exponent,
					   b_exponent,
					   m,
					   b,
					   linear,
					   analog_data_format,
					   val,
					   &(sdr_full_record->lower_critical_threshold)) < 0));
  
  FIID_OBJ_GET_CLEANUP (obj, "upper_critical_threshold", &val);

  ERR_CLEANUP (!(ipmi_sensor_decode_value (r_exponent,
					   b_exponent,
					   m,
					   b,
					   linear,
					   analog_data_format,
					   val,
					   &(sdr_full_record->upper_critical_threshold)) < 0));
  
  FIID_OBJ_GET_CLEANUP (obj, "lower_non_critical_threshold", &val);

  ERR_CLEANUP (!(ipmi_sensor_decode_value (r_exponent,
					   b_exponent,
					   m,
					   b,
					   linear,
					   analog_data_format,
					   val,
					   &(sdr_full_record->lower_non_critical_threshold)) < 0));
  
  FIID_OBJ_GET_CLEANUP (obj, "upper_non_critical_threshold", &val);

  ERR_CLEANUP (!(ipmi_sensor_decode_value (r_exponent,
					   b_exponent,
					   m,
					   b,
					   linear,
					   analog_data_format,
					   val,
					   &(sdr_full_record->upper_non_critical_threshold)) < 0));

  memset(sdr_full_record->sensor_name, '\0', 17);
  FIID_OBJ_GET_DATA_CLEANUP (obj,
			     "id_string",
			     (uint8_t *)sdr_full_record->sensor_name,
			     17);

 cleanup:
  FIID_OBJ_DESTROY_NO_RETURN(obj);
  return;
}

void 
get_sdr_compact_record (uint8_t *sdr_record_data, 
			uint32_t sdr_record_data_len,
			sdr_compact_record_t *sdr_compact_record)
{
  uint64_t val;
  
  fiid_obj_t obj = NULL;

  ERR_EINVAL_VOID_RETURN(sdr_record_data && sdr_compact_record);

  FIID_OBJ_CREATE_CLEANUP(obj, tmpl_sdr_compact_sensor_record);

  FIID_OBJ_SET_ALL_CLEANUP (obj, sdr_record_data, sdr_record_data_len);

  FIID_OBJ_GET_CLEANUP (obj, "sensor_owner_id.id", &val);
  sdr_compact_record->sensor_owner_id = val;
  
  FIID_OBJ_GET_CLEANUP (obj, "sensor_number", &val);
  sdr_compact_record->sensor_number = val;
  
  FIID_OBJ_GET_CLEANUP (obj, "sensor_type", &val);
  sdr_compact_record->sensor_type = val;
  
  FIID_OBJ_GET_CLEANUP (obj, "event_reading_type_code", &val);
  sdr_compact_record->event_reading_type_code = val;
  
  FIID_OBJ_GET_CLEANUP (obj, "sensor_unit2.base_unit", &val);
  sdr_compact_record->sensor_unit = val;
  
  FIID_OBJ_GET_CLEANUP (obj, "negative_going_threshold_hysteresis", &val);
  sdr_compact_record->negative_going_threshold_hysteresis = val;
  
  FIID_OBJ_GET_CLEANUP (obj, "positive_going_threshold_hysteresis", &val);
  sdr_compact_record->positive_going_threshold_hysteresis = val;
  
  memset(sdr_compact_record->sensor_name, '\0', 17);
  FIID_OBJ_GET_DATA_CLEANUP (obj,
			     "id_string",
			     (uint8_t *)sdr_compact_record->sensor_name,
			     17);

 cleanup:
  FIID_OBJ_DESTROY_NO_RETURN(obj);
  return;
}

void 
get_sdr_event_only_record (uint8_t *sdr_record_data, 
			   uint32_t sdr_record_data_len,
			   sdr_event_only_record_t *sdr_event_only_record)
{
  uint64_t val;  
  fiid_obj_t obj = NULL;

  ERR_EINVAL_VOID_RETURN(sdr_record_data && sdr_event_only_record);

  FIID_OBJ_CREATE_CLEANUP(obj, tmpl_sdr_event_only_sensor_record);

  FIID_OBJ_SET_ALL_CLEANUP (obj, sdr_record_data, sdr_record_data_len);
  
  FIID_OBJ_GET_CLEANUP (obj, "sensor_owner_id.id", &val);
  sdr_event_only_record->sensor_owner_id = val;
  
  FIID_OBJ_GET_CLEANUP (obj, "sensor_number", &val);
  sdr_event_only_record->sensor_number = val;
  
  FIID_OBJ_GET_CLEANUP (obj, "sensor_type", &val);
  sdr_event_only_record->sensor_type = val;
  
  FIID_OBJ_GET_CLEANUP (obj, "event_reading_type_code", &val);
  sdr_event_only_record->event_reading_type_code = val;
  
  memset(sdr_event_only_record->sensor_name, '\0', 17);
  FIID_OBJ_GET_DATA_CLEANUP (obj,
			     "id_string",
			     sdr_event_only_record->sensor_name,
			     17);

 cleanup:
  FIID_OBJ_DESTROY_NO_RETURN(obj);
  return;
}

void 
get_sdr_entity_association_record (uint8_t *sdr_record_data, 
				   uint32_t sdr_record_data_len,
				   sdr_entity_association_record_t *sdr_entity_association_record)
{
  uint64_t val;
  fiid_obj_t obj = NULL;

  ERR_EINVAL_VOID_RETURN(sdr_record_data && sdr_entity_association_record);

  FIID_OBJ_CREATE_CLEANUP(obj, tmpl_sdr_entity_association_sensor_record);

  FIID_OBJ_SET_ALL_CLEANUP (obj, sdr_record_data, sdr_record_data_len);

  FIID_OBJ_GET_CLEANUP (obj, "container_entity_id", &val);
  sdr_entity_association_record->container_entity_id = val;
  
  FIID_OBJ_GET_CLEANUP (obj, "container_entity_instance", &val);
  sdr_entity_association_record->container_entity_instance = val;
  
 cleanup:
  FIID_OBJ_DESTROY_NO_RETURN(obj);
  return;
}

void 
get_sdr_generic_device_locator_record (uint8_t *sdr_record_data, 
				       uint32_t sdr_record_data_len,
				       sdr_generic_device_locator_record_t *sdr_generic_device_locator_record)
{
  uint64_t val;
  fiid_obj_t obj = NULL;

  ERR_EINVAL_VOID_RETURN(sdr_record_data && sdr_generic_device_locator_record);

  FIID_OBJ_CREATE_CLEANUP(obj, tmpl_generic_device_locator_sensor_record);

  FIID_OBJ_SET_ALL_CLEANUP (obj, sdr_record_data, sdr_record_data_len);
   
  FIID_OBJ_GET_CLEANUP (obj, "direct_access_address", &val);
  sdr_generic_device_locator_record->direct_access_address = val;
  
  FIID_OBJ_GET_CLEANUP (obj, "channel_number", &val);
  sdr_generic_device_locator_record->channel_number = val;
  
  FIID_OBJ_GET_CLEANUP (obj, "device_slave_address", &val);
  sdr_generic_device_locator_record->device_slave_address = val;
  
  FIID_OBJ_GET_CLEANUP (obj, "private_bus_id", &val);
  sdr_generic_device_locator_record->private_bus_id = val;

  FIID_OBJ_GET_CLEANUP (obj, "lun_for_master_write_read_command", &val);
  sdr_generic_device_locator_record->lun_for_master_write_read_command = val;
  
  FIID_OBJ_GET_CLEANUP (obj, "address_span", &val);    
  sdr_generic_device_locator_record->address_span = val;
  
  FIID_OBJ_GET_CLEANUP (obj, "device_type", &val);
  sdr_generic_device_locator_record->device_type = val;
  
  FIID_OBJ_GET_CLEANUP (obj, "device_type_modifier", &val);
  sdr_generic_device_locator_record->device_type_modifier = val;
  
  FIID_OBJ_GET_CLEANUP (obj, "entity_id", &val);
  sdr_generic_device_locator_record->entity_id = val;
  
  FIID_OBJ_GET_CLEANUP (obj, "entity_instance", &val);
  sdr_generic_device_locator_record->entity_instance = val;
  
  memset(sdr_generic_device_locator_record->device_name, '\0', 17);
  FIID_OBJ_GET_DATA_CLEANUP (obj,
			     "device_id_string",
			     (uint8_t *)sdr_generic_device_locator_record->device_name,
			     17);

 cleanup:
  FIID_OBJ_DESTROY_NO_RETURN(obj);
  return;
}

void 
get_sdr_logical_fru_device_locator_record (uint8_t *sdr_record_data, 
					   uint32_t sdr_record_data_len,
					   sdr_logical_fru_device_locator_record_t *sdr_logical_fru_device_locator_record)
{
  uint64_t val;
  fiid_obj_t obj = NULL;

  ERR_EINVAL_VOID_RETURN(sdr_record_data && sdr_logical_fru_device_locator_record);

  FIID_OBJ_CREATE_CLEANUP(obj, tmpl_sdr_logical_fru_device_locator_sensor_record);

  FIID_OBJ_SET_ALL_CLEANUP (obj, sdr_record_data, sdr_record_data_len);
  
  FIID_OBJ_GET_CLEANUP (obj, "device_type", &val);
  sdr_logical_fru_device_locator_record->device_type = val;
  
  FIID_OBJ_GET_CLEANUP (obj, "device_type_modifier", &val);
  sdr_logical_fru_device_locator_record->device_type_modifier = val;
  
  FIID_OBJ_GET_CLEANUP (obj, "fru_entity_id", &val);
  sdr_logical_fru_device_locator_record->fru_entity_id = val;
  
  FIID_OBJ_GET_CLEANUP (obj, "fru_entity_instance", &val);
  sdr_logical_fru_device_locator_record->fru_entity_instance = val;
  
  memset(sdr_logical_fru_device_locator_record->device_name, '\0', 17);
  FIID_OBJ_GET_DATA_CLEANUP (obj,
			     "device_string",
			     (uint8_t *)sdr_logical_fru_device_locator_record->device_name,
			     17);
 
 cleanup:
  FIID_OBJ_DESTROY_NO_RETURN(obj);
  return;
}

void 
get_sdr_management_controller_device_locator_record (uint8_t *sdr_record_data, 
						     uint32_t sdr_record_data_len,
						     sdr_management_controller_device_locator_record_t *sdr_management_controller_device_locator_record)
{
  uint64_t val;
  fiid_obj_t obj = NULL;

  ERR_EINVAL_VOID_RETURN(sdr_record_data && sdr_management_controller_device_locator_record);

  FIID_OBJ_CREATE_CLEANUP(obj, tmpl_sdr_management_controller_device_locator_sensor_record);

  FIID_OBJ_SET_ALL_CLEANUP (obj, sdr_record_data, sdr_record_data_len);

  FIID_OBJ_GET_CLEANUP (obj, "entity_id", &val);
  sdr_management_controller_device_locator_record->entity_id = val;
  
  FIID_OBJ_GET_CLEANUP (obj, "entity_instance", &val);
  sdr_management_controller_device_locator_record->entity_instance = val;
  
  memset(sdr_management_controller_device_locator_record->device_name, '\0', 17);
  FIID_OBJ_GET_DATA_CLEANUP (obj,
			     "device_id_string",
			     (uint8_t *)sdr_management_controller_device_locator_record->device_name,
			     17);
  
 cleanup:
  FIID_OBJ_DESTROY_NO_RETURN(obj);
  return;
}

void 
get_sdr_oem_record (uint8_t *sdr_record_data, 
		    uint32_t sdr_record_data_len,
		    sdr_oem_record_t *sdr_oem_record)
{
  uint64_t val;
  fiid_obj_t obj = NULL;
  int32_t len;

  ERR_EINVAL_VOID_RETURN(sdr_record_data && sdr_oem_record);

  FIID_OBJ_CREATE_CLEANUP(obj, tmpl_sdr_oem_record);

  FIID_OBJ_SET_ALL_CLEANUP (obj, sdr_record_data, sdr_record_data_len);

  FIID_OBJ_GET_CLEANUP (obj, "manufacturer_id", &val);
  sdr_oem_record->manufacturer_id = val;

  memset(sdr_oem_record->oem_data, '\0', 55);
  FIID_OBJ_GET_DATA_LEN_CLEANUP (len,
				 obj,
				 "oem_data",
				 sdr_oem_record->oem_data,
				 55);
  sdr_oem_record->oem_data_length = len;

 cleanup:
  FIID_OBJ_DESTROY_NO_RETURN(obj);
  return;
}

int8_t 
get_sdr_sensor_record (ipmi_device_t *dev, 
		       uint16_t record_id, 
		       fiid_obj_t obj_cmd_rs, 
		       uint8_t *sensor_record,
		       uint32_t *sensor_record_len)
{
  uint64_t val = 0;
  
  uint8_t record_length = 0;
  uint16_t reservation_id = 0;
  uint8_t offset_into_record = 0;
  uint8_t bytes_to_read = 0; 
  uint8_t chunk_data[16];
  uint8_t *record_data = NULL;
  int8_t rv = -1;

  fiid_obj_t sensor_record_header = NULL;
  fiid_obj_t local_obj_cmd_rs = NULL;
  int32_t sensor_record_header_len;
  uint8_t *sensor_record_header_buf = NULL;

  ERR_EINVAL (dev 
	      && fiid_obj_valid(obj_cmd_rs)
	      && sensor_record
	      && sensor_record_len);

  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_get_sdr_rs);

  FIID_OBJ_CREATE_CLEANUP(sensor_record_header, tmpl_sdr_sensor_record_header);
    
  FIID_TEMPLATE_LEN_BYTES_CLEANUP (sensor_record_header_len, 
				   tmpl_sdr_sensor_record_header);

  ERR_CLEANUP ((sensor_record_header_buf = alloca (sensor_record_header_len)));
  memset (sensor_record_header_buf, 0, sensor_record_header_len);

  ERR_CLEANUP (!(ipmi_cmd_get_sdr (dev, 
				   0,
				   record_id, 
				   0, 
				   sensor_record_header_len, 
				   obj_cmd_rs)));
  
  FIID_OBJ_GET_DATA_CLEANUP (obj_cmd_rs,
			     "record_data",
			     sensor_record_header_buf,
			     sensor_record_header_len);
  
  FIID_OBJ_SET_ALL_CLEANUP (sensor_record_header, 
			    sensor_record_header_buf, 
			    sensor_record_header_len);

  FIID_OBJ_GET_CLEANUP (sensor_record_header, "record_length", &val);
  record_length = val;
  record_length += sensor_record_header_len;
  
  /* achu: where does the 16 come from? */
  if (record_length > 16)
    {
      FIID_OBJ_CREATE_CLEANUP(local_obj_cmd_rs, tmpl_reserve_sdr_repository_rs);
      
      ERR_CLEANUP (!(ipmi_cmd_reserve_sdr_repository (dev, local_obj_cmd_rs) < 0));
      
      FIID_OBJ_GET_CLEANUP (local_obj_cmd_rs, "reservation_id", &val);
      reservation_id = (uint16_t) val;
    }
  
  ERR_CLEANUP ((record_data = alloca (record_length)));
  memset (record_data, 0, record_length);
  
  for (offset_into_record = 0; offset_into_record < record_length; offset_into_record += 16)
    {
      bytes_to_read = 16;
      if ((offset_into_record + bytes_to_read) > record_length)
	bytes_to_read = record_length - offset_into_record;
      
      FIID_OBJ_CLEAR_CLEANUP (obj_cmd_rs);
      
      ERR_CLEANUP (!(ipmi_cmd_get_sdr (dev, 
				       reservation_id, 
				       record_id, 
				       offset_into_record, 
				       bytes_to_read, 
				       obj_cmd_rs) < 0));
      
      FIID_OBJ_GET_DATA_CLEANUP (obj_cmd_rs, "record_data", chunk_data, 16);

      memcpy (record_data + offset_into_record, chunk_data, bytes_to_read);
    }
  
  ERR_CLEANUP (!(*sensor_record_len < record_length));
  
  memcpy(sensor_record, record_data, record_length);
  *sensor_record_len = record_length;
  
  rv = 0;
 cleanup:
  FIID_OBJ_DESTROY_NO_RETURN(sensor_record_header);
  FIID_OBJ_DESTROY_NO_RETURN(local_obj_cmd_rs);
  return (rv);
}

int8_t 
get_sdr_record (ipmi_device_t *dev, 
		uint16_t record_id, 
		uint16_t *next_record_id, 
		sdr_record_t *sdr_record)
{
  fiid_obj_t obj_cmd_rs = NULL;
  fiid_obj_t obj_sdr_record = NULL;
  uint8_t sensor_record[1024];
  uint32_t sensor_record_len;
  uint64_t val = 0;
  int8_t rv = -1;

  ERR_EINVAL (dev
	      && next_record_id
	      && sdr_record);

  FIID_OBJ_CREATE_CLEANUP (obj_cmd_rs, tmpl_get_sdr_rs);
  FIID_OBJ_CREATE_CLEANUP (obj_sdr_record, tmpl_sdr_sensor_record_header);

  sensor_record_len = 1024;
  if (get_sdr_sensor_record (dev, 
			     record_id, 
			     obj_cmd_rs, 
			     sensor_record,
			     &sensor_record_len) < 0)
    {
      FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "cmd", &val);
      dev->cmd = val;
      
      FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "comp_code", &val);
      dev->comp_code = val;
      ipmi_strerror_cmd_r (obj_cmd_rs,
                           dev->net_fn,
                           dev->errmsg,
                           IPMI_ERR_STR_MAX_LEN);
      goto cleanup;
    }
  
  memset (sdr_record, 0, sizeof (sdr_record_t));
  
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "next_record_id", &val);
  *next_record_id = val;
  
  FIID_OBJ_SET_ALL_CLEANUP (obj_sdr_record, sensor_record, sensor_record_len);
  
  FIID_OBJ_GET_CLEANUP (obj_sdr_record, "record_id", &val);
  sdr_record->record_id = val;
  
  FIID_OBJ_GET_CLEANUP (obj_sdr_record, "record_type", &val);
  sdr_record->record_type = val;
  
  switch (sdr_record->record_type)
    {
    case IPMI_SDR_FORMAT_FULL_RECORD:
      get_sdr_full_record (sensor_record,
			   sensor_record_len,
			   &(sdr_record->record.sdr_full_record));
      
      if (ipmi_sensor_classify (sdr_record->record.sdr_full_record.event_reading_type_code) != 
	  IPMI_SENSOR_CLASS_THRESHOLD)
	{
	  break;
	}
      
      FIID_OBJ_CREATE_CLEANUP(obj_cmd_rs, tmpl_get_sensor_thresholds_rs);

      if (ipmi_cmd_get_sensor_thresholds (dev, 
					  sdr_record->record.sdr_full_record.sensor_number, 
					  obj_cmd_rs) != 0)
	{
	  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "cmd", &val);
	  dev->cmd = val;
	  
	  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "comp_code", &val);
	  dev->comp_code = val;
	  ipmi_strerror_cmd_r (obj_cmd_rs,
                               dev->net_fn,
			       dev->errmsg,
			       IPMI_ERR_STR_MAX_LEN);
	  /* This is ok */
	  break;
	}
      
      FIID_OBJ_GET_CLEANUP (obj_cmd_rs, 
                            "readable_thresholds.lower_critical_threshold", 
			    &val);
      sdr_record->record.sdr_full_record.readable_threshold_lower_critical_threshold = val;
      FIID_OBJ_GET_CLEANUP (obj_cmd_rs, 
			    "readable_thresholds.upper_critical_threshold", 
			    &val);
      sdr_record->record.sdr_full_record.readable_threshold_upper_critical_threshold = val;
      FIID_OBJ_GET_CLEANUP (obj_cmd_rs, 
			    "readable_thresholds.lower_non_critical_threshold", 
			    &val);
      sdr_record->record.sdr_full_record.readable_threshold_lower_non_critical_threshold = val;
      FIID_OBJ_GET_CLEANUP (obj_cmd_rs, 
			    "readable_thresholds.upper_non_critical_threshold", 
			    &val);
      sdr_record->record.sdr_full_record.readable_threshold_upper_non_critical_threshold = val;
      FIID_OBJ_GET_CLEANUP (obj_cmd_rs, 
			    "readable_thresholds.lower_non_recoverable_threshold", 
			    &val);
      sdr_record->record.sdr_full_record.readable_threshold_lower_non_recoverable_threshold = val;
      FIID_OBJ_GET_CLEANUP (obj_cmd_rs, 
			    "readable_thresholds.upper_non_recoverable_threshold", 
			    &val);
      sdr_record->record.sdr_full_record.readable_threshold_upper_non_recoverable_threshold = val;
      
      break;
    case IPMI_SDR_FORMAT_COMPACT_RECORD:
      get_sdr_compact_record (sensor_record,
			      sensor_record_len,
			      &(sdr_record->record.sdr_compact_record));
      break;
    case IPMI_SDR_FORMAT_EVENT_ONLY_RECORD:
      get_sdr_event_only_record (sensor_record,
				 sensor_record_len,
				 &(sdr_record->record.sdr_event_only_record));
      break;
    case IPMI_SDR_FORMAT_ENTITY_ASSO_RECORD:
      get_sdr_entity_association_record (sensor_record,
					 sensor_record_len,
					 &(sdr_record->record.sdr_entity_association_record));
      break;
    case IPMI_SDR_FORMAT_GEN_DEV_LOCATOR_RECORD:
      get_sdr_generic_device_locator_record (sensor_record,
					     sensor_record_len,
					     &(sdr_record->record.sdr_generic_device_locator_record));
      break;
    case IPMI_SDR_FORMAT_FRU_DEV_LOCATOR_RECORD:
      get_sdr_logical_fru_device_locator_record (sensor_record,
						 sensor_record_len,
						 &(sdr_record->record.sdr_logical_fru_device_locator_record));
      break;
    case IPMI_SDR_FORMAT_MGMT_CNTRLR_DEV_LOCATOR_RECORD:
      get_sdr_management_controller_device_locator_record (sensor_record,
							   sensor_record_len,
							   &(sdr_record->record.sdr_management_controller_device_locator_record));
      break;
    case IPMI_SDR_FORMAT_OEM_RECORD:
      get_sdr_oem_record (sensor_record,
			  sensor_record_len,
			  &(sdr_record->record.sdr_oem_record));
      break;
    case IPMI_SDR_FORMAT_DEV_ENTITY_ASSO_RECORD:
    case IPMI_SDR_FORMAT_MGMT_CNTRLR_CONFIRMATION_RECORD:
    case IPMI_SDR_FORMAT_BMC_MSG_CHANNEL_INFO_RECORD:
    default:
      {
#if defined (IPMI_SYSLOG)
	char errstr[IPMI_ERR_STR_MAX_LEN];
	snprintf (errstr, IPMI_ERR_STR_MAX_LEN, 
		  "%s: record_type = %02Xh and record_id = %d not handled.  "
		  "Please report to freeipmi-devel@gnu.org\n", 
		  __PRETTY_FUNCTION__, sdr_record->record_type, sdr_record->record_type);
	syslog (LOG_MAKEPRI(LOG_LOCAL1, LOG_ERR), errstr);
#endif /* IPMI_SYSLOG */
      }
    }
  
  rv = 0;
 cleanup:
  FIID_OBJ_DESTROY_NO_RETURN(obj_cmd_rs);
  FIID_OBJ_DESTROY_NO_RETURN(obj_sdr_record);
  return (rv);
}

int8_t 
get_sensor_reading (ipmi_device_t *dev, 
		    sdr_record_t *sdr_record, 
		    sensor_reading_t *sensor_reading)
{
  fiid_template_t l_tmpl_get_sensor_reading_threshold_rs =
    {
      {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
      {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
      
      {8, "sensor_reading", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
      
      {5, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
      {1, "reading_state", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
      {1, "sensor_scanning", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
      {1, "all_event_messages", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
      
      {6, "sensor_state", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
      {2, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
      
      /* optional byte */
      {8, "ignore", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED}, 
      
      {0,  "", 0}
    };
  
  fiid_template_t l_tmpl_get_sensor_reading_discrete_rs =
    {
      {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
      {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
      
      {8, "sensor_reading", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
      
      {5, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
      {1, "reading_state", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
      {1, "sensor_scanning", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
      {1, "all_event_messages", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
      
      {15, "sensor_state", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
      {1, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
      
      {0,  "", 0}
    };
  
  uint8_t slave_sys_soft_id;
  uint8_t event_reading_type_code;
  uint8_t sensor_number;
  uint8_t sensor_type;
  short b = 0;
  short m = 0;
  char r_exponent = 0;
  char b_exponent = 0;
  uint8_t linear = 0;
  uint8_t analog_data_format = 0;
  int rv = -1;
  fiid_obj_t obj_cmd_rs = NULL;  
  fiid_obj_t l_obj_cmd_rs = NULL;
  uint8_t buf[1024];
  int32_t len;
  uint64_t val;
  
  ERR_EINVAL (dev
	      && sdr_record
	      && sensor_reading);

  switch (sdr_record->record_type)
    {
    case IPMI_SDR_FORMAT_FULL_RECORD:
      slave_sys_soft_id = sdr_record->record.sdr_full_record.sensor_owner_id;
      if (get_system_software_type (slave_sys_soft_id) == IPMI_SYS_SOFT_ID_RESERVED)
	return -1;
      
      event_reading_type_code = sdr_record->record.sdr_full_record.event_reading_type_code;
      sensor_number = sdr_record->record.sdr_full_record.sensor_number;
      sensor_type = sdr_record->record.sdr_full_record.sensor_type;
      b = sdr_record->record.sdr_full_record.b;
      m = sdr_record->record.sdr_full_record.m;
      r_exponent = sdr_record->record.sdr_full_record.r_exponent;
      b_exponent = sdr_record->record.sdr_full_record.b_exponent;
      linear = sdr_record->record.sdr_full_record.linear;
      analog_data_format = sdr_record->record.sdr_full_record.analog_data_format;
      break;
    case IPMI_SDR_FORMAT_COMPACT_RECORD:
      slave_sys_soft_id = sdr_record->record.sdr_compact_record.sensor_owner_id;
      if (get_system_software_type (slave_sys_soft_id) == IPMI_SYS_SOFT_ID_RESERVED)
	return -1;
      
      event_reading_type_code = sdr_record->record.sdr_compact_record.event_reading_type_code;
      sensor_number = sdr_record->record.sdr_compact_record.sensor_number;
      sensor_type = sdr_record->record.sdr_compact_record.sensor_type;
      break;
    default:
      return -1;
    }
  
  switch (ipmi_sensor_classify (event_reading_type_code))
    {
    case IPMI_SENSOR_CLASS_THRESHOLD:
      FIID_OBJ_CREATE_CLEANUP(obj_cmd_rs, tmpl_get_sensor_reading_threshold_rs);
      FIID_OBJ_CREATE_CLEANUP(l_obj_cmd_rs, l_tmpl_get_sensor_reading_threshold_rs);

      if (ipmi_cmd_get_sensor_reading_threshold (dev, 
						 sensor_number, 
						 obj_cmd_rs) != 0)
	{
	  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "cmd", &val);
	  dev->cmd = val;
	  
	  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "comp_code", &val);
	  dev->comp_code = val;
	  ipmi_strerror_cmd_r (obj_cmd_rs,
                               dev->net_fn,
			       dev->errmsg,
			       IPMI_ERR_STR_MAX_LEN);
	  goto cleanup;
	}

      FIID_OBJ_GET_ALL_LEN_CLEANUP(len,
				   obj_cmd_rs,
				   buf,
				   1024);

      FIID_OBJ_SET_ALL_CLEANUP (l_obj_cmd_rs, buf, len);
      
      FIID_OBJ_GET_CLEANUP (l_obj_cmd_rs, "sensor_reading", &val);

      if (sdr_record->record_type == IPMI_SDR_FORMAT_FULL_RECORD)
	{
	  ERR_CLEANUP (!(ipmi_sensor_decode_value (r_exponent, 
						   b_exponent, 
						   m, 
						   b, 
						   linear, 
						   analog_data_format, 
						   (uint8_t) val,
						   &(sensor_reading->current_reading)) < 0));
	}
      else 
	{
	  sensor_reading->current_reading = val;
	}
      
      FIID_OBJ_GET_CLEANUP (l_obj_cmd_rs, 
			    "reading_state", 
			    &val);
      sensor_reading->reading_state = val;
      FIID_OBJ_GET_CLEANUP (l_obj_cmd_rs, 
			    "sensor_scanning", 
			    &val);
      sensor_reading->sensor_scanning = val;
      FIID_OBJ_GET_CLEANUP (l_obj_cmd_rs, 
			    "all_event_messages", 
			    &val);
      sensor_reading->event_messages_flag = val;
      
      FIID_OBJ_GET_CLEANUP (l_obj_cmd_rs, 
			    "sensor_state", 
			    &val);
      sensor_reading->event_message_list = 
	ipmi_get_generic_event_message_list (event_reading_type_code, val);
      
      rv = 0;
      break;
    case IPMI_SENSOR_CLASS_GENERIC_DISCRETE:
      FIID_OBJ_CREATE_CLEANUP(obj_cmd_rs, tmpl_get_sensor_reading_discrete_rs);

      FIID_OBJ_CREATE_CLEANUP(l_obj_cmd_rs, l_tmpl_get_sensor_reading_discrete_rs);

      if (ipmi_cmd_get_sensor_reading_discrete (dev, 
						sensor_number, 
						obj_cmd_rs) != 0)
	{
	  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "cmd", &val);
	  dev->cmd = val;
	  
	  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "comp_code", &val);
	  dev->comp_code = val;
	  ipmi_strerror_cmd_r (obj_cmd_rs,
                               dev->net_fn,
			       dev->errmsg,
			       IPMI_ERR_STR_MAX_LEN);
	  goto cleanup;
	}
      
      FIID_OBJ_GET_ALL_LEN_CLEANUP(len,
				   obj_cmd_rs,
				   buf,
				   1024);

      FIID_OBJ_SET_ALL_CLEANUP (l_obj_cmd_rs, buf, len);

      FIID_OBJ_GET_CLEANUP (l_obj_cmd_rs, "sensor_reading", &val);

      if (sdr_record->record_type == IPMI_SDR_FORMAT_FULL_RECORD)
	{
	  ERR_CLEANUP (!(ipmi_sensor_decode_value (r_exponent, 
						   b_exponent, 
						   m, 
						   b, 
						   linear, 
						   analog_data_format, 
						   (uint8_t) val,
						   &(sensor_reading->current_reading)) < 0));
	}
      else 
	{
	  sensor_reading->current_reading = val;
	}
      
      FIID_OBJ_GET_CLEANUP (l_obj_cmd_rs, 
			    "reading_state", 
			    &val);
      sensor_reading->reading_state = val;
      FIID_OBJ_GET_CLEANUP (l_obj_cmd_rs, 
			    "sensor_scanning", 
			    &val);
      sensor_reading->sensor_scanning = val;
      FIID_OBJ_GET_CLEANUP (l_obj_cmd_rs, 
			    "all_event_messages", 
			    &val);
      sensor_reading->event_messages_flag = val;
      
      FIID_OBJ_GET_CLEANUP (l_obj_cmd_rs, 
			    "sensor_state", 
			    &val);
      sensor_reading->event_message_list = 
	ipmi_get_generic_event_message_list (event_reading_type_code, val);
      
      rv = 0;
      break;
    case IPMI_SENSOR_CLASS_SENSOR_SPECIFIC_DISCRETE:
      FIID_OBJ_CREATE_CLEANUP(obj_cmd_rs, tmpl_get_sensor_reading_discrete_rs);

      FIID_OBJ_CREATE_CLEANUP(l_obj_cmd_rs, l_tmpl_get_sensor_reading_discrete_rs);

      if (ipmi_cmd_get_sensor_reading_discrete (dev, 
						sensor_number, 
						obj_cmd_rs) != 0)
	{
	  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "cmd", &val);
	  dev->cmd = val;
	  
	  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "comp_code", &val);
	  dev->comp_code = val;
	  ipmi_strerror_cmd_r (obj_cmd_rs,
                               dev->net_fn,
			       dev->errmsg,
			       IPMI_ERR_STR_MAX_LEN);
	  goto cleanup;
	}
      
      FIID_OBJ_GET_ALL_LEN_CLEANUP(len,
				   obj_cmd_rs,
				   buf,
				   1024);

      FIID_OBJ_SET_ALL_CLEANUP (l_obj_cmd_rs, buf, len);

      FIID_OBJ_GET_CLEANUP (l_obj_cmd_rs, "sensor_reading", &val);

      if (sdr_record->record_type == IPMI_SDR_FORMAT_FULL_RECORD)
	{
	  ERR_CLEANUP (!(ipmi_sensor_decode_value (r_exponent, 
						   b_exponent, 
						   m, 
						   b, 
						   linear, 
						   analog_data_format, 
						   (uint8_t) val,
						   &(sensor_reading->current_reading)) < 0));
	}
      else 
	{
	  sensor_reading->current_reading = val;
	}
      
      FIID_OBJ_GET_CLEANUP (l_obj_cmd_rs, 
			    "reading_state", 
			    &val);
      sensor_reading->reading_state = val;
      FIID_OBJ_GET_CLEANUP (l_obj_cmd_rs, 
			    "sensor_scanning", 
			    &val);
      sensor_reading->sensor_scanning = val;
      FIID_OBJ_GET_CLEANUP (l_obj_cmd_rs, 
			    "all_event_messages", 
			    &val);
      sensor_reading->event_messages_flag = val;
      
      FIID_OBJ_GET_CLEANUP (l_obj_cmd_rs, 
			    "sensor_state", 
			    &val);
      sensor_reading->event_message_list = 
	ipmi_get_event_message_list (sensor_type, val);
      
      rv = 0;
      break;
    case IPMI_SENSOR_CLASS_OEM:
      FIID_OBJ_CREATE_CLEANUP(obj_cmd_rs, tmpl_get_sensor_reading_discrete_rs);

      FIID_OBJ_CREATE_CLEANUP(l_obj_cmd_rs, l_tmpl_get_sensor_reading_discrete_rs);

      if (ipmi_cmd_get_sensor_reading_discrete (dev, 
						sensor_number, 
						obj_cmd_rs) != 0)
	{
	  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "cmd", &val);
	  dev->cmd = val;
	  
	  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "comp_code", &val);
	  dev->comp_code = val;
	  ipmi_strerror_cmd_r (obj_cmd_rs,
                               dev->net_fn,
			       dev->errmsg,
			       IPMI_ERR_STR_MAX_LEN);
	  goto cleanup;
	}
      
      FIID_OBJ_GET_ALL_LEN_CLEANUP(len,
				   obj_cmd_rs,
				   buf,
				   1024);

      FIID_OBJ_SET_ALL_CLEANUP (l_obj_cmd_rs, buf, len);

      FIID_OBJ_GET_CLEANUP (l_obj_cmd_rs, 
			    "sensor_reading",
			    &val);
      sensor_reading->current_reading = val;
      FIID_OBJ_GET_CLEANUP (l_obj_cmd_rs, 
			    "reading_state", 
			    &val);
      sensor_reading->reading_state = val;
      FIID_OBJ_GET_CLEANUP (l_obj_cmd_rs, 
			    "sensor_scanning", 
			    &val);
      sensor_reading->sensor_scanning = val;
      FIID_OBJ_GET_CLEANUP (l_obj_cmd_rs, 
			    "all_event_messages", 
			    &val);
      sensor_reading->event_messages_flag = val;
      
      FIID_OBJ_GET_CLEANUP (l_obj_cmd_rs, 
			    "sensor_state", 
			    &val);

      {
	char *event_message = NULL;
	asprintf (&event_message, 
		  "OEM State = %04Xh", 
		  (uint16_t) val);
	sensor_reading->event_message_list = (char **) malloc (sizeof (char *) * 2);
	sensor_reading->event_message_list[0] = event_message;
	sensor_reading->event_message_list[1] = NULL;
      }
      
      rv = 0;
      break;
    }
  
 cleanup:
  FIID_OBJ_DESTROY_NO_RETURN(obj_cmd_rs);
  FIID_OBJ_DESTROY_NO_RETURN(l_obj_cmd_rs);
  return (rv);
}

int 
ipmi_sensor_classify (uint8_t event_reading_type_code)
{
  if (event_reading_type_code == 0x01)
    return IPMI_SENSOR_CLASS_THRESHOLD;
  
  if (event_reading_type_code >= 0x02 && event_reading_type_code <= 0x0C)
    return IPMI_SENSOR_CLASS_GENERIC_DISCRETE;
  
  if (event_reading_type_code == 0x6F)
    return IPMI_SENSOR_CLASS_SENSOR_SPECIFIC_DISCRETE;
  
  if (event_reading_type_code >= 0x70 && event_reading_type_code <= 0x7F)
    return IPMI_SENSOR_CLASS_OEM;
  
  return IPMI_SENSOR_CLASS_NOT_AVAILABLE;
}

const char *
ipmi_get_sensor_group (int sensor_type)
{
  if (IPMI_SENSOR_TYPE_VALID(sensor_type))
    return (ipmi_sensor_types[sensor_type]);
  
  if (IPMI_SENSOR_TYPE_IS_OEM (sensor_type))
    return ipmi_oem_sensor_type;

  return NULL;
}

char **
ipmi_get_generic_event_message_list (uint8_t event_reading_type_code, uint16_t sensor_state)
{
  char **event_message_list = NULL;
  char *message_list[16];
  char buf[1024];
  int indx = 0;
  uint16_t offset;
  uint16_t bit; 
  int i;
  
  for (offset = 0; offset < 16; offset++)
    {
      bit = pow (2, offset);
      if (sensor_state & bit)
	{
	  if (ipmi_get_generic_event_message (event_reading_type_code,
					      offset,
					      buf,
					      1024) < 0)
            continue;

	  message_list[indx] = strdup(buf);
	  if (!message_list[indx])
	    goto cleanup;
	  else
	    indx++;
	  
	}
    }
  
  if (indx)
    {
      event_message_list = (char **) malloc (sizeof (char *) * (indx + 1));
      for (offset = 0; offset < indx; offset++)
	event_message_list[offset] = message_list[offset];
      event_message_list[indx] = NULL;
    }
  
  return event_message_list;

 cleanup:
  for (i = 0; i < indx; i++)
    free(message_list[indx]);
  return NULL;
}

char **
ipmi_get_event_message_list (int sensor_type_code, uint16_t sensor_state)
{
  char **event_message_list = NULL;
  char *message_list[16];
  char buf[1024];
  int indx = 0;
  uint16_t offset;
  uint16_t bit; 
  int i;
  
  for (offset = 0; offset < 16; offset++)
    {
      bit = pow (2, offset);
      if (sensor_state & bit)
	{
	  if (ipmi_get_sensor_type_code_message (sensor_type_code,
						 offset,
						 buf,
						 1024) < 0)
            continue;

	  message_list[indx] = strdup(buf);
	  if (!message_list[indx])
	    goto cleanup;
	  else
	    indx++;
	}
    }
  
  if (indx)
    {
      event_message_list = (char **) malloc (sizeof (char *) * (indx + 1));
      for (offset = 0; offset < indx; offset++)
	event_message_list[offset] = message_list[offset];
      event_message_list[indx] = NULL;
    }
  
  return event_message_list;

 cleanup:
  for (i = 0; i < indx; i++)
    free(message_list[indx]);
  return NULL;
}
