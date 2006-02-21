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

#include "freeipmi.h"
#include "err-wrappers.h"
#include "fiid-wrappers.h"

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

  if (!sdr_record_data || !sdr_full_record)
    {
      errno = EINVAL;
      return;
    }

  FIID_OBJ_CREATE_CLEANUP(obj, tmpl_sdr_full_sensor_record);

  FIID_OBJ_SET_ALL_CLEANUP (obj,sdr_record_data, sdr_record_data_len);
 
  ipmi_sensor_get_decode_params (sdr_record_data, 
				 sdr_record_data_len,
				 &analog_data_format, &r_exponent, &b_exponent, 
				 (char *)&linear, &b, &m);
  sdr_full_record->b = b;
  sdr_full_record->m = m;
  sdr_full_record->r_exponent = r_exponent;
  sdr_full_record->b_exponent = b_exponent;
  sdr_full_record->linear = linear;
  sdr_full_record->analog_data_format = analog_data_format;
  
  FIID_OBJ_GET_CLEANUP (obj, (uint8_t *)"sensor_owner_id.id", &val);
  sdr_full_record->sensor_owner_id = val;
  
  FIID_OBJ_GET_CLEANUP (obj, (uint8_t *)"sensor_number", &val);
  sdr_full_record->sensor_number = val;
  
  FIID_OBJ_GET_CLEANUP (obj, (uint8_t *)"sensor_type", &val);
  sdr_full_record->sensor_type = val;
  
  FIID_OBJ_GET_CLEANUP (obj, (uint8_t *)"event_reading_type_code", &val);
  sdr_full_record->event_reading_type_code = val;
  
  FIID_OBJ_GET_CLEANUP (obj, (uint8_t *)"sensor_unit2.base_unit", &val);
  sdr_full_record->sensor_unit = val;
  
  FIID_OBJ_GET_CLEANUP (obj, (uint8_t *)"nominal_reading", &val);
  sdr_full_record->nominal_reading = 
    ipmi_sensor_decode_value (r_exponent,
			      b_exponent,
			      m,
			      b,
			      linear,
			      analog_data_format,
			      val);
  
  FIID_OBJ_GET_CLEANUP (obj, (uint8_t *)"normal_minimum", &val);
  sdr_full_record->normal_minimum = 
    ipmi_sensor_decode_value (r_exponent,
			      b_exponent,
			      m,
			      b,
			      linear,
			      analog_data_format,
			      val);
  
  FIID_OBJ_GET_CLEANUP (obj, (uint8_t *)"normal_maximum", &val);
  sdr_full_record->normal_maximum = 
    ipmi_sensor_decode_value (r_exponent,
			      b_exponent,
			      m,
			      b,
			      linear,
			      analog_data_format,
			      val);
  
  FIID_OBJ_GET_CLEANUP (obj, (uint8_t *)"sensor_minimum_reading", &val);
  sdr_full_record->sensor_minimum_reading = 
    ipmi_sensor_decode_value (r_exponent,
			      b_exponent,
			      m,
			      b,
			      linear,
			      analog_data_format,
			      val);
  
  FIID_OBJ_GET_CLEANUP (obj, (uint8_t *)"sensor_maximum_reading", &val);
  sdr_full_record->sensor_maximum_reading =
    ipmi_sensor_decode_value (r_exponent,
			      b_exponent,
			      m,
			      b,
			      linear,
			      analog_data_format,
			      val);
  
  FIID_OBJ_GET_CLEANUP (obj, (uint8_t *)"negative_going_threshold_hysteresis", &val);
  sdr_full_record->negative_going_threshold_hysteresis = val;
  
  FIID_OBJ_GET_CLEANUP (obj, (uint8_t *)"positive_going_threshold_hysteresis", &val);
  sdr_full_record->positive_going_threshold_hysteresis = val;
  
  FIID_OBJ_GET_CLEANUP (obj, (uint8_t *)"lower_non_recoverable_threshold", &val);
  sdr_full_record->lower_non_recoverable_threshold = 
    ipmi_sensor_decode_value (r_exponent,
			      b_exponent,
			      m,
			      b,
			      linear,
			      analog_data_format,
			      val);
  
  FIID_OBJ_GET_CLEANUP (obj, (uint8_t *)"upper_non_recoverable_threshold", &val);
  sdr_full_record->upper_non_recoverable_threshold = 
    ipmi_sensor_decode_value (r_exponent,
			      b_exponent,
			      m,
			      b,
			      linear,
			      analog_data_format,
			      val);
  
  FIID_OBJ_GET_CLEANUP (obj, (uint8_t *)"lower_critical_threshold", &val);
  sdr_full_record->lower_critical_threshold = 
    ipmi_sensor_decode_value (r_exponent,
			      b_exponent,
			      m,
			      b,
			      linear,
			      analog_data_format,
			      val);
  
  FIID_OBJ_GET_CLEANUP (obj, (uint8_t *)"upper_critical_threshold", &val);
  sdr_full_record->upper_critical_threshold = 
    ipmi_sensor_decode_value (r_exponent,
			      b_exponent,
			      m,
			      b,
			      linear,
			      analog_data_format,
			      val);
  
  FIID_OBJ_GET_CLEANUP (obj, (uint8_t *)"lower_non_critical_threshold", &val);
  sdr_full_record->lower_non_critical_threshold = 
    ipmi_sensor_decode_value (r_exponent,
			      b_exponent,
			      m,
			      b,
			      linear,
			      analog_data_format,
			      val);
  
  FIID_OBJ_GET_CLEANUP (obj, (uint8_t *)"upper_non_critical_threshold", &val);
  sdr_full_record->upper_non_critical_threshold = 
    ipmi_sensor_decode_value (r_exponent,
			      b_exponent,
			      m,
			      b,
			      linear,
			      analog_data_format,
			      val);

  memset(sdr_full_record->sensor_name, '\0', 17);
  FIID_OBJ_GET_DATA_CLEANUP (obj,
			     (uint8_t *)"id_string",
			     sdr_full_record->sensor_name,
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

  if (!sdr_record_data || !sdr_compact_record)
    {
      errno = EINVAL;
      return;
    }

  FIID_OBJ_CREATE_CLEANUP(obj, tmpl_sdr_compact_sensor_record);

  FIID_OBJ_SET_ALL_CLEANUP (obj, sdr_record_data, sdr_record_data_len);

  FIID_OBJ_GET_CLEANUP (obj, (uint8_t *)"sensor_owner_id.id", &val);
  sdr_compact_record->sensor_owner_id = val;
  
  FIID_OBJ_GET_CLEANUP (obj, (uint8_t *)"sensor_number", &val);
  sdr_compact_record->sensor_number = val;
  
  FIID_OBJ_GET_CLEANUP (obj, (uint8_t *)"sensor_type", &val);
  sdr_compact_record->sensor_type = val;
  
  FIID_OBJ_GET_CLEANUP (obj, (uint8_t *)"event_reading_type_code", &val);
  sdr_compact_record->event_reading_type_code = val;
  
  FIID_OBJ_GET_CLEANUP (obj, (uint8_t *)"sensor_unit2.base_unit", &val);
  sdr_compact_record->sensor_unit = val;
  
  FIID_OBJ_GET_CLEANUP (obj, (uint8_t *)"negative_going_threshold_hysteresis", &val);
  sdr_compact_record->negative_going_threshold_hysteresis = val;
  
  FIID_OBJ_GET_CLEANUP (obj, (uint8_t *)"positive_going_threshold_hysteresis", &val);
  sdr_compact_record->positive_going_threshold_hysteresis = val;
  
  memset(sdr_compact_record->sensor_name, '\0', 17);
  FIID_OBJ_GET_DATA_CLEANUP (obj,
			     (uint8_t *)"id_string",
			     sdr_compact_record->sensor_name,
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

  if (!sdr_record_data || !sdr_event_only_record)
    {
      errno = EINVAL;
      return;
    }

  FIID_OBJ_CREATE_CLEANUP(obj, tmpl_sdr_event_only_sensor_record);

  FIID_OBJ_SET_ALL_CLEANUP (obj, sdr_record_data, sdr_record_data_len);
  
  FIID_OBJ_GET_CLEANUP (obj, (uint8_t *)"sensor_owner_id.id", &val);
  sdr_event_only_record->sensor_owner_id = val;
  
  FIID_OBJ_GET_CLEANUP (obj, (uint8_t *)"sensor_number", &val);
  sdr_event_only_record->sensor_number = val;
  
  FIID_OBJ_GET_CLEANUP (obj, (uint8_t *)"sensor_type", &val);
  sdr_event_only_record->sensor_type = val;
  
  FIID_OBJ_GET_CLEANUP (obj, (uint8_t *)"event_reading_type_code", &val);
  sdr_event_only_record->event_reading_type_code = val;
  
  memset(sdr_event_only_record->sensor_name, '\0', 17);
  FIID_OBJ_GET_DATA_CLEANUP (obj,
			     (uint8_t *)"id_string",
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

  if (!sdr_record_data || !sdr_entity_association_record)
    {
      errno = EINVAL;
      return;
    }

  FIID_OBJ_CREATE_CLEANUP(obj, tmpl_sdr_entity_association_sensor_record);

  FIID_OBJ_SET_ALL_CLEANUP (obj, sdr_record_data, sdr_record_data_len);

  FIID_OBJ_GET_CLEANUP (obj, (uint8_t *)"container_entity_id", &val);
  sdr_entity_association_record->container_entity_id = val;
  
  FIID_OBJ_GET_CLEANUP (obj, (uint8_t *)"container_entity_instance", &val);
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

  if (!sdr_record_data || !sdr_generic_device_locator_record)
    {
      errno = EINVAL;
      return;
    }

  FIID_OBJ_CREATE_CLEANUP(obj, tmpl_generic_device_locator_sensor_record);

  FIID_OBJ_SET_ALL_CLEANUP (obj, sdr_record_data, sdr_record_data_len);
   
  FIID_OBJ_GET_CLEANUP (obj, (uint8_t *)"direct_access_address", &val);
  sdr_generic_device_locator_record->direct_access_address = val;
  
  FIID_OBJ_GET_CLEANUP (obj, (uint8_t *)"channel_number", &val);
  sdr_generic_device_locator_record->channel_number = val;
  
  FIID_OBJ_GET_CLEANUP (obj, (uint8_t *)"device_slave_address", &val);
  sdr_generic_device_locator_record->device_slave_address = val;
  
  FIID_OBJ_GET_CLEANUP (obj, (uint8_t *)"private_bus_id", &val);
  sdr_generic_device_locator_record->private_bus_id = val;

  FIID_OBJ_GET_CLEANUP (obj, (uint8_t *)"lun_for_master_write_read_command", &val);
  sdr_generic_device_locator_record->lun_for_master_write_read_command = val;
  
  FIID_OBJ_GET_CLEANUP (obj, (uint8_t *)"address_span", &val);    
  sdr_generic_device_locator_record->address_span = val;
  
  FIID_OBJ_GET_CLEANUP (obj, (uint8_t *)"device_type", &val);
  sdr_generic_device_locator_record->device_type = val;
  
  FIID_OBJ_GET_CLEANUP (obj, (uint8_t *)"device_type_modifier", &val);
  sdr_generic_device_locator_record->device_type_modifier = val;
  
  FIID_OBJ_GET_CLEANUP (obj, (uint8_t *)"entity_id", &val);
  sdr_generic_device_locator_record->entity_id = val;
  
  FIID_OBJ_GET_CLEANUP (obj, (uint8_t *)"entity_instance", &val);
  sdr_generic_device_locator_record->entity_instance = val;
  
  memset(sdr_generic_device_locator_record->device_name, '\0', 17);
  FIID_OBJ_GET_DATA_CLEANUP (obj,
			     (uint8_t *)"device_id_string",
			     sdr_generic_device_locator_record->device_name,
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

  if (!sdr_record_data || !sdr_logical_fru_device_locator_record)
    {
      errno = EINVAL;
      return;
    }

  FIID_OBJ_CREATE_CLEANUP(obj, tmpl_sdr_logical_fru_device_locator_sensor_record);

  FIID_OBJ_SET_ALL_CLEANUP (obj, sdr_record_data, sdr_record_data_len);
  
  FIID_OBJ_GET_CLEANUP (obj, (uint8_t *)"device_type", &val);
  sdr_logical_fru_device_locator_record->device_type = val;
  
  FIID_OBJ_GET_CLEANUP (obj, (uint8_t *)"device_type_modifier", &val);
  sdr_logical_fru_device_locator_record->device_type_modifier = val;
  
  FIID_OBJ_GET_CLEANUP (obj, (uint8_t *)"fru_entity_id", &val);
  sdr_logical_fru_device_locator_record->fru_entity_id = val;
  
  FIID_OBJ_GET_CLEANUP (obj, (uint8_t *)"fru_entity_instance", &val);
  sdr_logical_fru_device_locator_record->fru_entity_instance = val;
  
  memset(sdr_logical_fru_device_locator_record->device_name, '\0', 17);
  FIID_OBJ_GET_DATA_CLEANUP (obj,
			     (uint8_t *)"device_string",
			     sdr_logical_fru_device_locator_record->device_name,
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

  if (!sdr_record_data || !sdr_management_controller_device_locator_record)
    {
      errno = EINVAL;
      return;
    }

  FIID_OBJ_CREATE_CLEANUP(obj, tmpl_sdr_management_controller_device_locator_sensor_record);

  FIID_OBJ_SET_ALL_CLEANUP (obj, sdr_record_data, sdr_record_data_len);

  FIID_OBJ_GET_CLEANUP (obj, (uint8_t *)"entity_id", &val);
  sdr_management_controller_device_locator_record->entity_id = val;
  
  FIID_OBJ_GET_CLEANUP (obj, (uint8_t *)"entity_instance", &val);
  sdr_management_controller_device_locator_record->entity_instance = val;
  
  memset(sdr_management_controller_device_locator_record->device_name, '\0', 17);
  FIID_OBJ_GET_DATA_CLEANUP (obj,
			     (uint8_t *)"device_id_string",
			     sdr_management_controller_device_locator_record->device_name,
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

  if (!sdr_record_data || !sdr_oem_record)
    {
      errno = EINVAL;
      return;
    }

  FIID_OBJ_CREATE_CLEANUP(obj, tmpl_sdr_oem_record);

  FIID_OBJ_SET_ALL_CLEANUP (obj, sdr_record_data, sdr_record_data_len);

  FIID_OBJ_GET_CLEANUP (obj, (uint8_t *)"manufacturer_id", &val);
  sdr_oem_record->manufacturer_id = val;

  memset(sdr_oem_record->oem_data, '\0', 55);
  FIID_OBJ_GET_DATA_LEN_CLEANUP (len,
				 obj,
				 (uint8_t *)"oem_data",
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

  if (!dev 
      || !fiid_obj_valid(obj_cmd_rs)
      || !sensor_record
      || !sensor_record_len)
    {
      errno = EINVAL;
      return -1;
    }

  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_get_sdr_rs);

  FIID_OBJ_CREATE_CLEANUP(sensor_record_header, tmpl_sdr_sensor_record_header);
    
  FIID_TEMPLATE_LEN_BYTES_CLEANUP (sensor_record_header_len, 
				   tmpl_sdr_sensor_record_header);

  ERR_CLEANUP ((sensor_record_header_buf = alloca (sensor_record_header_len)));
  memset (sensor_record_header_buf, 0, sensor_record_header_len);

  ERR_CLEANUP (!(ipmi_cmd_get_sdr2 (dev, 
				    0,
				    record_id, 
				    0, 
				    sensor_record_header_len, 
				    obj_cmd_rs)));
  
  FIID_OBJ_GET_DATA_CLEANUP (obj_cmd_rs,
			     (uint8_t *)"record_data",
			     sensor_record_header_buf,
			     sensor_record_header_len);
  
  FIID_OBJ_SET_ALL_CLEANUP (sensor_record_header, 
			    sensor_record_header_buf, 
			    sensor_record_header_len);

  FIID_OBJ_GET_CLEANUP (sensor_record_header, (uint8_t *)"record_length", &val);
  record_length = val;
  record_length += sensor_record_header_len;
  
  /* achu: where does the 16 come from? */
  if (record_length > 16)
    {
      FIID_OBJ_CREATE_CLEANUP(local_obj_cmd_rs, tmpl_reserve_sdr_repository_rs);
      
      ERR_CLEANUP (!(ipmi_cmd_reserve_sdr_repository2 (dev, local_obj_cmd_rs) < 0));
      
      FIID_OBJ_GET_CLEANUP (local_obj_cmd_rs, (uint8_t *)"reservation_id", &val);
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
      
      ERR_CLEANUP (!(ipmi_cmd_get_sdr2 (dev, 
					reservation_id, 
					record_id, 
					offset_into_record, 
					bytes_to_read, 
					obj_cmd_rs) < 0));
      
      FIID_OBJ_GET_DATA_CLEANUP (obj_cmd_rs,
				 (uint8_t *)"record_data",
				 chunk_data,
				 16);

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

  if (!dev
      || !next_record_id
      || !sdr_record)
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_CREATE_CLEANUP (obj_cmd_rs, tmpl_get_sdr_rs);
  FIID_OBJ_CREATE_CLEANUP (obj_sdr_record, tmpl_sdr_sensor_record_header);

  sensor_record_len = 1024;
  if (get_sdr_sensor_record (dev, 
			     record_id, 
			     obj_cmd_rs, 
			     sensor_record,
			     &sensor_record_len) < 0)
    {
      FIID_OBJ_GET_CLEANUP (obj_cmd_rs, (uint8_t *)"cmd", &val);
      dev->cmd = val;
      
      FIID_OBJ_GET_CLEANUP (obj_cmd_rs, (uint8_t *)"comp_code", &val);
      dev->comp_code = val;
      ipmi_strerror_cmd_r (obj_cmd_rs,
                           dev->errmsg,
                           IPMI_ERR_STR_MAX_LEN);
      goto cleanup;
    }
  
  memset (sdr_record, 0, sizeof (sdr_record_t));
  
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, (uint8_t *)"next_record_id", &val);
  *next_record_id = val;
  
  FIID_OBJ_SET_ALL_CLEANUP (obj_sdr_record, sensor_record, sensor_record_len);
  
  FIID_OBJ_GET_CLEANUP (obj_sdr_record, (uint8_t *)"record_id", &val);
  sdr_record->record_id = val;
  
  FIID_OBJ_GET_CLEANUP (obj_sdr_record, (uint8_t *)"record_type", &val);
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

      if (ipmi_cmd_get_sensor_thresholds2 (dev, 
					   sdr_record->record.sdr_full_record.sensor_number, 
					   obj_cmd_rs) != 0)
	{
	  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, (uint8_t *)"cmd", &val);
	  dev->cmd = val;
	  
	  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, (uint8_t *)"comp_code", &val);
	  dev->comp_code = val;
	  ipmi_strerror_cmd_r (obj_cmd_rs,
			       dev->errmsg,
			       IPMI_ERR_STR_MAX_LEN);
	  /* This is ok */
	  break;
	}
      
      FIID_OBJ_GET_CLEANUP (obj_cmd_rs, 
			    (uint8_t *)"readable_thresholds.lower_critical_threshold", 
			    &val);
      sdr_record->record.sdr_full_record.readable_threshold_lower_critical_threshold = val;
      FIID_OBJ_GET_CLEANUP (obj_cmd_rs, 
			    (uint8_t *)"readable_thresholds.upper_critical_threshold", 
			    &val);
      sdr_record->record.sdr_full_record.readable_threshold_upper_critical_threshold = val;
      FIID_OBJ_GET_CLEANUP (obj_cmd_rs, 
			    (uint8_t *)"readable_thresholds.lower_non_critical_threshold", 
			    &val);
      sdr_record->record.sdr_full_record.readable_threshold_lower_non_critical_threshold = val;
      FIID_OBJ_GET_CLEANUP (obj_cmd_rs, 
			    (uint8_t *)"readable_thresholds.upper_non_critical_threshold", 
			    &val);
      sdr_record->record.sdr_full_record.readable_threshold_upper_non_critical_threshold = val;
      FIID_OBJ_GET_CLEANUP (obj_cmd_rs, 
			    (uint8_t *)"readable_thresholds.lower_non_recoverable_threshold", 
			    &val);
      sdr_record->record.sdr_full_record.readable_threshold_lower_non_recoverable_threshold = val;
      FIID_OBJ_GET_CLEANUP (obj_cmd_rs, 
			    (uint8_t *)"readable_thresholds.upper_non_recoverable_threshold", 
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
	char errstr[IPMI_ERR_STR_MAX_LEN];
	snprintf (errstr, IPMI_ERR_STR_MAX_LEN, 
		  "%s: record_type = %02Xh and record_id = %d not handled.  "
		  "Please report to freeipmi-devel@gnu.org\n", 
		  __PRETTY_FUNCTION__, sdr_record->record_type, sdr_record->record_type);
	syslog (LOG_MAKEPRI(LOG_LOCAL1, LOG_ERR), errstr);
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
      {1, "status_reading_availability", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
      {1, "status_sensor_scanning", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
      {1, "status_all_event_messages", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
      
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
      {1, "status_reading_availability", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
      {1, "status_sensor_scanning", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
      {1, "status_all_event_messages", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
      
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
  
  if (!dev
      || !sdr_record
      || !sensor_reading)
    {
      errno = EINVAL;
      return (-1);
    }

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

      if (ipmi_cmd_get_sensor_reading_threshold2 (dev, 
                                                  sensor_number, 
                                                  obj_cmd_rs) != 0)
	{
	  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, (uint8_t *)"cmd", &val);
	  dev->cmd = val;
	  
	  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, (uint8_t *)"comp_code", &val);
	  dev->comp_code = val;
	  ipmi_strerror_cmd_r (obj_cmd_rs,
			       dev->errmsg,
			       IPMI_ERR_STR_MAX_LEN);
	  goto cleanup;
	}

      FIID_OBJ_GET_ALL_LEN_CLEANUP(len,
				   obj_cmd_rs,
				   buf,
				   1024);

      FIID_OBJ_SET_ALL_CLEANUP (l_obj_cmd_rs, buf, len);
      
      FIID_OBJ_GET_CLEANUP (l_obj_cmd_rs, (uint8_t *)"sensor_reading", &val);

      if (sdr_record->record_type == IPMI_SDR_FORMAT_FULL_RECORD)
	{
	  sensor_reading->current_reading = ipmi_sensor_decode_value (r_exponent, 
								      b_exponent, 
								      m, 
								      b, 
								      linear, 
								      analog_data_format, 
								      (uint8_t) val);
	}
      else 
	{
	  sensor_reading->current_reading = val;
	}
      
      FIID_OBJ_GET_CLEANUP (l_obj_cmd_rs, 
			    (uint8_t *)"status_reading_availability", 
			    &val);
      sensor_reading->reading_availability_flag = val;
      FIID_OBJ_GET_CLEANUP (l_obj_cmd_rs, 
			    (uint8_t *)"status_sensor_scanning", 
			    &val);
      sensor_reading->sensor_scanning = val;
      FIID_OBJ_GET_CLEANUP (l_obj_cmd_rs, 
			    (uint8_t *)"status_all_event_messages", 
			    &val);
      sensor_reading->event_messages_flag = val;
      
      FIID_OBJ_GET_CLEANUP (l_obj_cmd_rs, 
			    (uint8_t *)"sensor_state", 
			    &val);
      sensor_reading->event_message_list = 
	ipmi_get_generic_event_message_list (event_reading_type_code, val);
      
      rv = 0;
      break;
    case IPMI_SENSOR_CLASS_GENERIC_DISCRETE:
      FIID_OBJ_CREATE_CLEANUP(obj_cmd_rs, tmpl_get_sensor_reading_discrete_rs);

      FIID_OBJ_CREATE_CLEANUP(l_obj_cmd_rs, l_tmpl_get_sensor_reading_discrete_rs);

      if (ipmi_cmd_get_sensor_reading_discrete2 (dev, 
                                                 sensor_number, 
                                                 obj_cmd_rs) != 0)
	{
	  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, (uint8_t *)"cmd", &val);
	  dev->cmd = val;
	  
	  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, (uint8_t *)"comp_code", &val);
	  dev->comp_code = val;
	  ipmi_strerror_cmd_r (obj_cmd_rs,
			       dev->errmsg,
			       IPMI_ERR_STR_MAX_LEN);
	  goto cleanup;
	}
      
      FIID_OBJ_GET_ALL_LEN_CLEANUP(len,
				   obj_cmd_rs,
				   buf,
				   1024);

      FIID_OBJ_SET_ALL_CLEANUP (l_obj_cmd_rs, buf, len);

      FIID_OBJ_GET_CLEANUP (l_obj_cmd_rs, (uint8_t *)"sensor_reading", &val);

      if (sdr_record->record_type == IPMI_SDR_FORMAT_FULL_RECORD)
	{
	  sensor_reading->current_reading = ipmi_sensor_decode_value (r_exponent, 
								      b_exponent, 
								      m, 
								      b, 
								      linear, 
								      analog_data_format, 
								      (uint8_t) val);
	}
      else 
	{
	  sensor_reading->current_reading = val;
	}
      
      FIID_OBJ_GET_CLEANUP (l_obj_cmd_rs, 
			    (uint8_t *)"status_reading_availability", 
			    &val);
      sensor_reading->reading_availability_flag = val;
      FIID_OBJ_GET_CLEANUP (l_obj_cmd_rs, 
			    (uint8_t *)"status_sensor_scanning", 
			    &val);
      sensor_reading->sensor_scanning = val;
      FIID_OBJ_GET_CLEANUP (l_obj_cmd_rs, 
			    (uint8_t *)"status_all_event_messages", 
			    &val);
      sensor_reading->event_messages_flag = val;
      
      FIID_OBJ_GET_CLEANUP (l_obj_cmd_rs, 
			    (uint8_t *)"sensor_state", 
			    &val);
      sensor_reading->event_message_list = 
	ipmi_get_generic_event_message_list (event_reading_type_code, val);
      
      rv = 0;
      break;
    case IPMI_SENSOR_CLASS_SENSOR_SPECIFIC_DISCRETE:
      FIID_OBJ_CREATE_CLEANUP(obj_cmd_rs, tmpl_get_sensor_reading_discrete_rs);

      FIID_OBJ_CREATE_CLEANUP(l_obj_cmd_rs, l_tmpl_get_sensor_reading_discrete_rs);

      if (ipmi_cmd_get_sensor_reading_discrete2 (dev, 
                                                 sensor_number, 
                                                 obj_cmd_rs) != 0)
	{
	  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, (uint8_t *)"cmd", &val);
	  dev->cmd = val;
	  
	  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, (uint8_t *)"comp_code", &val);
	  dev->comp_code = val;
	  ipmi_strerror_cmd_r (obj_cmd_rs,
			       dev->errmsg,
			       IPMI_ERR_STR_MAX_LEN);
	  goto cleanup;
	}
      
      FIID_OBJ_GET_ALL_LEN_CLEANUP(len,
				   obj_cmd_rs,
				   buf,
				   1024);

      FIID_OBJ_SET_ALL_CLEANUP (l_obj_cmd_rs, buf, len);

      FIID_OBJ_GET_CLEANUP (l_obj_cmd_rs, (uint8_t *)"sensor_reading", &val);

      if (sdr_record->record_type == IPMI_SDR_FORMAT_FULL_RECORD)
	{
	  sensor_reading->current_reading = ipmi_sensor_decode_value (r_exponent, 
								      b_exponent, 
								      m, 
								      b, 
								      linear, 
								      analog_data_format, 
								      (uint8_t) val);
	}
      else 
	{
	  sensor_reading->current_reading = val;
	}
      
      FIID_OBJ_GET_CLEANUP (l_obj_cmd_rs, 
			    (uint8_t *)"status_reading_availability", 
			    &val);
      sensor_reading->reading_availability_flag = val;
      FIID_OBJ_GET_CLEANUP (l_obj_cmd_rs, 
			    (uint8_t *)"status_sensor_scanning", 
			    &val);
      sensor_reading->sensor_scanning = val;
      FIID_OBJ_GET_CLEANUP (l_obj_cmd_rs, 
			    (uint8_t *)"status_all_event_messages", 
			    &val);
      sensor_reading->event_messages_flag = val;
      
      FIID_OBJ_GET_CLEANUP (l_obj_cmd_rs, 
			    (uint8_t *)"sensor_state", 
			    &val);
      sensor_reading->event_message_list = 
	ipmi_get_event_message_list (sensor_type, val);
      
      rv = 0;
      break;
    case IPMI_SENSOR_CLASS_OEM:
      FIID_OBJ_CREATE_CLEANUP(obj_cmd_rs, tmpl_get_sensor_reading_discrete_rs);

      FIID_OBJ_CREATE_CLEANUP(l_obj_cmd_rs, l_tmpl_get_sensor_reading_discrete_rs);

      if (ipmi_cmd_get_sensor_reading_discrete2 (dev, 
                                                 sensor_number, 
                                                 obj_cmd_rs) != 0)
	{
	  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, (uint8_t *)"cmd", &val);
	  dev->cmd = val;
	  
	  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, (uint8_t *)"comp_code", &val);
	  dev->comp_code = val;
	  ipmi_strerror_cmd_r (obj_cmd_rs,
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
			    (uint8_t *)"sensor_reading",
			    &val);
      sensor_reading->current_reading = val;
      FIID_OBJ_GET_CLEANUP (l_obj_cmd_rs, 
			    (uint8_t *)"status_reading_availability", 
			    &val);
      sensor_reading->reading_availability_flag = val;
      FIID_OBJ_GET_CLEANUP (l_obj_cmd_rs, 
			    (uint8_t *)"status_sensor_scanning", 
			    &val);
      sensor_reading->sensor_scanning = val;
      FIID_OBJ_GET_CLEANUP (l_obj_cmd_rs, 
			    (uint8_t *)"status_all_event_messages", 
			    &val);
      sensor_reading->event_messages_flag = val;
      
      FIID_OBJ_GET_CLEANUP (l_obj_cmd_rs, 
			    (uint8_t *)"sensor_state", 
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

