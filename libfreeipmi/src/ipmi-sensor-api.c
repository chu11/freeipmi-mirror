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

  if (!(obj = fiid_obj_create(tmpl_sdr_full_sensor_record)))
    goto cleanup;

  if (fiid_obj_set_all(obj,
		       sdr_record_data,
		       sdr_record_data_len) < 0)
    goto cleanup;
 
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
  
  if (fiid_obj_get (obj, 
		    (uint8_t *)"slave_system_software_id", 
		    &val) < 0)
    goto cleanup;
  sdr_full_record->slave_system_software_id = val;
  
  if (fiid_obj_get (obj, 
		    (uint8_t *)"sensor_number", 
		    &val) < 0)
    goto cleanup;
  sdr_full_record->sensor_number = val;
  
  if (fiid_obj_get (obj, 
		    (uint8_t *)"sensor_type", 
		    &val) < 0)
    goto cleanup;
  sdr_full_record->sensor_type = val;
  
  if (fiid_obj_get (obj, 
		    (uint8_t *)"event_reading_type", 
		    &val) < 0)
    goto cleanup;
  sdr_full_record->event_reading_type = val;
  
  if (fiid_obj_get (obj, 
		    (uint8_t *)"sensor_base_unit", 
		    &val) < 0)
    goto cleanup;
  sdr_full_record->sensor_unit = val;
  
  if (fiid_obj_get (obj, 
		    (uint8_t *)"nominal_reading", 
		    &val) < 0)
    goto cleanup;
  sdr_full_record->nominal_reading = 
    ipmi_sensor_decode_value (r_exponent,
			      b_exponent,
			      m,
			      b,
			      linear,
			      analog_data_format,
			      val);
  
  if (fiid_obj_get (obj, 
		    (uint8_t *)"normal_min",
		    &val) < 0)
    goto cleanup;
  sdr_full_record->normal_min = 
    ipmi_sensor_decode_value (r_exponent,
			      b_exponent,
			      m,
			      b,
			      linear,
			      analog_data_format,
			      val);
  
  if (fiid_obj_get (obj, 
		    (uint8_t *)"normal_max",
		    &val) < 0)
    goto cleanup;
  sdr_full_record->normal_max = 
    ipmi_sensor_decode_value (r_exponent,
			      b_exponent,
			      m,
			      b,
			      linear,
			      analog_data_format,
			      val);
  
  if (fiid_obj_get (obj, 
		    (uint8_t *)"sensor_min_reading",
		    &val) < 0)
    goto cleanup;
  sdr_full_record->sensor_min_reading = 
    ipmi_sensor_decode_value (r_exponent,
			      b_exponent,
			      m,
			      b,
			      linear,
			      analog_data_format,
			      val);
  
  if (fiid_obj_get (obj, 
		    (uint8_t *)"sensor_max_reading",
		    &val) < 0)
    goto cleanup;
  sdr_full_record->sensor_max_reading =
    ipmi_sensor_decode_value (r_exponent,
			      b_exponent,
			      m,
			      b,
			      linear,
			      analog_data_format,
			      val);
  
  if (fiid_obj_get (obj, 
		    (uint8_t *)"negative_hysteresis", 
		    &val) < 0)
    goto cleanup;
  sdr_full_record->negative_hysteresis = val;
  
  if (fiid_obj_get (obj, 
		    (uint8_t *)"positive_hysteresis", 
		    &val) < 0)
    goto cleanup;
  sdr_full_record->positive_hysteresis = val;
  
  if (fiid_obj_get (obj, 
		    (uint8_t *)"lower_non_recoverable_threshold",
		    &val) < 0)
    goto cleanup;
  sdr_full_record->lower_non_recoverable_threshold = 
    ipmi_sensor_decode_value (r_exponent,
			      b_exponent,
			      m,
			      b,
			      linear,
			      analog_data_format,
			      val);
  
  if (fiid_obj_get (obj, 
		    (uint8_t *)"upper_non_recoverable_threshold",
		    &val) < 0)
    goto cleanup;
  sdr_full_record->upper_non_recoverable_threshold = 
    ipmi_sensor_decode_value (r_exponent,
			      b_exponent,
			      m,
			      b,
			      linear,
			      analog_data_format,
			      val);
  
  if (fiid_obj_get (obj, 
		    (uint8_t *)"lower_critical_threshold",
		    &val) < 0)
    goto cleanup;
  sdr_full_record->lower_critical_threshold = 
    ipmi_sensor_decode_value (r_exponent,
			      b_exponent,
			      m,
			      b,
			      linear,
			      analog_data_format,
			      val);
  
  if (fiid_obj_get (obj, 
		    (uint8_t *)"upper_critical_threshold",
		    &val) < 0)
    goto cleanup;
  sdr_full_record->upper_critical_threshold = 
    ipmi_sensor_decode_value (r_exponent,
			      b_exponent,
			      m,
			      b,
			      linear,
			      analog_data_format,
			      val);
  
  if (fiid_obj_get (obj, 
		    (uint8_t *)"lower_non_critical_threshold",
		    &val) < 0)
    goto cleanup;
  sdr_full_record->lower_non_critical_threshold = 
    ipmi_sensor_decode_value (r_exponent,
			      b_exponent,
			      m,
			      b,
			      linear,
			      analog_data_format,
			      val);
  
  if (fiid_obj_get (obj, 
		    (uint8_t *)"upper_non_critical_threshold",
		    &val) < 0)
    goto cleanup;
  sdr_full_record->upper_non_critical_threshold = 
    ipmi_sensor_decode_value (r_exponent,
			      b_exponent,
			      m,
			      b,
			      linear,
			      analog_data_format,
			      val);

  memset(sdr_full_record->sensor_name, '\0', 17);
  if (fiid_obj_get_data (obj,
			 (uint8_t *)"sensor_id_string",
			 sdr_full_record->sensor_name,
			 17) < 0)
    goto cleanup;

 cleanup:
  if (obj)
    fiid_obj_destroy(obj);
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

  if (!(obj = fiid_obj_create(tmpl_sdr_compact_sensor_record)))
    goto cleanup;

  if (fiid_obj_set_all(obj,
		       sdr_record_data,
		       sdr_record_data_len) < 0)
    goto cleanup;

  if (fiid_obj_get (obj, 
		    (uint8_t *)"slave_system_software_id", 
		    &val) < 0)
    goto cleanup;
  sdr_compact_record->slave_system_software_id = val;
  
  if (fiid_obj_get (obj, 
		    (uint8_t *)"sensor_number", 
		    &val) < 0)
    goto cleanup;
  sdr_compact_record->sensor_number = val;
  
  if (fiid_obj_get (obj, 
		    (uint8_t *)"sensor_type", 
		    &val) < 0)
    goto cleanup;
  sdr_compact_record->sensor_type = val;
  
  if (fiid_obj_get (obj, 
		    (uint8_t *)"event_reading_type", 
		    &val) < 0)
    goto cleanup;
  sdr_compact_record->event_reading_type = val;
  
  if (fiid_obj_get (obj, 
		    (uint8_t *)"sensor_base_unit", 
		    &val) < 0)
    goto cleanup;
  sdr_compact_record->sensor_unit = val;
  
  if (fiid_obj_get (obj, 
		    (uint8_t *)"negative_hysteresis", 
		    &val) < 0)
    goto cleanup;
  sdr_compact_record->negative_hysteresis = val;
  
  if (fiid_obj_get (obj, 
		    (uint8_t *)"positive_hysteresis", 
		    &val) < 0)
    goto cleanup;
  sdr_compact_record->positive_hysteresis = val;
  
  memset(sdr_compact_record->sensor_name, '\0', 17);
  if (fiid_obj_get_data (obj,
			 (uint8_t *)"sensor_id_string",
			 sdr_compact_record->sensor_name,
			 17) < 0)
    goto cleanup;

cleanup:
  if (obj)
    fiid_obj_destroy(obj);
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

  if (!(obj = fiid_obj_create(tmpl_sdr_event_only_sensor_record)))
    goto cleanup;

  if (fiid_obj_set_all(obj,
		       sdr_record_data,
		       sdr_record_data_len) < 0)
    goto cleanup;
  
  if (fiid_obj_get (obj, 
		    (uint8_t *)"slave_system_software_id", 
		    &val) < 0)
    goto cleanup;
  sdr_event_only_record->slave_system_software_id = val;
  
  if (fiid_obj_get (obj, 
		    (uint8_t *)"sensor_number", 
		    &val) < 0)
    goto cleanup;
  sdr_event_only_record->sensor_number = val;
  
  if (fiid_obj_get (obj, 
		    (uint8_t *)"sensor_type", 
		    &val) < 0)
    goto cleanup;
  sdr_event_only_record->sensor_type = val;
  
  if (fiid_obj_get (obj, 
		    (uint8_t *)"event_reading_type", 
		    &val) < 0)
    goto cleanup;
  sdr_event_only_record->event_reading_type = val;
  
  memset(sdr_event_only_record->sensor_name, '\0', 17);
  if (fiid_obj_get_data (obj,
			 (uint8_t *)"sensor_id_string",
			 sdr_event_only_record->sensor_name,
			 17) < 0)
    goto cleanup;

cleanup:
  if (obj)
    fiid_obj_destroy(obj);
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

  if (!(obj = fiid_obj_create(tmpl_sdr_entity_association_sensor_record)))
    goto cleanup;

  if (fiid_obj_set_all(obj,
		       sdr_record_data,
		       sdr_record_data_len) < 0)
    goto cleanup;

  if (fiid_obj_get (obj, 
		    (uint8_t *)"container_entity_id", 
		    &val) < 0)
    goto cleanup;
  sdr_entity_association_record->container_entity_id = val;
  
  if (fiid_obj_get (obj, 
		    (uint8_t *)"container_entity_instance", 
		    &val) < 0)
    goto cleanup;
  sdr_entity_association_record->container_entity_instance = val;
  
cleanup:
  if (obj)
    fiid_obj_destroy(obj);
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

  if (!(obj = fiid_obj_create(tmpl_generic_device_locator_sensor_record)))
    goto cleanup;

  if (fiid_obj_set_all(obj,
		       sdr_record_data,
		       sdr_record_data_len) < 0)
    goto cleanup;
   
  if (fiid_obj_get (obj, 
		    (uint8_t *)"direct_access_address", 
		    &val) < 0)
    goto cleanup;
  sdr_generic_device_locator_record->direct_access_address = val;
  
  if (fiid_obj_get (obj, 
		    (uint8_t *)"channel_number", 
		    &val) < 0)
    goto cleanup;
  sdr_generic_device_locator_record->channel_number = val;
  
  if (fiid_obj_get (obj, 
		    (uint8_t *)"device_slave_address", 
		    &val) < 0)
    goto cleanup;
  sdr_generic_device_locator_record->device_slave_address = val;
  
  if (fiid_obj_get (obj, 
		    (uint8_t *)"private_bus_id", 
		    &val) < 0)
    goto cleanup;
  sdr_generic_device_locator_record->private_bus_id = val;
  
  if (fiid_obj_get (obj, 
		    (uint8_t *)"lun_master_write_read_command", 
		    &val) < 0)
    goto cleanup;
  sdr_generic_device_locator_record->lun_master_write_read_command = val;
  
  if (fiid_obj_get (obj, 
		    (uint8_t *)"address_span", 
		    &val) < 0)    
    goto cleanup;
  sdr_generic_device_locator_record->address_span = val;
  
  if (fiid_obj_get (obj, 
		    (uint8_t *)"device_type", 
		    &val) < 0)
    goto cleanup;
  sdr_generic_device_locator_record->device_type = val;
  
  if (fiid_obj_get (obj, 
		    (uint8_t *)"device_type_modifier", 
		    &val) < 0)
    goto cleanup;
  sdr_generic_device_locator_record->device_type_modifier = val;
  
  if (fiid_obj_get (obj, 
		    (uint8_t *)"entity_id", 
		    &val) < 0)
    goto cleanup;
  sdr_generic_device_locator_record->entity_id = val;
  
  if (fiid_obj_get (obj, 
		    (uint8_t *)"entity_instance", 
		    &val) < 0)
    goto cleanup;
  sdr_generic_device_locator_record->entity_instance = val;
  
  memset(sdr_generic_device_locator_record->device_name, '\0', 17);
  if (fiid_obj_get_data (obj,
			 (uint8_t *)"device_name",
			 sdr_generic_device_locator_record->device_name,
			 17) < 0)
    goto cleanup;

cleanup:
  if (obj)
    fiid_obj_destroy(obj);
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

  if (!(obj = fiid_obj_create(tmpl_sdr_logical_fru_device_locator_sensor_record)))
    goto cleanup;

  if (fiid_obj_set_all(obj,
		       sdr_record_data,
		       sdr_record_data_len) < 0)
    goto cleanup;
  
  if (fiid_obj_get (obj, 
		    (uint8_t *)"device_type", 
		    &val) < 0)
    goto cleanup;
  sdr_logical_fru_device_locator_record->device_type = val;
  
  if (fiid_obj_get (obj, 
		    (uint8_t *)"device_type_modifier", 
		    &val) < 0)
    goto cleanup;
  sdr_logical_fru_device_locator_record->device_type_modifier = val;
  
  if (fiid_obj_get (obj, 
		    (uint8_t *)"fru_entity_id", 
		    &val) < 0)
    goto cleanup;
  sdr_logical_fru_device_locator_record->fru_entity_id = val;
  
  if (fiid_obj_get (obj, 
		    (uint8_t *)"fru_entity_instance", 
		    &val) < 0)
    goto cleanup;
  sdr_logical_fru_device_locator_record->fru_entity_instance = val;
  
  memset(sdr_logical_fru_device_locator_record->device_name, '\0', 17);
  if (fiid_obj_get_data (obj,
			 (uint8_t *)"device_id_string",
			 sdr_logical_fru_device_locator_record->device_name,
			 17) < 0)
    goto cleanup;
 
cleanup:
  if (obj)
    fiid_obj_destroy(obj);
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

  if (!(obj = fiid_obj_create(tmpl_sdr_management_controller_device_locator_sensor_record)))
    goto cleanup;

  if (fiid_obj_set_all(obj,
		       sdr_record_data,
		       sdr_record_data_len) < 0)
    goto cleanup;

  if (fiid_obj_get (obj, 
		    (uint8_t *)"entity_id", 
		    &val) < 0)
    goto cleanup;
  sdr_management_controller_device_locator_record->entity_id = val;
  
  if (fiid_obj_get (obj, 
		    (uint8_t *)"entity_instance", 
		    &val) < 0)
    goto cleanup;
  sdr_management_controller_device_locator_record->entity_instance = val;
  
  memset(sdr_management_controller_device_locator_record->device_name, '\0', 17);
  if (fiid_obj_get_data (obj,
			 (uint8_t *)"device_id_string",
			 sdr_management_controller_device_locator_record->device_name,
			 17) < 0)
    goto cleanup;
  
cleanup:
  if (obj)
    fiid_obj_destroy(obj);
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

  if (!(obj = fiid_obj_create(tmpl_sdr_oem_record)))
    goto cleanup;

  if (fiid_obj_set_all(obj,
		       sdr_record_data,
		       sdr_record_data_len) < 0)
    goto cleanup;

  if (fiid_obj_get (obj, 
		    (uint8_t *)"manufacturer_id", 
		    &val) < 0)
    goto cleanup;
  sdr_oem_record->manufacturer_id = val;

  memset(sdr_oem_record->oem_data, '\0', 55);
  if ((len = fiid_obj_get_data (obj,
				(uint8_t *)"oem_data",
				sdr_oem_record->oem_data,
				55)) < 0)
    goto cleanup;
  sdr_oem_record->oem_data_length = len;

cleanup:
  if (obj)
    fiid_obj_destroy(obj);
  return;
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

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_get_sdr_rs)))
    goto cleanup;

  if (!(obj_sdr_record = fiid_obj_create (tmpl_sdr_sensor_record_header)))
    goto cleanup;

  sensor_record_len = 1024;
  if (ipmi_cmd_get_sdr2 (dev, 
			 record_id, 
			 obj_cmd_rs, 
			 sensor_record,
			 &sensor_record_len) < 0)
    {
      if (fiid_obj_get (obj_cmd_rs,
                        (uint8_t *)"cmd",
                        &val) < 0)
        goto cleanup;
      dev->cmd = val;
      
      if (fiid_obj_get (obj_cmd_rs,
                        (uint8_t *)"comp_code",
                        &val) < 0)
        goto cleanup;
      dev->comp_code = val;
      ipmi_strerror_cmd_r (obj_cmd_rs,
                           dev->errmsg,
                           IPMI_ERR_STR_MAX_LEN);
      goto cleanup;
    }
  
  memset (sdr_record, 0, sizeof (sdr_record_t));
  
  if (fiid_obj_get (obj_cmd_rs, 
		    (uint8_t *)"next_record_id", 
		    &val) < 0)
    goto cleanup;
  *next_record_id = val;
  
  if (fiid_obj_set_all(obj_sdr_record,
		       sensor_record,
		       sensor_record_len) < 0)
    goto cleanup;
  
  if (fiid_obj_get (obj_sdr_record, 
		    (uint8_t *)"record_id", 
		    &val) < 0)
    goto cleanup;

  if (fiid_obj_get (obj_sdr_record, 
		    (uint8_t *)"record_id", 
		    &val) < 0)
    goto cleanup;
  sdr_record->record_id = val;
  
  if (fiid_obj_get (obj_sdr_record, 
		    (uint8_t *)"record_type", 
		    &val) < 0)
    goto cleanup;
  sdr_record->record_type = val;
  
  fiid_obj_destroy(obj_cmd_rs);
  obj_cmd_rs = NULL;

  switch (sdr_record->record_type)
    {
    case IPMI_SDR_FORMAT_FULL_RECORD:
      get_sdr_full_record (sensor_record,
			   sensor_record_len,
			   &(sdr_record->record.sdr_full_record));
      
      if (ipmi_sensor_classify (sdr_record->record.sdr_full_record.event_reading_type) != 
	  IPMI_SENSOR_CLASS_THRESHOLD)
	{
	  break;
	}
      
      if (!(obj_cmd_rs = fiid_obj_create(tmpl_get_sensor_thresholds_rs)))
	goto cleanup;

      if (ipmi_cmd_get_sensor_thresholds2 (dev, 
					   sdr_record->record.sdr_full_record.sensor_number, 
					   obj_cmd_rs) != 0)
	{
	  if (fiid_obj_get (obj_cmd_rs,
			    (uint8_t *)"cmd",
			    &val) < 0)
	    goto cleanup;
	  dev->cmd = val;
	  
	  if (fiid_obj_get (obj_cmd_rs,
			    (uint8_t *)"comp_code",
			    &val) < 0)
	    goto cleanup;
	  dev->comp_code = val;
	  ipmi_strerror_cmd_r (obj_cmd_rs,
			       dev->errmsg,
			       IPMI_ERR_STR_MAX_LEN);
	  /* This is ok */
	  break;
	}
      
      if (fiid_obj_get (obj_cmd_rs, 
			(uint8_t *)"readable_lower_critical_threshold", 
			&val) < 0)
	goto cleanup;
      sdr_record->record.sdr_full_record.readable_lower_critical_threshold = val;
      if (fiid_obj_get (obj_cmd_rs, 
			(uint8_t *)"readable_upper_critical_threshold", 
			&val) < 0)
	goto cleanup;
      sdr_record->record.sdr_full_record.readable_upper_critical_threshold = val;
      if (fiid_obj_get (obj_cmd_rs, 
			(uint8_t *)"readable_lower_non_critical_threshold", 
			&val) < 0)
	goto cleanup;
      sdr_record->record.sdr_full_record.readable_lower_non_critical_threshold = val;
      if (fiid_obj_get (obj_cmd_rs, 
			(uint8_t *)"readable_upper_non_critical_threshold", 
			&val) < 0)
	goto cleanup;
      sdr_record->record.sdr_full_record.readable_upper_non_critical_threshold = val;
      if (fiid_obj_get (obj_cmd_rs, 
			(uint8_t *)"readable_lower_non_recoverable_threshold", 
			&val) < 0)
	goto cleanup;
      sdr_record->record.sdr_full_record.readable_lower_non_recoverable_threshold = val;
      if (fiid_obj_get (obj_cmd_rs, 
			(uint8_t *)"readable_upper_non_recoverable_threshold", 
			&val) < 0)
	goto cleanup;
      sdr_record->record.sdr_full_record.readable_upper_non_recoverable_threshold = val;
      
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
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  if (obj_sdr_record)
    fiid_obj_destroy(obj_sdr_record);
  return (rv);
}

int8_t 
get_sensor_reading (ipmi_device_t *dev, 
		    sdr_record_t *sdr_record, 
		    sensor_reading_t *sensor_reading)
{
  fiid_template_t l_tmpl_get_sensor_threshold_reading_rs =
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
  
  fiid_template_t l_tmpl_get_sensor_discrete_reading_rs =
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
  uint8_t event_reading_type;
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
      slave_sys_soft_id = sdr_record->record.sdr_full_record.slave_system_software_id;
      if (ipmi_get_system_software_type (slave_sys_soft_id) == IPMI_SYS_SOFT_ID_RESERVED)
	return -1;
      
      event_reading_type = sdr_record->record.sdr_full_record.event_reading_type;
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
      slave_sys_soft_id = sdr_record->record.sdr_compact_record.slave_system_software_id;
      if (ipmi_get_system_software_type (slave_sys_soft_id) == IPMI_SYS_SOFT_ID_RESERVED)
	return -1;
      
      event_reading_type = sdr_record->record.sdr_compact_record.event_reading_type;
      sensor_number = sdr_record->record.sdr_compact_record.sensor_number;
      sensor_type = sdr_record->record.sdr_compact_record.sensor_type;
      break;
    default:
      return -1;
    }
  
  switch (ipmi_sensor_classify (event_reading_type))
    {
    case IPMI_SENSOR_CLASS_THRESHOLD:
      if (!(obj_cmd_rs = fiid_obj_create(tmpl_get_sensor_threshold_reading_rs)))
	goto cleanup;
      if (!(l_obj_cmd_rs = fiid_obj_create(l_tmpl_get_sensor_threshold_reading_rs)))
	goto cleanup;

      if (ipmi_cmd_get_threshold_reading2 (dev, 
					   sensor_number, 
					   obj_cmd_rs) != 0)
	{
	  if (fiid_obj_get (obj_cmd_rs,
			    (uint8_t *)"cmd",
			    &val) < 0)
	    goto cleanup;
	  dev->cmd = val;
	  
	  if (fiid_obj_get (obj_cmd_rs,
			    (uint8_t *)"comp_code",
			    &val) < 0)
	    goto cleanup;
	  dev->comp_code = val;
	  ipmi_strerror_cmd_r (obj_cmd_rs,
			       dev->errmsg,
			       IPMI_ERR_STR_MAX_LEN);
	  goto cleanup;
	}

      if ((len = fiid_obj_get_all(obj_cmd_rs,
				  buf,
				  1024)) < 0)
	goto cleanup;

      if (fiid_obj_set_all(l_obj_cmd_rs,
			   buf,
			   len) < 0)
	goto cleanup;
      
      if (fiid_obj_get (l_obj_cmd_rs, 
			(uint8_t *)"sensor_reading", 
			&val) < 0)
	goto cleanup;
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
      
      if (fiid_obj_get (l_obj_cmd_rs, 
			(uint8_t *)"status_reading_availability", 
			&val) < 0)
	goto cleanup;
      sensor_reading->reading_availability_flag = val;
      if (fiid_obj_get (l_obj_cmd_rs, 
			(uint8_t *)"status_sensor_scanning", 
			&val) < 0)
	goto cleanup;
      sensor_reading->sensor_scanning_flag = val;
      if (fiid_obj_get (l_obj_cmd_rs, 
			(uint8_t *)"status_all_event_messages", 
			&val) < 0)
	goto cleanup;
      sensor_reading->event_messages_flag = val;
      
      if (fiid_obj_get (l_obj_cmd_rs, 
			(uint8_t *)"sensor_state", 
			&val) < 0)
	goto cleanup;
      sensor_reading->event_message_list = 
	ipmi_get_generic_event_message_list (event_reading_type, val);
      
      rv = 0;
      break;
    case IPMI_SENSOR_CLASS_GENERIC_DISCRETE:
      if (!(obj_cmd_rs = fiid_obj_create(tmpl_get_sensor_discrete_reading_rs)))
	goto cleanup;

      if (!(l_obj_cmd_rs = fiid_obj_create(l_tmpl_get_sensor_discrete_reading_rs)))
	goto cleanup;

      if (ipmi_cmd_get_discrete_reading2 (dev, 
					  sensor_number, 
					  obj_cmd_rs) != 0)
	{
	  if (fiid_obj_get (obj_cmd_rs,
			    (uint8_t *)"cmd",
			    &val) < 0)
	    goto cleanup;
	  dev->cmd = val;
	  
	  if (fiid_obj_get (obj_cmd_rs,
			    (uint8_t *)"comp_code",
			    &val) < 0)
	    goto cleanup;
	  dev->comp_code = val;
	  ipmi_strerror_cmd_r (obj_cmd_rs,
			       dev->errmsg,
			       IPMI_ERR_STR_MAX_LEN);
	  goto cleanup;
	}
      
      if ((len = fiid_obj_get_all(obj_cmd_rs,
				  buf,
				  1024)) < 0)
	goto cleanup;

      if (fiid_obj_set_all(l_obj_cmd_rs,
			   buf,
			   len) < 0)
	goto cleanup;

      if (fiid_obj_get (l_obj_cmd_rs, 
			(uint8_t *)"sensor_reading", 
			&val) < 0)
	goto cleanup;

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
      
      if (fiid_obj_get (l_obj_cmd_rs, 
			(uint8_t *)"status_reading_availability", 
			&val) < 0)
	goto cleanup;
      sensor_reading->reading_availability_flag = val;
      if (fiid_obj_get (l_obj_cmd_rs, 
			(uint8_t *)"status_sensor_scanning", 
			&val) < 0)
	goto cleanup;
      sensor_reading->sensor_scanning_flag = val;
      if (fiid_obj_get (l_obj_cmd_rs, 
			(uint8_t *)"status_all_event_messages", 
			&val) < 0)
	goto cleanup;
      sensor_reading->event_messages_flag = val;
      
      if (fiid_obj_get (l_obj_cmd_rs, 
			(uint8_t *)"sensor_state", 
			&val) < 0)
	goto cleanup;
      sensor_reading->event_message_list = 
	ipmi_get_generic_event_message_list (event_reading_type, val);
      
      rv = 0;
      break;
    case IPMI_SENSOR_CLASS_SENSOR_SPECIFIC_DISCRETE:
      if (!(obj_cmd_rs = fiid_obj_create(tmpl_get_sensor_discrete_reading_rs)))
	goto cleanup;

      if (!(l_obj_cmd_rs = fiid_obj_create(l_tmpl_get_sensor_discrete_reading_rs)))
	goto cleanup;

      if (ipmi_cmd_get_discrete_reading2 (dev, 
					  sensor_number, 
					  obj_cmd_rs) != 0)
	{
	  if (fiid_obj_get (obj_cmd_rs,
			    (uint8_t *)"cmd",
			    &val) < 0)
	    goto cleanup;
	  dev->cmd = val;
	  
	  if (fiid_obj_get (obj_cmd_rs,
			    (uint8_t *)"comp_code",
			    &val) < 0)
	    goto cleanup;
	  dev->comp_code = val;
	  ipmi_strerror_cmd_r (obj_cmd_rs,
			       dev->errmsg,
			       IPMI_ERR_STR_MAX_LEN);
	  goto cleanup;
	}
      
      if ((len = fiid_obj_get_all(obj_cmd_rs,
				  buf,
				  1024)) < 0)
	goto cleanup;

      if (fiid_obj_set_all(l_obj_cmd_rs,
			   buf,
			   len) < 0)
	goto cleanup;

      if (fiid_obj_get (l_obj_cmd_rs, 
			(uint8_t *)"sensor_reading", 
			&val) < 0)
	goto cleanup;
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
      
      if (fiid_obj_get (l_obj_cmd_rs, 
			(uint8_t *)"status_reading_availability", 
			&val) < 0)
	goto cleanup;
      sensor_reading->reading_availability_flag = val;
      if (fiid_obj_get (l_obj_cmd_rs, 
			(uint8_t *)"status_sensor_scanning", 
			&val) < 0)
	goto cleanup;
      sensor_reading->sensor_scanning_flag = val;
      if (fiid_obj_get (l_obj_cmd_rs, 
			(uint8_t *)"status_all_event_messages", 
			&val) < 0)
	goto cleanup;
      sensor_reading->event_messages_flag = val;
      
      if (fiid_obj_get (l_obj_cmd_rs, 
			(uint8_t *)"sensor_state", 
			&val) < 0)
	goto cleanup;
      sensor_reading->event_message_list = 
	ipmi_get_event_message_list (sensor_type, val);
      
      rv = 0;
      break;
    case IPMI_SENSOR_CLASS_OEM:
      if (!(obj_cmd_rs = fiid_obj_create(tmpl_get_sensor_discrete_reading_rs)))
	goto cleanup;

      if (!(l_obj_cmd_rs = fiid_obj_create(l_tmpl_get_sensor_discrete_reading_rs)))
	goto cleanup;

      if (ipmi_cmd_get_discrete_reading2 (dev, 
					  sensor_number, 
					  obj_cmd_rs) != 0)
	{
	  if (fiid_obj_get (obj_cmd_rs,
			    (uint8_t *)"cmd",
			    &val) < 0)
	    goto cleanup;
	  dev->cmd = val;
	  
	  if (fiid_obj_get (obj_cmd_rs,
			    (uint8_t *)"comp_code",
			    &val) < 0)
	    goto cleanup;
	  dev->comp_code = val;
	  ipmi_strerror_cmd_r (obj_cmd_rs,
			       dev->errmsg,
			       IPMI_ERR_STR_MAX_LEN);
	  goto cleanup;
	}
      
      if ((len = fiid_obj_get_all(obj_cmd_rs,
				  buf,
				  1024)) < 0)
	goto cleanup;

      if (fiid_obj_set_all(l_obj_cmd_rs,
			   buf,
			   len) < 0)
	goto cleanup;

      if (fiid_obj_get (l_obj_cmd_rs, 
			(uint8_t *)"sensor_reading", 
			&val) < 0)
	goto cleanup;
      sensor_reading->current_reading = val;
      if (fiid_obj_get (l_obj_cmd_rs, 
			(uint8_t *)"status_reading_availability", 
			&val) < 0)
	goto cleanup;
      sensor_reading->reading_availability_flag = val;
      if (fiid_obj_get (l_obj_cmd_rs, 
			(uint8_t *)"status_sensor_scanning", 
			&val) < 0)
	goto cleanup;
      sensor_reading->sensor_scanning_flag = val;
      if (fiid_obj_get (l_obj_cmd_rs, 
			(uint8_t *)"status_all_event_messages", 
			&val) < 0)
	goto cleanup;
      sensor_reading->event_messages_flag = val;
      
      if (fiid_obj_get (l_obj_cmd_rs, 
			(uint8_t *)"sensor_state", 
			&val) < 0)
	goto cleanup;

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
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  if (l_obj_cmd_rs)
    fiid_obj_destroy(l_obj_cmd_rs);
  return (rv);
}
