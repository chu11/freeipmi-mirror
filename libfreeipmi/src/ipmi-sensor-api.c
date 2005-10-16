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
   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.  

*/

#include "freeipmi.h"

void 
get_sdr_full_record (u_int8_t *sdr_record_data, 
		     sdr_full_record_t *sdr_full_record)
{
  u_int64_t val;
  int i;
  int c;
  
  short b;
  short m;
  char r_exponent;
  char b_exponent;
  u_int8_t linear;
  u_int8_t analog_data_format;
  
  u_int8_t record_length;
  fiid_obj_get (sdr_record_data, 
		tmpl_sdr_full_sensor_record, 
		"record_length", 
		&val);
  record_length = val;
  record_length += fiid_obj_len_bytes (tmpl_sdr_sensor_record_header);
  
  ipmi_sensor_get_decode_params (sdr_record_data, 
				 &analog_data_format, &r_exponent, &b_exponent, 
				 &linear, &b, &m);
  sdr_full_record->b = b;
  sdr_full_record->m = m;
  sdr_full_record->r_exponent = r_exponent;
  sdr_full_record->b_exponent = b_exponent;
  sdr_full_record->linear = linear;
  sdr_full_record->analog_data_format = analog_data_format;
  
  fiid_obj_get (sdr_record_data, 
		tmpl_sdr_full_sensor_record, 
		"slave_system_software_id", 
		&val);
  sdr_full_record->slave_system_software_id = val;
  
  fiid_obj_get (sdr_record_data, 
		tmpl_sdr_full_sensor_record, 
		"sensor_number", 
		&val);
  sdr_full_record->sensor_number = val;
  
  fiid_obj_get (sdr_record_data, 
		tmpl_sdr_full_sensor_record, 
		"sensor_type", 
		&val);
  sdr_full_record->sensor_type = val;
  
  fiid_obj_get (sdr_record_data, 
		tmpl_sdr_full_sensor_record, 
		"event_reading_type", 
		&val);
  sdr_full_record->event_reading_type = val;
  
  fiid_obj_get (sdr_record_data, 
		tmpl_sdr_full_sensor_record, 
		"sensor_base_unit", 
		&val);
  sdr_full_record->sensor_unit = val;
  
  fiid_obj_get (sdr_record_data, 
		tmpl_sdr_full_sensor_record, 
		"nominal_reading", 
		&val);
  sdr_full_record->nominal_reading = 
    ipmi_sensor_decode_value (r_exponent,
			      b_exponent,
			      m,
			      b,
			      linear,
			      analog_data_format,
			      val);
  
  fiid_obj_get (sdr_record_data, 
		tmpl_sdr_full_sensor_record,
		"normal_min",
		&val);
  sdr_full_record->normal_min = 
    ipmi_sensor_decode_value (r_exponent,
			      b_exponent,
			      m,
			      b,
			      linear,
			      analog_data_format,
			      val);
  
  fiid_obj_get (sdr_record_data, 
		tmpl_sdr_full_sensor_record,
		"normal_max",
		&val);
  sdr_full_record->normal_max = 
    ipmi_sensor_decode_value (r_exponent,
			      b_exponent,
			      m,
			      b,
			      linear,
			      analog_data_format,
			      val);
  
  fiid_obj_get (sdr_record_data, 
		tmpl_sdr_full_sensor_record,
		"sensor_min_reading",
		&val);
  sdr_full_record->sensor_min_reading = 
    ipmi_sensor_decode_value (r_exponent,
			      b_exponent,
			      m,
			      b,
			      linear,
			      analog_data_format,
			      val);
  
  fiid_obj_get (sdr_record_data, 
		tmpl_sdr_full_sensor_record,
		"sensor_max_reading",
		&val);
  sdr_full_record->sensor_max_reading =
    ipmi_sensor_decode_value (r_exponent,
			      b_exponent,
			      m,
			      b,
			      linear,
			      analog_data_format,
			      val);
  
  fiid_obj_get (sdr_record_data, 
		tmpl_sdr_full_sensor_record,
		"negative_hysteresis", 
		&val);
  sdr_full_record->negative_hysteresis = val;
  
  fiid_obj_get (sdr_record_data, 
		tmpl_sdr_full_sensor_record,
		"positive_hysteresis", 
		&val);
  sdr_full_record->positive_hysteresis = val;
  
  fiid_obj_get (sdr_record_data, 
		tmpl_sdr_full_sensor_record,
		"lower_non_recoverable_threshold",
		&val);
  sdr_full_record->lower_non_recoverable_threshold = 
    ipmi_sensor_decode_value (r_exponent,
			      b_exponent,
			      m,
			      b,
			      linear,
			      analog_data_format,
			      val);
  
  fiid_obj_get (sdr_record_data, 
		tmpl_sdr_full_sensor_record,
		"upper_non_recoverable_threshold",
		&val);
  sdr_full_record->upper_non_recoverable_threshold = 
    ipmi_sensor_decode_value (r_exponent,
			      b_exponent,
			      m,
			      b,
			      linear,
			      analog_data_format,
			      val);
  
  fiid_obj_get (sdr_record_data, 
		tmpl_sdr_full_sensor_record,
		"lower_critical_threshold",
		&val);
  sdr_full_record->lower_critical_threshold = 
    ipmi_sensor_decode_value (r_exponent,
			      b_exponent,
			      m,
			      b,
			      linear,
			      analog_data_format,
			      val);
  
  fiid_obj_get (sdr_record_data, 
		tmpl_sdr_full_sensor_record,
		"upper_critical_threshold",
		&val);
  sdr_full_record->upper_critical_threshold = 
    ipmi_sensor_decode_value (r_exponent,
			      b_exponent,
			      m,
			      b,
			      linear,
			      analog_data_format,
			      val);
  
  fiid_obj_get (sdr_record_data, 
		tmpl_sdr_full_sensor_record,
		"lower_non_critical_threshold",
		&val);
  sdr_full_record->lower_non_critical_threshold = 
    ipmi_sensor_decode_value (r_exponent,
			      b_exponent,
			      m,
			      b,
			      linear,
			      analog_data_format,
			      val);
  
  fiid_obj_get (sdr_record_data, 
		tmpl_sdr_full_sensor_record,
		"upper_non_critical_threshold",
		&val);
  sdr_full_record->upper_non_critical_threshold = 
    ipmi_sensor_decode_value (r_exponent,
			      b_exponent,
			      m,
			      b,
			      linear,
			      analog_data_format,
			      val);
  
  for (c = 0, i = fiid_obj_field_start_bytes (tmpl_sdr_full_sensor_record, 
					      "sensor_id_string"); 
       i < record_length; 
       i++, c++)
    sdr_full_record->sensor_name[c] = sdr_record_data[i];
  sdr_full_record->sensor_name[c] = '\0';
  
  return;
}

void 
get_sdr_compact_record (u_int8_t *sdr_record_data, 
			sdr_compact_record_t *sdr_compact_record)
{
  u_int64_t val;
  int i;
  int c;
  
  u_int8_t record_length;
  fiid_obj_get (sdr_record_data, 
		tmpl_sdr_compact_sensor_record, 
		"record_length", 
		&val);
  record_length = val;
  record_length += fiid_obj_len_bytes (tmpl_sdr_sensor_record_header);
  
  fiid_obj_get (sdr_record_data, 
		tmpl_sdr_compact_sensor_record, 
		"slave_system_software_id", 
		&val);
  sdr_compact_record->slave_system_software_id = val;
  
  fiid_obj_get (sdr_record_data, 
		tmpl_sdr_compact_sensor_record, 
		"sensor_number", 
		&val);
  sdr_compact_record->sensor_number = val;
  
  fiid_obj_get (sdr_record_data, 
		tmpl_sdr_compact_sensor_record, 
		"sensor_type", 
		&val);
  sdr_compact_record->sensor_type = val;
  
  fiid_obj_get (sdr_record_data, 
		tmpl_sdr_compact_sensor_record, 
		"event_reading_type", 
		&val);
  sdr_compact_record->event_reading_type = val;
  
  fiid_obj_get (sdr_record_data, 
		tmpl_sdr_compact_sensor_record, 
		"sensor_base_unit", 
		&val);
  sdr_compact_record->sensor_unit = val;
  
  fiid_obj_get (sdr_record_data, 
		tmpl_sdr_compact_sensor_record,
		"negative_hysteresis", 
		&val);
  sdr_compact_record->negative_hysteresis = val;
  
  fiid_obj_get (sdr_record_data, 
		tmpl_sdr_compact_sensor_record,
		"positive_hysteresis", 
		&val);
  sdr_compact_record->positive_hysteresis = val;
  
  for (c = 0, i = fiid_obj_field_start_bytes (tmpl_sdr_compact_sensor_record, 
					      "sensor_id_string"); 
       i < record_length; 
       i++, c++)
    sdr_compact_record->sensor_name[c] = sdr_record_data[i];
  sdr_compact_record->sensor_name[c] = '\0';
  
  return;
}

void 
get_sdr_event_only_record (u_int8_t *sdr_record_data, 
			   sdr_event_only_record_t *sdr_event_only_record)
{
  u_int64_t val;
  int i;
  int c;
  
  u_int8_t record_length;
  fiid_obj_get (sdr_record_data, 
		tmpl_sdr_event_only_sensor_record, 
		"record_length", 
		&val);
  record_length = val;
  record_length += fiid_obj_len_bytes (tmpl_sdr_sensor_record_header);
  
  fiid_obj_get (sdr_record_data, 
		tmpl_sdr_compact_sensor_record, 
		"slave_system_software_id", 
		&val);
  sdr_event_only_record->slave_system_software_id = val;
  
  fiid_obj_get (sdr_record_data, 
		tmpl_sdr_event_only_sensor_record, 
		"sensor_number", 
		&val);
  sdr_event_only_record->sensor_number = val;
  
  fiid_obj_get (sdr_record_data, 
		tmpl_sdr_event_only_sensor_record, 
		"sensor_type", 
		&val);
  sdr_event_only_record->sensor_type = val;
  
  fiid_obj_get (sdr_record_data, 
		tmpl_sdr_event_only_sensor_record, 
		"event_reading_type", 
		&val);
  sdr_event_only_record->event_reading_type = val;
  
  for (c = 0, i = fiid_obj_field_start_bytes (tmpl_sdr_event_only_sensor_record, 
					      "sensor_id_string"); 
       i < record_length; 
       i++, c++)
    sdr_event_only_record->sensor_name[c] = sdr_record_data[i];
  sdr_event_only_record->sensor_name[c] = '\0';
  
  return;
}

void 
get_sdr_entity_association_record (u_int8_t *sdr_record_data, 
				   sdr_entity_association_record_t *sdr_entity_association_record)
{
  u_int64_t val;
  
  fiid_obj_get (sdr_record_data, 
		tmpl_sdr_entity_association_sensor_record, 
		"container_entity_id", 
		&val);
  sdr_entity_association_record->container_entity_id = val;
  
  fiid_obj_get (sdr_record_data, 
		tmpl_sdr_entity_association_sensor_record, 
		"container_entity_instance", 
		&val);
  sdr_entity_association_record->container_entity_instance = val;
  
  return;
}

void 
get_sdr_generic_device_locator_record (u_int8_t *sdr_record_data, 
				       sdr_generic_device_locator_record_t *sdr_generic_device_locator_record)
{
  u_int64_t val;
  int i;
  int c;
  
  u_int8_t record_length;
  fiid_obj_get (sdr_record_data, 
		tmpl_generic_device_locator_sensor_record, 
		"record_length", 
		&val);
  record_length = val;
  record_length += fiid_obj_len_bytes (tmpl_sdr_sensor_record_header);
  
  fiid_obj_get (sdr_record_data, 
		tmpl_generic_device_locator_sensor_record, 
		"direct_access_address", 
		&val);
  sdr_generic_device_locator_record->direct_access_address = val;
  
  fiid_obj_get (sdr_record_data, 
		tmpl_generic_device_locator_sensor_record, 
		"channel_number", 
		&val);
  sdr_generic_device_locator_record->channel_number = val;
  
  fiid_obj_get (sdr_record_data, 
		tmpl_generic_device_locator_sensor_record, 
		"device_slave_address", 
		&val);
  sdr_generic_device_locator_record->device_slave_address = val;
  
  fiid_obj_get (sdr_record_data, 
		tmpl_generic_device_locator_sensor_record, 
		"private_bus_id", 
		&val);
  sdr_generic_device_locator_record->private_bus_id = val;
  
  fiid_obj_get (sdr_record_data, 
		tmpl_generic_device_locator_sensor_record, 
		"lun_master_write_read_command", 
		&val);
  sdr_generic_device_locator_record->lun_master_write_read_command = val;
  
  fiid_obj_get (sdr_record_data, 
		tmpl_generic_device_locator_sensor_record, 
		"address_span", 
		&val);
  sdr_generic_device_locator_record->address_span = val;
  
  fiid_obj_get (sdr_record_data, 
		tmpl_generic_device_locator_sensor_record, 
		"device_type", 
		&val);
  sdr_generic_device_locator_record->device_type = val;
  
  fiid_obj_get (sdr_record_data, 
		tmpl_generic_device_locator_sensor_record, 
		"device_type_modifier", 
		&val);
  sdr_generic_device_locator_record->device_type_modifier = val;
  
  fiid_obj_get (sdr_record_data, 
		tmpl_generic_device_locator_sensor_record, 
		"entity_id", 
		&val);
  sdr_generic_device_locator_record->entity_id = val;
  
  fiid_obj_get (sdr_record_data, 
		tmpl_generic_device_locator_sensor_record, 
		"entity_instance", 
		&val);
  sdr_generic_device_locator_record->entity_instance = val;
  
  for (c = 0, i = fiid_obj_field_start_bytes (tmpl_generic_device_locator_sensor_record, 
					      "device_id_string"); 
       i < record_length; 
       i++, c++)
    sdr_generic_device_locator_record->device_name[c] = sdr_record_data[i];
  sdr_generic_device_locator_record->device_name[c] = '\0';
  
  return;
}

void 
get_sdr_logical_fru_device_locator_record (u_int8_t *sdr_record_data, 
					   sdr_logical_fru_device_locator_record_t *sdr_logical_fru_device_locator_record)
{
  u_int64_t val;
  int i;
  int c;
  
  u_int8_t record_length;
  fiid_obj_get (sdr_record_data, 
		tmpl_sdr_logical_fru_device_locator_sensor_record, 
		"record_length", 
		&val);
  record_length = val;
  record_length += fiid_obj_len_bytes (tmpl_sdr_sensor_record_header);
  
  fiid_obj_get (sdr_record_data, 
		tmpl_sdr_logical_fru_device_locator_sensor_record, 
		"device_type", 
		&val);
  sdr_logical_fru_device_locator_record->device_type = val;
  
  fiid_obj_get (sdr_record_data, 
		tmpl_sdr_logical_fru_device_locator_sensor_record, 
		"device_type_modifier", 
		&val);
  sdr_logical_fru_device_locator_record->device_type_modifier = val;
  
  fiid_obj_get (sdr_record_data, 
		tmpl_sdr_logical_fru_device_locator_sensor_record, 
		"fru_entity_id", 
		&val);
  sdr_logical_fru_device_locator_record->fru_entity_id = val;
  
  fiid_obj_get (sdr_record_data, 
		tmpl_sdr_logical_fru_device_locator_sensor_record, 
		"fru_entity_instance", 
		&val);
  sdr_logical_fru_device_locator_record->fru_entity_instance = val;
  
  for (c = 0, i = fiid_obj_field_start_bytes (tmpl_sdr_logical_fru_device_locator_sensor_record, 
					      "device_id_string"); 
       i < record_length; 
       i++, c++)
    sdr_logical_fru_device_locator_record->device_name[c] = sdr_record_data[i];
  sdr_logical_fru_device_locator_record->device_name[c] = '\0';
  
  return;
}

void 
get_sdr_management_controller_device_locator_record (u_int8_t *sdr_record_data, 
						     sdr_management_controller_device_locator_record_t *sdr_management_controller_device_locator_record)
{
  u_int64_t val;
  int i;
  int c;
  
  u_int8_t record_length;
  fiid_obj_get (sdr_record_data, 
		tmpl_sdr_management_controller_device_locator_sensor_record, 
		"record_length", 
		&val);
  record_length = val;
  record_length += fiid_obj_len_bytes (tmpl_sdr_sensor_record_header);
  
  fiid_obj_get (sdr_record_data, 
		tmpl_sdr_management_controller_device_locator_sensor_record, 
		"entity_id", 
		&val);
  sdr_management_controller_device_locator_record->entity_id = val;
  
  fiid_obj_get (sdr_record_data, 
		tmpl_sdr_management_controller_device_locator_sensor_record, 
		"entity_instance", 
		&val);
  sdr_management_controller_device_locator_record->entity_instance = val;
  
  for (c = 0, i = fiid_obj_field_start_bytes (tmpl_sdr_management_controller_device_locator_sensor_record, "device_id_string"); 
       i < record_length; 
       i++, c++)
    sdr_management_controller_device_locator_record->device_name[c] = sdr_record_data[i];
  sdr_management_controller_device_locator_record->device_name[c] = '\0';
  
  return;
}

void 
get_sdr_oem_record (u_int8_t *sdr_record_data, 
		    sdr_oem_record_t *sdr_oem_record)
{
  u_int64_t val;
  int i;
  int c;
  
  u_int8_t record_length;
  fiid_obj_get (sdr_record_data, 
		tmpl_sdr_oem_record, 
		"record_length", 
		&val);
  record_length = val;
  record_length += fiid_obj_len_bytes (tmpl_sdr_sensor_record_header);
  
  fiid_obj_get (sdr_record_data, 
		tmpl_sdr_oem_record, 
		"manufacturer_id", 
		&val);
  sdr_oem_record->manufacturer_id = val;
  
  for (c = 0, i = fiid_obj_field_start_bytes (tmpl_sdr_oem_record, 
					      "oem_data"); 
       i < record_length; 
       i++, c++)
    sdr_oem_record->oem_data[c] = sdr_record_data[i];
  sdr_oem_record->oem_data_length = c;
  
  return;
}

u_int8_t 
get_sdr_record (ipmi_device_t *dev, 
		u_int16_t record_id, 
		u_int16_t *next_record_id, 
		sdr_record_t *sdr_record)
{
  fiid_obj_t obj_cmd_rs = NULL;
  fiid_obj_t obj_sdr_record = NULL;
  u_int64_t val = 0;
  
  fiid_obj_alloca (obj_cmd_rs, tmpl_get_sdr_rs);
  if (ipmi_cmd_get_sdr2 (dev, 
			 record_id, 
			 obj_cmd_rs, 
			 &obj_sdr_record) != 0)
    {
      ipmi_error (obj_cmd_rs, "ipmi_cmd_get_sdr2()");
      return (-1);
    }
  
  memset (sdr_record, 0, sizeof (sdr_record_t));
  
  fiid_obj_get (obj_cmd_rs, 
		tmpl_get_sdr_rs, 
		"next_record_id", 
		&val);
  *next_record_id = val;
  
  fiid_obj_get (obj_sdr_record, 
		tmpl_sdr_sensor_record_header, 
		"record_id", 
		&val);
  sdr_record->record_id = val;
  
  fiid_obj_get (obj_sdr_record, 
		tmpl_sdr_sensor_record_header, 
		"record_type", 
		&val);
  sdr_record->record_type = val;
  
  switch (sdr_record->record_type)
    {
    case IPMI_SDR_FORMAT_FULL_RECORD:
      get_sdr_full_record (obj_sdr_record, 
			   &(sdr_record->record.sdr_full_record));
      
      if (ipmi_sensor_classify (sdr_record->record.sdr_full_record.event_reading_type) != 
	  IPMI_SENSOR_CLASS_THRESHOLD)
	{
	  break;
	}
      
      fiid_obj_alloca (obj_cmd_rs, tmpl_get_sensor_thresholds_rs);
      if (ipmi_cmd_get_sensor_thresholds2 (dev, 
					   sdr_record->record.sdr_full_record.sensor_number, 
					   obj_cmd_rs) != 0)
	{
	  ipmi_error (obj_cmd_rs, "ipmi_cmd_get_sensor_thresholds2()");
	  /* This is ok */
	  break;
	}
      
      fiid_obj_get (obj_cmd_rs, 
		    tmpl_get_sensor_thresholds_rs, 
		    "readable_lower_critical_threshold", 
		    &val);
      sdr_record->record.sdr_full_record.readable_lower_critical_threshold = val;
      fiid_obj_get (obj_cmd_rs, 
		    tmpl_get_sensor_thresholds_rs, 
		    "readable_upper_critical_threshold", 
		    &val);
      sdr_record->record.sdr_full_record.readable_upper_critical_threshold = val;
      fiid_obj_get (obj_cmd_rs, 
		    tmpl_get_sensor_thresholds_rs, 
		    "readable_lower_non_critical_threshold", 
		    &val);
      sdr_record->record.sdr_full_record.readable_lower_non_critical_threshold = val;
      fiid_obj_get (obj_cmd_rs, 
		    tmpl_get_sensor_thresholds_rs, 
		    "readable_upper_non_critical_threshold", 
		    &val);
      sdr_record->record.sdr_full_record.readable_upper_non_critical_threshold = val;
      fiid_obj_get (obj_cmd_rs, 
		    tmpl_get_sensor_thresholds_rs, 
		    "readable_lower_non_recoverable_threshold", 
		    &val);
      sdr_record->record.sdr_full_record.readable_lower_non_recoverable_threshold = val;
      fiid_obj_get (obj_cmd_rs, 
		    tmpl_get_sensor_thresholds_rs, 
		    "readable_upper_non_recoverable_threshold", 
		    &val);
      sdr_record->record.sdr_full_record.readable_upper_non_recoverable_threshold = val;
      
      break;
    case IPMI_SDR_FORMAT_COMPACT_RECORD:
      get_sdr_compact_record (obj_sdr_record, 
			      &(sdr_record->record.sdr_compact_record));
      break;
    case IPMI_SDR_FORMAT_EVENT_ONLY_RECORD:
      get_sdr_event_only_record (obj_sdr_record, 
				 &(sdr_record->record.sdr_event_only_record));
      break;
    case IPMI_SDR_FORMAT_ENTITY_ASSO_RECORD:
      get_sdr_entity_association_record (obj_sdr_record, 
					 &(sdr_record->record.sdr_entity_association_record));
      break;
    case IPMI_SDR_FORMAT_GEN_DEV_LOCATOR_RECORD:
      get_sdr_generic_device_locator_record (obj_sdr_record, 
					     &(sdr_record->record.sdr_generic_device_locator_record));
      break;
    case IPMI_SDR_FORMAT_FRU_DEV_LOCATOR_RECORD:
      get_sdr_logical_fru_device_locator_record (obj_sdr_record, 
						 &(sdr_record->record.sdr_logical_fru_device_locator_record));
      break;
    case IPMI_SDR_FORMAT_MGMT_CNTRLR_DEV_LOCATOR_RECORD:
      get_sdr_management_controller_device_locator_record (obj_sdr_record, 
							   &(sdr_record->record.sdr_management_controller_device_locator_record));
      break;
    case IPMI_SDR_FORMAT_OEM_RECORD:
      get_sdr_oem_record (obj_sdr_record, 
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
  
  ipmi_xfree (obj_sdr_record);
  
  return 0;
}

u_int8_t 
get_sensor_reading (ipmi_device_t *dev, 
		    sdr_record_t *sdr_record, 
		    sensor_reading_t *sensor_reading)
{
  fiid_template_t l_tmpl_get_sensor_threshold_reading_rs =
    {
      {8, "cmd"}, 
      {8, "comp_code"}, 
      
      {8, "sensor_reading"}, 
      
      {5, "reserved1"}, 
      {1, "status_reading_availability"}, 
      {1, "status_sensor_scanning"}, 
      {1, "status_all_event_messages"}, 
      
      {6, "sensor_state"}, 
      {2, "reserved2"}, 
      
      /* optional byte */
      {8, "ignore"}, 
      
      {0,  ""}
    };
  
  fiid_template_t l_tmpl_get_sensor_discrete_reading_rs =
    {
      {8, "cmd"}, 
      {8, "comp_code"}, 
      
      {8, "sensor_reading"}, 
      
      {5, "reserved1"}, 
      {1, "status_reading_availability"}, 
      {1, "status_sensor_scanning"}, 
      {1, "status_all_event_messages"}, 
      
      {15, "sensor_state"}, 
      {1, "reserved2"}, 
      
      {0,  ""}
    };
  
  u_int8_t slave_sys_soft_id;
  u_int8_t event_reading_type;
  u_int8_t sensor_number;
  u_int8_t sensor_type;
  short b = 0;
  short m = 0;
  char r_exponent = 0;
  char b_exponent = 0;
  u_int8_t linear = 0;
  u_int8_t analog_data_format = 0;
  
  fiid_obj_t obj_cmd_rs; 
  u_int64_t val;
  
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
  
  fiid_obj_alloca (obj_cmd_rs, l_tmpl_get_sensor_threshold_reading_rs);
  
  switch (ipmi_sensor_classify (event_reading_type))
    {
    case IPMI_SENSOR_CLASS_THRESHOLD:
      if (ipmi_cmd_get_threshold_reading2 (dev, 
					   sensor_number, 
					   obj_cmd_rs) != 0)
	{
	  ipmi_error (obj_cmd_rs, "ipmi_cmd_get_threshold_reading2()");
	  return (-1);
	}
      
      fiid_obj_get (obj_cmd_rs, 
		    l_tmpl_get_sensor_threshold_reading_rs, 
		    "sensor_reading", 
		    &val);
      if (sdr_record->record_type == IPMI_SDR_FORMAT_FULL_RECORD)
	{
	  sensor_reading->current_reading = ipmi_sensor_decode_value (r_exponent, 
								      b_exponent, 
								      m, 
								      b, 
								      linear, 
								      analog_data_format, 
								      (u_int8_t) val);
	}
      else 
	{
	  sensor_reading->current_reading = val;
	}
      
      fiid_obj_get (obj_cmd_rs, 
		    l_tmpl_get_sensor_threshold_reading_rs, 
		    "status_reading_availability", 
		    &val);
      sensor_reading->reading_availability_flag = val;
      fiid_obj_get (obj_cmd_rs, 
		    l_tmpl_get_sensor_threshold_reading_rs, 
		    "status_sensor_scanning", 
		    &val);
      sensor_reading->sensor_scanning_flag = val;
      fiid_obj_get (obj_cmd_rs, 
		    l_tmpl_get_sensor_threshold_reading_rs, 
		    "status_all_event_messages", 
		    &val);
      sensor_reading->event_messages_flag = val;
      
      fiid_obj_get (obj_cmd_rs, 
		    l_tmpl_get_sensor_threshold_reading_rs, 
		    "sensor_state", 
		    &val);
      sensor_reading->event_message_list = 
	ipmi_get_generic_event_message_list (event_reading_type, val);
      
      return 0;
    case IPMI_SENSOR_CLASS_GENERIC_DISCRETE:
      if (ipmi_cmd_get_discrete_reading2 (dev, 
					  sensor_number, 
					  obj_cmd_rs) != 0)
	{
	  ipmi_error (obj_cmd_rs, "ipmi_cmd_get_discrete_reading2()");
	  return (-1);
	}
      
      fiid_obj_get (obj_cmd_rs, 
		    l_tmpl_get_sensor_discrete_reading_rs, 
		    "sensor_reading", 
		    &val);
      if (sdr_record->record_type == IPMI_SDR_FORMAT_FULL_RECORD)
	{
	  sensor_reading->current_reading = ipmi_sensor_decode_value (r_exponent, 
								      b_exponent, 
								      m, 
								      b, 
								      linear, 
								      analog_data_format, 
								      (u_int8_t) val);
	}
      else 
	{
	  sensor_reading->current_reading = val;
	}
      
      fiid_obj_get (obj_cmd_rs, 
		    l_tmpl_get_sensor_discrete_reading_rs, 
		    "status_reading_availability", 
		    &val);
      sensor_reading->reading_availability_flag = val;
      fiid_obj_get (obj_cmd_rs, 
		    l_tmpl_get_sensor_discrete_reading_rs, 
		    "status_sensor_scanning", 
		    &val);
      sensor_reading->sensor_scanning_flag = val;
      fiid_obj_get (obj_cmd_rs, 
		    l_tmpl_get_sensor_discrete_reading_rs, 
		    "status_all_event_messages", 
		    &val);
      sensor_reading->event_messages_flag = val;
      
      fiid_obj_get (obj_cmd_rs, 
		    l_tmpl_get_sensor_discrete_reading_rs, 
		    "sensor_state", 
		    &val);
      sensor_reading->event_message_list = 
	ipmi_get_generic_event_message_list (event_reading_type, val);
      
      return 0;
    case IPMI_SENSOR_CLASS_SENSOR_SPECIFIC_DISCRETE:
      if (ipmi_cmd_get_discrete_reading2 (dev, 
					  sensor_number, 
					  obj_cmd_rs) != 0)
	{
	  ipmi_error (obj_cmd_rs, "ipmi_cmd_get_discrete_reading2()");
	  return (-1);
	}
      
      fiid_obj_get (obj_cmd_rs, 
		    l_tmpl_get_sensor_discrete_reading_rs, 
		    "sensor_reading", 
		    &val);
      if (sdr_record->record_type == IPMI_SDR_FORMAT_FULL_RECORD)
	{
	  sensor_reading->current_reading = ipmi_sensor_decode_value (r_exponent, 
								      b_exponent, 
								      m, 
								      b, 
								      linear, 
								      analog_data_format, 
								      (u_int8_t) val);
	}
      else 
	{
	  sensor_reading->current_reading = val;
	}
      
      fiid_obj_get (obj_cmd_rs, 
		    l_tmpl_get_sensor_discrete_reading_rs, 
		    "status_reading_availability", 
		    &val);
      sensor_reading->reading_availability_flag = val;
      fiid_obj_get (obj_cmd_rs, 
		    l_tmpl_get_sensor_discrete_reading_rs, 
		    "status_sensor_scanning", 
		    &val);
      sensor_reading->sensor_scanning_flag = val;
      fiid_obj_get (obj_cmd_rs, 
		    l_tmpl_get_sensor_discrete_reading_rs, 
		    "status_all_event_messages", 
		    &val);
      sensor_reading->event_messages_flag = val;
      
      fiid_obj_get (obj_cmd_rs, 
		    l_tmpl_get_sensor_discrete_reading_rs, 
		    "sensor_state", 
		    &val);
      sensor_reading->event_message_list = 
	ipmi_get_event_message_list (sensor_type, val);
      
      return 0;
    case IPMI_SENSOR_CLASS_OEM:
      if (ipmi_cmd_get_discrete_reading2 (dev, 
					  sensor_number, 
					  obj_cmd_rs) != 0)
	{
	  ipmi_error (obj_cmd_rs, "ipmi_cmd_get_discrete_reading2()");
	  return (-1);
	}
      
      fiid_obj_get (obj_cmd_rs, 
		    l_tmpl_get_sensor_discrete_reading_rs, 
		    "sensor_reading", 
		    &val);
      sensor_reading->current_reading = val;
      fiid_obj_get (obj_cmd_rs, 
		    l_tmpl_get_sensor_discrete_reading_rs, 
		    "status_reading_availability", 
		    &val);
      sensor_reading->reading_availability_flag = val;
      fiid_obj_get (obj_cmd_rs, 
		    l_tmpl_get_sensor_discrete_reading_rs, 
		    "status_sensor_scanning", 
		    &val);
      sensor_reading->sensor_scanning_flag = val;
      fiid_obj_get (obj_cmd_rs, 
		    l_tmpl_get_sensor_discrete_reading_rs, 
		    "status_all_event_messages", 
		    &val);
      sensor_reading->event_messages_flag = val;
      
      fiid_obj_get (obj_cmd_rs, 
		    l_tmpl_get_sensor_discrete_reading_rs, 
		    "sensor_state", 
		    &val);
      {
	char *event_message = NULL;
	asprintf (&event_message, 
		  "OEM State = %04Xh", 
		  (u_int16_t) val);
	sensor_reading->event_message_list = (char **) malloc (sizeof (char *) * 2);
	sensor_reading->event_message_list[0] = event_message;
	sensor_reading->event_message_list[1] = NULL;
      }
      
      return 0;
    }
  
  return 0;
}
