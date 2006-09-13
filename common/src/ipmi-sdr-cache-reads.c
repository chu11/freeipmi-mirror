/*
ipmi-sdr-cache-reads.c: SDR cache creation and management apis.
Copyright (C) 2006 FreeIPMI Core Team

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA
*/

#if HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#ifdef HAVE_STDINT_H
#include <stdint.h>
#endif
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#if HAVE_UNISTD_H
#include <unistd.h>
#endif	/* HAVE_UNISTD_H */
#ifdef HAVE_ERROR_H
#include <error.h>
#endif
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <sys/resource.h>
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
#include <sys/stat.h>
#include <sys/param.h>
#include <arpa/inet.h>
#include <pwd.h>
#include <errno.h>

#include "freeipmi/fiid.h"
#include "freeipmi/udm/ipmi-sdr-repository-cmds-udm.h"
#include "freeipmi/udm/ipmi-sensor-cmds-udm.h"

#include "bit-ops.h"
#include "err-wrappers.h"
#include "fiid-wrappers.h"
#include "freeipmi-portability.h"
#include "ipmi-common.h"

#include "common-utils.h"
#include "ipmi-sdr-api.h"

#define GET_INT_VALUE_BY_KEY(__cache_record, __key, __i) \
do 							 \
  {							 \
    if (_get_int_value_by_key (__cache_record, 		 \
			       __key, 			 \
			       __i) == -1)      	 \
      {							 \
	return (-1);					 \
      }							 \
  }                                                      \
 while (0)

#define GET_DOUBLE_VALUE_BY_KEY(__cache_record, __key, __d) \
  do 							    \
{							    \
    if (_get_double_value_by_key (__cache_record,           \
				  __key, 		    \
				  __d) == -1)      	    \
      {							    \
	return (-1);					    \
      }							    \
  }                                                         \
 while (0)



#define GET_STRING_VALUE_BY_KEY(__cache_record, __key, __s)  \
do 							     \
  {							     \
    if (_get_string_value_by_key (__cache_record, 	     \
				  __key, 		     \
				  __s) == -1)		     \
      {							     \
	return (-1);					     \
      }							     \
  }                                                          \
 while (0)

static int 
_get_value_by_key (char *cache_record, 
		   char *key, 
		   char **value)
{
  char *skey = NULL;
  char *start_pos = NULL;
  char *value_ptr = NULL;
  char *value_end_ptr = NULL;
  
  skey = alloca (strlen (key) + 3);
  strcpy (skey, key);
  strcat (skey, "=");
  
  start_pos = strcasestr (cache_record, skey);
  if (start_pos != cache_record)
    {
      strcpy (skey, "\n");
      strcat (skey, key);
      strcat (skey, "=");
      start_pos = strcasestr (cache_record, skey);
    }
  
  if (start_pos == NULL)
    return (-1);
  
  value_ptr = start_pos + strlen (skey);
  value_end_ptr = strcasestr (value_ptr, "\n");
  if (value_end_ptr == NULL)
    {
      *value = strdup (value_ptr);
      return 0;
    }
  *value = strndup (value_ptr, (value_end_ptr - value_ptr));
  return 0;
}

static int 
_get_int_value_by_key (char *cache_record, 
		       char *key, 
		       int *i)
{
  char *value_ptr = NULL;
  int rv = 0;
  
  if (_get_value_by_key (cache_record, 
			 key, 
			 &value_ptr) == -1)
    {
      return (-1);
    }
  
  rv = str2int (value_ptr, 0, i);
  
  free (value_ptr);
  
  return rv;
}

static int 
_get_double_value_by_key (char *cache_record, 
			  char *key, 
			  double *d)
{
  char *value_ptr = NULL;
  int rv = 0;
  
  if (_get_value_by_key (cache_record, 
			 key, 
			 &value_ptr) == -1)
    {
      return (-1);
    }
  
  rv = str2double (value_ptr, d);
  
  free (value_ptr);
  
  return rv;
}

static int 
_get_string_value_by_key (char *cache_record, 
			  char *key, 
			  char **s)
{
  char *value_ptr = NULL;
  
  if (_get_value_by_key (cache_record, 
			 key, 
			 &value_ptr) == -1)
    {
      return (-1);
    }
  
  *s = value_ptr;
  
  return 0;
}

int 
read_sdr_repository_info (char *cache_record, 
			  sdr_repository_info_t *sdr_info)
{
  ERR_EINVAL (cache_record && sdr_info);
  
  GET_INT_VALUE_BY_KEY (cache_record, 
			"sdr_version_major", 
			&(sdr_info->sdr_version_major));
  GET_INT_VALUE_BY_KEY (cache_record, 
			"sdr_version_minor", 
			&(sdr_info->sdr_version_minor));
  GET_INT_VALUE_BY_KEY (cache_record, 
			"record_count", 
			&(sdr_info->record_count));
  GET_INT_VALUE_BY_KEY (cache_record, 
			"free_space", 
			&(sdr_info->free_space));
  GET_INT_VALUE_BY_KEY (cache_record, 
			"most_recent_addition_timestamp", 
			&(sdr_info->most_recent_addition_timestamp));
  GET_INT_VALUE_BY_KEY (cache_record, 
			"most_recent_erase_timestamp", 
			&(sdr_info->most_recent_erase_timestamp));
  GET_INT_VALUE_BY_KEY (cache_record, 
			"get_sdr_repository_allocation_info_command_supported", 
			&(sdr_info->get_sdr_repository_allocation_info_command_supported));
  GET_INT_VALUE_BY_KEY (cache_record, 
			"reserve_sdr_repository_command_supported", 
			&(sdr_info->reserve_sdr_repository_command_supported));
  GET_INT_VALUE_BY_KEY (cache_record, 
			"partial_add_sdr_command_supported", 
			&(sdr_info->partial_add_sdr_command_supported));
  GET_INT_VALUE_BY_KEY (cache_record, 
			"delete_sdr_command_supported", 
			&(sdr_info->delete_sdr_command_supported));
  GET_INT_VALUE_BY_KEY (cache_record, 
			"modal_non_modal_sdr_repository_update_operation_supported", 
			&(sdr_info->modal_non_modal_sdr_repository_update_operation_supported));
  GET_INT_VALUE_BY_KEY (cache_record, 
			"overflow_flag", 
			&(sdr_info->overflow_flag));
  
  return 0;
}

static int 
_read_sdr_full_record (char *cache_record, 
		       sdr_full_record_t *record)
{
  int int_value = 0;
  char *string_value = NULL;
  
  ERR_EINVAL (cache_record && record);
  
  GET_INT_VALUE_BY_KEY (cache_record, 
			"b", 
			&int_value);
  record->b = int_value;
  GET_INT_VALUE_BY_KEY (cache_record, 
			"m", 
			&int_value);
  record->m = int_value;
  GET_INT_VALUE_BY_KEY (cache_record, 
			"r_exponent", 
			&int_value);
  record->r_exponent = int_value;
  GET_INT_VALUE_BY_KEY (cache_record, 
			"b_exponent", 
			&int_value);
  record->b_exponent = int_value;
  GET_INT_VALUE_BY_KEY (cache_record, 
			"linear", 
			&int_value);
  record->linear = int_value;
  GET_INT_VALUE_BY_KEY (cache_record, 
			"analog_data_format", 
			&int_value);
  record->analog_data_format = int_value;
  GET_INT_VALUE_BY_KEY (cache_record, 
			"sensor_owner_id", 
			&int_value);
  record->sensor_owner_id = int_value;
  GET_INT_VALUE_BY_KEY (cache_record, 
			"sensor_number", 
			&int_value);
  record->sensor_number = int_value;
  GET_INT_VALUE_BY_KEY (cache_record, 
			"sensor_type", 
			&int_value);
  record->sensor_type = int_value;
  GET_INT_VALUE_BY_KEY (cache_record, 
			"event_reading_type_code", 
			&int_value);
  record->event_reading_type_code = int_value;
  GET_INT_VALUE_BY_KEY (cache_record, 
			"sensor_unit", 
			&int_value);
  record->sensor_unit = int_value;
  GET_DOUBLE_VALUE_BY_KEY (cache_record, 
			   "nominal_reading", 
			   &(record->nominal_reading));
  GET_DOUBLE_VALUE_BY_KEY (cache_record, 
			   "normal_minimum", 
			   &(record->normal_minimum));
  GET_DOUBLE_VALUE_BY_KEY (cache_record, 
			   "normal_maximum", 
			   &(record->normal_maximum));
  GET_DOUBLE_VALUE_BY_KEY (cache_record, 
			   "sensor_minimum_reading", 
			   &(record->sensor_minimum_reading));
  GET_DOUBLE_VALUE_BY_KEY (cache_record, 
			   "sensor_maximum_reading", 
			   &(record->sensor_maximum_reading));
  GET_INT_VALUE_BY_KEY (cache_record, 
			"negative_going_threshold_hysteresis", 
			&int_value);
  record->negative_going_threshold_hysteresis = int_value;
  GET_INT_VALUE_BY_KEY (cache_record, 
			"positive_going_threshold_hysteresis", 
			&int_value);
  record->positive_going_threshold_hysteresis = int_value;
  GET_DOUBLE_VALUE_BY_KEY (cache_record, 
			   "lower_non_recoverable_threshold", 
			   &(record->lower_non_recoverable_threshold));
  GET_DOUBLE_VALUE_BY_KEY (cache_record, 
			   "upper_non_recoverable_threshold", 
			   &(record->upper_non_recoverable_threshold));
  GET_DOUBLE_VALUE_BY_KEY (cache_record, 
			   "lower_critical_threshold", 
			   &(record->lower_critical_threshold));
  GET_DOUBLE_VALUE_BY_KEY (cache_record, 
			   "upper_critical_threshold", 
			   &(record->upper_critical_threshold));
  GET_DOUBLE_VALUE_BY_KEY (cache_record, 
			   "lower_non_critical_threshold", 
			   &(record->lower_non_critical_threshold));
  GET_DOUBLE_VALUE_BY_KEY (cache_record, 
			   "upper_non_critical_threshold", 
			   &(record->upper_non_critical_threshold));
  string_value = NULL;
  GET_STRING_VALUE_BY_KEY (cache_record, 
			   "sensor_name", 
			   &string_value);
  if (strlen (string_value) < IPMI_SENSOR_NAME_MAX)
    {
      strcpy (record->sensor_name, string_value);
    }
  else 
    {
      strncpy (record->sensor_name, string_value, IPMI_SENSOR_NAME_MAX - 1);
      record->sensor_name[IPMI_SENSOR_NAME_MAX - 1] = '\0';
    }
  free (string_value);
  
  GET_INT_VALUE_BY_KEY (cache_record, 
			"readable_threshold_lower_critical_threshold", 
			&int_value);
  record->readable_threshold_lower_critical_threshold = int_value;
  GET_INT_VALUE_BY_KEY (cache_record, 
			"readable_threshold_upper_critical_threshold", 
			&int_value);
  record->readable_threshold_upper_critical_threshold = int_value;
  GET_INT_VALUE_BY_KEY (cache_record, 
			"readable_threshold_lower_non_critical_threshold", 
			&int_value);
  record->readable_threshold_lower_non_critical_threshold = int_value;
  GET_INT_VALUE_BY_KEY (cache_record, 
			"readable_threshold_upper_non_critical_threshold", 
			&int_value);
  record->readable_threshold_upper_non_critical_threshold = int_value;
  GET_INT_VALUE_BY_KEY (cache_record, 
			"readable_threshold_lower_non_recoverable_threshold", 
			&int_value);
  record->readable_threshold_lower_non_recoverable_threshold = int_value;
  GET_INT_VALUE_BY_KEY (cache_record, 
			"readable_threshold_upper_non_recoverable_threshold", 
			&int_value);
  record->readable_threshold_upper_non_recoverable_threshold = int_value;
  
  return 0;
}

static int 
_read_sdr_compact_record (char *cache_record, 
			  sdr_compact_record_t *record)
{
  int int_value = 0;
  char *string_value = NULL;
  
  ERR_EINVAL (cache_record && record);
  
  GET_INT_VALUE_BY_KEY (cache_record, 
			"sensor_owner_id", 
			&int_value);
  record->sensor_owner_id = int_value;
  GET_INT_VALUE_BY_KEY (cache_record, 
			"sensor_number", 
			&int_value);
  record->sensor_number = int_value;
  GET_INT_VALUE_BY_KEY (cache_record, 
			"sensor_type", 
			&int_value);
  record->sensor_type = int_value;
  GET_INT_VALUE_BY_KEY (cache_record, 
			"event_reading_type_code", 
			&int_value);
  record->event_reading_type_code = int_value;
  GET_INT_VALUE_BY_KEY (cache_record, 
			"sensor_unit", 
			&int_value);
  record->sensor_unit = int_value;
  GET_INT_VALUE_BY_KEY (cache_record, 
			"negative_going_threshold_hysteresis", 
			&int_value);
  record->negative_going_threshold_hysteresis = int_value;
  GET_INT_VALUE_BY_KEY (cache_record, 
			"positive_going_threshold_hysteresis", 
			&int_value);
  record->positive_going_threshold_hysteresis = int_value;
  GET_STRING_VALUE_BY_KEY (cache_record, 
			   "sensor_name", 
			   &string_value);
  if (strlen (string_value) < IPMI_SENSOR_NAME_MAX)
    {
      strcpy (record->sensor_name, string_value);
    }
  else 
    {
      strncpy (record->sensor_name, string_value, IPMI_SENSOR_NAME_MAX - 1);
      record->sensor_name[IPMI_SENSOR_NAME_MAX - 1] = '\0';
    }
  free (string_value);
  
  return 0;
}

static int 
_read_sdr_event_only_record (char *cache_record, 
			     sdr_event_only_record_t *record)
{
  int int_value = 0;
  char *string_value = NULL;
  
  ERR_EINVAL (cache_record && record);
  
  GET_INT_VALUE_BY_KEY (cache_record, 
			"sensor_owner_id", 
			&int_value);
  record->sensor_owner_id = int_value;
  GET_INT_VALUE_BY_KEY (cache_record, 
			"sensor_number", 
			&int_value);
  record->sensor_number = int_value;
  GET_INT_VALUE_BY_KEY (cache_record, 
			"sensor_type", 
			&int_value);
  record->sensor_type = int_value;
  GET_INT_VALUE_BY_KEY (cache_record, 
			"event_reading_type_code", 
			&int_value);
  record->event_reading_type_code = int_value;
  GET_STRING_VALUE_BY_KEY (cache_record, 
			   "sensor_name", 
			   &string_value);
  if (strlen (string_value) < IPMI_SENSOR_NAME_MAX)
    {
      strcpy (record->sensor_name, string_value);
    }
  else 
    {
      strncpy (record->sensor_name, string_value, IPMI_SENSOR_NAME_MAX - 1);
      record->sensor_name[IPMI_SENSOR_NAME_MAX - 1] = '\0';
    }
  free (string_value);
  
  return 0;
}

static int 
_read_sdr_entity_association_record (char *cache_record, 
				     sdr_entity_association_record_t *record)
{
  int int_value = 0;
  
  ERR_EINVAL (cache_record && record);
  
  GET_INT_VALUE_BY_KEY (cache_record, 
			"container_entity_id", 
			&int_value);
  record->container_entity_id = int_value;
  GET_INT_VALUE_BY_KEY (cache_record, 
			"container_entity_instance", 
			&int_value);
  record->container_entity_instance = int_value;
  
  return 0;
}

static int 
_read_sdr_generic_device_locator_record (char *cache_record, 
					 sdr_generic_device_locator_record_t *record)
{
  int int_value = 0;
  char *string_value = NULL;
  
  ERR_EINVAL (cache_record && record);
  
  GET_INT_VALUE_BY_KEY (cache_record, 
			"direct_access_address", 
			&int_value);
  record->direct_access_address = int_value;
  GET_INT_VALUE_BY_KEY (cache_record, 
			"channel_number", 
			&int_value);
  record->channel_number = int_value;
  GET_INT_VALUE_BY_KEY (cache_record, 
			"device_slave_address", 
			&int_value);
  record->device_slave_address = int_value;
  GET_INT_VALUE_BY_KEY (cache_record, 
			"private_bus_id", 
			&int_value);
  record->private_bus_id = int_value;
  GET_INT_VALUE_BY_KEY (cache_record, 
			"lun_for_master_write_read_command", 
			&int_value);
  record->lun_for_master_write_read_command = int_value;
  GET_INT_VALUE_BY_KEY (cache_record, 
			"address_span", 
			&int_value);
  record->address_span = int_value;
  GET_INT_VALUE_BY_KEY (cache_record, 
			"device_type", 
			&int_value);
  record->device_type = int_value;
  GET_INT_VALUE_BY_KEY (cache_record, 
			"device_type_modifier", 
			&int_value);
  record->device_type_modifier = int_value;
  GET_INT_VALUE_BY_KEY (cache_record, 
			"entity_id", 
			&int_value);
  record->entity_id = int_value;
  GET_INT_VALUE_BY_KEY (cache_record, 
			"entity_instance", 
			&int_value);
  record->entity_instance = int_value;
  GET_STRING_VALUE_BY_KEY (cache_record, 
			   "device_name", 
			   &string_value);
  if (strlen (string_value) < IPMI_DEVICE_NAME_MAX)
    {
      strcpy (record->device_name, string_value);
    }
  else 
    {
      strncpy (record->device_name, string_value, IPMI_DEVICE_NAME_MAX - 1);
      record->device_name[IPMI_DEVICE_NAME_MAX - 1] = '\0';
    }
  free (string_value);
  
  return 0;
}

static int 
_read_sdr_logical_fru_device_locator_record (char *cache_record, 
					     sdr_logical_fru_device_locator_record_t *record)
{
  int int_value = 0;
  char *string_value = NULL;
  
  ERR_EINVAL (cache_record && record);
  
  GET_INT_VALUE_BY_KEY (cache_record, 
			"device_type", 
			&int_value);
  record->device_type = int_value;
  GET_INT_VALUE_BY_KEY (cache_record, 
			"device_type_modifier", 
			&int_value);
  record->device_type_modifier = int_value;
  GET_INT_VALUE_BY_KEY (cache_record, 
			"fru_entity_id", 
			&int_value);
  record->fru_entity_id = int_value;
  GET_INT_VALUE_BY_KEY (cache_record, 
			"fru_entity_instance", 
			&int_value);
  record->fru_entity_instance = int_value;
  GET_STRING_VALUE_BY_KEY (cache_record, 
			   "device_name", 
			   &string_value);
  if (strlen (string_value) < IPMI_DEVICE_NAME_MAX)
    {
      strcpy (record->device_name, string_value);
    }
  else 
    {
      strncpy (record->device_name, string_value, IPMI_DEVICE_NAME_MAX - 1);
      record->device_name[IPMI_DEVICE_NAME_MAX - 1] = '\0';
    }
  free (string_value);
  
  return 0;
}

static int 
_read_sdr_management_controller_device_locator_record (char *cache_record, 
						       sdr_management_controller_device_locator_record_t *record)
{
  int int_value = 0;
  char *string_value = NULL;
  
  ERR_EINVAL (cache_record && record);
  
  GET_INT_VALUE_BY_KEY (cache_record, 
			"entity_id", 
			&int_value);
  record->entity_id = int_value;
  GET_INT_VALUE_BY_KEY (cache_record, 
			"entity_instance", 
			&int_value);
  record->entity_instance = int_value;
  GET_STRING_VALUE_BY_KEY (cache_record, 
			   "device_name", 
			   &string_value);
  if (strlen (string_value) < IPMI_DEVICE_NAME_MAX)
    {
      strcpy (record->device_name, string_value);
    }
  else 
    {
      strncpy (record->device_name, string_value, IPMI_DEVICE_NAME_MAX - 1);
      record->device_name[IPMI_DEVICE_NAME_MAX - 1] = '\0';
    }
  free (string_value);
  
  return 0;
}

static int 
_get_oem_data_count (char *oem_data_string)
{
  int count = 0;
  char *dlist = NULL;
  
  if (oem_data_string == NULL)
    return (-1);
  
  dlist = strdupa (oem_data_string);
  while (oem_data_string)
    {
      char *str = NULL;
      
      str = get_token (&dlist);
      if (str == NULL)
	break;
      free (str);
      
      count++;
    }
  
  return count;
}

static int 
_get_oem_data (char *oem_data_string, unsigned int *oem_data, int count)
{
  int i = 0;
  char *dlist = NULL;
  
  if (oem_data_string == NULL || 
      oem_data == NULL)
    return (-1);
  
  dlist = strdupa (oem_data_string);
  for (i = 0; i < count; i++)
    {
      unsigned int value = 0;
      char *str = NULL;
      
      str = get_token (&dlist);
      if (str == NULL)
	break;
      
      str2uint (str, 16, &value);
      oem_data[i] = value;
      
      free (str);
    }
  
  return 0;
}

static int 
_read_sdr_oem_record (char *cache_record, 
		      sdr_oem_record_t *record)
{
  int int_value = 0;

  char *oem_data_string = NULL;
  
  ERR_EINVAL (cache_record && record);
  
  GET_INT_VALUE_BY_KEY (cache_record, 
			"manufacturer_id", 
			&int_value);
  record->manufacturer_id = int_value;
  GET_STRING_VALUE_BY_KEY (cache_record, 
			   "oem_data", 
			   &oem_data_string);
  record->oem_data_length = _get_oem_data_count (oem_data_string);
  _get_oem_data (oem_data_string, 
		 (unsigned int *) record->oem_data, 
		 record->oem_data_length);
  free (oem_data_string);
  
  return 0;
}

int 
read_sdr_record (char *cache_record, sdr_record_t *record)
{
  int int_value = 0;
  
  ERR_EINVAL (cache_record && record);
  
  GET_INT_VALUE_BY_KEY (cache_record, 
			"record_id", 
			&int_value);
  record->record_id = int_value;
  GET_INT_VALUE_BY_KEY (cache_record, 
			"record_type", 
			&int_value);
  record->record_type = int_value;
  
  switch (record->record_type)
    {
    case IPMI_SDR_FORMAT_FULL_RECORD:
      return _read_sdr_full_record (cache_record, 
				    &(record->record.sdr_full_record));
    case IPMI_SDR_FORMAT_COMPACT_RECORD:
      return _read_sdr_compact_record (cache_record, 
				       &(record->record.sdr_compact_record));
    case IPMI_SDR_FORMAT_EVENT_ONLY_RECORD:
      return _read_sdr_event_only_record (cache_record, 
					  &(record->record.sdr_event_only_record));
    case IPMI_SDR_FORMAT_ENTITY_ASSOCIATION_RECORD:
      return _read_sdr_entity_association_record (cache_record, 
						  &(record->record.sdr_entity_association_record));
    case IPMI_SDR_FORMAT_GENERIC_DEVICE_LOCATOR_RECORD:
      return _read_sdr_generic_device_locator_record (cache_record, 
						      &(record->record.sdr_generic_device_locator_record));
    case IPMI_SDR_FORMAT_FRU_DEVICE_LOCATOR_RECORD:
      return _read_sdr_logical_fru_device_locator_record (cache_record, 
							  &(record->record.sdr_logical_fru_device_locator_record));
    case IPMI_SDR_FORMAT_MANAGEMENT_CONTROLLER_DEVICE_LOCATOR_RECORD:
      return _read_sdr_management_controller_device_locator_record (cache_record, 
								    &(record->record.sdr_management_controller_device_locator_record));
    case IPMI_SDR_FORMAT_OEM_RECORD:
      return _read_sdr_oem_record (cache_record, 
				   &(record->record.sdr_oem_record));
    }
  
  return (-1);
}

