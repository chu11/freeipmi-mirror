/*
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
#include <stdint.h>
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
#include <sys/resource.h>
#include <sys/stat.h>
#include <sys/param.h>
#include <arpa/inet.h>
#include <pwd.h>
#include <errno.h>
#include <assert.h>

#include "freeipmi/fiid.h"
#include "freeipmi/ipmi-sdr-repository-cmds.h"
#include "freeipmi/ipmi-sdr-record-format.h"
#include "freeipmi/udm/ipmi-sdr-repository-cmds-udm.h"
#include "freeipmi/udm/ipmi-sensor-cmds-udm.h"

#include "bit-ops.h"
#include "freeipmi-portability.h"

#include "common-utils.h"
#include "ipmi-sdr-cache.h"
#include "ipmi-sdr-cache-defs.h"

#define GET_INT_VALUE_BY_KEY(__ctx, __cache_record, __key, __i) \
do 							        \
  {							        \
    if (_get_int_value_by_key (__ctx,                           \
                               __cache_record, 		        \
			       __key, 			        \
			       __i) < 0)      	                \
      {							        \
        ctx->errnum = SDR_CACHE_CTX_ERR_CACHE_INVALID;          \
        goto cleanup;                                           \
      }							        \
  }                                                             \
 while (0)

#define GET_UINT_VALUE_BY_KEY(__ctx, __cache_record, __key, __i) \
do 							         \
  {							         \
    if (_get_uint_value_by_key (__ctx,                           \
                                __cache_record, 		 \
	 		        __key, 			         \
	 		        __i) < 0)      	                 \
      {							         \
        ctx->errnum = SDR_CACHE_CTX_ERR_CACHE_INVALID;           \
        goto cleanup;                                            \
      }							         \
  }                                                              \
 while (0)

#define GET_DOUBLE_VALUE_BY_KEY(__ctx, __cache_record, __key, __d) \
  do 							           \
{							           \
    if (_get_double_value_by_key (__ctx,                           \
                                  __cache_record,                  \
				  __key, 		           \
				  __d) < 0)      	           \
      {							           \
        ctx->errnum = SDR_CACHE_CTX_ERR_CACHE_INVALID;             \
        goto cleanup;                                              \
      }							           \
  }                                                                \
 while (0)



#define GET_STRING_VALUE_BY_KEY(__ctx,__cache_record, __key, __s)  \
do 							           \
  {							           \
    if (_get_string_value_by_key (__ctx,                           \
                                  __cache_record, 	           \
				  __key, 		           \
				  __s) < 0)		           \
      {							           \
        ctx->errnum = SDR_CACHE_CTX_ERR_CACHE_INVALID;             \
        goto cleanup;                                              \
      }							           \
  }                                                                \
 while (0)

static int 
_get_value_by_key (sdr_cache_ctx_t ctx,
                   char *cache_record, 
		   char *key, 
		   char **value)
{
  char *skey = NULL;
  char *start_pos = NULL;
  char *value_ptr = NULL;
  char *value_end_ptr = NULL;
  
  assert(ctx);
  assert(ctx->magic == SDR_CACHE_CTX_MAGIC);
  assert(cache_record);
  assert(key);
  assert(value);

  if (!(skey = alloca (strlen (key) + 3)))
    {
      ctx->errnum = SDR_CACHE_CTX_ERR_OUTMEM;
      return (-1);
    }
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
    {
      ctx->errnum = SDR_CACHE_CTX_ERR_CACHE_INVALID;
      return (-1);
    }
  
  value_ptr = start_pos + strlen (skey);
  value_end_ptr = strcasestr (value_ptr, "\n");
  if (value_end_ptr == NULL)
    {
      if (!(*value = strdup (value_ptr)))
        {
          ctx->errnum = SDR_CACHE_CTX_ERR_OUTMEM;
          return (-1);
        }
      return 0;
    }

  if (!(*value = strndup (value_ptr, (value_end_ptr - value_ptr))))
    {
      ctx->errnum = SDR_CACHE_CTX_ERR_OUTMEM;
      return (-1);
    }
  return 0;
}

static int 
_get_int_value_by_key (sdr_cache_ctx_t ctx,
                       char *cache_record, 
		       char *key, 
		       int *i)
{
  char *value_ptr = NULL;
  int rv = -1;
  
  assert(ctx);
  assert(ctx->magic == SDR_CACHE_CTX_MAGIC);
  assert(cache_record);
  assert(key);
  assert(i);

  if (_get_value_by_key (ctx,
                         cache_record, 
			 key, 
			 &value_ptr) < 0)
    goto cleanup;
  
  if (str2int (value_ptr, 0, i) < 0)
    {
      ctx->errnum = SDR_CACHE_CTX_ERR_CACHE_INVALID;
      goto cleanup;
    }
  
  rv = 0;
 cleanup:
  if (value_ptr)
    free (value_ptr);
  return rv;
}

static int 
_get_uint_value_by_key (sdr_cache_ctx_t ctx,
                        char *cache_record, 
                        char *key, 
                        unsigned int *i)
{
  char *value_ptr = NULL;
  int rv = -1;
  
  assert(ctx);
  assert(ctx->magic == SDR_CACHE_CTX_MAGIC);
  assert(cache_record);
  assert(key);
  assert(i);

  if (_get_value_by_key (ctx,
                         cache_record, 
			 key, 
			 &value_ptr) < 0)
    goto cleanup;
  
  if (str2uint (value_ptr, 0, i) < 0)
    {
      ctx->errnum = SDR_CACHE_CTX_ERR_CACHE_INVALID;
      goto cleanup;
    }
  
  rv = 0;
 cleanup:
  if (value_ptr)
    free (value_ptr);
  return rv;
}

static int 
_get_double_value_by_key (sdr_cache_ctx_t ctx,
                          char *cache_record, 
			  char *key, 
			  double *d)
{
  char *value_ptr = NULL;
  int rv = 0;
  
  assert(ctx);
  assert(ctx->magic == SDR_CACHE_CTX_MAGIC);
  assert(cache_record);
  assert(key);
  assert(d);

  if (_get_value_by_key (ctx,
                         cache_record, 
			 key, 
			 &value_ptr) < 0)
    goto cleanup;
  
  if (str2double (value_ptr, d) < 0)
    {
      ctx->errnum = SDR_CACHE_CTX_ERR_CACHE_INVALID;
      goto cleanup;
    }
  
  rv = 0;
 cleanup:
  if (value_ptr)
    free (value_ptr);
  return rv;
}

static int 
_get_string_value_by_key (sdr_cache_ctx_t ctx,
                          char *cache_record, 
			  char *key, 
			  char **s)
{
  char *value_ptr = NULL;
  int rv = -1;
  if (_get_value_by_key (ctx,
                         cache_record, 
			 key, 
			 &value_ptr) < 0)
    goto cleanup;
  
  *s = value_ptr;
  rv = 0;
 cleanup:
  return rv;
}

int 
sdr_cache_read_repository_info_timestamps (sdr_cache_ctx_t ctx,
                                           char *cache_record, 
                                           unsigned int *addition_timestamp,
                                           unsigned int *erase_timestamp)
{
  int rv = -1;

  assert(ctx);
  assert(ctx->magic == SDR_CACHE_CTX_MAGIC);
  assert(cache_record);
  assert(addition_timestamp);
  assert(erase_timestamp);
  
#if 0
  GET_INT_VALUE_BY_KEY (ctx,
                        cache_record, 
			"sdr_version_major", 
			&(sdr_info->sdr_version_major));
  GET_INT_VALUE_BY_KEY (ctx,
                        cache_record, 
			"sdr_version_minor", 
			&(sdr_info->sdr_version_minor));
  GET_INT_VALUE_BY_KEY (ctx,
                        cache_record, 
			"record_count", 
			&(sdr_info->record_count));
  GET_INT_VALUE_BY_KEY (ctx,
                        cache_record, 
			"free_space", 
			&(sdr_info->free_space));
#endif
  GET_UINT_VALUE_BY_KEY (ctx,
                         cache_record, 
                         "most_recent_addition_timestamp", 
                         addition_timestamp);
  GET_UINT_VALUE_BY_KEY (ctx,
                         cache_record, 
                         "most_recent_erase_timestamp", 
                         erase_timestamp);
#if 0
  GET_INT_VALUE_BY_KEY (ctx,
                        cache_record, 
			"get_sdr_repository_allocation_info_command_supported", 
			&(sdr_info->get_sdr_repository_allocation_info_command_supported));
  GET_INT_VALUE_BY_KEY (ctx,
                        cache_record, 
			"reserve_sdr_repository_command_supported", 
			&(sdr_info->reserve_sdr_repository_command_supported));
  GET_INT_VALUE_BY_KEY (ctx,
                        cache_record, 
			"partial_add_sdr_command_supported", 
			&(sdr_info->partial_add_sdr_command_supported));
  GET_INT_VALUE_BY_KEY (ctx,
                        cache_record, 
			"delete_sdr_command_supported", 
			&(sdr_info->delete_sdr_command_supported));
  GET_INT_VALUE_BY_KEY (ctx,
                        cache_record, 
			"modal_non_modal_sdr_repository_update_operation_supported", 
			&(sdr_info->modal_non_modal_sdr_repository_update_operation_supported));
  GET_INT_VALUE_BY_KEY (ctx,
                        cache_record, 
			"overflow_flag", 
			&(sdr_info->overflow_flag));
#endif

  rv = 0;
 cleanup:
  return rv;
}

static int 
_read_sdr_full_record (sdr_cache_ctx_t ctx,
                       char *cache_record, 
		       sdr_full_record_t *record)
{
  int int_value = 0;
  char *string_value = NULL;
  int rv = -1;
  
  assert(ctx);
  assert(ctx->magic == SDR_CACHE_CTX_MAGIC);
  assert(cache_record);
  assert(record);
  
  GET_INT_VALUE_BY_KEY (ctx,
                        cache_record, 
			"b", 
			&int_value);
  record->b = int_value;
  GET_INT_VALUE_BY_KEY (ctx,
                        cache_record, 
			"m", 
			&int_value);
  record->m = int_value;
  GET_INT_VALUE_BY_KEY (ctx,
                        cache_record, 
			"r_exponent", 
			&int_value);
  record->r_exponent = int_value;
  GET_INT_VALUE_BY_KEY (ctx,
                        cache_record, 
			"b_exponent", 
			&int_value);
  record->b_exponent = int_value;
  GET_INT_VALUE_BY_KEY (ctx,
                        cache_record, 
			"linear", 
			&int_value);
  record->linear = int_value;
  GET_INT_VALUE_BY_KEY (ctx,
                        cache_record, 
			"analog_data_format", 
			&int_value);
  record->analog_data_format = int_value;
  GET_INT_VALUE_BY_KEY (ctx,
                        cache_record, 
			"sensor_owner_id", 
			&int_value);
  record->sensor_owner_id = int_value;
  GET_INT_VALUE_BY_KEY (ctx,
                        cache_record, 
			"sensor_number", 
			&int_value);
  record->sensor_number = int_value;
  GET_INT_VALUE_BY_KEY (ctx,
                        cache_record, 
			"sensor_type", 
			&int_value);
  record->sensor_type = int_value;
  GET_INT_VALUE_BY_KEY (ctx,
                        cache_record, 
			"event_reading_type_code", 
			&int_value);
  record->event_reading_type_code = int_value;
  GET_INT_VALUE_BY_KEY (ctx,
                        cache_record, 
			"sensor_unit", 
			&int_value);
  record->sensor_unit = int_value;
  GET_DOUBLE_VALUE_BY_KEY (ctx,
                           cache_record, 
			   "nominal_reading", 
			   &(record->nominal_reading));
  GET_DOUBLE_VALUE_BY_KEY (ctx,
                           cache_record, 
			   "normal_minimum", 
			   &(record->normal_minimum));
  GET_DOUBLE_VALUE_BY_KEY (ctx,
                           cache_record, 
			   "normal_maximum", 
			   &(record->normal_maximum));
  GET_DOUBLE_VALUE_BY_KEY (ctx,
                           cache_record, 
			   "sensor_minimum_reading", 
			   &(record->sensor_minimum_reading));
  GET_DOUBLE_VALUE_BY_KEY (ctx,
                           cache_record, 
			   "sensor_maximum_reading", 
			   &(record->sensor_maximum_reading));
  GET_INT_VALUE_BY_KEY (ctx,
                        cache_record, 
			"negative_going_threshold_hysteresis", 
			&int_value);
  record->negative_going_threshold_hysteresis = int_value;
  GET_INT_VALUE_BY_KEY (ctx,
                        cache_record, 
			"positive_going_threshold_hysteresis", 
			&int_value);
  record->positive_going_threshold_hysteresis = int_value;
  GET_DOUBLE_VALUE_BY_KEY (ctx,
                           cache_record, 
			   "lower_non_recoverable_threshold", 
			   &(record->lower_non_recoverable_threshold));
  GET_DOUBLE_VALUE_BY_KEY (ctx,
                           cache_record, 
			   "upper_non_recoverable_threshold", 
			   &(record->upper_non_recoverable_threshold));
  GET_DOUBLE_VALUE_BY_KEY (ctx,
                           cache_record, 
			   "lower_critical_threshold", 
			   &(record->lower_critical_threshold));
  GET_DOUBLE_VALUE_BY_KEY (ctx,
                           cache_record, 
			   "upper_critical_threshold", 
			   &(record->upper_critical_threshold));
  GET_DOUBLE_VALUE_BY_KEY (ctx,
                           cache_record, 
			   "lower_non_critical_threshold", 
			   &(record->lower_non_critical_threshold));
  GET_DOUBLE_VALUE_BY_KEY (ctx,
                           cache_record, 
			   "upper_non_critical_threshold", 
			   &(record->upper_non_critical_threshold));
  string_value = NULL;
  GET_STRING_VALUE_BY_KEY (ctx,
                           cache_record, 
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
  
  GET_INT_VALUE_BY_KEY (ctx,
                        cache_record, 
			"readable_threshold_lower_critical_threshold", 
			&int_value);
  record->readable_threshold_lower_critical_threshold = int_value;
  GET_INT_VALUE_BY_KEY (ctx,
                        cache_record, 
			"readable_threshold_upper_critical_threshold", 
			&int_value);
  record->readable_threshold_upper_critical_threshold = int_value;
  GET_INT_VALUE_BY_KEY (ctx,
                        cache_record, 
			"readable_threshold_lower_non_critical_threshold", 
			&int_value);
  record->readable_threshold_lower_non_critical_threshold = int_value;
  GET_INT_VALUE_BY_KEY (ctx,
                        cache_record, 
			"readable_threshold_upper_non_critical_threshold", 
			&int_value);
  record->readable_threshold_upper_non_critical_threshold = int_value;
  GET_INT_VALUE_BY_KEY (ctx,
                        cache_record, 
			"readable_threshold_lower_non_recoverable_threshold", 
			&int_value);
  record->readable_threshold_lower_non_recoverable_threshold = int_value;
  GET_INT_VALUE_BY_KEY (ctx,
                        cache_record, 
			"readable_threshold_upper_non_recoverable_threshold", 
			&int_value);
  record->readable_threshold_upper_non_recoverable_threshold = int_value;
  
  rv = 0;
 cleanup:
  if (string_value)
    free (string_value);
  return rv;
}

static int 
_read_sdr_compact_record (sdr_cache_ctx_t ctx,
                          char *cache_record, 
			  sdr_compact_record_t *record)
{
  int int_value = 0;
  char *string_value = NULL;
  int rv = -1;
  
  assert(ctx);
  assert(ctx->magic == SDR_CACHE_CTX_MAGIC);
  assert(cache_record);
  assert(record);
  
  GET_INT_VALUE_BY_KEY (ctx,
                        cache_record, 
			"sensor_owner_id", 
			&int_value);
  record->sensor_owner_id = int_value;
  GET_INT_VALUE_BY_KEY (ctx,
                        cache_record, 
			"sensor_number", 
			&int_value);
  record->sensor_number = int_value;
  GET_INT_VALUE_BY_KEY (ctx,
                        cache_record, 
			"sensor_type", 
			&int_value);
  record->sensor_type = int_value;
  GET_INT_VALUE_BY_KEY (ctx,
                        cache_record, 
			"event_reading_type_code", 
			&int_value);
  record->event_reading_type_code = int_value;
  GET_INT_VALUE_BY_KEY (ctx,
                        cache_record, 
			"sensor_unit", 
			&int_value);
  record->sensor_unit = int_value;
  GET_INT_VALUE_BY_KEY (ctx,
                        cache_record, 
			"negative_going_threshold_hysteresis", 
			&int_value);
  record->negative_going_threshold_hysteresis = int_value;
  GET_INT_VALUE_BY_KEY (ctx,
                        cache_record, 
			"positive_going_threshold_hysteresis", 
			&int_value);
  record->positive_going_threshold_hysteresis = int_value;
  GET_STRING_VALUE_BY_KEY (ctx,
                           cache_record, 
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
  
  rv = 0;
 cleanup:
  if (string_value)
    free (string_value);
  return rv;
}

static int 
_read_sdr_event_only_record (sdr_cache_ctx_t ctx,
                             char *cache_record, 
			     sdr_event_only_record_t *record)
{
  int int_value = 0;
  char *string_value = NULL;
  int rv = -1;
  
  assert(ctx);
  assert(ctx->magic == SDR_CACHE_CTX_MAGIC);
  assert(cache_record);
  assert(record);
  
  GET_INT_VALUE_BY_KEY (ctx,
                        cache_record, 
			"sensor_owner_id", 
			&int_value);
  record->sensor_owner_id = int_value;
  GET_INT_VALUE_BY_KEY (ctx,
                        cache_record, 
			"sensor_number", 
			&int_value);
  record->sensor_number = int_value;
  GET_INT_VALUE_BY_KEY (ctx,
                        cache_record, 
			"sensor_type", 
			&int_value);
  record->sensor_type = int_value;
  GET_INT_VALUE_BY_KEY (ctx,
                        cache_record, 
			"event_reading_type_code", 
			&int_value);
  record->event_reading_type_code = int_value;
  GET_STRING_VALUE_BY_KEY (ctx,
                           cache_record, 
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
  
  rv = 0;
 cleanup:
  if (string_value)
    free (string_value);
  return rv;
}

static int 
_read_sdr_entity_association_record (sdr_cache_ctx_t ctx,
                                     char *cache_record, 
				     sdr_entity_association_record_t *record)
{
  int int_value = 0;
  int rv = -1;
  
  assert(ctx);
  assert(ctx->magic == SDR_CACHE_CTX_MAGIC);
  assert(cache_record);
  assert(record);
  
  GET_INT_VALUE_BY_KEY (ctx,
                        cache_record, 
			"container_entity_id", 
			&int_value);
  record->container_entity_id = int_value;
  GET_INT_VALUE_BY_KEY (ctx,
                        cache_record, 
			"container_entity_instance", 
			&int_value);
  record->container_entity_instance = int_value;
  
  rv = 0;
 cleanup:
  return rv;
}

static int 
_read_sdr_generic_device_locator_record (sdr_cache_ctx_t ctx,
                                         char *cache_record, 
					 sdr_generic_device_locator_record_t *record)
{
  int int_value = 0;
  char *string_value = NULL;
  int rv = -1;
  
  assert(ctx);
  assert(ctx->magic == SDR_CACHE_CTX_MAGIC);
  assert(cache_record);
  assert(record);
  
  GET_INT_VALUE_BY_KEY (ctx,
                        cache_record, 
			"direct_access_address", 
			&int_value);
  record->direct_access_address = int_value;
  GET_INT_VALUE_BY_KEY (ctx,
                        cache_record, 
			"channel_number", 
			&int_value);
  record->channel_number = int_value;
  GET_INT_VALUE_BY_KEY (ctx,
                        cache_record, 
			"device_slave_address", 
			&int_value);
  record->device_slave_address = int_value;
  GET_INT_VALUE_BY_KEY (ctx,
                        cache_record, 
			"private_bus_id", 
			&int_value);
  record->private_bus_id = int_value;
  GET_INT_VALUE_BY_KEY (ctx,
                        cache_record, 
			"lun_for_master_write_read_command", 
			&int_value);
  record->lun_for_master_write_read_command = int_value;
  GET_INT_VALUE_BY_KEY (ctx,
                        cache_record, 
			"address_span", 
			&int_value);
  record->address_span = int_value;
  GET_INT_VALUE_BY_KEY (ctx,
                        cache_record, 
			"device_type", 
			&int_value);
  record->device_type = int_value;
  GET_INT_VALUE_BY_KEY (ctx,
                        cache_record, 
			"device_type_modifier", 
			&int_value);
  record->device_type_modifier = int_value;
  GET_INT_VALUE_BY_KEY (ctx,
                        cache_record, 
			"entity_id", 
			&int_value);
  record->entity_id = int_value;
  GET_INT_VALUE_BY_KEY (ctx,
                        cache_record, 
			"entity_instance", 
			&int_value);
  record->entity_instance = int_value;
  GET_STRING_VALUE_BY_KEY (ctx,
                           cache_record, 
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
  
  rv = 0;
 cleanup:
  if (string_value)
    free (string_value);
  return rv;
}

static int 
_read_sdr_fru_device_locator_record (sdr_cache_ctx_t ctx,
                                     char *cache_record, 
				     sdr_fru_device_locator_record_t *record)
{
  int int_value = 0;
  char *string_value = NULL;
  int rv = -1;
  
  assert(ctx);
  assert(ctx->magic == SDR_CACHE_CTX_MAGIC);
  assert(cache_record);
  assert(record);
  
  GET_INT_VALUE_BY_KEY (ctx,
                        cache_record, 
                        "logical_fru_device_device_slave_address",
                        &int_value);
  record->logical_fru_device_device_slave_address = int_value;
  
  GET_INT_VALUE_BY_KEY (ctx,
                        cache_record,
                        "logical_physical_fru_device",
                        &int_value);
  record->logical_physical_fru_device = int_value;
  
  GET_INT_VALUE_BY_KEY (ctx,
                        cache_record,
			"device_type", 
			&int_value);
  record->device_type = int_value;
  GET_INT_VALUE_BY_KEY (ctx,
                        cache_record, 
			"device_type_modifier", 
			&int_value);
  record->device_type_modifier = int_value;
  GET_INT_VALUE_BY_KEY (ctx,
                        cache_record, 
			"fru_entity_id", 
			&int_value);
  record->fru_entity_id = int_value;
  GET_INT_VALUE_BY_KEY (ctx,
                        cache_record, 
			"fru_entity_instance", 
			&int_value);
  record->fru_entity_instance = int_value;
  GET_STRING_VALUE_BY_KEY (ctx,
                           cache_record, 
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
  
  rv = 0;
 cleanup:
  if (string_value)
    free (string_value);
  return rv;
}

static int 
_read_sdr_management_controller_device_locator_record (sdr_cache_ctx_t ctx,
                                                       char *cache_record, 
						       sdr_management_controller_device_locator_record_t *record)
{
  int int_value = 0;
  char *string_value = NULL;
  int rv = -1;
  
  assert(ctx);
  assert(ctx->magic == SDR_CACHE_CTX_MAGIC);
  assert(cache_record);
  assert(record);
  
  GET_INT_VALUE_BY_KEY (ctx,
                        cache_record, 
			"entity_id", 
			&int_value);
  record->entity_id = int_value;
  GET_INT_VALUE_BY_KEY (ctx,
                        cache_record, 
			"entity_instance", 
			&int_value);
  record->entity_instance = int_value;
  GET_STRING_VALUE_BY_KEY (ctx,
                           cache_record, 
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
 
  rv = 0;
 cleanup:
  if (string_value)
    free (string_value);
  return rv;
}

static int 
_get_oem_data_count (sdr_cache_ctx_t ctx,
                     char *oem_data_string,
                     uint8_t *oem_data_length)
{
  int count = 0;
  char *dlist = NULL;
  int rv = -1;

  assert(ctx);
  assert(ctx->magic == SDR_CACHE_CTX_MAGIC);
  assert(oem_data_string);
  assert(oem_data_length);

  if (!(dlist = strdupa (oem_data_string)))
    {
      ctx->errnum = SDR_CACHE_CTX_ERR_OUTMEM;
      goto cleanup;
    }

  while (oem_data_string)
    {
      char *str = NULL;
      
      if (!(str = get_token (&dlist)))
	break;
      free (str);
      
      count++;
    }

  /* cache has too much data, just truncate it */
  if (count > IPMI_OEM_DATA_MAX)
    count = IPMI_OEM_DATA_MAX;

  *oem_data_length = count;
  rv = 0;
 cleanup:  
  return rv;
}

static int 
_get_oem_data (sdr_cache_ctx_t ctx,
               char *oem_data_string, 
               uint8_t *oem_data, 
               int count)
{
  int i = 0;
  char *dlist = NULL;
  
  assert(ctx);
  assert(ctx->magic == SDR_CACHE_CTX_MAGIC);
  assert(oem_data_string);
  assert(oem_data);

  if (!(dlist = strdupa (oem_data_string)))
    {
      ctx->errnum = SDR_CACHE_CTX_ERR_OUTMEM;
      return -1;
    }

  for (i = 0; i < count; i++)
    {
      unsigned int value = 0;
      char *str = NULL;
      
      if (!(str = get_token (&dlist)))
	break;
      
      str2uint (str, 16, &value);
      oem_data[i] = (uint8_t)value;

      free (str);

    }
  
  return 0;
}

static int 
_read_sdr_oem_record (sdr_cache_ctx_t ctx,
                      char *cache_record, 
		      sdr_oem_record_t *record)
{
  char *oem_data_string = NULL;
  int int_value = 0;
  int rv = -1;
  
  assert(ctx);
  assert(ctx->magic == SDR_CACHE_CTX_MAGIC);
  assert(cache_record);
  assert(record);
  
  GET_INT_VALUE_BY_KEY (ctx,
                        cache_record, 
			"manufacturer_id", 
			&int_value);
  record->manufacturer_id = int_value;

  GET_STRING_VALUE_BY_KEY (ctx,
                           cache_record, 
			   "oem_data", 
			   &oem_data_string);
  
  if (_get_oem_data_count (ctx,
                           oem_data_string,
                           &(record->oem_data_length)) < 0)
     goto cleanup;

  if (_get_oem_data (ctx,
                     oem_data_string, 
                     record->oem_data, 
                     record->oem_data_length) < 0)
    goto cleanup;

  rv = 0;
 cleanup:
  if (oem_data_string)
    free (oem_data_string);
  return rv;
}

int 
sdr_cache_read_record (sdr_cache_ctx_t ctx,
                       char *cache_record, 
                       sdr_record_t *record)
{
  int int_value = 0;
  int rv = -1;

  assert(ctx);
  assert(ctx->magic == SDR_CACHE_CTX_MAGIC);
  assert(cache_record);
  assert(record);
  
  GET_INT_VALUE_BY_KEY (ctx,
                        cache_record, 
			"record_id", 
			&int_value);
  record->record_id = int_value;
  GET_INT_VALUE_BY_KEY (ctx,
                        cache_record, 
			"record_type", 
			&int_value);
  record->record_type = int_value;
  
  switch (record->record_type)
    {
    case IPMI_SDR_FORMAT_FULL_RECORD:
      return _read_sdr_full_record (ctx,
                                    cache_record, 
				    &(record->record.sdr_full_record));
    case IPMI_SDR_FORMAT_COMPACT_RECORD:
      return _read_sdr_compact_record (ctx,
                                       cache_record, 
				       &(record->record.sdr_compact_record));
    case IPMI_SDR_FORMAT_EVENT_ONLY_RECORD:
      return _read_sdr_event_only_record (ctx,
                                          cache_record, 
					  &(record->record.sdr_event_only_record));
    case IPMI_SDR_FORMAT_ENTITY_ASSOCIATION_RECORD:
      return _read_sdr_entity_association_record (ctx,
                                                  cache_record, 
						  &(record->record.sdr_entity_association_record));
    case IPMI_SDR_FORMAT_GENERIC_DEVICE_LOCATOR_RECORD:
      return _read_sdr_generic_device_locator_record (ctx,
                                                      cache_record, 
						      &(record->record.sdr_generic_device_locator_record));
    case IPMI_SDR_FORMAT_FRU_DEVICE_LOCATOR_RECORD:
      return _read_sdr_fru_device_locator_record (ctx,
                                                  cache_record, 
						  &(record->record.sdr_fru_device_locator_record));
    case IPMI_SDR_FORMAT_MANAGEMENT_CONTROLLER_DEVICE_LOCATOR_RECORD:
      return _read_sdr_management_controller_device_locator_record (ctx,
                                                                    cache_record, 
								    &(record->record.sdr_management_controller_device_locator_record));
    case IPMI_SDR_FORMAT_OEM_RECORD:
      return _read_sdr_oem_record (ctx,
                                   cache_record, 
				   &(record->record.sdr_oem_record));
    }
  
  rv = 0;
 cleanup:
  return rv;
}

   
