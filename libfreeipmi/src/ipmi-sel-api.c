/* 
   ipmi-sel-api.c - IPMI SEL commands API

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

#if HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#ifdef STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#if TIME_WITH_SYS_TIME
#include <sys/time.h>
#include <time.h>
#else /* !TIME_WITH_SYS_TIME */
#if HAVE_SYS_TIME_H
#include <sys/time.h>
#else /* !HAVE_SYS_TIME_H */
#ifdef __FreeBSD__
#include <sys/time.h>
#else  /* !__FreeBSD */
#include <time.h>
#endif /* !__FreeBSD */
#endif /* !HAVE_SYS_TIME_H */
#endif  /* !TIME_WITH_SYS_TIME */
#include <errno.h>

#include "freeipmi/ipmi-sel-api.h"
#include "freeipmi/fiid.h"
#include "freeipmi/ipmi-sel-cmds.h"
#include "freeipmi/ipmi-sel-record-types.h"
#include "freeipmi/ipmi-sensor-event-messages.h"
#include "freeipmi/ipmi-sensor-api.h"
#include "freeipmi/udm/ipmi-sel-cmds-udm.h"

#include "freeipmi-portability.h"
#include "err-wrappers.h"
#include "fiid-wrappers.h"

#include "ipmi-common.h"

int 
ipmi_sel_get_first_entry (ipmi_device_t *dev, 
			  sel_descriptor_t *seld, 
			  uint8_t *record_data,
                          uint32_t *record_data_len)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  int rv = -1;
  int32_t len;

  if (!dev || !seld || !record_data || !record_data_len)
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_CREATE (obj_cmd_rs, tmpl_get_sel_entry_rs);

  if (ipmi_cmd_get_sel_entry (dev, 
			      0,
			      IPMI_SEL_GET_RECORD_ID_FIRST_ENTRY, 
			      0,
			      IPMI_SEL_READ_ENTIRE_RECORD_BYTES_TO_READ,
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
  
  seld->first_record_id = IPMI_SEL_GET_RECORD_ID_FIRST_ENTRY;
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, (uint8_t *)"next_record_id", &val);
  seld->next_record_id = val;
  
  FIID_OBJ_GET_DATA_LEN_CLEANUP (len,
				 obj_cmd_rs, 
				 (uint8_t *)"record_data", 
				 record_data,
				 *record_data_len);
  *record_data_len = len;
  
  rv = 0;
 cleanup:
  FIID_OBJ_DESTROY_NO_RETURN(obj_cmd_rs);
  return (rv);
}

int 
ipmi_sel_get_next_entry (ipmi_device_t *dev, 
			 sel_descriptor_t *seld, 
			 uint8_t *record_data,
                         uint32_t *record_data_len)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  int rv = -1;
  int32_t len;

  if (!dev || !seld || !record_data || !record_data_len)
    {
      errno = EINVAL;
      return (-1);
    }

  if (seld->next_record_id == IPMI_SEL_GET_RECORD_ID_LAST_ENTRY)
    return (-1);

  FIID_OBJ_CREATE (obj_cmd_rs, tmpl_get_sel_entry_rs);

  if (ipmi_cmd_get_sel_entry (dev, 
			      0,
			      seld->next_record_id, 
			      0,
			      IPMI_SEL_READ_ENTIRE_RECORD_BYTES_TO_READ,
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
  
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, (uint8_t *)"next_record_id", &val);
  seld->next_record_id = val;
  
  FIID_OBJ_GET_DATA_LEN_CLEANUP (len,
				 obj_cmd_rs, 
				 (uint8_t *)"record_data", 
				 record_data,
				 *record_data_len);
  *record_data_len = len;
  
  rv = 0;
 cleanup:
  FIID_OBJ_DESTROY_NO_RETURN(obj_cmd_rs);
  return (rv);
}

static int 
get_sel_system_event_record (uint8_t *record_data, 
			     uint32_t record_data_len,
			     sel_record_t *sel_record)
{
  uint16_t record_id;
  uint32_t timestamp;
  uint8_t generator_id_type;
  uint8_t generator_id;
  uint8_t channel_number;
  uint8_t sensor_type;
  uint8_t sensor_number;
  uint8_t event_type_code;
  uint8_t event_dir;
  uint8_t offset_from_event_reading_type_code;
  uint8_t event_data2_flag;
  uint8_t event_data3_flag;
  uint8_t event_data2;
  uint8_t event_data3;
  
  uint64_t val;
  fiid_obj_t obj = NULL;
  int8_t rv = -1;

  if (!record_data || !sel_record)
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_CREATE (obj, tmpl_sel_system_event_record);

  FIID_OBJ_SET_ALL_CLEANUP(obj, record_data, record_data_len);

  FIID_OBJ_GET_CLEANUP (obj, (uint8_t *)"record_id", &val);
  record_id = val;
  
  FIID_OBJ_GET_CLEANUP (obj, (uint8_t *)"timestamp", &val);
  timestamp = val;
  
  FIID_OBJ_GET_CLEANUP (obj, (uint8_t *)"generator_id.id_type", &val);
  generator_id_type = val;
  
  FIID_OBJ_GET_CLEANUP (obj, (uint8_t *)"generator_id.id", &val);
  generator_id = val;
  
  FIID_OBJ_GET_CLEANUP (obj, (uint8_t *)"channel_number", &val);
  channel_number = val;
  
  FIID_OBJ_GET_CLEANUP (obj, (uint8_t *)"sensor_type", &val);
  sensor_type = val;
  
  FIID_OBJ_GET_CLEANUP (obj, (uint8_t *)"sensor_number", &val);
  sensor_number = val;
  
  FIID_OBJ_GET_CLEANUP (obj, (uint8_t *)"event_type_code", &val);
  event_type_code = val;
  
  FIID_OBJ_GET_CLEANUP (obj, (uint8_t *)"event_dir", &val);
  event_dir = val;
  
  FIID_OBJ_GET_CLEANUP (obj, (uint8_t *)"offset_from_event_reading_type_code", &val);
  offset_from_event_reading_type_code = val;
  
  FIID_OBJ_GET_CLEANUP (obj, (uint8_t *)"event_data2_flag", &val);
  event_data2_flag = val;
  
  FIID_OBJ_GET_CLEANUP (obj, (uint8_t *)"event_data3_flag", &val);
  event_data3_flag = val;
  
  FIID_OBJ_GET_CLEANUP (obj, (uint8_t *)"event_data2", &val);
  event_data2 = val;
  
  FIID_OBJ_GET_CLEANUP (obj, (uint8_t *)"event_data3", &val);
  event_data3 = val;
  
  sel_record->record_id = record_id;
  {
    char buffer[256];
    time_t time;
    
    time = timestamp;
    strftime (buffer, 256, "%d-%b-%Y %H:%M:%S", localtime (&time));
    
    sel_record->timestamp = strdup (buffer);
  }
  sel_record->sensor_info = NULL;
  asprintf (&(sel_record->sensor_info), 
	    "%s #%d", 
	    ipmi_get_sensor_group (sensor_type), sensor_number);
  {
    char buffer[1024];
    int rv;

    rv = ipmi_get_sensor_type_code_message(sensor_type,
					   offset_from_event_reading_type_code,
					   buffer,
					   1024);
    if (!rv)
      ERR_CLEANUP ((sel_record->event_message = strdup(buffer)));
    else
      sel_record->event_message = NULL;
  }
  switch (ipmi_sensor_classify (event_type_code))
    {
    case IPMI_SENSOR_CLASS_THRESHOLD:
      {
	sel_record->event_data2_message = NULL;
	switch (event_data2_flag)
	  {
	  case IPMI_SEL_TRIGGER_READING:
	    asprintf (&(sel_record->event_data2_message), 
		      "Trigger reading = %02Xh", 
		      event_data2);
	    break;
	  case IPMI_SEL_OEM_CODE:
	    asprintf (&(sel_record->event_data2_message), 
		      "OEM code = %02Xh", 
		      event_data2);
	    break;
	  case IPMI_SEL_SENSOR_SPECIFIC_EVENT_EXT_CODE:
	    {
	      char buffer[1024];
	      int rv;

	      rv = ipmi_get_event_data2_message (sensor_type, 
						 offset_from_event_reading_type_code, 
						 event_data2,
						 buffer,
						 1024);
	      if (!rv)
		ERR_CLEANUP ((sel_record->event_data2_message = strdup(buffer)));
	      else
		sel_record->event_data2_message = NULL;
	    }
	    break;
	  }
	
	sel_record->event_data3_message = NULL;
	switch (event_data3_flag)
	  {
	  case IPMI_SEL_TRIGGER_THRESHOLD_VALUE:
	    asprintf (&(sel_record->event_data3_message), 
		      "Trigger reading = %02Xh", 
		      event_data3);
	    break;
	  case IPMI_SEL_OEM_CODE:
	    asprintf (&(sel_record->event_data3_message), 
		      "OEM code = %02Xh", 
		      event_data3);
	    break;
	  case IPMI_SEL_SENSOR_SPECIFIC_EVENT_EXT_CODE:
	    {
	      char buffer[1024];
	      int rv;

	      rv = ipmi_get_event_data3_message (sensor_type, 
						 offset_from_event_reading_type_code, 
						 event_data2,
						 event_data3,
						 buffer,
						 1024);
	      if (!rv)
		ERR_CLEANUP ((sel_record->event_data3_message = strdup(buffer)));
	      else
		sel_record->event_data3_message = NULL;
	    }
	    break;
	  }
	
	break;
      }
    case IPMI_SENSOR_CLASS_GENERIC_DISCRETE:
    case IPMI_SENSOR_CLASS_SENSOR_SPECIFIC_DISCRETE:
      {
	sel_record->event_data2_message = NULL;
	switch (event_data2_flag)
	  {
	  case IPMI_SEL_OEM_CODE:
	    asprintf (&(sel_record->event_data2_message),
		      "OEM code = %02Xh",
		      event_data2);
	    break;
	  case IPMI_SEL_PREV_STATE_SEVERITY:
	  case IPMI_SEL_SENSOR_SPECIFIC_EVENT_EXT_CODE:
	    {
	      char buffer[1024];
	      int rv;

	      rv = ipmi_get_event_data2_message (sensor_type, 
						 offset_from_event_reading_type_code, 
						 event_data2,
						 buffer,
						 1024);
	      if (!rv)
		ERR_CLEANUP ((sel_record->event_data2_message = strdup(buffer)));
	      else
		sel_record->event_data2_message = NULL;
	    }
	  }
	
	sel_record->event_data3_message = NULL;
	switch (event_data3_flag)
	  {
	  case IPMI_SEL_OEM_CODE:
	    asprintf (&(sel_record->event_data3_message),
		      "OEM code = %02Xh",
		      event_data3);
	    break;
	  case IPMI_SEL_SENSOR_SPECIFIC_EVENT_EXT_CODE:
	    {
	      char buffer[1024];
	      int rv;

	      rv = ipmi_get_event_data3_message (sensor_type, 
						 offset_from_event_reading_type_code, 
						 event_data2,
						 event_data3,
						 buffer,
						 1024);
	      if (!rv)
		ERR_CLEANUP ((sel_record->event_data3_message = strdup(buffer)));
	      else
		sel_record->event_data3_message = NULL;
	    }
	    break;
	  }
	
	break;
      }
    case IPMI_SENSOR_CLASS_OEM:
      {
	asprintf (&(sel_record->event_data2_message), 
		  "Event Data2 = %02Xh", 
		  event_data2);
	asprintf (&(sel_record->event_data3_message), 
		  "Event Data3 = %02Xh", 
		  event_data3);
	break;
      }
    default:
      sel_record->event_data2_message = NULL;
      sel_record->event_data3_message = NULL;
    }
  
  rv = 0;
 cleanup:
  FIID_OBJ_DESTROY_NO_RETURN(obj);
  return (rv);
}

static int 
get_sel_timestamped_oem_record (uint8_t *record_data, 
				uint32_t record_data_len,
				sel_record_t *sel_record)
{
  uint16_t record_id;
  uint32_t timestamp;
  uint32_t manufacturer_id;
  uint64_t oem_defined;
  uint64_t val;
  fiid_obj_t obj = NULL;
  int8_t rv = -1;

  if (!record_data || !sel_record)
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_CREATE (obj, tmpl_sel_timestamped_oem_record);
  
  FIID_OBJ_SET_ALL_CLEANUP(obj, record_data, record_data_len);

  FIID_OBJ_GET_CLEANUP (obj, (uint8_t *)"record_id", &val);
  record_id = val;
  
  FIID_OBJ_GET_CLEANUP (obj, (uint8_t *)"timestamp", &val);
  timestamp = val;
  
  FIID_OBJ_GET_CLEANUP (obj, (uint8_t *)"manufacturer_id", &val);
  manufacturer_id = val;
  
  FIID_OBJ_GET_CLEANUP (obj, (uint8_t *)"oem_defined", &val);
  oem_defined = val;
  
  sel_record->record_id = record_id;
  {
    char buffer[256];
    time_t time;
    
    time = timestamp;
    strftime (buffer, 256, "%d-%b-%Y %H:%M:%S", localtime (&time));
    
    sel_record->timestamp = strdup (buffer);
  }
  asprintf (&(sel_record->sensor_info), 
	    "Manufacturer ID %02Xh", 
	    manufacturer_id);
  asprintf (&(sel_record->event_message), 
	    "OEM Defined = " FI_64 "Xh",
	    oem_defined);
  
  sel_record->event_data2_message = NULL;
  sel_record->event_data3_message = NULL;
  
  rv = 0;
 cleanup:
  FIID_OBJ_DESTROY_NO_RETURN(obj);
  return (rv);
}

static int 
get_sel_non_timestamped_oem_record (uint8_t *record_data, uint32_t record_data_len, sel_record_t *sel_record)
{
  uint16_t record_id;
  uint64_t val;
  fiid_obj_t obj = NULL;
  int8_t rv = -1;
  uint8_t buf[1024];
  int32_t len;
  char *str = NULL;
  char *tmp_str = NULL;
  int i;

  if (!record_data || !sel_record)
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_CREATE (obj, tmpl_sel_non_timestamped_oem_record);
  
  FIID_OBJ_SET_ALL_CLEANUP(obj, record_data, record_data_len);
  
  FIID_OBJ_GET_CLEANUP (obj, (uint8_t *)"record_id", &val);
  record_id = val;
  
  memset(buf, '\0', 1024);
  FIID_OBJ_GET_DATA_LEN_CLEANUP (len, 
				 obj,
				 (uint8_t *)"oem_defined",
				 buf,
				 1024);
  
  sel_record->record_id = record_id;
  sel_record->timestamp = NULL;
  sel_record->sensor_info = NULL;
  sel_record->event_message = NULL;
  
  for (i = 0; i < len; i++)
    {
      tmp_str = str;
      if (str)
	{
	  str = NULL;
	  asprintf (&str, "%s %02X", tmp_str, buf[i]);
	  free (tmp_str);
	}
      else
	asprintf (&str, "%02X", buf[i]);
    }
  
  if (str)
    {
      asprintf (&(sel_record->event_message), "OEM defined = %s", str);
      free (str);
    }
  
  sel_record->event_data2_message = NULL;
  sel_record->event_data3_message = NULL;
  
  rv = 0;
 cleanup:
  FIID_OBJ_DESTROY_NO_RETURN(obj);
  return (rv);
}

int 
get_sel_record (uint8_t *record_data, 
		uint32_t record_data_len,
		sel_record_t *sel_record)
{
  fiid_obj_t obj = NULL;
  uint8_t record_type;
  uint64_t val;
  int rv = -1;

  if (!record_data || !sel_record)
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_CREATE (obj, tmpl_sel_record_header);

  FIID_OBJ_SET_ALL_CLEANUP(obj, record_data, record_data_len);

  FIID_OBJ_GET_CLEANUP (obj, (uint8_t *)"record_type", &val);
  record_type = val;

  switch (ipmi_get_sel_record_type (record_type))
    {
    case IPMI_SEL_RECORD_TYPE_SYSTEM_EVENT_RECORD:
      rv = get_sel_system_event_record (record_data, record_data_len, sel_record);
      break;
    case IPMI_SEL_RECORD_TYPE_TIMESTAMPED_OEM_RECORD:
      rv = get_sel_timestamped_oem_record (record_data, record_data_len, sel_record);
      break;
    case IPMI_SEL_RECORD_TYPE_NON_TIMESTAMPED_OEM_RECORD:
      rv = get_sel_non_timestamped_oem_record (record_data, record_data_len, sel_record);
      break;
    }

 cleanup:
  FIID_OBJ_DESTROY_NO_RETURN(obj);
  return (rv);
}

int 
get_sel_info (ipmi_device_t *dev, sel_info_t *pinfo)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  int rv = -1;
  
  if (!dev || !pinfo)
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_CREATE (obj_cmd_rs, tmpl_get_sel_info_rs);

  if (ipmi_cmd_get_sel_info (dev, obj_cmd_rs) != 0)
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
  
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, (uint8_t *)"sel_version_major", &val);
  pinfo->version_major = val;
  
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, (uint8_t *)"sel_version_minor", &val);
  pinfo->version_minor = val;
  
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, (uint8_t *)"entries", &val);
  pinfo->entry_count = val;
  
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, (uint8_t *)"free_space", &val);
  pinfo->free_space = val;
  
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, (uint8_t *)"most_recent_addition_timestamp", &val);
  pinfo->last_add_time = val;
  
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, (uint8_t *)"most_recent_erase_timestamp", &val);
  pinfo->last_erase_time = val;
  
  pinfo->flags = 0;
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, (uint8_t *)"get_sel_allocation_info_command_supported", &val);
  if (val) pinfo->flags |= get_sel_alloc_info_cmd_support;
  
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, (uint8_t *)"reserve_sel_command_supported", &val);
  if (val) pinfo->flags |= reserve_sel_cmd_support;
  
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, (uint8_t *)"partial_add_sel_entry_command_supported", &val);
  if (val) pinfo->flags |= partial_add_sel_entry_cmd_support;
  
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, (uint8_t *)"delete_sel_command_supported", &val);
  if (val) pinfo->flags |= delete_sel_cmd_support;
  
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, (uint8_t *)"overflow_flag", &val);
  if (val) pinfo->flags |= overflow_flag;
  
  rv = 0;
 cleanup:
  FIID_OBJ_DESTROY_NO_RETURN(obj_cmd_rs);
  return (rv);
}

