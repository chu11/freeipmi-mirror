/*
ipmi-sdr-api.c: SDR cache creation and management apis.
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

#define SDR_CACHE_DIR                "sdr-cache"
#define SDR_CACHE_FILENAME_PREFIX    "sdr-cache"
#define FREEIPMI_CONFIG_DIRECTORY_MODE    0700

#ifndef MAXHOSTNAMELEN
#define MAXHOSTNAMELEN 64
#endif /* MAXHOSTNAMELEN */

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
#include "ipmi-sdr-cache-writes.h"
#include "ipmi-sdr-cache-reads.h"

static int 
_fread_record (FILE *fp, char **cache_record)
{
  char *record;
  
  record = NULL;
  
  while (1)
    {
      char *line = NULL;
      size_t n = 0;
      
      if (getline (&line, &n, fp) == -1)
	{
	  return (-1);
	}
      else 
	{
	  char *lineptr = line;
	  char *tmp_lineptr = line;
	  line = strdupa (stripwhite (tmp_lineptr));
	  free (lineptr);
	}
      if (strlen (line) == 0)
	{
	  *cache_record = NULL;
	  if (record)
	    {
	      *cache_record = strdup (record);
	    }
	  return 0;
	}
      
      {
	char *l_record = NULL;
	int len = 0;
	
	if (record)
	  {
	    len = strlen (record) + strlen (line) + 2;
	    l_record = alloca (len);
	    strcpy (l_record, record);
	    strcat (l_record, line);
	    strcat (l_record, "\n");
	    record = l_record;
	  }
	else 
	  {
	    len = strlen (line) + 2;
	    l_record = alloca (len);
	    strcpy (l_record, line);
	    strcat (l_record, "\n");
	    record = l_record;
	  }
      }
    }
  
  return 0;
}

static int 
_get_record_count (FILE *fp, int *count)
{
  char *cache_record = NULL;
  int rcount = 0;
  
  while (1)
    {
      cache_record = NULL;
      if (_fread_record (fp, &cache_record) == -1)
	{
	  *count = rcount;
	  return 0;
	}
      rcount++;
      free (cache_record);
    }
  
  return (-1);
}

static char *
_get_home_directory ()
{
  uid_t user_id;
  struct passwd *user_passwd = alloca (sizeof (*user_passwd));
  char *home_dir = NULL;
  long int buf_len = sysconf (_SC_GETPW_R_SIZE_MAX);
  char *buf = alloca (buf_len);

  user_id = getuid ();
  if (getpwuid_r (user_id, user_passwd, buf, buf_len, &user_passwd) != 0)
    return NULL;

  if (user_passwd->pw_dir)
    {
      if (access (user_passwd->pw_dir, R_OK|W_OK|X_OK) == 0) {
        home_dir = strdup (user_passwd->pw_dir);
        return home_dir;
      }
    }

  asprintf (&home_dir,
            "/tmp/.%s-%s",
	    PACKAGE_NAME, 
	    user_passwd->pw_name);
  if (mkdir (home_dir, FREEIPMI_CONFIG_DIRECTORY_MODE) == 0)
    return home_dir;
  
  free (home_dir);
  return NULL;
}

static char *
_get_freeipmi_config_directory ()
{
  char *home_dir = NULL;
  char *config_dir = NULL;
  
  if ((home_dir = _get_home_directory ()))
    {
      asprintf (&config_dir, "%s/.%s", home_dir, PACKAGE_NAME);
      free (home_dir);
      return (config_dir);
    }
  
  return NULL;
}

static char *
_get_sdr_cache_directory ()
{
  char *config_dir = NULL;
  char *cache_dir = NULL;
  
  if ((config_dir = _get_freeipmi_config_directory ()))
    {
      asprintf (&cache_dir, "%s/%s", config_dir, SDR_CACHE_DIR);
      free (config_dir);
      return (cache_dir);
    }
  
  return NULL;
}

static char *
_get_ipmi_host_ip_address (char *host)
{
  if (host != NULL) /* OUT-OF-BAND */
    {
      struct hostent *hostinfo = NULL;
      struct in_addr *in_addr = NULL;
      
      hostinfo = gethostbyname (host);
      if (hostinfo == NULL)
	return NULL;
      
      in_addr = (struct in_addr *) hostinfo->h_addr_list[0];
      
      return strdup (inet_ntoa (*in_addr));
    }
  else /* IN-BAND */
    {
      return strdup ("127.0.0.1");
    }
}

int 
get_sdr_repository_info (ipmi_device_t dev, sdr_repository_info_t *sdr_info)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  int rv = -1;
  
  ERR_EINVAL (dev && sdr_info);
  
  FIID_OBJ_CREATE (obj_cmd_rs, tmpl_cmd_get_sdr_repository_info_rs);
  
  if (ipmi_cmd_get_sdr_repository_info (dev, obj_cmd_rs) != 0)
    goto cleanup;
  
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "sdr_version_major", &val);
  sdr_info->sdr_version_major = val;
  
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "sdr_version_minor", &val);
  sdr_info->sdr_version_minor = val;
  
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "record_count", &val);
  sdr_info->record_count = val;
  
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "free_space", &val);
  sdr_info->free_space = val;
  
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "most_recent_addition_timestamp", &val);
  sdr_info->most_recent_addition_timestamp = val;
  
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "most_recent_erase_timestamp", &val);
  sdr_info->most_recent_erase_timestamp = val;
  
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "get_sdr_repository_allocation_info_command_supported", &val);
  sdr_info->get_sdr_repository_allocation_info_command_supported = val;
  
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "reserve_sdr_repository_command_supported", &val);
  sdr_info->reserve_sdr_repository_command_supported = val;
  
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "partial_add_sdr_command_supported", &val);
  sdr_info->partial_add_sdr_command_supported = val;
  
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "delete_sdr_command_supported", &val);
  sdr_info->delete_sdr_command_supported = val;
  
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "modal_non_modal_sdr_repository_update_operation_supported", &val);
  sdr_info->modal_non_modal_sdr_repository_update_operation_supported = val;
  
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "overflow_flag", &val);
  sdr_info->overflow_flag = val;
  
  rv = 0;
 cleanup:
  FIID_OBJ_DESTROY_NO_RETURN(obj_cmd_rs);
  return (rv);
}

char *
get_sdr_cache_filename (char *host, char *user_cache_dir)
{
  char *ipmi_host_ip_address = NULL;
  char *cache_dir = NULL;
  char *cache_filename = NULL;
  char hostbuf[MAXHOSTNAMELEN];
  char *ptr;
 
  memset(hostbuf, '\0', MAXHOSTNAMELEN);
  if (gethostname(hostbuf, MAXHOSTNAMELEN) < 0)
    {
      snprintf(hostbuf, MAXHOSTNAMELEN, "localhost");
    }
  hostbuf[MAXHOSTNAMELEN - 1] = '\0';

  /* shorten hostname if necessary */
  if ((ptr = strchr(hostbuf, '.')))
    *ptr = '\0';
  
  if ((ipmi_host_ip_address = _get_ipmi_host_ip_address (host)))
    {
      char *sdr_cache_dir = NULL;
      
      if (user_cache_dir != NULL)
	{
	  sdr_cache_dir = user_cache_dir;
	}
      else
	{
	  cache_dir = _get_sdr_cache_directory ();
	  sdr_cache_dir = cache_dir;
	}
      
      if (sdr_cache_dir != NULL)
	{
	  asprintf (&cache_filename, 
		    "%s/%s-%s.%s", 
		    sdr_cache_dir, 
		    SDR_CACHE_FILENAME_PREFIX, 
                    hostbuf,
		    ipmi_host_ip_address);
	  if (cache_dir)
	    free (cache_dir);
	  free (ipmi_host_ip_address);
	  return cache_filename;
	}
    }
  
  free (cache_dir);
  free (ipmi_host_ip_address);
  
  return NULL;
}

int 
setup_sdr_cache_directory ()
{
  char *dir = NULL;
  int errnum = 0;
  int retval = -1;
  
  if ((dir = _get_freeipmi_config_directory ()))
    {
      errno = 0;
      retval = mkdir (dir, FREEIPMI_CONFIG_DIRECTORY_MODE);
      errnum = errno;
      free (dir);
      if (!(retval == 0 || errnum == EEXIST))
	return (-1);
    }
  else 
    {
      return (-1);
    }
  
  if ((dir = _get_sdr_cache_directory ()))
    {
      errno = 0;
      retval = mkdir (dir, FREEIPMI_CONFIG_DIRECTORY_MODE);
      errnum = errno;
      free (dir);
      if (!(retval == 0 || errnum == EEXIST))
	return (-1);
    }
  else 
    {
      return (-1);
    }
  
  return 0;
}

int 
flush_sdr_cache_file (char *host, char *user_cache_dir)
{
  char *cache_file = NULL;
  int rv = -1;
  
  if ((cache_file = get_sdr_cache_filename (host, user_cache_dir)))
    {
      rv = unlink (cache_file);
      free (cache_file);
    }
  
  return rv;
}

static int
_get_decode_parameters (fiid_obj_t obj,
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
  
  ERR_EINVAL (fiid_obj_valid(obj)
	      && analog_data_format
	      && r_exponent
	      && b_exponent
	      && linear
	      && b
	      && m);

  FIID_OBJ_TEMPLATE_COMPARE (obj, tmpl_sdr_full_sensor_record);

  FIID_OBJ_GET (obj, "r_exponent", &val);
  *r_exponent = (char) val;
  if (*r_exponent & 0x08)
    *r_exponent |= 0xF0;
  
  FIID_OBJ_GET_CLEANUP (obj, "b_exponent", &val);
  *b_exponent = (char) val;
  if (*b_exponent & 0x08)
    *b_exponent |= 0xF0;
  
  FIID_OBJ_GET (obj, "m_ls", &m_ls);
  FIID_OBJ_GET (obj, "m_ms", &m_ms);
  ERR_CLEANUP (!(bits_merge (m_ls, 8, 10, m_ms, &val) < 0));
  *m = (short) val;
  if (*m & 0x200)
    *m |= 0xFE00;
  
  FIID_OBJ_GET (obj, "b_ls", &b_ls);
  FIID_OBJ_GET (obj, "b_ms", &b_ms);
  ERR_CLEANUP (!(bits_merge (b_ls, 8, 10, b_ms, &val) < 0));
  *b = (short) val;
  if (*b & 0x200)
    *b |= 0xFE00;
  
  FIID_OBJ_GET (obj, "sensor_unit1.analog_data_format", &val);
  *analog_data_format = (uint8_t) val;

  FIID_OBJ_GET (obj, "linearization", &val);
  *linear = (char)val;

  return (0);

 cleanup:
  FIID_OBJ_DESTROY_NO_RETURN(obj);
  return (-1);
}

static void 
_get_sdr_full_record (uint8_t *sdr_record_data, 
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
 
  ERR_CLEANUP (!(_get_decode_parameters (obj,
                                         &analog_data_format,
                                         &r_exponent,
                                         &b_exponent, 
                                         (char *)&linear, 
                                         &b, 
                                         &m) < 0));
  sdr_full_record->b = b;
  sdr_full_record->m = m;
  sdr_full_record->r_exponent = r_exponent;
  sdr_full_record->b_exponent = b_exponent;
  sdr_full_record->linear = linear;
  sdr_full_record->analog_data_format = analog_data_format;
  
  FIID_OBJ_GET_CLEANUP (obj, "sensor_owner_id", &val);
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

static void 
_get_sdr_compact_record (uint8_t *sdr_record_data, 
                         uint32_t sdr_record_data_len,
                         sdr_compact_record_t *sdr_compact_record)
{
  uint64_t val;
  
  fiid_obj_t obj = NULL;

  ERR_EINVAL_VOID_RETURN(sdr_record_data && sdr_compact_record);

  FIID_OBJ_CREATE_CLEANUP(obj, tmpl_sdr_compact_sensor_record);

  FIID_OBJ_SET_ALL_CLEANUP (obj, sdr_record_data, sdr_record_data_len);

  FIID_OBJ_GET_CLEANUP (obj, "sensor_owner_id", &val);
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

static void 
_get_sdr_event_only_record (uint8_t *sdr_record_data, 
                            uint32_t sdr_record_data_len,
                            sdr_event_only_record_t *sdr_event_only_record)
{
  uint64_t val;  
  fiid_obj_t obj = NULL;

  ERR_EINVAL_VOID_RETURN(sdr_record_data && sdr_event_only_record);

  FIID_OBJ_CREATE_CLEANUP(obj, tmpl_sdr_event_only_record);

  FIID_OBJ_SET_ALL_CLEANUP (obj, sdr_record_data, sdr_record_data_len);
  
  FIID_OBJ_GET_CLEANUP (obj, "sensor_owner_id", &val);
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
			     (uint8_t *)sdr_event_only_record->sensor_name,
			     17);

 cleanup:
  FIID_OBJ_DESTROY_NO_RETURN(obj);
  return;
}

static void 
_get_sdr_entity_association_record (uint8_t *sdr_record_data, 
                                    uint32_t sdr_record_data_len,
                                    sdr_entity_association_record_t *sdr_entity_association_record)
{
  uint64_t val;
  fiid_obj_t obj = NULL;

  ERR_EINVAL_VOID_RETURN(sdr_record_data && sdr_entity_association_record);

  FIID_OBJ_CREATE_CLEANUP(obj, tmpl_sdr_entity_association_record);

  FIID_OBJ_SET_ALL_CLEANUP (obj, sdr_record_data, sdr_record_data_len);

  FIID_OBJ_GET_CLEANUP (obj, "container_entity_id", &val);
  sdr_entity_association_record->container_entity_id = val;
  
  FIID_OBJ_GET_CLEANUP (obj, "container_entity_instance", &val);
  sdr_entity_association_record->container_entity_instance = val;
  
 cleanup:
  FIID_OBJ_DESTROY_NO_RETURN(obj);
  return;
}

static void 
_get_sdr_generic_device_locator_record (uint8_t *sdr_record_data, 
                                        uint32_t sdr_record_data_len,
                                        sdr_generic_device_locator_record_t *sdr_generic_device_locator_record)
{
  uint64_t val;
  fiid_obj_t obj = NULL;

  ERR_EINVAL_VOID_RETURN(sdr_record_data && sdr_generic_device_locator_record);

  FIID_OBJ_CREATE_CLEANUP(obj, tmpl_sdr_generic_device_locator_record);

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

static void 
_get_sdr_logical_fru_device_locator_record (uint8_t *sdr_record_data, 
                                            uint32_t sdr_record_data_len,
                                            sdr_logical_fru_device_locator_record_t *sdr_logical_fru_device_locator_record)
{
  uint64_t val;
  fiid_obj_t obj = NULL;

  ERR_EINVAL_VOID_RETURN(sdr_record_data && sdr_logical_fru_device_locator_record);

  FIID_OBJ_CREATE_CLEANUP(obj, tmpl_sdr_logical_fru_device_locator_record);

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

static void 
_get_sdr_management_controller_device_locator_record (uint8_t *sdr_record_data, 
                                                      uint32_t sdr_record_data_len,
                                                      sdr_management_controller_device_locator_record_t *sdr_management_controller_device_locator_record)
{
  uint64_t val;
  fiid_obj_t obj = NULL;

  ERR_EINVAL_VOID_RETURN(sdr_record_data && sdr_management_controller_device_locator_record);

  FIID_OBJ_CREATE_CLEANUP(obj, tmpl_sdr_management_controller_device_locator_record);

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

static void 
_get_sdr_oem_record (uint8_t *sdr_record_data, 
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

int
_get_sdr_sensor_record (ipmi_device_t dev, 
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

  fiid_obj_t record_header = NULL;
  fiid_obj_t local_obj_cmd_rs = NULL;
  int32_t record_header_len;
  uint8_t *record_header_buf = NULL;

  ERR_EINVAL (dev 
	      && fiid_obj_valid(obj_cmd_rs)
	      && sensor_record
	      && sensor_record_len);

  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_cmd_get_sdr_rs);

  FIID_OBJ_CREATE_CLEANUP(record_header, tmpl_sdr_record_header);
    
  FIID_TEMPLATE_LEN_BYTES_CLEANUP (record_header_len, 
				   tmpl_sdr_record_header);

  ERR_CLEANUP ((record_header_buf = alloca (record_header_len)));
  memset (record_header_buf, 0, record_header_len);

  ERR_CLEANUP (!(ipmi_cmd_get_sdr (dev, 
				   0,
				   record_id, 
				   0, 
				   record_header_len, 
				   obj_cmd_rs)));
  
  FIID_OBJ_GET_DATA_CLEANUP (obj_cmd_rs,
			     "record_data",
			     record_header_buf,
			     record_header_len);
  
  FIID_OBJ_SET_ALL_CLEANUP (record_header, 
			    record_header_buf, 
			    record_header_len);

  FIID_OBJ_GET_CLEANUP (record_header, "record_length", &val);
  record_length = val;
  record_length += record_header_len;
  
  /* achu: where does the 16 come from? */
  if (record_length > 16)
    {
      FIID_OBJ_CREATE_CLEANUP(local_obj_cmd_rs, tmpl_cmd_reserve_sdr_repository_rs);
      
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
  FIID_OBJ_DESTROY_NO_RETURN(record_header);
  FIID_OBJ_DESTROY_NO_RETURN(local_obj_cmd_rs);
  return (rv);
}

static int
_get_sdr_record (ipmi_device_t dev, 
                 uint16_t record_id, 
                 uint16_t *next_record_id, 
                 sdr_record_t *sdr_record)
{
  fiid_obj_t obj_cmd_rs = NULL;
  fiid_obj_t obj_sdr_record = NULL;
  uint8_t sensor_record[1024];
  uint32_t sensor_record_len;
  uint64_t val = 0;
  int rv = -1;

  ERR_EINVAL (dev
	      && next_record_id
	      && sdr_record);

  FIID_OBJ_CREATE_CLEANUP (obj_cmd_rs, tmpl_cmd_get_sdr_rs);
  FIID_OBJ_CREATE_CLEANUP (obj_sdr_record, tmpl_sdr_record_header);

  sensor_record_len = 1024;
  if (_get_sdr_sensor_record (dev, 
                              record_id, 
                              obj_cmd_rs, 
                              sensor_record,
                              &sensor_record_len) < 0)
    goto cleanup;
  
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
      _get_sdr_full_record (sensor_record,
                            sensor_record_len,
                            &(sdr_record->record.sdr_full_record));
  

      /* 0x01 == THRESHOLD sensor class */
      if (sdr_record->record.sdr_full_record.event_reading_type_code != 0x01)
        break;
      
      FIID_OBJ_CREATE_CLEANUP(obj_cmd_rs, tmpl_cmd_get_sensor_thresholds_rs);

      if (ipmi_cmd_get_sensor_thresholds (dev, 
					  sdr_record->record.sdr_full_record.sensor_number, 
					  obj_cmd_rs) != 0)
        /* This is ok - no biggie if we can't get thresholds*/
        break;
      
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
      _get_sdr_compact_record (sensor_record,
                               sensor_record_len,
                               &(sdr_record->record.sdr_compact_record));
      break;
    case IPMI_SDR_FORMAT_EVENT_ONLY_RECORD:
      _get_sdr_event_only_record (sensor_record,
                                  sensor_record_len,
                                  &(sdr_record->record.sdr_event_only_record));
      break;
    case IPMI_SDR_FORMAT_ENTITY_ASSOCIATION_RECORD:
      _get_sdr_entity_association_record (sensor_record,
                                          sensor_record_len,
                                          &(sdr_record->record.sdr_entity_association_record));
      break;
    case IPMI_SDR_FORMAT_GENERIC_DEVICE_LOCATOR_RECORD:
      _get_sdr_generic_device_locator_record (sensor_record,
                                              sensor_record_len,
                                              &(sdr_record->record.sdr_generic_device_locator_record));
      break;
    case IPMI_SDR_FORMAT_FRU_DEVICE_LOCATOR_RECORD:
      _get_sdr_logical_fru_device_locator_record (sensor_record,
                                                  sensor_record_len,
                                                  &(sdr_record->record.sdr_logical_fru_device_locator_record));
      break;
    case IPMI_SDR_FORMAT_MANAGEMENT_CONTROLLER_DEVICE_LOCATOR_RECORD:
      _get_sdr_management_controller_device_locator_record (sensor_record,
                                                            sensor_record_len,
                                                            &(sdr_record->record.sdr_management_controller_device_locator_record));
      break;
    case IPMI_SDR_FORMAT_OEM_RECORD:
      _get_sdr_oem_record (sensor_record,
                           sensor_record_len,
                           &(sdr_record->record.sdr_oem_record));
      break;
    case IPMI_SDR_FORMAT_DEVICE_RELATIVE_ENTITY_ASSOCIATION_RECORD:
    case IPMI_SDR_FORMAT_MANAGEMENT_CONTROLLER_CONFIRMATION_RECORD:
    case IPMI_SDR_FORMAT_BMC_MESAAGE_CHANNEL_INFO_RECORD:
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

int 
create_sdr_cache (ipmi_device_t dev, FILE *fp, int verbose)
{
  sdr_repository_info_t sdr_info;

  ERR_EINVAL (dev && fp);
              
  memset (&sdr_info, 0, sizeof (sdr_repository_info_t));
  if (verbose)
    {
      fprintf (stderr, "Fetching SDR repository information... ");
    }
  if (get_sdr_repository_info (dev, &sdr_info) == -1)
    {
      if (verbose)
	{
	  fprintf (stderr, "FAILED: Failure to retrieve SDR Repository Info\n");
	}
      return (-1);
    }

  if (write_sdr_repository_info (fp, &sdr_info) == -1)
    {
      if (verbose)
	{
	  fprintf (stderr, "FAILED: fprintf: %s\n", strerror(errno));
	}
      return (-1);
    }
  if (verbose)
    {
      fprintf (stderr, "done\n");
    }
  
  {
    uint16_t record_id = 0;
    uint16_t next_record_id = 0;
    sdr_record_t sdr_record;
    int i = 0;
    
    while (record_id != 0xFFFF)
      {
	memset (&sdr_record, 0, sizeof (sdr_record_t));

	if (verbose)
	  {
	    fprintf (stderr, "Fetching record %d of %d (current record ID %d) \r", 
		     i++, 
		     sdr_info.record_count, 
		     record_id);
	  }

	if (_get_sdr_record (dev, 
			     record_id, 
			     &next_record_id, 
			     &sdr_record) == -1)
	  {
	    if (verbose)
	      {
		fprintf (stderr, "FAILED: Retrieval of SDR record 0x%X\n", record_id);
	      }
	    return (-1);
	  }
	
	if (write_sdr_record (fp, &sdr_record) == -1)
	  {
	    if (verbose)
	      {
                fprintf (stderr, "FAILED: fprintf: %s\n", strerror(errno));
	      }
	    return (-1);
	  }
	
	record_id = next_record_id;
	next_record_id = 0;
      }
  }
  
  if (verbose)
    {
      fprintf (stderr, "\n");
    }
  return 0;
}

int 
load_sdr_cache (FILE *fp, sdr_repository_info_t *sdr_info, 
		sdr_record_t **sdr_record_list, int *count)
{
  char *cache_record = NULL;
  int rv = -1;
  
  ERR_EINVAL (fp 
              && sdr_info
              && sdr_record_list
              && count);
  
  memset (sdr_info, 0, sizeof (sdr_repository_info_t));
  if (_fread_record (fp, &cache_record) == -1)
    {
      fprintf (stderr, "FAILED: _fread_record: %s\n", strerror(errno));
      return (-1);
    }
  rv = read_sdr_repository_info (cache_record, 
				 sdr_info);
  free (cache_record);
  if (rv == -1)
    {
      return (-1);
    }
  
  {
    long int current_position;
    sdr_record_t *l_sdr_record_list;
    int l_count;
    int i;
    
    if ((current_position = ftell (fp)) == -1)
      {
        fprintf (stderr, "FAILED: ftell: %s\n", strerror(errno));
	return (-1);
      }
    if (_get_record_count (fp, &l_count) == -1)
      {
        fprintf (stderr, "FAILED: _get_record_count: %s\n", strerror(errno));
	return (-1);
      }
    if (fseek (fp, current_position, SEEK_SET))
      {
        fprintf (stderr, "FAILED: fseek: %s\n", strerror(errno));
	return (-1);
      }
    
    l_sdr_record_list = calloc (l_count, sizeof (sdr_record_t));
    for (i = 0; i < l_count; i++)
      {
	if (_fread_record (fp, &cache_record) == -1)
	  {
	    fprintf (stderr, "FAILED: _fread_record: %s\n", strerror(errno));
	    free (l_sdr_record_list);
	    return (-1);
	  }
	rv = read_sdr_record (cache_record, (l_sdr_record_list + i));
	free (cache_record);
	if (rv == -1)
	  {
	    free (l_sdr_record_list);
	    return (-1);
	  }
      }
    
    *sdr_record_list = l_sdr_record_list;
    *count = l_count;
  }
  
  return 0;
}
