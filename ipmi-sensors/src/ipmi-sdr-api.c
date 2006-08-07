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
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#if HAVE_UNISTD_H
#include <unistd.h>
#endif	/* HAVE_UNISTD_H */
#include <error.h>
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
#include <arpa/inet.h>
#include <pwd.h>
#include <errno.h>

#define SDR_CACHE_DIR                "sdr-cache"
#define SDR_CACHE_FILENAME_PREFIX    "sdr-cache"
#define FREEIPMI_CONFIG_DIRECTORY_MODE    0700

#include "freeipmi/fiid.h"
#include "freeipmi/udm/ipmi-sdr-repository-cmds-udm.h"

/* #include "freeipmi/ipmi-sdr-record-types.h" */
/* #include "freeipmi/ipmi-sdr-repository-cmds.h" */
/* #include "freeipmi/ipmi-sensor-and-event-code-tables.h" */
/* #include "freeipmi/ipmi-sensor-types-spec.h" */
/* #include "freeipmi/ipmi-sensor-utils.h" */
/* #include "freeipmi/ipmi-sensor-cmds.h" */
/* #include "freeipmi/ipmi-sensor-units-spec.h" */
/* #include "freeipmi/udm/ipmi-sensor-cmds-udm.h" */


#include "err-wrappers.h"
#include "fiid-wrappers.h"
#include "freeipmi-portability.h"
#include "ipmi-common.h"
#include "ipmi-sensor-api.h"

#include "ipmi-sdr-api.h"

static char *
_get_home_directory ()
{
  uid_t user_id;
  struct passwd *user_passwd = NULL;
  char *home_dir = NULL;
  
  user_id = getuid ();
  user_passwd = getpwuid (user_id);
  
  if (user_passwd->pw_dir)
    {
      if (access (user_passwd->pw_dir, R_OK|W_OK|X_OK) == 0)
	return strdup (user_passwd->pw_dir);
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
get_sdr_cache_filename (char *host)
{
  char *ipmi_host_ip_address = NULL;
  char *cache_dir = NULL;
  
  char *cache_filename = NULL;
  
  if ((ipmi_host_ip_address = _get_ipmi_host_ip_address (host)))
    {
      if ((cache_dir = _get_sdr_cache_directory ()))
	{
	  asprintf (&cache_filename, 
		    "%s/%s.%s", 
		    cache_dir, 
		    SDR_CACHE_FILENAME_PREFIX, 
		    ipmi_host_ip_address);
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
flush_sdr_cache_file (char *host)
{
  char *cache_file = NULL;
  int rv = -1;
  
  if ((cache_file = get_sdr_cache_filename (host)))
    {
      rv = unlink (cache_file);
      free (cache_file);
    }
  
  return rv;
}

int 
create_sdr_cache (ipmi_device_t dev, FILE *fp, int verbose)
{
  sdr_repository_info_t sdr_info;
  
  if (fp == NULL)
    return (-1);

  memset (&sdr_info, 0, sizeof (sdr_repository_info_t));
  if (verbose)
    {
      fprintf (stderr, "Fetching SDR repository information... ");
    }
  if (get_sdr_repository_info (dev, &sdr_info) == -1)
    {
      if (verbose)
	{
	  fprintf (stderr, "FAILED\n");
	}
      return (-1);
    }
  if (fwrite (&sdr_info, sizeof (sdr_repository_info_t), 1, fp) != 1)
    {
      if (verbose)
	{
	  fprintf (stderr, "FAILED\n");
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
	if (get_sdr_record (dev, 
			    record_id, 
			    &next_record_id, 
			    &sdr_record) == -1)
	  {
	    if (verbose)
	      {
		fprintf (stderr, "\n");
	      }
	    return (-1);
	  }
	if (fwrite (&sdr_record, sizeof (sdr_record_t), 1, fp) != 1)
	  {
	    if (verbose)
	      {
		fprintf (stderr, "\n");
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
  if (fp == NULL || sdr_info == NULL || sdr_record_list == NULL || count == NULL)
    return (-1);

  {
    memset (sdr_info, 0, sizeof (sdr_repository_info_t));
    if (fread (sdr_info, sizeof (sdr_repository_info_t), 1, fp) != 1)
      {
	return (-1);
      }
  }
  
  {
    long int current_position;
    long int eof_position;
    
    if ((current_position = ftell (fp)) == -1)
      {
	return (-1);
      }
    if (fseek (fp, 0, SEEK_END))
      {
	return (-1);
      }
    if ((eof_position = ftell (fp)) == -1)
      {
	return (-1);
      }
    
    if ((eof_position - current_position) % sizeof (sdr_record_t))
      {
	return (-1);
      }
    
    *count = (eof_position - current_position) / sizeof (sdr_record_t);
    
    if (fseek (fp, current_position, SEEK_SET))
      {
	return (-1);
      }
    
    *sdr_record_list = calloc (*count, sizeof (sdr_record_t));
    if (fread (*sdr_record_list, sizeof (sdr_record_t), *count, fp) != *count)
      {
	return (-1);
      }
  }
  
  return 0;
}

