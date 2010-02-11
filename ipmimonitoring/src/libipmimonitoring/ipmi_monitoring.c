/*****************************************************************************\
 *  $Id: ipmi_monitoring.c,v 1.75 2010-02-11 21:35:54 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2007-2010 Lawrence Livermore National Security, LLC.
 *  Copyright (C) 2006-2007 The Regents of the University of California.
 *  Produced at Lawrence Livermore National Laboratory (cf, DISCLAIMER).
 *  Written by Albert Chu <chu11@llnl.gov>
 *  UCRL-CODE-222073
 *
 *  This file is part of Ipmimonitoring, an IPMI sensor monitoring
 *  library.  For details, see http://www.llnl.gov/linux/.
 *
 *  Ipmimonitoring is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by the
 *  Free Software Foundation; either version 3 of the License, or (at your
 *  option) any later version.
 *
 *  Ipmimonitoring is distributed in the hope that it will be useful, but
 *  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 *  for more details.
 *
 *  You should have received a copy of the GNU General Public License along
 *  with Ipmimonitoring.  If not, see <http://www.gnu.org/licenses/>.
\*****************************************************************************/

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <sys/types.h>
#include <sys/stat.h>
#if HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */
#include <assert.h>
#include <errno.h>

#include "ipmi_monitoring.h"
#include "ipmi_monitoring_defs.h"
#include "ipmi_monitoring_debug.h"
#include "ipmi_monitoring_ipmi_communication.h"
#include "ipmi_monitoring_sdr_cache.h"
#include "ipmi_monitoring_sensor_reading.h"

#include "freeipmi-portability.h"
#include "secure.h"

static char *ipmi_monitoring_errmsgs[] =
  {
    "success",
    "ctx null",
    "ctx invalid",
    "invalid parameters",
    "permission denied",
    "library uninitialized",
    "sensor config file does not exist",
    "sensor config file parse error",
    "sdr cache permission error",
    "sdr cache filesystem error",
    "hostname invalid",
    "sensor not found",
    "no sensor readings available",
    "end of sensor readings list reached",
    "connection timeout",
    "session timeout",
    "invalid username",
    "invalid password",
    "password verification timeout",
    "k_g invalid",
    "privilege level insufficient",
    "privilege level cannot be obtained for this user",
    "authentication type unavailable for attempted privilege level",
    "ipmi 2.0 unavailable",
    "cipher suite id unavailable",
    "callback error",
    "BMC busy",
    "out of memory",
    "internal IPMI error",
    "internal system error",
    "internal error",
    "errnum out of range",
    NULL
  };

static int _ipmi_monitoring_initialized = 0;

uint32_t _ipmi_monitoring_flags = 0;

char sensor_config_file[MAXPATHLEN+1];
int sensor_config_file_set;

extern char sdr_cache_directory[MAXPATHLEN+1];
extern int sdr_cache_directory_set;

extern char sdr_cache_filename_format[MAXPATHLEN+1];
extern int sdr_cache_filename_format_set;

int
ipmi_monitoring_init (unsigned int flags, int *errnum)
{
  if (flags & ~IPMI_MONITORING_FLAGS_MASK)
    {
      if (errnum)
        *errnum = IPMI_MONITORING_ERR_PARAMETERS;
      return (-1);
    }

  if (_ipmi_monitoring_initialized)
    return (0);

  _ipmi_monitoring_flags = flags;

  _ipmi_monitoring_initialized++;
  if (errnum)
    *errnum = IPMI_MONITORING_ERR_SUCCESS;
  return (0);
}

static void
_init_ctx (ipmi_monitoring_ctx_t c)
{
  assert (c);
  assert (c->magic == IPMI_MONITORING_MAGIC);

  c->sdr_cache_ctx = NULL;
}

static void
_destroy_sensor_reading (void *x)
{
  struct ipmi_monitoring_sensor_reading *reading;

  assert (x);

  reading = (struct ipmi_monitoring_sensor_reading *)x;

  if (reading->sensor_bitmask_strings)
    {
      unsigned int i = 0;

      while (reading->sensor_bitmask_strings[i])
        {
          free (reading->sensor_bitmask_strings[i]);
          i++;
        }

      free (reading->sensor_bitmask_strings);
    }

  free (reading);
}

static void
_destroy_ctx (ipmi_monitoring_ctx_t c)
{
  assert (c);
  assert (c->magic == IPMI_MONITORING_MAGIC);

  if (c->interpret_ctx)
    {
      ipmi_interpret_ctx_destroy (c->interpret_ctx);
      c->interpret_ctx = NULL;
    }
    
  if (c->sdr_cache_ctx)
    {
      ipmi_sdr_cache_ctx_destroy (c->sdr_cache_ctx);
      c->sdr_cache_ctx = NULL;
    }

  /* Note: destroy iterator first */
  if (c->sensor_readings_itr)
    {
      list_iterator_destroy (c->sensor_readings_itr);
      c->sensor_readings_itr = NULL;
    }

  if (c->sensor_readings)
    {
      list_destroy (c->sensor_readings);
      c->sensor_readings = NULL;
    }

  c->current_sensor_reading = NULL;

  c->magic = ~IPMI_MONITORING_MAGIC;
  if (_ipmi_monitoring_flags & IPMI_MONITORING_FLAGS_LOCK_MEMORY)
    secure_free (c, sizeof (struct ipmi_monitoring_ctx));
  else
    free (c);
}

ipmi_monitoring_ctx_t
ipmi_monitoring_ctx_create (void)
{
  struct ipmi_monitoring_ctx *c = NULL;

  if (_ipmi_monitoring_flags & IPMI_MONITORING_FLAGS_LOCK_MEMORY)
    {
      if (!(c = (ipmi_monitoring_ctx_t)secure_malloc (sizeof (struct ipmi_monitoring_ctx))))
        return (NULL);
      /* secure_memset called in secure_malloc()*/
    }
  else
    {
      if (!(c = (ipmi_monitoring_ctx_t)malloc (sizeof (struct ipmi_monitoring_ctx))))
        return (NULL);
      secure_memset (c, '\0', sizeof (struct ipmi_monitoring_ctx));
    }
  c->magic = IPMI_MONITORING_MAGIC;

  if (!(c->interpret_ctx = ipmi_interpret_ctx_create ()))
    goto cleanup;

  if (!(c->sensor_readings = list_create ((ListDelF)_destroy_sensor_reading)))
    goto cleanup;

  _init_ctx (c);
  return (c);

 cleanup:
  if (c)
    _destroy_ctx (c);
  return (NULL);
}

void
ipmi_monitoring_ctx_destroy (ipmi_monitoring_ctx_t c)
{
  if (!c || c->magic != IPMI_MONITORING_MAGIC)
    return;

  _destroy_ctx (c);
}

int
ipmi_monitoring_ctx_errnum (ipmi_monitoring_ctx_t c)
{
  if (!c)
    return (IPMI_MONITORING_ERR_CTX_NULL);
  else if (c->magic != IPMI_MONITORING_MAGIC)
    return (IPMI_MONITORING_ERR_CTX_INVALID);
  else
    return (c->errnum);
}

char *
ipmi_monitoring_ctx_strerror (int errnum)
{
  if (errnum >= IPMI_MONITORING_ERR_SUCCESS && errnum <= IPMI_MONITORING_ERR_ERRNUMRANGE)
    return (ipmi_monitoring_errmsgs[errnum]);
  else
    return (ipmi_monitoring_errmsgs[IPMI_MONITORING_ERR_ERRNUMRANGE]);
}

char *
ipmi_monitoring_ctx_errormsg (ipmi_monitoring_ctx_t c)
{
  return (ipmi_monitoring_ctx_strerror (ipmi_monitoring_ctx_errnum (c)));
}

static void
_interpret_ctx_error_convert (ipmi_monitoring_ctx_t c)
{
  assert (c);
  assert (c->magic == IPMI_MONITORING_MAGIC);

  if (ipmi_interpret_ctx_errnum (c->interpret_ctx) == IPMI_INTERPRET_ERR_OUT_OF_MEMORY)
    c->errnum = IPMI_MONITORING_ERR_OUT_OF_MEMORY;
  else if (ipmi_interpret_ctx_errnum (c->interpret_ctx) == IPMI_INTERPRET_ERR_PERMISSION)
    c->errnum = IPMI_MONITORING_ERR_PERMISSION;
  else if (ipmi_interpret_ctx_errnum (c->interpret_ctx) == IPMI_INTERPRET_ERR_SENSOR_CONFIG_FILE_DOES_NOT_EXIST)
    c->errnum = IPMI_MONITORING_ERR_SENSOR_CONFIG_FILE_DOES_NOT_EXIST;
  else if (ipmi_interpret_ctx_errnum (c->interpret_ctx) == IPMI_INTERPRET_ERR_SENSOR_CONFIG_FILE_PARSE)
    c->errnum = IPMI_MONITORING_ERR_SENSOR_CONFIG_FILE_PARSE;
  else
    c->errnum = IPMI_MONITORING_ERR_INTERNAL_ERROR;
}

int
ipmi_monitoring_ctx_sensor_config_file (ipmi_monitoring_ctx_t c,
                                        const char *sensor_config_file)
{
  if (!c || c->magic != IPMI_MONITORING_MAGIC)
    return (-1);
  
  if (sensor_config_file)
    {
      if (ipmi_interpret_load_sensor_config (c->interpret_ctx,
                                             sensor_config_file) < 0)
        {
          _interpret_ctx_error_convert (c);
          return (-1);
        }
    }
  else
    {
      /* legacy */
      if (ipmi_interpret_load_sensor_config (c->interpret_ctx,
                                             IPMI_MONITORING_SENSOR_CONFIG_FILE_DEFAULT) < 0)
        {
          if (ipmi_interpret_ctx_errnum (c->interpret_ctx) != IPMI_INTERPRET_ERR_SENSOR_CONFIG_FILE_DOES_NOT_EXIST)
            {
              _interpret_ctx_error_convert (c);
              return (-1);
            }
        }
      else
        goto out;

      if (ipmi_interpret_load_sensor_config (c->interpret_ctx, NULL) < 0)
        {
          _interpret_ctx_error_convert (c);
          return (-1);
        }
    }
  
 out:
  c->errnum = IPMI_MONITORING_ERR_SUCCESS;
  return (0);
}

int
ipmi_monitoring_ctx_sdr_cache_directory (ipmi_monitoring_ctx_t c, const char *dir)
{
  struct stat buf;

  if (!c || c->magic != IPMI_MONITORING_MAGIC)
    return (-1);

  if (!_ipmi_monitoring_initialized)
    {
      c->errnum = IPMI_MONITORING_ERR_LIBRARY_UNINITIALIZED;
      return (-1);
    }

  if (!dir || (strlen (dir) > MAXPATHLEN))
    {
      c->errnum = IPMI_MONITORING_ERR_PARAMETERS;
      return (-1);
    }

  if (stat (dir, &buf) < 0)
    {
      if (errno == EACCES || errno == EPERM)
        c->errnum = IPMI_MONITORING_ERR_PERMISSION;
      else
        c->errnum = IPMI_MONITORING_ERR_PARAMETERS;
      return (-1);
    }

  strncpy (c->sdr_cache_directory, dir, MAXPATHLEN);
  c->sdr_cache_directory_set = 1;

  c->errnum = IPMI_MONITORING_ERR_SUCCESS;
  return (0);
}

int
ipmi_monitoring_ctx_sdr_cache_filenames (ipmi_monitoring_ctx_t c, const char *format)
{
  if (!c || c->magic != IPMI_MONITORING_MAGIC)
    return (-1);

  if (!_ipmi_monitoring_initialized)
    {
      c->errnum = IPMI_MONITORING_ERR_LIBRARY_UNINITIALIZED;
      return (-1);
    }

  if (!format)
    {
      c->errnum = IPMI_MONITORING_ERR_PARAMETERS;
      return (-1);
    }

  if (strchr (format, '/'))
    {
      c->errnum = IPMI_MONITORING_ERR_PARAMETERS;
      return (-1);
    }

  if (!strstr (format, "%H"))
    {
      c->errnum = IPMI_MONITORING_ERR_PARAMETERS;
      return (-1);
    }

  strncpy (c->sdr_cache_filename_format, format, MAXPATHLEN);
  c->sdr_cache_filename_format_set = 1;

  c->errnum = IPMI_MONITORING_ERR_SUCCESS;
  return (0);
}

static int
_ipmi_monitoring_sensor_readings_flags_common (ipmi_monitoring_ctx_t c,
                                               const char *hostname,
                                               struct ipmi_monitoring_ipmi_config *config,
                                               unsigned int sensor_reading_flags)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  int rv = -1;

  assert (c);
  assert (c->magic == IPMI_MONITORING_MAGIC);
  assert (_ipmi_monitoring_initialized);
  assert (!(sensor_reading_flags & ~IPMI_MONITORING_SENSOR_READING_FLAGS_MASK));

  if (sensor_reading_flags & IPMI_MONITORING_SENSOR_READING_FLAGS_REREAD_SDR_CACHE)
    {
      if (ipmi_monitoring_sdr_cache_flush (c, hostname) < 0)
        goto cleanup;
    }

  if (sensor_reading_flags & IPMI_MONITORING_SENSOR_READING_FLAGS_BRIDGE_SENSORS)
    {
      if (ipmi_sensor_read_ctx_set_flags (c->sensor_read_ctx, IPMI_SENSOR_READ_FLAGS_BRIDGE_SENSORS) < 0)
        {
          IPMI_MONITORING_DEBUG (("ipmi_sensor_read_ctx_set_flags: %s", ipmi_sensor_read_ctx_errormsg (c->sensor_read_ctx)));
          c->errnum = IPMI_MONITORING_ERR_INTERNAL_ERROR;
          goto cleanup;
        }
    }

  if (sensor_reading_flags & IPMI_MONITORING_SENSOR_READING_FLAGS_INTERPRET_OEM_DATA)
    {
      if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_get_device_id_rs)))
        {
          IPMI_MONITORING_DEBUG (("fiid_obj_create: %s", strerror(errno)));
          c->errnum = IPMI_MONITORING_ERR_OUT_OF_MEMORY;
          goto cleanup;
        }

      if (ipmi_cmd_get_device_id (c->ipmi_ctx, obj_cmd_rs) < 0)
        {
          IPMI_MONITORING_DEBUG (("ipmi_cmd_get_device_id: %s", ipmi_ctx_errormsg (c->ipmi_ctx)));
          ipmi_monitoring_ipmi_ctx_error_convert (c);
          goto cleanup;
        }

      if (FIID_OBJ_GET (obj_cmd_rs, "manufacturer_id.id", &val) < 0)
        {
          IPMI_MONITORING_DEBUG (("FIID_OBJ_GET: %s", fiid_obj_errormsg (obj_cmd_rs)));
          c->errnum = IPMI_MONITORING_ERR_INTERNAL_ERROR;
          goto cleanup;
        }
      c->manufacturer_id = val;
          
      if (FIID_OBJ_GET (obj_cmd_rs, "product_id", &val) < 0)
        {
          IPMI_MONITORING_DEBUG (("FIID_OBJ_GET: %s", fiid_obj_errormsg (obj_cmd_rs)));
          c->errnum = IPMI_MONITORING_ERR_INTERNAL_ERROR;
          goto cleanup;
        }
      c->product_id = val;

      if (ipmi_interpret_ctx_set_flags (c->interpret_ctx, IPMI_INTERPRET_FLAGS_INTERPRET_OEM_DATA) < 0)
	{
	  IPMI_MONITORING_DEBUG (("ipmi_interpret_ctx_set_flags: %s", ipmi_interpret_ctx_errormsg (c->interpret_ctx)));
	  c->errnum = IPMI_MONITORING_ERR_INTERNAL_ERROR;
	  goto cleanup;
	}

      if (ipmi_interpret_ctx_set_manufacturer_id (c->interpret_ctx, c->manufacturer_id) < 0)
	{
	  IPMI_MONITORING_DEBUG (("ipmi_interpret_ctx_set_manufacturer_id: %s", ipmi_interpret_ctx_errormsg (c->interpret_ctx)));
	  c->errnum = IPMI_MONITORING_ERR_INTERNAL_ERROR;
	  goto cleanup;
	}

      if (ipmi_interpret_ctx_set_product_id (c->interpret_ctx, c->product_id) < 0)
	{
	  IPMI_MONITORING_DEBUG (("ipmi_interpret_ctx_set_product_id: %s", ipmi_interpret_ctx_errormsg (c->interpret_ctx)));
	  c->errnum = IPMI_MONITORING_ERR_INTERNAL_ERROR;
	  goto cleanup;
	}
    }
  else
    {
      if (ipmi_interpret_ctx_set_flags (c->interpret_ctx, IPMI_INTERPRET_FLAGS_DEFAULT) < 0)
        {
          IPMI_MONITORING_DEBUG (("ipmi_interpret_ctx_set_flags: %s", ipmi_interpret_ctx_errormsg (c->interpret_ctx)));
          c->errnum = IPMI_MONITORING_ERR_INTERNAL_ERROR;
          goto cleanup;
        }
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

/* returns 1 on success, 0 on fallback and do reading, -1 on error */
static int
_ipmi_monitoring_get_sensor_reading_shared (ipmi_monitoring_ctx_t c,
                                            unsigned int sensor_reading_flags,
                                            uint8_t *sdr_record,
                                            unsigned int sdr_record_len,
                                            unsigned int *sensor_types,
                                            unsigned int sensor_types_len)
{
  uint8_t record_type;
  uint8_t share_count;
  int i;

  assert (c);
  assert (c->magic == IPMI_MONITORING_MAGIC);
  assert (_ipmi_monitoring_initialized);
  assert (!(sensor_reading_flags & ~IPMI_MONITORING_SENSOR_READING_FLAGS_MASK));
  assert (sensor_reading_flags & IPMI_MONITORING_SENSOR_READING_FLAGS_SHARED_SENSORS);
  
  if (ipmi_sdr_parse_record_id_and_type (c->sdr_parse_ctx,
                                         sdr_record,
                                         sdr_record_len,
                                         NULL,
                                         &record_type) < 0)
    {
      IPMI_MONITORING_DEBUG (("ipmi_sdr_parse_record_id_and_type: %s",
                              ipmi_sdr_parse_ctx_errormsg (c->sdr_parse_ctx)));
      c->errnum = IPMI_MONITORING_ERR_INTERNAL_ERROR;
      return (-1);
    }
  
  if (record_type != IPMI_SDR_FORMAT_COMPACT_SENSOR_RECORD)
    return (0);

  if (ipmi_sdr_parse_sensor_record_sharing (c->sdr_parse_ctx,
                                            sdr_record,
                                            sdr_record_len,
                                            &share_count,
                                            NULL,
                                            NULL,
                                            NULL) < 0)
    {
      IPMI_MONITORING_DEBUG (("ipmi_sdr_parse_sensor_record_sharing: %s",
                              ipmi_sdr_parse_ctx_errormsg (c->sdr_parse_ctx)));
      c->errnum = IPMI_MONITORING_ERR_INTERNAL_ERROR;
      return (-1);
    }

  if (share_count <= 1)
    return (0);

  
  /* IPMI spec gives the following example:
   *
   * "If the starting sensor number was 10, and the share
   * count was 3, then sensors 10, 11, and 12 would share
   * the record"
   */
  for (i = 0; i < share_count; i++)
    {
      if (ipmi_monitoring_get_sensor_reading (c,
                                              sensor_reading_flags,
                                              sdr_record,
                                              sdr_record_len,
                                              i,
                                              sensor_types,
                                              sensor_types_len) < 0)
        return (-1);
    }

  return (1);
}

static int
_ipmi_monitoring_sensor_readings_by_record_id (ipmi_monitoring_ctx_t c,
                                               const char *hostname,
                                               struct ipmi_monitoring_ipmi_config *config,
                                               unsigned int sensor_reading_flags,
                                               unsigned int *record_ids,
                                               unsigned int record_ids_len)
{
  uint16_t record_count;
  unsigned int i;
  int rv = -1;

  assert (c);
  assert (c->magic == IPMI_MONITORING_MAGIC);
  assert (_ipmi_monitoring_initialized);
  assert (!(sensor_reading_flags & ~IPMI_MONITORING_SENSOR_READING_FLAGS_MASK));

  ipmi_monitoring_sensor_iterator_destroy (c);

  if (ipmi_monitoring_ipmi_communication_init (c, hostname, config) < 0)
    goto cleanup;

  if (ipmi_monitoring_sensor_reading_init (c) < 0)
    goto cleanup;

  if (_ipmi_monitoring_sensor_readings_flags_common (c,
                                                     hostname,
                                                     config,
                                                     sensor_reading_flags) < 0)
    goto cleanup;

  if (ipmi_monitoring_sdr_cache_load (c, hostname) < 0)
    goto cleanup;

  if (!record_ids)
    {
      if (ipmi_sdr_cache_record_count (c->sdr_cache_ctx, &record_count) < 0)
        {
          IPMI_MONITORING_DEBUG (("ipmi_sdr_cache_record_count: %s", ipmi_sdr_cache_ctx_errormsg (c->sdr_cache_ctx)));
          c->errnum = IPMI_MONITORING_ERR_INTERNAL_ERROR;
          goto cleanup;
        }

      for (i = 0; i < record_count; i++, ipmi_sdr_cache_next (c->sdr_cache_ctx))
        {
          uint8_t sdr_record[IPMI_MONITORING_MAX_SDR_RECORD_LENGTH];
          int sdr_record_len;
          int shared_ret = 0;

          memset (sdr_record, '\0', IPMI_MONITORING_MAX_SDR_RECORD_LENGTH);
          if ((sdr_record_len = ipmi_sdr_cache_record_read (c->sdr_cache_ctx,
                                                            sdr_record,
                                                            IPMI_MONITORING_MAX_SDR_RECORD_LENGTH)) < 0)
            {
              IPMI_MONITORING_DEBUG (("ipmi_sdr_cache_record_read: %s", ipmi_sdr_cache_ctx_errormsg (c->sdr_cache_ctx)));
              c->errnum = IPMI_MONITORING_ERR_INTERNAL_ERROR;
              goto cleanup;
            }

          if (sensor_reading_flags & IPMI_MONITORING_SENSOR_READING_FLAGS_SHARED_SENSORS)
            {
              if ((shared_ret = _ipmi_monitoring_get_sensor_reading_shared (c,
                                                                            sensor_reading_flags,
                                                                            sdr_record,
                                                                            sdr_record_len,
                                                                            NULL,
                                                                            0)) < 0)
                goto cleanup;
            }
          
          if (!shared_ret)
            {
              if (ipmi_monitoring_get_sensor_reading (c,
                                                      sensor_reading_flags,
                                                      sdr_record,
                                                      sdr_record_len,
                                                      0,
                                                      NULL,
                                                      0) < 0)
                goto cleanup;
            }
        }
    }
  else
    {
      for (i = 0; i < record_ids_len; i++)
        {
          uint8_t sdr_record[IPMI_MONITORING_MAX_SDR_RECORD_LENGTH];
          int sdr_record_len;
          int shared_ret = 0;

          if (ipmi_sdr_cache_search_record_id (c->sdr_cache_ctx, record_ids[i]) < 0)
            {
              if (ipmi_sdr_cache_ctx_errnum (c->sdr_cache_ctx) == IPMI_SDR_CACHE_ERR_NOT_FOUND)
                {
                  c->errnum = IPMI_MONITORING_ERR_SENSOR_NOT_FOUND;
                  goto cleanup;
                }
              IPMI_MONITORING_DEBUG (("ipmi_sdr_cache_search_record_id: %s", ipmi_sdr_cache_ctx_errormsg (c->sdr_cache_ctx)));
              c->errnum = IPMI_MONITORING_ERR_INTERNAL_ERROR;
              goto cleanup;
            }

          memset (sdr_record, '\0', IPMI_MONITORING_MAX_SDR_RECORD_LENGTH);
          if ((sdr_record_len = ipmi_sdr_cache_record_read (c->sdr_cache_ctx,
                                                            sdr_record,
                                                            IPMI_MONITORING_MAX_SDR_RECORD_LENGTH)) < 0)
            {
              IPMI_MONITORING_DEBUG (("ipmi_sdr_cache_record_read: %s", ipmi_sdr_cache_ctx_errormsg (c->sdr_cache_ctx)));
              c->errnum = IPMI_MONITORING_ERR_INTERNAL_ERROR;
              goto cleanup;
            }

          if (sensor_reading_flags & IPMI_MONITORING_SENSOR_READING_FLAGS_SHARED_SENSORS)
            {
              if ((shared_ret = _ipmi_monitoring_get_sensor_reading_shared (c,
                                                                            sensor_reading_flags,
                                                                            sdr_record,
                                                                            sdr_record_len,
                                                                            NULL,
                                                                            0)) < 0)
                goto cleanup;
            }

          if (!shared_ret)
            {
              if (ipmi_monitoring_get_sensor_reading (c,
                                                      sensor_reading_flags,
                                                      sdr_record,
                                                      sdr_record_len,
                                                      0,
                                                      NULL,
                                                      0) < 0)
                goto cleanup;
            }
        }
    }

  if ((rv = list_count (c->sensor_readings)) > 0)
    {
      if (!(c->sensor_readings_itr = list_iterator_create (c->sensor_readings)))
        {
          IPMI_MONITORING_DEBUG (("list_iterator_create: %s", strerror (errno)));
          c->errnum = IPMI_MONITORING_ERR_INTERNAL_ERROR;
          goto cleanup;
        }
      c->current_sensor_reading = list_next (c->sensor_readings_itr);
    }

  ipmi_monitoring_sdr_cache_unload (c);
  ipmi_monitoring_ipmi_communication_cleanup (c);
  ipmi_monitoring_sensor_reading_cleanup (c);
  c->errnum = IPMI_MONITORING_ERR_SUCCESS;
  return (rv);

 cleanup:
  ipmi_monitoring_sdr_cache_unload (c);
  ipmi_monitoring_sensor_iterator_destroy (c);
  ipmi_monitoring_ipmi_communication_cleanup (c);
  ipmi_monitoring_sensor_reading_cleanup (c);
  return (-1);
}

int
ipmi_monitoring_sensor_readings_by_record_id (ipmi_monitoring_ctx_t c,
                                              const char *hostname,
                                              struct ipmi_monitoring_ipmi_config *config,
                                              unsigned int sensor_reading_flags,
                                              unsigned int *record_ids,
                                              unsigned int record_ids_len,
                                              Ipmi_Monitoring_Sensor_Readings_Callback callback,
                                              void *callback_data)
{
  int rv;

  if (!c || c->magic != IPMI_MONITORING_MAGIC)
    return (-1);

  if (!_ipmi_monitoring_initialized)
    {
      c->errnum = IPMI_MONITORING_ERR_LIBRARY_UNINITIALIZED;
      return (-1);
    }

  if ((sensor_reading_flags & ~IPMI_MONITORING_SENSOR_READING_FLAGS_MASK)
      || (record_ids && !record_ids_len))
    {
      c->errnum = IPMI_MONITORING_ERR_PARAMETERS;
      return (-1);
    }

  if (record_ids && record_ids_len)
    {
      unsigned int i;

      for (i = 0; i < record_ids_len; i++)
        {
          if (record_ids[i] > IPMI_SDR_RECORD_ID_LAST)
            {
              c->errnum = IPMI_MONITORING_ERR_PARAMETERS;
              return (-1);
            }
        }
    }

  c->callback = callback;
  c->callback_data = callback_data;
  c->callback_sensor_reading = NULL;

  rv = _ipmi_monitoring_sensor_readings_by_record_id (c,
                                                      hostname,
                                                      config,
                                                      sensor_reading_flags,
                                                      record_ids,
                                                      record_ids_len);

  c->callback_sensor_reading = NULL;

  return (rv);
}

static int
_ipmi_monitoring_sensor_readings_by_sensor_type (ipmi_monitoring_ctx_t c,
                                                 const char *hostname,
                                                 struct ipmi_monitoring_ipmi_config *config,
                                                 unsigned int sensor_reading_flags,
                                                 unsigned int *sensor_types,
                                                 unsigned int sensor_types_len)
{
  uint16_t record_count;
  unsigned int i;
  int rv = -1;

  assert (c);
  assert (c->magic == IPMI_MONITORING_MAGIC);
  assert (_ipmi_monitoring_initialized);
  assert (!(sensor_reading_flags & ~IPMI_MONITORING_SENSOR_READING_FLAGS_MASK));
  assert (!(sensor_types && !sensor_types_len));

  ipmi_monitoring_sensor_iterator_destroy (c);

  if (ipmi_monitoring_ipmi_communication_init (c, hostname, config) < 0)
    goto cleanup;

  if (ipmi_monitoring_sensor_reading_init (c) < 0)
    goto cleanup;

  if (_ipmi_monitoring_sensor_readings_flags_common (c,
                                                     hostname,
                                                     config,
                                                     sensor_reading_flags) < 0)
    goto cleanup;

  if (ipmi_monitoring_sdr_cache_load (c, hostname) < 0)
    goto cleanup;

  if (ipmi_sdr_cache_record_count (c->sdr_cache_ctx, &record_count) < 0)
    {
      IPMI_MONITORING_DEBUG (("ipmi_sdr_cache_record_count: %s", ipmi_sdr_cache_ctx_errormsg (c->sdr_cache_ctx)));
      c->errnum = IPMI_MONITORING_ERR_INTERNAL_ERROR;
      goto cleanup;
    }

  for (i = 0; i < record_count; i++, ipmi_sdr_cache_next (c->sdr_cache_ctx))
    {
      uint8_t sdr_record[IPMI_MONITORING_MAX_SDR_RECORD_LENGTH];
      int sdr_record_len;
      int shared_ret = 0;

      memset (sdr_record, '\0', IPMI_MONITORING_MAX_SDR_RECORD_LENGTH);
      if ((sdr_record_len = ipmi_sdr_cache_record_read (c->sdr_cache_ctx,
                                                        sdr_record,
                                                        IPMI_MONITORING_MAX_SDR_RECORD_LENGTH)) < 0)
        {
          IPMI_MONITORING_DEBUG (("ipmi_sdr_cache_record_read: %s", ipmi_sdr_cache_ctx_errormsg (c->sdr_cache_ctx)));
          c->errnum = IPMI_MONITORING_ERR_INTERNAL_ERROR;
          goto cleanup;
        }

      if (sensor_reading_flags & IPMI_MONITORING_SENSOR_READING_FLAGS_SHARED_SENSORS)
        {
          if ((shared_ret = _ipmi_monitoring_get_sensor_reading_shared (c,
                                                                        sensor_reading_flags,
                                                                        sdr_record,
                                                                        sdr_record_len,
                                                                        sensor_types,
                                                                        sensor_types_len)) < 0)
            goto cleanup;
        }

      if (!shared_ret)
        {
          if (ipmi_monitoring_get_sensor_reading (c,
                                                  sensor_reading_flags,
                                                  sdr_record,
                                                  sdr_record_len,
                                                  0,
                                                  sensor_types,
                                                  sensor_types_len) < 0)
            goto cleanup;
        }
    }

  if ((rv = list_count (c->sensor_readings)) > 0)
    {
      if (!(c->sensor_readings_itr = list_iterator_create (c->sensor_readings)))
        {
          IPMI_MONITORING_DEBUG (("list_iterator_create: %s", strerror (errno)));
          c->errnum = IPMI_MONITORING_ERR_INTERNAL_ERROR;
          goto cleanup;
        }
      c->current_sensor_reading = list_next (c->sensor_readings_itr);
    }

  ipmi_monitoring_sdr_cache_unload (c);
  ipmi_monitoring_ipmi_communication_cleanup (c);
  ipmi_monitoring_sensor_reading_cleanup (c);
  c->errnum = IPMI_MONITORING_ERR_SUCCESS;
  return (rv);

 cleanup:
  ipmi_monitoring_sdr_cache_unload (c);
  ipmi_monitoring_sensor_iterator_destroy (c);
  ipmi_monitoring_ipmi_communication_cleanup (c);
  ipmi_monitoring_sensor_reading_cleanup (c);
  return (-1);
}

int
ipmi_monitoring_sensor_readings_by_sensor_type (ipmi_monitoring_ctx_t c,
                                                const char *hostname,
                                                struct ipmi_monitoring_ipmi_config *config,
                                                unsigned int sensor_reading_flags,
                                                unsigned int *sensor_types,
                                                unsigned int sensor_types_len,
                                                Ipmi_Monitoring_Sensor_Readings_Callback callback,
                                                void *callback_data)
{
  int rv;

  if (!c || c->magic != IPMI_MONITORING_MAGIC)
    return (-1);

  if (!_ipmi_monitoring_initialized)
    {
      c->errnum = IPMI_MONITORING_ERR_LIBRARY_UNINITIALIZED;
      return (-1);
    }

  if ((sensor_reading_flags & ~IPMI_MONITORING_SENSOR_READING_FLAGS_MASK)
      || (sensor_types && !sensor_types_len))
    {
      c->errnum = IPMI_MONITORING_ERR_PARAMETERS;
      return (-1);
    }

  c->callback = callback;
  c->callback_data = callback_data;
  c->callback_sensor_reading = NULL;

  rv = _ipmi_monitoring_sensor_readings_by_sensor_type (c,
                                                        hostname,
                                                        config,
                                                        sensor_reading_flags,
                                                        sensor_types,
                                                        sensor_types_len);

  c->callback_sensor_reading = NULL;

  return (rv);
}

int
ipmi_monitoring_sensor_iterator_first (ipmi_monitoring_ctx_t c)
{
  if (!c || c->magic != IPMI_MONITORING_MAGIC)
    return (-1);

  if (!c->sensor_readings_itr)
    {
      c->errnum = IPMI_MONITORING_ERR_NO_SENSOR_READINGS;
      return (-1);
    }

  list_iterator_reset (c->sensor_readings_itr);
  c->current_sensor_reading = list_next (c->sensor_readings_itr);
  c->errnum = IPMI_MONITORING_ERR_SUCCESS;
  return (0);
}

int
ipmi_monitoring_sensor_iterator_next (ipmi_monitoring_ctx_t c)
{
  if (!c || c->magic != IPMI_MONITORING_MAGIC)
    return (-1);

  if (!c->sensor_readings_itr)
    {
      c->errnum = IPMI_MONITORING_ERR_NO_SENSOR_READINGS;
      return (-1);
    }

  c->current_sensor_reading = list_next (c->sensor_readings_itr);
  c->errnum = IPMI_MONITORING_ERR_SUCCESS;
  return ((c->current_sensor_reading) ? 1 : 0);
}

static int
_sensor_readings_delete_all (void *x, void *y)
{
  return (1);
}

void
ipmi_monitoring_sensor_iterator_destroy (ipmi_monitoring_ctx_t c)
{
  if (!c || c->magic != IPMI_MONITORING_MAGIC)
    return;

  list_delete_all (c->sensor_readings, _sensor_readings_delete_all, "dummyvalue");

  if (c->sensor_readings_itr)
    {
      list_iterator_destroy (c->sensor_readings_itr);
      c->sensor_readings_itr = NULL;
    }

  c->current_sensor_reading = NULL;
}

static int
_ipmi_monitoring_sensor_read_common (ipmi_monitoring_ctx_t c,
                                     struct ipmi_monitoring_sensor_reading **sensor_reading)
{
  assert (sensor_reading);

  if (!c || c->magic != IPMI_MONITORING_MAGIC)
    return (-1);

  if (c->callback_sensor_reading)
    {
      (*sensor_reading) = c->callback_sensor_reading;
      return (0);
    }

  if (!c->sensor_readings_itr)
    {
      c->errnum = IPMI_MONITORING_ERR_NO_SENSOR_READINGS;
      return (-1);
    }

  if (!c->current_sensor_reading)
    {
      c->errnum = IPMI_MONITORING_ERR_SENSOR_READINGS_LIST_END;
      return (-1);
    }

  (*sensor_reading) = c->current_sensor_reading;
  return (0);
}

int
ipmi_monitoring_sensor_read_record_id (ipmi_monitoring_ctx_t c)
{
  struct ipmi_monitoring_sensor_reading *sensor_reading = NULL;

  if (_ipmi_monitoring_sensor_read_common (c, &sensor_reading) < 0)
    return (-1);

  c->errnum = IPMI_MONITORING_ERR_SUCCESS;
  return (sensor_reading->record_id);
}

int
ipmi_monitoring_sensor_read_sensor_number (ipmi_monitoring_ctx_t c)
{
  struct ipmi_monitoring_sensor_reading *sensor_reading = NULL;

  if (_ipmi_monitoring_sensor_read_common (c, &sensor_reading) < 0)
    return (-1);

  c->errnum = IPMI_MONITORING_ERR_SUCCESS;
  return (sensor_reading->sensor_number);
}

int
ipmi_monitoring_sensor_read_sensor_type (ipmi_monitoring_ctx_t c)
{
  struct ipmi_monitoring_sensor_reading *sensor_reading = NULL;

  if (_ipmi_monitoring_sensor_read_common (c, &sensor_reading) < 0)
    return (-1);

  c->errnum = IPMI_MONITORING_ERR_SUCCESS;
  return (sensor_reading->sensor_type);
}

char *
ipmi_monitoring_sensor_read_sensor_name (ipmi_monitoring_ctx_t c)
{
  struct ipmi_monitoring_sensor_reading *sensor_reading = NULL;

  if (_ipmi_monitoring_sensor_read_common (c, &sensor_reading) < 0)
    return (NULL);

  c->errnum = IPMI_MONITORING_ERR_SUCCESS;
  return (sensor_reading->sensor_name);
}


int
ipmi_monitoring_sensor_read_sensor_state (ipmi_monitoring_ctx_t c)
{
  struct ipmi_monitoring_sensor_reading *sensor_reading = NULL;

  if (_ipmi_monitoring_sensor_read_common (c, &sensor_reading) < 0)
    return (-1);

  c->errnum = IPMI_MONITORING_ERR_SUCCESS;
  return (sensor_reading->sensor_state);
}

int
ipmi_monitoring_sensor_read_sensor_units (ipmi_monitoring_ctx_t c)
{
  struct ipmi_monitoring_sensor_reading *sensor_reading = NULL;

  if (_ipmi_monitoring_sensor_read_common (c, &sensor_reading) < 0)
    return (-1);

  c->errnum = IPMI_MONITORING_ERR_SUCCESS;
  return (sensor_reading->sensor_units);
}

int
ipmi_monitoring_sensor_read_sensor_bitmask_type (ipmi_monitoring_ctx_t c)
{
  struct ipmi_monitoring_sensor_reading *sensor_reading = NULL;

  if (_ipmi_monitoring_sensor_read_common (c, &sensor_reading) < 0)
    return (-1);

  c->errnum = IPMI_MONITORING_ERR_SUCCESS;
  return (sensor_reading->sensor_bitmask_type);
}

int
ipmi_monitoring_sensor_read_sensor_bitmask (ipmi_monitoring_ctx_t c)
{
  struct ipmi_monitoring_sensor_reading *sensor_reading = NULL;

  if (_ipmi_monitoring_sensor_read_common (c, &sensor_reading) < 0)
    return (-1);

  c->errnum = IPMI_MONITORING_ERR_SUCCESS;
  return (sensor_reading->sensor_bitmask);
}

char **
ipmi_monitoring_sensor_read_sensor_bitmask_strings (ipmi_monitoring_ctx_t c)
{
  struct ipmi_monitoring_sensor_reading *sensor_reading = NULL;

  if (_ipmi_monitoring_sensor_read_common (c, &sensor_reading) < 0)
    return (NULL);

  c->errnum = IPMI_MONITORING_ERR_SUCCESS;
  return (sensor_reading->sensor_bitmask_strings);
}

int
ipmi_monitoring_sensor_read_sensor_reading_type (ipmi_monitoring_ctx_t c)
{
  struct ipmi_monitoring_sensor_reading *sensor_reading = NULL;

  if (_ipmi_monitoring_sensor_read_common (c, &sensor_reading) < 0)
    return (-1);

  c->errnum = IPMI_MONITORING_ERR_SUCCESS;
  return (sensor_reading->sensor_reading_type);
}

void *
ipmi_monitoring_sensor_read_sensor_reading (ipmi_monitoring_ctx_t c)
{
  struct ipmi_monitoring_sensor_reading *sensor_reading = NULL;
  void *rv = NULL;

  if (_ipmi_monitoring_sensor_read_common (c, &sensor_reading) < 0)
    return (NULL);

  if (sensor_reading->sensor_reading_type == IPMI_MONITORING_SENSOR_READING_TYPE_UNSIGNED_INTEGER8_BOOL)
    rv = &(sensor_reading->sensor_reading.bool_val);
  else if (sensor_reading->sensor_reading_type == IPMI_MONITORING_SENSOR_READING_TYPE_UNSIGNED_INTEGER32)
    rv = &(sensor_reading->sensor_reading.integer_val);
  else if (sensor_reading->sensor_reading_type == IPMI_MONITORING_SENSOR_READING_TYPE_DOUBLE)
    rv = &(sensor_reading->sensor_reading.double_val);

  c->errnum = IPMI_MONITORING_ERR_SUCCESS;
  return (rv);
}
