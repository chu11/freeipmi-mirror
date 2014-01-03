/*****************************************************************************\
 *  $Id: ipmi_monitoring.c,v 1.79 2010-08-04 20:41:36 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2007-2014 Lawrence Livermore National Security, LLC.
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
#include <assert.h>
#include <errno.h>

#include "ipmi_monitoring.h"
#include "ipmi_monitoring_defs.h"
#include "ipmi_monitoring_debug.h"
#include "ipmi_monitoring_ipmi_communication.h"
#include "ipmi_monitoring_sdr_cache.h"
#include "ipmi_monitoring_sel.h"
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
    "sel config file does not exist",
    "sel config file parse error",
    "sensor config file does not exist",
    "sensor config file parse error",
    "sdr cache permission error",
    "sdr cache filesystem error",
    "hostname invalid",
    "sensor not found",
    "no sel records available",
    "end of sel records list reached",
    "sel record data not available",
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

#define IPMI_MONITORING_SEL_RECORD_TYPE_CLASS_SYSTEM_EVENT_RECORD_ACCEPTABLE        0x01
#define IPMI_MONITORING_SEL_RECORD_TYPE_CLASS_TIMESTAMPED_OEM_RECORD_ACCEPTABLE     0x02
#define IPMI_MONITORING_SEL_RECORD_TYPE_CLASS_NON_TIMESTAMPED_OEM_RECORD_ACCEPTABLE 0x04
#define IPMI_MONITORING_SEL_RECORD_TYPE_CLASS_TIMESTAMP                             (IPMI_MONITORING_SEL_RECORD_TYPE_CLASS_SYSTEM_EVENT_RECORD_ACCEPTABLE \
                                                                                     | IPMI_MONITORING_SEL_RECORD_TYPE_CLASS_TIMESTAMPED_OEM_RECORD_ACCEPTABLE)
#define IPMI_MONITORING_SEL_RECORD_TYPE_CLASS_OEM                                   (IPMI_MONITORING_SEL_RECORD_TYPE_CLASS_TIMESTAMPED_OEM_RECORD_ACCEPTABLE \
                                                                                     | IPMI_MONITORING_SEL_RECORD_TYPE_CLASS_NON_TIMESTAMPED_OEM_RECORD_ACCEPTABLE)
#define IPMI_MONITORING_SEL_RECORD_TYPE_CLASS_ALL                                   (IPMI_MONITORING_SEL_RECORD_TYPE_CLASS_SYSTEM_EVENT_RECORD_ACCEPTABLE \
                                                                                     | IPMI_MONITORING_SEL_RECORD_TYPE_CLASS_TIMESTAMPED_OEM_RECORD_ACCEPTABLE \
                                                                                     | IPMI_MONITORING_SEL_RECORD_TYPE_CLASS_NON_TIMESTAMPED_OEM_RECORD_ACCEPTABLE)

struct ipmi_monitoring_sdr_callback
{
  ipmi_monitoring_ctx_t c;
  unsigned int sensor_reading_flags;
  unsigned int *sensor_types;
  unsigned int sensor_types_len;
};

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
_destroy_sel_record (void *x)
{
  struct ipmi_monitoring_sel_record *record;

  assert (x);

  record = (struct ipmi_monitoring_sel_record *)x;

  free (record->event_offset_string);
  free (record);
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

  ipmi_interpret_ctx_destroy (c->interpret_ctx);
    
  /* Note: destroy iterator first */
  if (c->sel_records_itr)
    {
      list_iterator_destroy (c->sel_records_itr);
      c->sel_records_itr = NULL;
    }

  if (c->sel_records)
    {
      list_destroy (c->sel_records);
      c->sel_records = NULL;
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

  if (!(c->sel_records = list_create ((ListDelF)_destroy_sel_record)))
    goto cleanup;

  if (!(c->sensor_readings = list_create ((ListDelF)_destroy_sensor_reading)))
    goto cleanup;

  return (c);

 cleanup:
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
  else if (ipmi_interpret_ctx_errnum (c->interpret_ctx) == IPMI_INTERPRET_ERR_SEL_CONFIG_FILE_DOES_NOT_EXIST)
    c->errnum = IPMI_MONITORING_ERR_SEL_CONFIG_FILE_DOES_NOT_EXIST;
  else if (ipmi_interpret_ctx_errnum (c->interpret_ctx) == IPMI_INTERPRET_ERR_SEL_CONFIG_FILE_PARSE)
    c->errnum = IPMI_MONITORING_ERR_SEL_CONFIG_FILE_PARSE;
  else if (ipmi_interpret_ctx_errnum (c->interpret_ctx) == IPMI_INTERPRET_ERR_SENSOR_CONFIG_FILE_DOES_NOT_EXIST)
    c->errnum = IPMI_MONITORING_ERR_SENSOR_CONFIG_FILE_DOES_NOT_EXIST;
  else if (ipmi_interpret_ctx_errnum (c->interpret_ctx) == IPMI_INTERPRET_ERR_SENSOR_CONFIG_FILE_PARSE)
    c->errnum = IPMI_MONITORING_ERR_SENSOR_CONFIG_FILE_PARSE;
  else
    c->errnum = IPMI_MONITORING_ERR_INTERNAL_ERROR;
}

int
ipmi_monitoring_ctx_sel_config_file (ipmi_monitoring_ctx_t c,
                                     const char *sel_config_file)
{
  if (!c || c->magic != IPMI_MONITORING_MAGIC)
    return (-1);
  
  if (ipmi_interpret_load_sel_config (c->interpret_ctx,
                                      sel_config_file) < 0)
    {
      _interpret_ctx_error_convert (c);
      return (-1);
    }
  
  c->errnum = IPMI_MONITORING_ERR_SUCCESS;
  return (0);
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
                                             IPMI_MONITORING_SENSOR_CONFIG_FILE_LEGACY) < 0)
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
_ipmi_monitoring_interpret_oem_data (ipmi_monitoring_ctx_t c, int enable_interpret_oem_data)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  int rv = -1;

  assert (c);
  assert (c->magic == IPMI_MONITORING_MAGIC);
  assert (c->interpret_ctx);
  assert (c->ipmi_ctx);
  assert (_ipmi_monitoring_initialized);
  
  if (enable_interpret_oem_data)
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

static int
_ipmi_monitoring_sel (ipmi_monitoring_ctx_t c,
                      const char *hostname,
                      struct ipmi_monitoring_ipmi_config *config,
                      unsigned int sel_flags,
                      unsigned int *record_ids,
                      unsigned int record_ids_len,
                      unsigned int *sensor_types,
                      unsigned int sensor_types_len,
                      unsigned int *date_begin,
                      unsigned int *date_end)
{
  unsigned int sdr_create_flags = IPMI_SDR_CACHE_CREATE_FLAGS_DEFAULT;
  int rv = -1;

  assert (c);
  assert (c->magic == IPMI_MONITORING_MAGIC);
  assert (_ipmi_monitoring_initialized);
  assert (!(sel_flags & ~IPMI_MONITORING_SEL_FLAGS_MASK));

  ipmi_monitoring_sel_iterator_destroy (c);

  if (ipmi_monitoring_ipmi_communication_init (c, hostname, config) < 0)
    goto cleanup;

  if (sel_flags & IPMI_MONITORING_SEL_FLAGS_REREAD_SDR_CACHE)
    {
      if (ipmi_monitoring_sdr_cache_flush (c, hostname) < 0)
        goto cleanup;
    }

  if (sel_flags & IPMI_MONITORING_SEL_FLAGS_INTERPRET_OEM_DATA)
    {
      if (_ipmi_monitoring_interpret_oem_data (c, 1) < 0)
        goto cleanup;
    }
  else
    {
      if (_ipmi_monitoring_interpret_oem_data (c, 0) < 0)
        goto cleanup;
    }

  if (sel_flags & IPMI_MONITORING_SEL_FLAGS_ASSUME_MAX_SDR_RECORD_COUNT)
    sdr_create_flags |= IPMI_SDR_CACHE_CREATE_FLAGS_ASSUME_MAX_SDR_RECORD_COUNT;

  if (ipmi_monitoring_sdr_cache_load (c, hostname, sdr_create_flags) < 0)
    goto cleanup;

  if (ipmi_monitoring_sel_init (c) < 0)
    goto cleanup;

  if (ipmi_monitoring_get_sel (c,
                               sel_flags,
                               record_ids,
                               record_ids_len,
                               sensor_types,
                               sensor_types_len,
                               date_begin,
                               date_end) < 0)
    goto cleanup;

  if ((rv = list_count (c->sel_records)) > 0)
    {
      if (!(c->sel_records_itr = list_iterator_create (c->sel_records)))
        {
          IPMI_MONITORING_DEBUG (("list_iterator_create: %s", strerror (errno)));
          c->errnum = IPMI_MONITORING_ERR_INTERNAL_ERROR;
          goto cleanup;
        }
      c->current_sel_record = list_next (c->sel_records_itr);
    }

  ipmi_monitoring_sdr_cache_unload (c);
  ipmi_monitoring_ipmi_communication_cleanup (c);
  ipmi_monitoring_sel_cleanup (c);
  c->errnum = IPMI_MONITORING_ERR_SUCCESS;
  return (rv);

 cleanup:
  ipmi_monitoring_sdr_cache_unload (c);
  ipmi_monitoring_sel_iterator_destroy (c);
  ipmi_monitoring_ipmi_communication_cleanup (c);
  ipmi_monitoring_sel_cleanup (c);
  return (-1);
}

int
ipmi_monitoring_sel_by_record_id (ipmi_monitoring_ctx_t c,
                                  const char *hostname,
                                  struct ipmi_monitoring_ipmi_config *config,
                                  unsigned int sel_flags,
                                  unsigned int *record_ids,
                                  unsigned int record_ids_len,
                                  Ipmi_Monitoring_Callback callback,
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

  if ((sel_flags & ~IPMI_MONITORING_SEL_FLAGS_MASK)
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
          if (record_ids[i] > IPMI_SEL_GET_RECORD_ID_LAST_ENTRY)
            {
              c->errnum = IPMI_MONITORING_ERR_PARAMETERS;
              return (-1);
            }
        }
    }

  c->callback = callback;
  c->callback_data = callback_data;

  rv = _ipmi_monitoring_sel (c,
                             hostname,
                             config,
                             sel_flags,
                             record_ids,
                             record_ids_len,
                             NULL,
                             0,
                             NULL,
                             NULL);

  c->callback_sel_record = NULL;

  return (rv);
}                             

int
ipmi_monitoring_sel_by_sensor_type (ipmi_monitoring_ctx_t c,
                                    const char *hostname,
                                    struct ipmi_monitoring_ipmi_config *config,
                                    unsigned int sel_flags,
                                    unsigned int *sensor_types,
                                    unsigned int sensor_types_len,
                                    Ipmi_Monitoring_Callback callback,
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

  if ((sel_flags & ~IPMI_MONITORING_SEL_FLAGS_MASK)
      || (sensor_types && !sensor_types_len))
    {
      c->errnum = IPMI_MONITORING_ERR_PARAMETERS;
      return (-1);
    }

  c->callback = callback;
  c->callback_data = callback_data;

  rv = _ipmi_monitoring_sel (c,
                             hostname,
                             config,
                             sel_flags,
                             NULL,
                             0,
                             sensor_types,
                             sensor_types_len,
                             NULL,
                             NULL);

  c->callback_sel_record = NULL;

  return (rv);
}

int
_ipmi_monitoring_date_parse (ipmi_monitoring_ctx_t c,
                             const char *date,
                             unsigned int *date_val)
{
  time_t t;
  struct tm tm;

  assert (c);
  assert (c->magic == IPMI_MONITORING_MAGIC);
  assert (_ipmi_monitoring_initialized);
  assert (date);
  assert (date_val);

  /* Posix says individual calls need not clear/set all portions of
   * 'struct tm', thus passing 'struct tm' between functions could
   * have issues.  So we need to memset.
   */
  memset (&tm, '\0', sizeof (struct tm));
  
  if (!strptime (date, "%m/%d/%Y", &tm))
    {
      if (!strptime (date, "%b/%d/%Y", &tm))
        {
          if (!strptime (date, "%m-%d-%Y", &tm))
            {
              if (!strptime (date, "%b-%d-%Y", &tm))
                {
                  c->errnum = IPMI_MONITORING_ERR_PARAMETERS;
                  return (-1);
                }
            }
        }
    }

  /* strptime() does not set tm_isdst.  Set so mktime() will not
   * adjust for daylight savings time.
   */
  tm.tm_isdst = -1;

  if ((t = mktime (&tm)) == (time_t)-1)
    {
      c->errnum = IPMI_MONITORING_ERR_PARAMETERS;
      return (-1);
    }
  
  (*date_val) = t;
  return (0);
}

int
ipmi_monitoring_sel_by_date_range (ipmi_monitoring_ctx_t c,
                                   const char *hostname,
                                   struct ipmi_monitoring_ipmi_config *config,
                                   unsigned int sel_flags,
                                   const char *date_begin,
                                   const char *date_end,
                                   Ipmi_Monitoring_Callback callback,
                                   void *callback_data)
{
  unsigned int date_begin_val;
  unsigned int date_end_val;
  int rv;

  if (!c || c->magic != IPMI_MONITORING_MAGIC)
    return (-1);

  if (!_ipmi_monitoring_initialized)
    {
      c->errnum = IPMI_MONITORING_ERR_LIBRARY_UNINITIALIZED;
      return (-1);
    }

  if (sel_flags & ~IPMI_MONITORING_SEL_FLAGS_MASK)
    {
      c->errnum = IPMI_MONITORING_ERR_PARAMETERS;
      return (-1);
    }

  if (date_begin)
    {
      if (_ipmi_monitoring_date_parse (c,
                                       date_begin,
                                       &date_begin_val) < 0)
        return (-1);
    }
  else
    date_begin_val = 0;
  
  if (date_end)
    {
      if (_ipmi_monitoring_date_parse (c,
                                       date_end,
                                       &date_end_val) < 0)
        return (-1);

      /* Date range input means beginning of begin date to end of end
       * date, so we might need to add seconds to the end to get to
       * the end of the day.
       */
      date_end_val = date_end_val + (24 * 60 * 60);
    }
  else
    date_end_val = time (NULL);

  c->callback = callback;
  c->callback_data = callback_data;

  rv = _ipmi_monitoring_sel (c,
                             hostname,
                             config,
                             sel_flags,
                             NULL,
                             0,
                             NULL,
                             0,
                             &date_begin_val,
                             &date_end_val);

  c->callback_sel_record = NULL;

  return (rv);
}

static int
_list_delete_all (void *x, void *y)
{
  return (1);
}

int
ipmi_monitoring_sel_iterator_first (ipmi_monitoring_ctx_t c)
{
  if (!c || c->magic != IPMI_MONITORING_MAGIC)
    return (-1);

  if (!c->sel_records_itr)
    {
      c->errnum = IPMI_MONITORING_ERR_NO_SENSOR_READINGS;
      return (-1);
    }

  list_iterator_reset (c->sel_records_itr);
  c->current_sel_record = list_next (c->sel_records_itr);
  c->errnum = IPMI_MONITORING_ERR_SUCCESS;
  return (0);
}

int
ipmi_monitoring_sel_iterator_next (ipmi_monitoring_ctx_t c)
{
  if (!c || c->magic != IPMI_MONITORING_MAGIC)
    return (-1);

  if (!c->sel_records_itr)
    {
      c->errnum = IPMI_MONITORING_ERR_NO_SENSOR_READINGS;
      return (-1);
    }

  c->current_sel_record = list_next (c->sel_records_itr);
  c->errnum = IPMI_MONITORING_ERR_SUCCESS;
  return ((c->current_sel_record) ? 1 : 0);
}

void
ipmi_monitoring_sel_iterator_destroy (ipmi_monitoring_ctx_t c)
{
  if (!c || c->magic != IPMI_MONITORING_MAGIC)
    return;

  list_delete_all (c->sensor_readings, _list_delete_all, "dummyvalue");

  if (c->sel_records_itr)
    {
      list_iterator_destroy (c->sel_records_itr);
      c->sel_records_itr = NULL;
    }

  c->current_sel_record = NULL;
}

static int
_ipmi_monitoring_sel_record_common (ipmi_monitoring_ctx_t c,
                                    unsigned int acceptable_record_classes,
                                    struct ipmi_monitoring_sel_record **sel_record)
{
  assert (acceptable_record_classes);
  assert (sel_record);

  if (!c || c->magic != IPMI_MONITORING_MAGIC)
    return (-1);

  if (c->callback_sel_record)
    {
      if ((c->callback_sel_record->record_type_class == IPMI_MONITORING_SEL_RECORD_TYPE_CLASS_SYSTEM_EVENT_RECORD
           && !(acceptable_record_classes & IPMI_MONITORING_SEL_RECORD_TYPE_CLASS_SYSTEM_EVENT_RECORD_ACCEPTABLE))
          || (c->callback_sel_record->record_type_class == IPMI_MONITORING_SEL_RECORD_TYPE_CLASS_TIMESTAMPED_OEM_RECORD
              && !(acceptable_record_classes & IPMI_MONITORING_SEL_RECORD_TYPE_CLASS_TIMESTAMPED_OEM_RECORD_ACCEPTABLE))
          || (c->callback_sel_record->record_type_class == IPMI_MONITORING_SEL_RECORD_TYPE_CLASS_NON_TIMESTAMPED_OEM_RECORD
              && !(acceptable_record_classes & IPMI_MONITORING_SEL_RECORD_TYPE_CLASS_NON_TIMESTAMPED_OEM_RECORD_ACCEPTABLE)))
        {
          c->errnum = IPMI_MONITORING_ERR_SEL_RECORD_DATA_NOT_AVAILABLE;
          return (-1);
        }

      (*sel_record) = c->callback_sel_record;
      return (0);
    }

  if (!c->sel_records_itr)
    {
      c->errnum = IPMI_MONITORING_ERR_NO_SEL_RECORDS;
      return (-1);
    }

  if (!c->current_sel_record)
    {
      c->errnum = IPMI_MONITORING_ERR_SEL_RECORDS_LIST_END;
      return (-1);
    }

  if ((c->current_sel_record->record_type_class == IPMI_MONITORING_SEL_RECORD_TYPE_CLASS_SYSTEM_EVENT_RECORD
       && !(acceptable_record_classes & IPMI_MONITORING_SEL_RECORD_TYPE_CLASS_SYSTEM_EVENT_RECORD_ACCEPTABLE))
      || (c->current_sel_record->record_type_class == IPMI_MONITORING_SEL_RECORD_TYPE_CLASS_TIMESTAMPED_OEM_RECORD
          && !(acceptable_record_classes & IPMI_MONITORING_SEL_RECORD_TYPE_CLASS_TIMESTAMPED_OEM_RECORD_ACCEPTABLE))
      || (c->current_sel_record->record_type_class == IPMI_MONITORING_SEL_RECORD_TYPE_CLASS_NON_TIMESTAMPED_OEM_RECORD
          && !(acceptable_record_classes & IPMI_MONITORING_SEL_RECORD_TYPE_CLASS_NON_TIMESTAMPED_OEM_RECORD_ACCEPTABLE)))
    {
      c->errnum = IPMI_MONITORING_ERR_SEL_RECORD_DATA_NOT_AVAILABLE;
      return (-1);
    }
  
  (*sel_record) = c->current_sel_record;
  return (0);
}

int
ipmi_monitoring_sel_read_record_id (ipmi_monitoring_ctx_t c)
{
  struct ipmi_monitoring_sel_record *sel_record = NULL;

  if (_ipmi_monitoring_sel_record_common (c,
                                          IPMI_MONITORING_SEL_RECORD_TYPE_CLASS_ALL,
                                          &sel_record) < 0)
    return (-1);

  c->errnum = IPMI_MONITORING_ERR_SUCCESS;
  return (sel_record->record_id);
}

int
ipmi_monitoring_sel_read_record_type (ipmi_monitoring_ctx_t c)
{
  struct ipmi_monitoring_sel_record *sel_record = NULL;

  if (_ipmi_monitoring_sel_record_common (c,
                                          IPMI_MONITORING_SEL_RECORD_TYPE_CLASS_ALL,
                                          &sel_record) < 0)
    return (-1);

  c->errnum = IPMI_MONITORING_ERR_SUCCESS;
  return (sel_record->record_type);
}

int
ipmi_monitoring_sel_read_record_type_class (ipmi_monitoring_ctx_t c)
{
  struct ipmi_monitoring_sel_record *sel_record = NULL;

  if (_ipmi_monitoring_sel_record_common (c,
                                          IPMI_MONITORING_SEL_RECORD_TYPE_CLASS_ALL,
                                          &sel_record) < 0)
    return (-1);

  c->errnum = IPMI_MONITORING_ERR_SUCCESS;
  return (sel_record->record_type_class);
}

int
ipmi_monitoring_sel_read_sel_state (ipmi_monitoring_ctx_t c)
{
  struct ipmi_monitoring_sel_record *sel_record = NULL;

  if (_ipmi_monitoring_sel_record_common (c,
                                          IPMI_MONITORING_SEL_RECORD_TYPE_CLASS_ALL,
                                          &sel_record) < 0)
    return (-1);

  c->errnum = IPMI_MONITORING_ERR_SUCCESS;
  return (sel_record->sel_state);
}

int
ipmi_monitoring_sel_read_timestamp (ipmi_monitoring_ctx_t c, unsigned int *timestamp)
{
  struct ipmi_monitoring_sel_record *sel_record = NULL;

  if (_ipmi_monitoring_sel_record_common (c,
                                          IPMI_MONITORING_SEL_RECORD_TYPE_CLASS_TIMESTAMP,
                                          &sel_record) < 0)
    return (-1);

  if (!timestamp)
    {
      c->errnum = IPMI_MONITORING_ERR_PARAMETERS;
      return (-1);
    }

  (*timestamp) = sel_record->timestamp;
  c->errnum = IPMI_MONITORING_ERR_SUCCESS;
  return (0);
}

int
ipmi_monitoring_sel_read_sensor_type (ipmi_monitoring_ctx_t c)
{
  struct ipmi_monitoring_sel_record *sel_record = NULL;

  if (_ipmi_monitoring_sel_record_common (c,
                                          IPMI_MONITORING_SEL_RECORD_TYPE_CLASS_SYSTEM_EVENT_RECORD_ACCEPTABLE,
                                          &sel_record) < 0)
    return (-1);

  c->errnum = IPMI_MONITORING_ERR_SUCCESS;
  return (sel_record->sensor_type);
}

int
ipmi_monitoring_sel_read_sensor_number (ipmi_monitoring_ctx_t c)
{
  struct ipmi_monitoring_sel_record *sel_record = NULL;

  if (_ipmi_monitoring_sel_record_common (c,
                                          IPMI_MONITORING_SEL_RECORD_TYPE_CLASS_SYSTEM_EVENT_RECORD_ACCEPTABLE,
                                          &sel_record) < 0)
    return (-1);

  c->errnum = IPMI_MONITORING_ERR_SUCCESS;
  return (sel_record->sensor_number);
}

char *
ipmi_monitoring_sel_read_sensor_name (ipmi_monitoring_ctx_t c)
{
  struct ipmi_monitoring_sel_record *sel_record = NULL;

  if (_ipmi_monitoring_sel_record_common (c,
                                          IPMI_MONITORING_SEL_RECORD_TYPE_CLASS_SYSTEM_EVENT_RECORD_ACCEPTABLE,
                                          &sel_record) < 0)
    return (NULL);

  c->errnum = IPMI_MONITORING_ERR_SUCCESS;
  return (sel_record->sensor_name);
}

int
ipmi_monitoring_sel_read_event_direction (ipmi_monitoring_ctx_t c)
{
  struct ipmi_monitoring_sel_record *sel_record = NULL;

  if (_ipmi_monitoring_sel_record_common (c,
                                          IPMI_MONITORING_SEL_RECORD_TYPE_CLASS_SYSTEM_EVENT_RECORD_ACCEPTABLE,
                                          &sel_record) < 0)
    return (-1);

  c->errnum = IPMI_MONITORING_ERR_SUCCESS;
  return (sel_record->event_direction);
}

int
ipmi_monitoring_sel_read_event_offset_type (ipmi_monitoring_ctx_t c)
{
  struct ipmi_monitoring_sel_record *sel_record = NULL;

  if (_ipmi_monitoring_sel_record_common (c,
                                          IPMI_MONITORING_SEL_RECORD_TYPE_CLASS_SYSTEM_EVENT_RECORD_ACCEPTABLE,
                                          &sel_record) < 0)
    return (-1);

  c->errnum = IPMI_MONITORING_ERR_SUCCESS;
  return (sel_record->event_offset_type);
}

int
ipmi_monitoring_sel_read_event_offset (ipmi_monitoring_ctx_t c)
{
  struct ipmi_monitoring_sel_record *sel_record = NULL;

  if (_ipmi_monitoring_sel_record_common (c,
                                          IPMI_MONITORING_SEL_RECORD_TYPE_CLASS_SYSTEM_EVENT_RECORD_ACCEPTABLE,
                                          &sel_record) < 0)
    return (-1);

  c->errnum = IPMI_MONITORING_ERR_SUCCESS;
  return (sel_record->event_offset);
}

char *
ipmi_monitoring_sel_read_event_offset_string (ipmi_monitoring_ctx_t c)
{
  struct ipmi_monitoring_sel_record *sel_record = NULL;

  if (_ipmi_monitoring_sel_record_common (c,
                                          IPMI_MONITORING_SEL_RECORD_TYPE_CLASS_SYSTEM_EVENT_RECORD_ACCEPTABLE,
                                          &sel_record) < 0)
    return (NULL);

  c->errnum = IPMI_MONITORING_ERR_SUCCESS;
  return (sel_record->event_offset_string);
}

int
ipmi_monitoring_sel_read_event_type_code (ipmi_monitoring_ctx_t c)
{
  struct ipmi_monitoring_sel_record *sel_record = NULL;

  if (_ipmi_monitoring_sel_record_common (c,
                                          IPMI_MONITORING_SEL_RECORD_TYPE_CLASS_SYSTEM_EVENT_RECORD_ACCEPTABLE,
                                          &sel_record) < 0)
    return (-1);

  c->errnum = IPMI_MONITORING_ERR_SUCCESS;
  return (sel_record->event_type_code);
}

int
ipmi_monitoring_sel_read_event_data (ipmi_monitoring_ctx_t c,
                                     unsigned int *event_data1,
                                     unsigned int *event_data2,
                                     unsigned int *event_data3)
{
  struct ipmi_monitoring_sel_record *sel_record = NULL;

  if (_ipmi_monitoring_sel_record_common (c,
                                          IPMI_MONITORING_SEL_RECORD_TYPE_CLASS_SYSTEM_EVENT_RECORD_ACCEPTABLE,
                                          &sel_record) < 0)
    return (-1);

  if (!event_data1
      && !event_data2
      && !event_data3)
    {
      c->errnum = IPMI_MONITORING_ERR_PARAMETERS;
      return (-1);
    }

  if (event_data1)
    (*event_data1) = sel_record->event_data1;
  if (event_data2)
    (*event_data2) = sel_record->event_data2;
  if (event_data3)
    (*event_data3) = sel_record->event_data3;
  c->errnum = IPMI_MONITORING_ERR_SUCCESS;
  return (0);
}

int
ipmi_monitoring_sel_read_manufacturer_id (ipmi_monitoring_ctx_t c)
{
  struct ipmi_monitoring_sel_record *sel_record = NULL;

  if (_ipmi_monitoring_sel_record_common (c,
                                          IPMI_MONITORING_SEL_RECORD_TYPE_CLASS_TIMESTAMPED_OEM_RECORD_ACCEPTABLE,
                                          &sel_record) < 0)
    return (-1);

  c->errnum = IPMI_MONITORING_ERR_SUCCESS;
  return (sel_record->manufacturer_id);
}

int
ipmi_monitoring_sel_read_oem_data (ipmi_monitoring_ctx_t c,
                                   void *oem_data,
                                   unsigned int oem_data_len)
{
  struct ipmi_monitoring_sel_record *sel_record = NULL;

  if (_ipmi_monitoring_sel_record_common (c,
                                          IPMI_MONITORING_SEL_RECORD_TYPE_CLASS_OEM,
                                          &sel_record) < 0)
    return (-1);

  if (!oem_data
      || !oem_data_len)
    {
      c->errnum = IPMI_MONITORING_ERR_PARAMETERS;
      return (-1);
    }

  if (sel_record->oem_data_len > oem_data_len)
    {
      c->errnum = IPMI_MONITORING_ERR_PARAMETERS;
      return (-1);
    }

  memcpy (oem_data, sel_record->oem_data, sel_record->oem_data_len);
  c->errnum = IPMI_MONITORING_ERR_SUCCESS;
  return ((int)sel_record->oem_data_len);
}

static int
_ipmi_monitoring_sensor_readings_flags_common (ipmi_monitoring_ctx_t c,
                                               const char *hostname,
                                               struct ipmi_monitoring_ipmi_config *config,
                                               unsigned int sensor_reading_flags)
{
  unsigned int sensor_read_ctx_flags = 0;
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
    sensor_read_ctx_flags |= IPMI_SENSOR_READ_FLAGS_BRIDGE_SENSORS;

  if (sensor_reading_flags & IPMI_MONITORING_SENSOR_READING_FLAGS_DISCRETE_READING)
    sensor_read_ctx_flags |= IPMI_SENSOR_READ_FLAGS_DISCRETE_READING;

  if (sensor_reading_flags & IPMI_MONITORING_SENSOR_READING_FLAGS_IGNORE_SCANNING_DISABLED)
    sensor_read_ctx_flags |= IPMI_SENSOR_READ_FLAGS_IGNORE_SCANNING_DISABLED;

  if (sensor_reading_flags & IPMI_MONITORING_SENSOR_READING_FLAGS_ASSUME_BMC_OWNER)
    sensor_read_ctx_flags |= IPMI_SENSOR_READ_FLAGS_ASSUME_BMC_OWNER;

  if (sensor_read_ctx_flags)
    {
      if (ipmi_sensor_read_ctx_set_flags (c->sensor_read_ctx, sensor_read_ctx_flags) < 0)
        {
          IPMI_MONITORING_DEBUG (("ipmi_sensor_read_ctx_set_flags: %s", ipmi_sensor_read_ctx_errormsg (c->sensor_read_ctx)));
          c->errnum = IPMI_MONITORING_ERR_INTERNAL_ERROR;
          goto cleanup;
        }
    }

  if (sensor_reading_flags & IPMI_MONITORING_SENSOR_READING_FLAGS_INTERPRET_OEM_DATA)
    {
      if (_ipmi_monitoring_interpret_oem_data (c, 1) < 0)
        goto cleanup;
    }
  else
    {
      if (_ipmi_monitoring_interpret_oem_data (c, 0) < 0)
        goto cleanup;
    }

  rv = 0;
 cleanup:
  return (rv);
}

/* returns 1 on success, 0 on fallback and do reading, -1 on error */
static int
_ipmi_monitoring_get_sensor_reading_shared (ipmi_monitoring_ctx_t c,
                                            unsigned int sensor_reading_flags,
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
  
  if (ipmi_sdr_parse_record_id_and_type (c->sdr_ctx,
					 NULL,
					 0,
                                         NULL,
                                         &record_type) < 0)
    {
      IPMI_MONITORING_DEBUG (("ipmi_sdr_parse_record_id_and_type: %s",
                              ipmi_sdr_ctx_errormsg (c->sdr_ctx)));
      c->errnum = IPMI_MONITORING_ERR_INTERNAL_ERROR;
      return (-1);
    }
  
  if (record_type != IPMI_SDR_FORMAT_COMPACT_SENSOR_RECORD)
    return (0);

  if (ipmi_sdr_parse_sensor_record_sharing (c->sdr_ctx,
					    NULL,
					    0,
                                            &share_count,
                                            NULL,
                                            NULL,
                                            NULL) < 0)
    {
      IPMI_MONITORING_DEBUG (("ipmi_sdr_parse_sensor_record_sharing: %s",
                              ipmi_sdr_ctx_errormsg (c->sdr_ctx)));
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
                                              i,
                                              sensor_types,
                                              sensor_types_len) < 0)
        return (-1);
    }

  return (1);
}

static int
_ipmi_monitoring_sensor_readings_sdr_callback (ipmi_sdr_ctx_t sdr_ctx,
					       uint8_t record_type,
					       const void *sdr_record,
					       unsigned int sdr_record_len,
					       void *arg)
{
  struct ipmi_monitoring_sdr_callback *sdr_callback_arg;
  int shared_ret = 0;

  assert (sdr_ctx);
  assert (sdr_record);
  assert (sdr_record_len);
  assert (arg);

  sdr_callback_arg = (struct ipmi_monitoring_sdr_callback *)arg;

  if (sdr_callback_arg->sensor_reading_flags & IPMI_MONITORING_SENSOR_READING_FLAGS_SHARED_SENSORS)
    {
      if ((shared_ret = _ipmi_monitoring_get_sensor_reading_shared (sdr_callback_arg->c,
								    sdr_callback_arg->sensor_reading_flags,
								    sdr_callback_arg->sensor_types,
								    sdr_callback_arg->sensor_types_len)) < 0)
	return (-1);
    }
          
  if (!shared_ret)
    {
      if (ipmi_monitoring_get_sensor_reading (sdr_callback_arg->c,
					      sdr_callback_arg->sensor_reading_flags,
					      0,
					      sdr_callback_arg->sensor_types,
					      sdr_callback_arg->sensor_types_len) < 0)
	return (-1);
    }

  return (0);
}

static int
_ipmi_monitoring_sensor_readings_by_record_id (ipmi_monitoring_ctx_t c,
                                               const char *hostname,
                                               struct ipmi_monitoring_ipmi_config *config,
                                               unsigned int sensor_reading_flags,
                                               unsigned int *record_ids,
                                               unsigned int record_ids_len)
{
  unsigned int sdr_create_flags = IPMI_SDR_CACHE_CREATE_FLAGS_DEFAULT;
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

  if (sensor_reading_flags & IPMI_MONITORING_SENSOR_READING_FLAGS_ASSUME_MAX_SDR_RECORD_COUNT)
    sdr_create_flags |= IPMI_SDR_CACHE_CREATE_FLAGS_ASSUME_MAX_SDR_RECORD_COUNT;

  if (ipmi_monitoring_sdr_cache_load (c, hostname, sdr_create_flags) < 0)
    goto cleanup;

  if (!record_ids)
    {
      struct ipmi_monitoring_sdr_callback sdr_callback_arg;

      sdr_callback_arg.c = c;
      sdr_callback_arg.sensor_reading_flags = sensor_reading_flags;
      sdr_callback_arg.sensor_types = NULL;
      sdr_callback_arg.sensor_types_len = 0;

      if (ipmi_sdr_cache_iterate (c->sdr_ctx,
				  _ipmi_monitoring_sensor_readings_sdr_callback,
				  &sdr_callback_arg) < 0)
	{
          IPMI_MONITORING_DEBUG (("ipmi_sdr_cache_iterate: %s", ipmi_sdr_ctx_errormsg (c->sdr_ctx)));
          c->errnum = IPMI_MONITORING_ERR_INTERNAL_ERROR;
          goto cleanup;
        }
    }
  else
    {
      unsigned int i;

      for (i = 0; i < record_ids_len; i++)
        {
          int shared_ret = 0;

          if (ipmi_sdr_cache_search_record_id (c->sdr_ctx, record_ids[i]) < 0)
            {
              if (ipmi_sdr_ctx_errnum (c->sdr_ctx) == IPMI_SDR_ERR_NOT_FOUND)
                {
                  c->errnum = IPMI_MONITORING_ERR_SENSOR_NOT_FOUND;
                  goto cleanup;
                }
              IPMI_MONITORING_DEBUG (("ipmi_sdr_cache_search_record_id: %s", ipmi_sdr_ctx_errormsg (c->sdr_ctx)));
              c->errnum = IPMI_MONITORING_ERR_INTERNAL_ERROR;
              goto cleanup;
            }

          if (sensor_reading_flags & IPMI_MONITORING_SENSOR_READING_FLAGS_SHARED_SENSORS)
            {
              if ((shared_ret = _ipmi_monitoring_get_sensor_reading_shared (c,
                                                                            sensor_reading_flags,
                                                                            NULL,
                                                                            0)) < 0)
                goto cleanup;
            }

          if (!shared_ret)
            {
              if (ipmi_monitoring_get_sensor_reading (c,
                                                      sensor_reading_flags,
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
                                              Ipmi_Monitoring_Callback callback,
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
  unsigned int sdr_create_flags = IPMI_SDR_CACHE_CREATE_FLAGS_DEFAULT;
  struct ipmi_monitoring_sdr_callback sdr_callback_arg;
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

  if (sensor_reading_flags & IPMI_MONITORING_SENSOR_READING_FLAGS_ASSUME_MAX_SDR_RECORD_COUNT)
    sdr_create_flags |= IPMI_SDR_CACHE_CREATE_FLAGS_ASSUME_MAX_SDR_RECORD_COUNT;

  if (ipmi_monitoring_sdr_cache_load (c, hostname, sdr_create_flags) < 0)
    goto cleanup;

  sdr_callback_arg.c = c;
  sdr_callback_arg.sensor_reading_flags = sensor_reading_flags;
  sdr_callback_arg.sensor_types = sensor_types;
  sdr_callback_arg.sensor_types_len = sensor_types_len;

  if (ipmi_sdr_cache_iterate (c->sdr_ctx,
			      _ipmi_monitoring_sensor_readings_sdr_callback,
			      &sdr_callback_arg) < 0)
    {
      IPMI_MONITORING_DEBUG (("ipmi_sdr_cache_iterate: %s", ipmi_sdr_ctx_errormsg (c->sdr_ctx)));
      c->errnum = IPMI_MONITORING_ERR_INTERNAL_ERROR;
      goto cleanup;
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
                                                Ipmi_Monitoring_Callback callback,
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

void
ipmi_monitoring_sensor_iterator_destroy (ipmi_monitoring_ctx_t c)
{
  if (!c || c->magic != IPMI_MONITORING_MAGIC)
    return;

  list_delete_all (c->sensor_readings, _list_delete_all, "dummyvalue");

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

int
ipmi_monitoring_sensor_read_event_reading_type_code (ipmi_monitoring_ctx_t c)
{
  struct ipmi_monitoring_sensor_reading *sensor_reading = NULL;

  if (_ipmi_monitoring_sensor_read_common (c, &sensor_reading) < 0)
    return (-1);

  c->errnum = IPMI_MONITORING_ERR_SUCCESS;
  return (sensor_reading->event_reading_type_code);
}
