/*****************************************************************************\
 *  $Id: ipmi_monitoring_sdr_cache.c,v 1.4.8.2 2007-07-14 00:32:25 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2006 The Regents of the University of California.
 *  Produced at Lawrence Livermore National Laboratory (cf, DISCLAIMER).
 *  Written by Albert Chu <chu11@llnl.gov>
 *  UCRL-CODE-222073
 *
 *  This file is part of Ipmimonitoring, an IPMI sensor monitoring
 *  library.  For details, see http://www.llnl.gov/linux/.
 *
 *  Ipmimonitoring is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by the
 *  Free Software Foundation; either version 2 of the License, or (at your
 *  option) any later version.
 *
 *  Ipmimonitoring is distributed in the hope that it will be useful, but
 *  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 *  for more details.
 *
 *  You should have received a copy of the GNU General Public License along
 *  with Ipmimonitoring; if not, write to the Free Software Foundation, Inc.,
 *  51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA.
\*****************************************************************************/

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <assert.h>
#include <errno.h>

#include "ipmi_monitoring.h"
#include "ipmi_monitoring_debug.h"
#include "ipmi_monitoring_defs.h"
#include "ipmi_monitoring_fiid_wrappers.h"
#include "ipmi_monitoring_ipmi_communication.h"
#include "ipmi_monitoring_sdr_cache.h"

#include "ipmi_sdr_cache.h"

#ifndef NDEBUG
#define IPMI_MONITORING_SDR_CACHE_DIRECTORY      "/tmp"
#else  /* !NDEBUG */
#define IPMI_MONITORING_SDR_CACHE_DIRECTORY      IPMI_MONITORING_SDR_CACHE_DIR
#endif /* !NDEBUG */

#define IPMI_MONITORING_SDR_CACHE_FILENAME       "ipmimonitoringsdrcache"
#define IPMI_MONITORING_SDR_CACHE_INBAND         "localhost"

#define IPMI_MONITORING_MAX_SDR_RECORD_LENGTH    1024
#define IPMI_MONITORING_MAX_RESERVATION_ID_RETRY 4

#define IPMI_MONITORING_BYTES_TO_READ_START      16
#define IPMI_MONITORING_BYTES_TO_READ_DECREMENT  4

char sdr_cache_dir[MAXPATHLEN+1];
int sdr_cache_dir_set = 0;

static int
_ipmi_monitoring_sdr_cache_filename(ipmi_monitoring_ctx_t c,
                                    const char *hostname,
                                    char *buf,
                                    unsigned int buflen)
{
  char *dir;

  assert(c);
  assert(c->magic == IPMI_MONITORING_MAGIC);
  assert(buf);
  assert(buflen);

  if (sdr_cache_dir_set)
    dir = sdr_cache_dir;
  else
    dir = IPMI_MONITORING_SDR_CACHE_DIRECTORY;

  memset(buf, '\0', buflen);
  if (hostname)
    snprintf(buf, buflen - 1, "%s/%s.%s", 
             dir,
             IPMI_MONITORING_SDR_CACHE_FILENAME,
             hostname);
  else
    snprintf(buf, buflen - 1, "%s/%s.%s", 
             dir,
             IPMI_MONITORING_SDR_CACHE_FILENAME, 
             IPMI_MONITORING_SDR_CACHE_INBAND);

  return 0;
}

static int
_ipmi_monitoring_sdr_cache_info(ipmi_monitoring_ctx_t c,
                                uint8_t *version,
                                uint16_t *record_count)
{
  fiid_obj_t obj_cmd_rq = NULL;
  fiid_obj_t obj_cmd_rs = NULL;
  int ret, rv = -1;
  uint64_t val;

  assert(c);
  assert(c->magic == IPMI_MONITORING_MAGIC);
  assert(version);
  assert(record_count);

  if (!(obj_cmd_rq = Fiid_obj_create(c, tmpl_cmd_get_sdr_repository_info_rq)))
    goto cleanup;

  if (!(obj_cmd_rs = Fiid_obj_create(c, tmpl_cmd_get_sdr_repository_info_rs)))
    goto cleanup;

  if (fill_cmd_get_repository_info(obj_cmd_rq) < 0)
    {
      IPMI_MONITORING_DEBUG(("fill_cmd_get_repository_info: %s", strerror(errno)));
      c->errnum = IPMI_MONITORING_ERR_INTERNAL_ERROR;
      goto cleanup;
    }

  if (ipmi_monitoring_ipmi_sendrecv (c,
                                     IPMI_BMC_IPMB_LUN_BMC,
                                     IPMI_NET_FN_STORAGE_RQ,
                                     obj_cmd_rq,
                                     obj_cmd_rs) < 0)
    goto cleanup;
  
  if ((ret = ipmi_check_completion_code_success(obj_cmd_rs)) < 0)
    {
      IPMI_MONITORING_DEBUG(("ipmi_check_completion_code_success: %s", strerror(errno)));
      c->errnum = IPMI_MONITORING_ERR_INTERNAL_ERROR;
      goto cleanup;
    }

  if (!ret)
    {
      if (Fiid_obj_get(c, obj_cmd_rs, "comp_code", &val) < 0)
        IPMI_MONITORING_DEBUG(("fiid_obj_get: %s", strerror(errno)));
      else
        IPMI_MONITORING_DEBUG(("bad completion code: 0x%X", val));
      c->errnum = IPMI_MONITORING_ERR_IPMI_ERROR;
      goto cleanup;
    }

  *version = 0;
  if (Fiid_obj_get(c,
                   obj_cmd_rs,
                   "sdr_version_major",
                   &val) < 0)
    goto cleanup;
  *version = val;

  if (Fiid_obj_get(c,
                   obj_cmd_rs,
                   "sdr_version_minor",
                   &val) < 0)
    goto cleanup;
  *version |= (val << 4);

  *record_count = 0;
  if (Fiid_obj_get(c,
                   obj_cmd_rs,
                   "record_count",
                   &val) < 0)
    goto cleanup;
  *record_count = val;
  
  rv = 0;
 cleanup:
  if (obj_cmd_rq)
    Fiid_obj_destroy(c, obj_cmd_rq);
  if (obj_cmd_rs)
    Fiid_obj_destroy(c, obj_cmd_rs);
  return rv;
}

static int
_ipmi_monitoring_sdr_cache_reservation_id(ipmi_monitoring_ctx_t c,
                                          uint16_t *reservation_id)
{
  fiid_obj_t obj_cmd_rq = NULL;
  fiid_obj_t obj_cmd_rs = NULL;
  int ret, rv = -1;
  uint64_t val;

  assert(c);
  assert(c->magic == IPMI_MONITORING_MAGIC);
  assert(reservation_id);

  if (!(obj_cmd_rq = Fiid_obj_create(c, tmpl_cmd_reserve_sdr_repository_rq)))
    goto cleanup;

  if (!(obj_cmd_rs = Fiid_obj_create(c, tmpl_cmd_reserve_sdr_repository_rs)))
    goto cleanup;

  if (fill_cmd_reserve_sdr_repository(obj_cmd_rq) < 0)
    {
      IPMI_MONITORING_DEBUG(("fill_cmd_reserve_sdr_repository: %s", strerror(errno)));
      c->errnum = IPMI_MONITORING_ERR_INTERNAL_ERROR;
      goto cleanup;
    }

  if (ipmi_monitoring_ipmi_sendrecv (c,
                                     IPMI_BMC_IPMB_LUN_BMC,
                                     IPMI_NET_FN_STORAGE_RQ,
                                     obj_cmd_rq,
                                     obj_cmd_rs) < 0)
    goto cleanup;
  
  if ((ret = ipmi_check_completion_code_success(obj_cmd_rs)) < 0)
    {
      IPMI_MONITORING_DEBUG(("ipmi_check_completion_code_success: %s", strerror(errno)));
      c->errnum = IPMI_MONITORING_ERR_INTERNAL_ERROR;
      goto cleanup;
    }

  if (!ret)
    {
      if (Fiid_obj_get(c, obj_cmd_rs, "comp_code", &val) < 0)
        IPMI_MONITORING_DEBUG(("fiid_obj_get: %s", strerror(errno)));
      else
        IPMI_MONITORING_DEBUG(("bad completion code: 0x%X", val));
      c->errnum = IPMI_MONITORING_ERR_IPMI_ERROR;
      goto cleanup;
    }
  
  *reservation_id = 0;
  if (Fiid_obj_get(c,
                   obj_cmd_rs,
                   "reservation_id",
                   &val) < 0)
    goto cleanup;
  *reservation_id = val;

  rv = 0;
 cleanup:
  if (obj_cmd_rq)
    Fiid_obj_destroy(c, obj_cmd_rq);
  if (obj_cmd_rs)
    Fiid_obj_destroy(c, obj_cmd_rs);
  return rv;
}

static int
_ipmi_monitoring_sdr_cache_get_record(ipmi_monitoring_ctx_t c,
                                      uint16_t record_id,
                                      char *record_buf,
                                      unsigned int record_buf_len,
                                      uint16_t *reservation_id,
                                      uint16_t *next_record_id)
{
  fiid_obj_t obj_cmd_rq = NULL;
  fiid_obj_t obj_cmd_rs = NULL;
  fiid_obj_t obj_sdr_record_header = NULL;
  int32_t sdr_record_header_length = 0;
  int32_t record_length = 0;
  int ret, rv = -1;
  uint32_t bytes_to_read = IPMI_MONITORING_BYTES_TO_READ_START;
  uint32_t offset_into_record = 0;
  unsigned int reservation_id_retry_count = 0;
  uint64_t val;

  assert(c);
  assert(c->magic == IPMI_MONITORING_MAGIC);
  assert(record_buf);
  assert(record_buf_len);
  assert(reservation_id);
  assert(next_record_id);

  if (!(obj_cmd_rq = Fiid_obj_create(c, tmpl_cmd_get_sdr_rq)))
    goto cleanup;

  if (!(obj_cmd_rs = Fiid_obj_create(c, tmpl_cmd_get_sdr_rs)))
    goto cleanup;

  if (!(obj_sdr_record_header = Fiid_obj_create(c, tmpl_sdr_record_header)))
    goto cleanup;

  if ((sdr_record_header_length = Fiid_template_len_bytes(c, tmpl_sdr_record_header)) < 0)
    goto cleanup;

  reservation_id_retry_count = 0;
  while (!record_length)
    {
      char record_header_buf[IPMI_MONITORING_MAX_SDR_RECORD_LENGTH];
      int sdr_record_header_len;

      if (fill_cmd_get_sdr(*reservation_id,
                           record_id,
                           0,
                           sdr_record_header_length,
                           obj_cmd_rq) < 0)
        {
          IPMI_MONITORING_DEBUG(("fill_cmd_get_sdr: %s", strerror(errno)));
          c->errnum = IPMI_MONITORING_ERR_INTERNAL_ERROR;
          goto cleanup;
        }
      
      if (ipmi_monitoring_ipmi_sendrecv (c,
                                         IPMI_BMC_IPMB_LUN_BMC,
                                         IPMI_NET_FN_STORAGE_RQ,
                                         obj_cmd_rq,
                                         obj_cmd_rs) < 0)
        goto cleanup;
      
      if ((ret = ipmi_check_completion_code_success(obj_cmd_rs)) < 0)
        {
          IPMI_MONITORING_DEBUG(("ipmi_check_completion_code_success: %s", strerror(errno)));
          c->errnum = IPMI_MONITORING_ERR_INTERNAL_ERROR;
          goto cleanup;
        }
      
      if (!ret)
        {
          if (Fiid_obj_get(c, obj_cmd_rs, "comp_code", &val) < 0)
            IPMI_MONITORING_DEBUG(("fiid_obj_get: %s", strerror(errno)));

          if (val == IPMI_COMP_CODE_RESERVATION_CANCELLED
              && (reservation_id_retry_count < IPMI_MONITORING_MAX_RESERVATION_ID_RETRY))
            {
              if (_ipmi_monitoring_sdr_cache_reservation_id(c,
                                                            reservation_id) < 0)
                goto cleanup;
              reservation_id_retry_count++;
              continue;
            }

          IPMI_MONITORING_DEBUG(("bad completion code: 0x%X", val));
          c->errnum = IPMI_MONITORING_ERR_IPMI_ERROR;
          goto cleanup;
        }

      if ((sdr_record_header_len = Fiid_obj_get_data(c,
                                                     obj_cmd_rs,
                                                     "record_data",
                                                     record_header_buf,
                                                     IPMI_MONITORING_MAX_SDR_RECORD_LENGTH)) < 0)
        goto cleanup;

      if (sdr_record_header_len < sdr_record_header_length)
        {
          IPMI_MONITORING_DEBUG(("sdr_record_header_len = %d; sdr_record_header_length = %d", sdr_record_header_len, sdr_record_header_length));
          c->errnum = IPMI_MONITORING_ERR_IPMI_ERROR;
          goto cleanup;
        }

      if (Fiid_obj_set_all(c,
                           obj_sdr_record_header,
                           record_header_buf,
                           sdr_record_header_len) < 0)
        goto cleanup;

      if (Fiid_obj_get(c,
                       obj_sdr_record_header,
                       "record_length",
                       &val) < 0)
        goto cleanup;
      
      record_length = val + sdr_record_header_length;
    }
  
  if (record_length > record_buf_len)
    {
      IPMI_MONITORING_DEBUG(("record_length = %d, record_buf_len = %d", record_length, record_buf_len));
      goto cleanup;
    }
  
  if (Fiid_obj_get(c,
                   obj_cmd_rs,
                   "next_record_id",
                   &val) < 0)
    goto cleanup;
  *next_record_id = val;

  reservation_id_retry_count = 0;
  while (offset_into_record < record_length) 
    {
      int32_t record_data_len;

      if ((record_length - offset_into_record) < bytes_to_read)
        bytes_to_read = record_length - offset_into_record;

      if (fill_cmd_get_sdr(*reservation_id,
                           record_id,
                           offset_into_record,
                           bytes_to_read,
                           obj_cmd_rq) < 0)
        {
          IPMI_MONITORING_DEBUG(("fill_cmd_get_sdr: %s", strerror(errno)));
          c->errnum = IPMI_MONITORING_ERR_INTERNAL_ERROR;
          goto cleanup;
        }
      
      if (ipmi_monitoring_ipmi_sendrecv (c,
                                         IPMI_BMC_IPMB_LUN_BMC,
                                         IPMI_NET_FN_STORAGE_RQ,
                                         obj_cmd_rq,
                                         obj_cmd_rs) < 0)
        goto cleanup;
      
      if ((ret = ipmi_check_completion_code_success(obj_cmd_rs)) < 0)
        {
          IPMI_MONITORING_DEBUG(("ipmi_check_completion_code_success: %s", strerror(errno)));
          c->errnum = IPMI_MONITORING_ERR_INTERNAL_ERROR;
          goto cleanup;
        }

      if (!ret)
        {
          if (Fiid_obj_get(c, obj_cmd_rs, "comp_code", &val) < 0)
            IPMI_MONITORING_DEBUG(("fiid_obj_get: %s", strerror(errno)));

          if (val == IPMI_COMP_CODE_RESERVATION_CANCELLED
              && reservation_id_retry_count < IPMI_MONITORING_MAX_RESERVATION_ID_RETRY)
            {
              if (_ipmi_monitoring_sdr_cache_reservation_id(c,
                                                            reservation_id) < 0)
                goto cleanup;
              reservation_id_retry_count++;
              continue;
            }
          else if  (val == IPMI_COMP_CODE_CANNOT_RETURN_REQUESTED_NUMBER_OF_BYTES
                    && bytes_to_read > sdr_record_header_length)
            {
              bytes_to_read -= IPMI_MONITORING_BYTES_TO_READ_DECREMENT;
              if (bytes_to_read < sdr_record_header_length)
                bytes_to_read = sdr_record_header_length;
              IPMI_MONITORING_DEBUG(("lower bytes_to_read: %d", bytes_to_read));
              continue;
            }

          IPMI_MONITORING_DEBUG(("bad completion code: 0x%X", val));
          c->errnum = IPMI_MONITORING_ERR_IPMI_ERROR;
          goto cleanup;
        }
      
      if ((record_data_len = Fiid_obj_get_data(c,
                                               obj_cmd_rs,
                                               "record_data",
                                               record_buf + offset_into_record,
                                               record_buf_len - offset_into_record)) < 0)
        goto cleanup;

      offset_into_record += record_data_len;
    }

  rv = offset_into_record;
 cleanup:
  if (obj_cmd_rq)
    Fiid_obj_destroy(c, obj_cmd_rq);
  if (obj_cmd_rs)
    Fiid_obj_destroy(c, obj_cmd_rs);
  if (obj_sdr_record_header)
    Fiid_obj_destroy(c, obj_sdr_record_header);
  return rv;
}
     
static int
_ipmi_monitoring_sdr_cache_retrieve(ipmi_monitoring_ctx_t c,
                                    const char *hostname,
                                    char *filename)
{
  uint8_t version;
  uint16_t record_count;
  uint16_t reservation_id;
  uint16_t record_id;
  uint16_t next_record_id;

  assert(c);
  assert(c->magic == IPMI_MONITORING_MAGIC);
  assert(c->sc);
  assert(c->comm.dev);
  assert(filename && strlen(filename));
  
  if (_ipmi_monitoring_sdr_cache_info(c,
                                      &version,
                                      &record_count) < 0)
    return -1;

  if (ipmi_sdr_cache_create(c->sc,
                            filename,
                            version,
                            record_count,
                            IPMI_SDR_CACHE_CREATE_FLAGS_DEFAULT,
                            IPMI_SDR_CACHE_VALIDATION_FLAGS_DEFAULT) < 0)
    {
      IPMI_MONITORING_DEBUG(("ipmi_sdr_cache_create: %s", ipmi_sdr_cache_ctx_strerror(ipmi_sdr_cache_ctx_errnum(c->sc))));
      if (ipmi_sdr_cache_ctx_errnum(c->sc) == IPMI_SDR_CACHE_ERR_FILESYSTEM)
        c->errnum = IPMI_MONITORING_ERR_SDR_CACHE_FILESYSTEM;
      else if (ipmi_sdr_cache_ctx_errnum(c->sc) == IPMI_SDR_CACHE_ERR_PERMISSION)
        c->errnum = IPMI_MONITORING_ERR_SDR_CACHE_PERMISSION;
      else
        c->errnum = IPMI_MONITORING_ERR_INTERNAL_ERROR;
      return -1;
    }

  if (_ipmi_monitoring_sdr_cache_reservation_id(c,
                                                &reservation_id) < 0)
    return -1;

  next_record_id = IPMI_SDR_RECORD_ID_FIRST;
  while (next_record_id != IPMI_SDR_RECORD_ID_LAST)
    {
      char record_buf[IPMI_MONITORING_MAX_SDR_RECORD_LENGTH];
      int record_len;
      
      record_id = next_record_id;
      if ((record_len = _ipmi_monitoring_sdr_cache_get_record(c,
                                                              record_id,
                                                              record_buf,
                                                              IPMI_MONITORING_MAX_SDR_RECORD_LENGTH,
                                                              &reservation_id,
                                                              &next_record_id)) < 0)
        return -1;

      if (ipmi_sdr_cache_record_write(c->sc,
                                      record_buf,
                                      record_len) < 0)
        {
          IPMI_MONITORING_DEBUG(("ipmi_sdr_cache_record_write: %s", ipmi_sdr_cache_ctx_strerror(ipmi_sdr_cache_ctx_errnum(c->sc))));
          if (ipmi_sdr_cache_ctx_errnum(c->sc) == IPMI_SDR_CACHE_ERR_CACHE_CREATE_INVALID_RECORD_LENGTH)
            c->errnum = IPMI_MONITORING_ERR_IPMI_ERROR;
          else
            c->errnum = IPMI_MONITORING_ERR_INTERNAL_ERROR;
          return -1;
        }
    }
  
  if (ipmi_sdr_cache_complete(c->sc) < 0)
    {
      IPMI_MONITORING_DEBUG(("ipmi_sdr_cache_complete: %s", ipmi_sdr_cache_ctx_strerror(ipmi_sdr_cache_ctx_errnum(c->sc))));
      c->errnum = IPMI_MONITORING_ERR_INTERNAL_ERROR;
      return -1;
    }
         
  return 0;
}

static int
_ipmi_monitoring_sdr_cache_delete(ipmi_monitoring_ctx_t c,
                                  const char *hostname,
                                  char *filename)
{
  assert(c);
  assert(c->magic == IPMI_MONITORING_MAGIC);
  assert(c->sc);
  assert(c->comm.dev);

  if (ipmi_sdr_cache_delete(c->sc, filename) < 0)
    {
      if (ipmi_sdr_cache_ctx_errnum(c->sc) !=  IPMI_SDR_CACHE_ERR_FILENAME_INVALID)
        {
          IPMI_MONITORING_DEBUG(("ipmi_sdr_cache_delete: %s", ipmi_sdr_cache_ctx_strerror(ipmi_sdr_cache_ctx_errnum(c->sc))));
          if (ipmi_sdr_cache_ctx_errnum(c->sc) == IPMI_SDR_CACHE_ERR_PERMISSION)
            c->errnum = IPMI_MONITORING_ERR_SDR_CACHE_PERMISSION;
          else 
            c->errnum = IPMI_MONITORING_ERR_INTERNAL_ERROR;
          return -1;
        }
    }

  return 0;
}

int
ipmi_monitoring_sdr_cache_load(ipmi_monitoring_ctx_t c,
                               const char *hostname)
{
  char filename[MAXPATHLEN+1];
  
  assert(c);
  assert(c->magic == IPMI_MONITORING_MAGIC);
  assert(c->comm.dev);

  if (_ipmi_monitoring_sdr_cache_filename(c, hostname, filename, MAXPATHLEN + 1) < 0)
    goto cleanup;

  if (!(c->sc))
    {
      if (!(c->sc = ipmi_sdr_cache_ctx_create()))
        {
          IPMI_MONITORING_DEBUG(("ipmi_sdr_cache_create: %s", strerror(errno)));
          if (errno == EPERM || errno == EACCES)
            c->errnum = IPMI_MONITORING_ERR_PERMISSION;
          else
            c->errnum = IPMI_MONITORING_ERR_INTERNAL_ERROR;
          goto cleanup;
        }
    }

  if (ipmi_sdr_cache_open(c->sc, filename) < 0)
    {
      if (ipmi_sdr_cache_ctx_errnum(c->sc) == IPMI_SDR_CACHE_ERR_CACHE_READ_CACHE_DOES_NOT_EXIST)
        {
          if (_ipmi_monitoring_sdr_cache_retrieve(c, hostname, filename) < 0)
            goto cleanup;
        }
      else if (ipmi_sdr_cache_ctx_errnum(c->sc) == IPMI_SDR_CACHE_ERR_CACHE_INVALID)
        {
          if (_ipmi_monitoring_sdr_cache_delete(c, hostname, filename) < 0)
            goto cleanup;

          if (_ipmi_monitoring_sdr_cache_retrieve(c, hostname, filename) < 0)
            goto cleanup;
        }
      else if (ipmi_sdr_cache_ctx_errnum(c->sc) == IPMI_SDR_CACHE_ERR_FILESYSTEM)
        {
          c->errnum = IPMI_MONITORING_ERR_SDR_CACHE_FILESYSTEM;
          goto cleanup;
        }
      else if (ipmi_sdr_cache_ctx_errnum(c->sc) == IPMI_SDR_CACHE_ERR_PERMISSION)
        {
          c->errnum = IPMI_MONITORING_ERR_SDR_CACHE_PERMISSION;
          goto cleanup;
        }
      else
        {
          IPMI_MONITORING_DEBUG(("ipmi_sdr_cache_open: %s", ipmi_sdr_cache_ctx_strerror(ipmi_sdr_cache_ctx_errnum(c->sc))));
          c->errnum = IPMI_MONITORING_ERR_INTERNAL_ERROR;
          goto cleanup;
        }

      /* 2nd try after the sdr was retrieved*/
      if (ipmi_sdr_cache_open(c->sc, filename) < 0)
        {
          if (ipmi_sdr_cache_ctx_errnum(c->sc) == IPMI_SDR_CACHE_ERR_FILESYSTEM)
            {
              c->errnum = IPMI_MONITORING_ERR_SDR_CACHE_FILESYSTEM;
              goto cleanup;
            }
          else if (ipmi_sdr_cache_ctx_errnum(c->sc) == IPMI_SDR_CACHE_ERR_PERMISSION)
            {
              c->errnum = IPMI_MONITORING_ERR_SDR_CACHE_PERMISSION;
              goto cleanup;
            }
          else
            {
              IPMI_MONITORING_DEBUG(("ipmi_sdr_cache_open: %s", ipmi_sdr_cache_ctx_strerror(ipmi_sdr_cache_ctx_errnum(c->sc))));
              c->errnum = IPMI_MONITORING_ERR_INTERNAL_ERROR;
              goto cleanup;
            }
        }
    }

  return 0;

 cleanup:
  if (c->sc)
    {
      if (strlen(filename))
        ipmi_sdr_cache_delete(c->sc, filename);
      ipmi_sdr_cache_ctx_destroy(c->sc);
      c->sc = NULL;
    }
  return -1;
}

int
ipmi_monitoring_sdr_cache_unload(ipmi_monitoring_ctx_t c)
{
  assert(c);
  assert(c->magic == IPMI_MONITORING_MAGIC);

  if (c->sc)
    {
      ipmi_sdr_cache_close(c->sc);
      ipmi_sdr_cache_ctx_destroy(c->sc);
      c->sc = NULL;
    }
  return 0;
}

int
ipmi_monitoring_sdr_cache_flush(ipmi_monitoring_ctx_t c,
                                const char *hostname)
{
  char filename[MAXPATHLEN+1];
  
  assert(c);
  assert(c->magic == IPMI_MONITORING_MAGIC);

  if (_ipmi_monitoring_sdr_cache_filename(c, hostname, filename, MAXPATHLEN + 1) < 0)
    goto cleanup;

  if (!(c->sc))
    {
      if (!(c->sc = ipmi_sdr_cache_ctx_create()))
        {
          IPMI_MONITORING_DEBUG(("ipmi_sdr_cache_create: %s", strerror(errno)));
          if (errno == EPERM || errno == EACCES)
            c->errnum = IPMI_MONITORING_ERR_PERMISSION;
          else
            c->errnum = IPMI_MONITORING_ERR_INTERNAL_ERROR;
          goto cleanup;
        }
    }

  if (_ipmi_monitoring_sdr_cache_delete(c, hostname, filename) < 0)
    goto cleanup;

  return 0;

 cleanup:
  if (c->sc)
    {
      ipmi_sdr_cache_ctx_destroy(c->sc);
      c->sc = NULL;
    }
  return -1;
}
