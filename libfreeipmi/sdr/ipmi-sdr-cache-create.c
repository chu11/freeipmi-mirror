/*****************************************************************************\
 *  $Id: ipmi-sdr-cache-create.c,v 1.42 2010-02-08 22:09:40 chu11 Exp $
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
#if HAVE_FCNTL_H
#include <fcntl.h>
#endif /* HAVE_FCNTL_H */
#if HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */
#include <assert.h>
#include <errno.h>

#include "freeipmi/sdr/ipmi-sdr.h"
#include "freeipmi/api/ipmi-sdr-repository-cmds-api.h"
#include "freeipmi/cmds/ipmi-sdr-repository-cmds.h"
#include "freeipmi/fiid/fiid.h"
#include "freeipmi/debug/ipmi-debug.h"
#include "freeipmi/record-format/ipmi-sdr-record-format.h"
#include "freeipmi/spec/ipmi-comp-code-spec.h"
#include "freeipmi/util/ipmi-util.h"

#include "ipmi-sdr-common.h"
#include "ipmi-sdr-defs.h"
#include "ipmi-sdr-trace.h"
#include "ipmi-sdr-util.h"

#include "libcommon/ipmi-fiid-util.h"

#include "freeipmi-portability.h"
#include "debug-util.h"
#include "fd.h"

#define IPMI_SDR_CACHE_MAX_RESERVATION_ID_RETRY 4

/* achu: bytes to read start = 16 specifically chosen to 16 because it
 * appears most motherboards can handle 16.  Many cannot handle larger
 * numbers like 32.
 */
#define IPMI_SDR_CACHE_BYTES_TO_READ_START      16
#define IPMI_SDR_CACHE_BYTES_TO_READ_DECREMENT  4

static int
_sdr_cache_header_write (ipmi_sdr_ctx_t ctx,
                         ipmi_ctx_t ipmi_ctx,
                         int fd,
                         unsigned int *total_bytes_written,
                         uint8_t sdr_version,
                         uint16_t record_count,
                         uint32_t most_recent_addition_timestamp,
                         uint32_t most_recent_erase_timestamp)
{
  char sdr_cache_magic_buf[4];
  char sdr_cache_version_buf[4];
  char record_count_buf[2];
  char most_recent_addition_timestamp_buf[4];
  char most_recent_erase_timestamp_buf[4];
  uint8_t header_checksum_buf[512];
  unsigned int header_checksum_buf_len = 0;
  uint8_t header_checksum;
  ssize_t n;

  assert (ctx);
  assert (ctx->magic == IPMI_SDR_CTX_MAGIC);
  assert (ipmi_ctx);
  assert (fd);
  assert (total_bytes_written);

  sdr_cache_magic_buf[0] = IPMI_SDR_CACHE_FILE_MAGIC_0;
  sdr_cache_magic_buf[1] = IPMI_SDR_CACHE_FILE_MAGIC_1;
  sdr_cache_magic_buf[2] = IPMI_SDR_CACHE_FILE_MAGIC_2;
  sdr_cache_magic_buf[3] = IPMI_SDR_CACHE_FILE_MAGIC_3;

  if ((n = fd_write_n (fd, sdr_cache_magic_buf, 4)) < 0)
    {
      SDR_ERRNO_TO_SDR_ERRNUM (ctx, errno);
      return (-1);
    }
  if (n != 4)
    {
      SDR_SET_ERRNUM (ctx, IPMI_SDR_ERR_SYSTEM_ERROR);
      return (-1);
    }
  (*total_bytes_written) += 4;

  memcpy(&header_checksum_buf[header_checksum_buf_len], sdr_cache_magic_buf, 4);
  header_checksum_buf_len += 4;

  sdr_cache_version_buf[0] = IPMI_SDR_CACHE_FILE_VERSION_1_2_0;
  sdr_cache_version_buf[1] = IPMI_SDR_CACHE_FILE_VERSION_1_2_1;
  sdr_cache_version_buf[2] = IPMI_SDR_CACHE_FILE_VERSION_1_2_2;
  sdr_cache_version_buf[3] = IPMI_SDR_CACHE_FILE_VERSION_1_2_3;

  if ((n = fd_write_n (fd, sdr_cache_version_buf, 4)) < 0)
    {
      SDR_ERRNO_TO_SDR_ERRNUM (ctx, errno);
      return (-1);
    }
  if (n != 4)
    {
      SDR_SET_ERRNUM (ctx, IPMI_SDR_ERR_SYSTEM_ERROR);
      return (-1);
    }
  (*total_bytes_written) += 4;

  memcpy(&header_checksum_buf[header_checksum_buf_len], sdr_cache_version_buf, 4);
  header_checksum_buf_len += 4;

  if ((n = fd_write_n (fd, (char *)&sdr_version, 1)) < 0)
    {
      SDR_ERRNO_TO_SDR_ERRNUM (ctx, errno);
      return (-1);
    }
  if (n != 1)
    {
      SDR_SET_ERRNUM (ctx, IPMI_SDR_ERR_SYSTEM_ERROR);
      return (-1);
    }
  (*total_bytes_written) += 1;

  memcpy(&header_checksum_buf[header_checksum_buf_len], &sdr_version, 1);
  header_checksum_buf_len += 1;

  /* Store record count little-endian */
  record_count_buf[0] = (record_count & 0x00FF);
  record_count_buf[1] = (record_count & 0xFF00) >> 8;

  if ((n = fd_write_n (fd, record_count_buf, 2)) < 0)
    {
      SDR_ERRNO_TO_SDR_ERRNUM (ctx, errno);
      return (-1);
    }
  if (n != 2)
    {
      SDR_SET_ERRNUM (ctx, IPMI_SDR_ERR_SYSTEM_ERROR);
      return (-1);
    }
  (*total_bytes_written) += 2;

  memcpy(&header_checksum_buf[header_checksum_buf_len], record_count_buf, 2);
  header_checksum_buf_len += 2;

  /* Store most recent addition timestamp little-endian */
  most_recent_addition_timestamp_buf[0] = (most_recent_addition_timestamp & 0x000000FF);
  most_recent_addition_timestamp_buf[1] = (most_recent_addition_timestamp & 0x0000FF00) >> 8;
  most_recent_addition_timestamp_buf[2] = (most_recent_addition_timestamp & 0x00FF0000) >> 16;
  most_recent_addition_timestamp_buf[3] = (most_recent_addition_timestamp & 0xFF000000) >> 24;

  if ((n = fd_write_n (fd, most_recent_addition_timestamp_buf, 4)) < 0)
    {
      SDR_ERRNO_TO_SDR_ERRNUM (ctx, errno);
      return (-1);
    }
  if (n != 4)
    {
      SDR_SET_ERRNUM (ctx, IPMI_SDR_ERR_SYSTEM_ERROR);
      return (-1);
    }
  (*total_bytes_written) += 4;

  memcpy(&header_checksum_buf[header_checksum_buf_len], most_recent_addition_timestamp_buf, 4);
  header_checksum_buf_len += 4;

  /* Store most recent erase timestamp little-endian */
  most_recent_erase_timestamp_buf[0] = (most_recent_erase_timestamp & 0x000000FF);
  most_recent_erase_timestamp_buf[1] = (most_recent_erase_timestamp & 0x0000FF00) >> 8;
  most_recent_erase_timestamp_buf[2] = (most_recent_erase_timestamp & 0x00FF0000) >> 16;
  most_recent_erase_timestamp_buf[3] = (most_recent_erase_timestamp & 0xFF000000) >> 24;

  if ((n = fd_write_n (fd, most_recent_erase_timestamp_buf, 4)) < 0)
    {
      SDR_ERRNO_TO_SDR_ERRNUM (ctx, errno);
      return (-1);
    }
  if (n != 4)
    {
      SDR_SET_ERRNUM (ctx, IPMI_SDR_ERR_SYSTEM_ERROR);
      return (-1);
    }
  (*total_bytes_written) += 4;

  memcpy(&header_checksum_buf[header_checksum_buf_len], most_recent_erase_timestamp_buf, 4);
  header_checksum_buf_len += 4;

  header_checksum = ipmi_checksum (header_checksum_buf, header_checksum_buf_len);

  if ((n = fd_write_n (fd, (char *)&header_checksum, 1)) < 0)
    {
      SDR_ERRNO_TO_SDR_ERRNUM (ctx, errno);
      return (-1);
    }
  if (n != 1)
    {
      SDR_SET_ERRNUM (ctx, IPMI_SDR_ERR_SYSTEM_ERROR);
      return (-1);
    }
  (*total_bytes_written) += 1;

  return (0);
}

static int
_sdr_cache_trailer_write (ipmi_sdr_ctx_t ctx,
			  ipmi_ctx_t ipmi_ctx,
			  int fd,
			  unsigned int total_bytes_written,
			  uint8_t trailer_checksum)
{
  char total_bytes_written_buf[4];
  ssize_t n;

  assert (ctx);
  assert (ctx->magic == IPMI_SDR_CTX_MAGIC);
  assert (ipmi_ctx);
  assert (fd);

  /* + 4 for this value, + 1 for checksum at end */
  total_bytes_written += 4;
  total_bytes_written += 1;

  total_bytes_written_buf[0] = (total_bytes_written & 0x000000FF);
  total_bytes_written_buf[1] = (total_bytes_written & 0x0000FF00) >> 8;
  total_bytes_written_buf[2] = (total_bytes_written & 0x00FF0000) >> 16;
  total_bytes_written_buf[3] = (total_bytes_written & 0xFF000000) >> 24;

  if ((n = fd_write_n (fd, total_bytes_written_buf, 4)) < 0)
    {
      SDR_ERRNO_TO_SDR_ERRNUM (ctx, errno);
      return (-1);
    }
  if (n != 4)
    {
      SDR_SET_ERRNUM (ctx, IPMI_SDR_ERR_SYSTEM_ERROR);
      return (-1);
    }

  trailer_checksum = ipmi_checksum_final (total_bytes_written_buf, 4, trailer_checksum);

  if ((n = fd_write_n (fd, (char *)&trailer_checksum, 1)) < 0)
    {
      SDR_ERRNO_TO_SDR_ERRNUM (ctx, errno);
      return (-1);
    }
  if (n != 1)
    {
      SDR_SET_ERRNUM (ctx, IPMI_SDR_ERR_SYSTEM_ERROR);
      return (-1);
    }

  return (0);
}

static int
_sdr_cache_reservation_id (ipmi_sdr_ctx_t ctx,
                           ipmi_ctx_t ipmi_ctx,
                           uint16_t *reservation_id)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  int rv = -1;

  assert (ctx);
  assert (ctx->magic == IPMI_SDR_CTX_MAGIC);
  assert (ipmi_ctx);
  assert (reservation_id);

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_reserve_sdr_repository_rs)))
    {
      SDR_ERRNO_TO_SDR_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (ipmi_cmd_reserve_sdr_repository (ipmi_ctx, obj_cmd_rs) < 0)
    {
      SDR_SET_ERRNUM (ctx, IPMI_SDR_ERR_IPMI_ERROR);
      goto cleanup;
    }

  *reservation_id = 0;
  if (FIID_OBJ_GET (obj_cmd_rs,
                    "reservation_id",
                    &val) < 0)
    {
      SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_cmd_rs);
      goto cleanup;
    }
  *reservation_id = val;

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static int
_sdr_cache_get_record (ipmi_sdr_ctx_t ctx,
                       ipmi_ctx_t ipmi_ctx,
                       uint16_t record_id,
                       void *record_buf,
                       unsigned int record_buf_len,
                       uint16_t *reservation_id,
                       uint16_t *next_record_id)
{
  fiid_obj_t obj_cmd_rs = NULL;
  fiid_obj_t obj_sdr_record_header = NULL;
  int sdr_record_header_length = 0;
  int sdr_record_len = 0;
  unsigned int record_length = 0;
  int rv = -1;
  unsigned int bytes_to_read = IPMI_SDR_CACHE_BYTES_TO_READ_START;
  unsigned int offset_into_record = 0;
  unsigned int reservation_id_retry_count = 0;
  uint8_t temp_record_buf[IPMI_SDR_MAX_RECORD_LENGTH];
  uint64_t val;

  assert (ctx);
  assert (ctx->magic == IPMI_SDR_CTX_MAGIC);
  assert (ipmi_ctx);
  assert (record_buf);
  assert (record_buf_len);
  assert (reservation_id);
  assert (next_record_id);

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_get_sdr_rs)))
    {
      SDR_ERRNO_TO_SDR_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (!(obj_sdr_record_header = fiid_obj_create (tmpl_sdr_record_header)))
    {
      SDR_ERRNO_TO_SDR_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if ((sdr_record_header_length = fiid_template_len_bytes (tmpl_sdr_record_header)) < 0)
    {
      SDR_ERRNO_TO_SDR_ERRNUM (ctx, errno);
      goto cleanup;
    }
  
  /* achu:
   *
   * Many motherboards now allow you to read the full SDR record, try
   * that first.  If it fails for any reason, bail and try to read via
   * partial reads.
   */
 
  reservation_id_retry_count = 0;
  while (!offset_into_record)
    {
      if (ipmi_cmd_get_sdr (ipmi_ctx,
			    *reservation_id,
			    record_id,
			    0,
			    IPMI_SDR_READ_ENTIRE_RECORD_BYTES_TO_READ,
			    obj_cmd_rs) < 0)
	{
          if (ipmi_ctx_errnum (ipmi_ctx) == IPMI_ERR_BAD_COMPLETION_CODE)
	    {
              uint8_t comp_code;
	      
              if (FIID_OBJ_GET (obj_cmd_rs,
                                "comp_code",
                                &val) < 0)
                {
                  SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_cmd_rs);
                  goto cleanup;
                }
              comp_code = val;

              if (comp_code == IPMI_COMP_CODE_RESERVATION_CANCELLED
                  && (reservation_id_retry_count < IPMI_SDR_CACHE_MAX_RESERVATION_ID_RETRY))
                {
                  if (_sdr_cache_reservation_id (ctx,
                                                 ipmi_ctx,
                                                 reservation_id) < 0)
                    goto cleanup;
                  reservation_id_retry_count++;
                  continue;
                }
            }
	  
	  goto partial_read;
	}
  
      if ((sdr_record_len = fiid_obj_get_data (obj_cmd_rs,
					       "record_data",
					       temp_record_buf,
					       IPMI_SDR_MAX_RECORD_LENGTH)) < 0)
	{
	  SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_cmd_rs);
	  goto cleanup;
	}
      
      /* Assume this is an "IPMI Error", fall through to partial reads */
      if (sdr_record_len < sdr_record_header_length)
        goto partial_read;

      /* 
       * IPMI Workaround (achu)
       *
       * Discovered on Xyratex HB-F8-SRAY
       *
       * For some reason reading the entire SDR record (with
       * IPMI_SDR_READ_ENTIRE_RECORD_BYTES_TO_READ) the response
       * returns fewer bytes than the actual length of the record.
       * However, when reading with partial reads things ultimately
       * succeed.  If we notice the length is off, we fall out and do
       * a partial read.
       */
      if ((((uint8_t)temp_record_buf[IPMI_SDR_RECORD_LENGTH_INDEX]) + IPMI_SDR_RECORD_HEADER_LENGTH) > sdr_record_len)
	goto partial_read;
  
      if (sdr_record_len > record_buf_len)
	{
	  SDR_SET_ERRNUM (ctx, IPMI_SDR_ERR_INTERNAL_ERROR);
	  goto cleanup;
	}
  
      if (FIID_OBJ_GET (obj_cmd_rs,
			"next_record_id",
			&val) < 0)
	{
	  SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_cmd_rs);
	  goto cleanup;
	}
      *next_record_id = val;

      memcpy (record_buf, temp_record_buf, sdr_record_len);
      offset_into_record += sdr_record_len;
      goto out;
    }

 partial_read:

  reservation_id_retry_count = 0;
  while (!record_length)
    {
      uint8_t record_header_buf[IPMI_SDR_MAX_RECORD_LENGTH];
      int sdr_record_header_len;

      if (ipmi_cmd_get_sdr (ipmi_ctx,
                            *reservation_id,
                            record_id,
                            0,
                            sdr_record_header_length,
                            obj_cmd_rs) < 0)
        {
          if (ipmi_ctx_errnum (ipmi_ctx) != IPMI_ERR_BAD_COMPLETION_CODE)
            {
              SDR_SET_ERRNUM (ctx, IPMI_SDR_ERR_IPMI_ERROR);
              goto cleanup;
            }
          else
            {
              uint8_t comp_code;

              if (FIID_OBJ_GET (obj_cmd_rs,
                                "comp_code",
                                &val) < 0)
                {
                  SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_cmd_rs);
                  goto cleanup;
                }
              comp_code = val;

              if (comp_code == IPMI_COMP_CODE_RESERVATION_CANCELLED
                  && (reservation_id_retry_count < IPMI_SDR_CACHE_MAX_RESERVATION_ID_RETRY))
                {
                  if (_sdr_cache_reservation_id (ctx,
                                                 ipmi_ctx,
                                                 reservation_id) < 0)
                    goto cleanup;
                  reservation_id_retry_count++;
                  continue;
                }

              SDR_SET_ERRNUM (ctx, IPMI_SDR_ERR_IPMI_ERROR);
              goto cleanup;
            }
        }

      if ((sdr_record_header_len = fiid_obj_get_data (obj_cmd_rs,
                                                      "record_data",
                                                      record_header_buf,
                                                      IPMI_SDR_MAX_RECORD_LENGTH)) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_cmd_rs);
          goto cleanup;
        }

      if (sdr_record_header_len < sdr_record_header_length)
        {
          SDR_SET_ERRNUM (ctx, IPMI_SDR_ERR_IPMI_ERROR);
          goto cleanup;
        }

      if (fiid_obj_set_all (obj_sdr_record_header,
                            record_header_buf,
                            sdr_record_header_len) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record_header);
          goto cleanup;
        }

      if (FIID_OBJ_GET (obj_sdr_record_header,
                        "record_length",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record_header);
          goto cleanup;
        }

      if (sdr_record_header_len > record_buf_len)
	{
	  SDR_SET_ERRNUM (ctx, IPMI_SDR_ERR_INTERNAL_ERROR);
	  goto cleanup;
	}

      /* copy header into buf */
      memcpy (record_buf, record_header_buf, sdr_record_header_len);
      offset_into_record += sdr_record_header_len;
      record_length = val + sdr_record_header_length;
    }

  if (record_length > record_buf_len)
    {
      SDR_SET_ERRNUM (ctx, IPMI_SDR_ERR_INTERNAL_ERROR);
      goto cleanup;
    }

  if (FIID_OBJ_GET (obj_cmd_rs,
                    "next_record_id",
                    &val) < 0)
    {
      SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_cmd_rs);
      goto cleanup;
    }
  *next_record_id = val;

  reservation_id_retry_count = 0;
  while (offset_into_record < record_length)
    {
      int record_data_len;

      if ((record_length - offset_into_record) < bytes_to_read)
        bytes_to_read = record_length - offset_into_record;

      if (ipmi_cmd_get_sdr (ipmi_ctx,
                            *reservation_id,
                            record_id,
                            offset_into_record,
                            bytes_to_read,
                            obj_cmd_rs) < 0)
        {
          if (ipmi_ctx_errnum (ipmi_ctx) != IPMI_ERR_BAD_COMPLETION_CODE)
            {
              SDR_SET_ERRNUM (ctx, IPMI_SDR_ERR_IPMI_ERROR);
              goto cleanup;
            }
          else
            {
              uint8_t comp_code;

              if (FIID_OBJ_GET (obj_cmd_rs,
                                "comp_code",
                                &val) < 0)
                {
                  SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_cmd_rs);
                  goto cleanup;
                }
              comp_code = val;

              if (comp_code == IPMI_COMP_CODE_RESERVATION_CANCELLED
                  && (reservation_id_retry_count < IPMI_SDR_CACHE_MAX_RESERVATION_ID_RETRY))
                {
                  if (_sdr_cache_reservation_id (ctx,
                                                 ipmi_ctx,
                                                 reservation_id) < 0)
                    goto cleanup;
                  reservation_id_retry_count++;
                  continue;
                }
              else if  ((comp_code == IPMI_COMP_CODE_CANNOT_RETURN_REQUESTED_NUMBER_OF_BYTES
                         || comp_code == IPMI_COMP_CODE_UNSPECIFIED_ERROR)
                        && bytes_to_read > sdr_record_header_length)
                {
                  bytes_to_read -= IPMI_SDR_CACHE_BYTES_TO_READ_DECREMENT;
                  if (bytes_to_read < sdr_record_header_length)
                    bytes_to_read = sdr_record_header_length;
                  continue;
                }

              SDR_SET_ERRNUM (ctx, IPMI_SDR_ERR_IPMI_ERROR);
              goto cleanup;
            }
        }

      if ((record_data_len = fiid_obj_get_data (obj_cmd_rs,
                                                "record_data",
                                                record_buf + offset_into_record,
                                                record_buf_len - offset_into_record)) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_cmd_rs);
          goto cleanup;
        }

      offset_into_record += record_data_len;
    }

 out:
  rv = offset_into_record;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  fiid_obj_destroy (obj_sdr_record_header);
  return (rv);
}

static int
_sdr_cache_record_write (ipmi_sdr_ctx_t ctx,
                         int fd,
                         unsigned int *total_bytes_written,
                         uint16_t *record_ids,
                         unsigned int *record_ids_count,
                         uint8_t *buf,
                         unsigned int buflen,
			 uint8_t *trailer_checksum)
{
  ssize_t n;

  assert (ctx);
  assert (ctx->magic == IPMI_SDR_CTX_MAGIC);
  assert (fd);
  assert (total_bytes_written);
  assert (!record_ids || (record_ids && record_ids_count));
  assert (buf);
  assert (buflen);
  assert (trailer_checksum);

  /* Record header bytes are 5 bytes */
  if (buflen < IPMI_SDR_RECORD_HEADER_LENGTH)
    {
      SDR_SET_ERRNUM (ctx, IPMI_SDR_ERR_CACHE_CREATE_INVALID_RECORD_LENGTH);
      return (-1);
    }

  /* Record Length plus the header bytes should match buflen. */
  if ((((uint8_t)buf[IPMI_SDR_RECORD_LENGTH_INDEX]) + IPMI_SDR_RECORD_HEADER_LENGTH) != buflen)
    {
      /* 
       * IPMI Workaround (achu)
       *
       * Discovered on HP Proliant DL585G7
       *
       * When reading an entire SDR record (using
       * IPMI_SDR_READ_ENTIRE_RECORD_BYTES_TO_READ), sometimes records
       * are returned with an excess of bytes.  The following
       * truncates the buffer length to the correct size.
       */
      if ((((uint8_t)buf[IPMI_SDR_RECORD_LENGTH_INDEX]) + IPMI_SDR_RECORD_HEADER_LENGTH) <= buflen)
	buflen = ((uint8_t)buf[IPMI_SDR_RECORD_LENGTH_INDEX]) + IPMI_SDR_RECORD_HEADER_LENGTH;
      else
	{
	  SDR_SET_ERRNUM (ctx, IPMI_SDR_ERR_CACHE_CREATE_INVALID_RECORD_LENGTH);
	  return (-1);
	}
    }

  if (record_ids)
    {
      uint16_t record_id;
      unsigned int i;

      /* Record ID stored little endian */
      record_id = ((uint16_t)buf[IPMI_SDR_RECORD_ID_INDEX_LS] & 0xFF);
      record_id |= ((uint16_t)buf[IPMI_SDR_RECORD_ID_INDEX_MS] & 0xFF) << 8;

      for (i = 0; i < *record_ids_count; i++)
        {
          if (record_ids[i] == record_id)
            {
              SDR_SET_ERRNUM (ctx, IPMI_SDR_ERR_CACHE_CREATE_DUPLICATE_RECORD_ID);
              return (-1);
            }
        }
      record_ids[*record_ids_count] = record_id;
      (*record_ids_count)++;
    }

  if ((n = fd_write_n (fd, buf, buflen)) < 0)
    {
      SDR_ERRNO_TO_SDR_ERRNUM (ctx, errno);
      return (-1);
    }

  if (n != buflen)
    {
      /* Try to lseek back to our original spot */
      lseek (fd, SEEK_SET, *total_bytes_written);
      SDR_SET_ERRNUM (ctx, IPMI_SDR_ERR_SYSTEM_ERROR);
      return (-1);
    }
  (*total_bytes_written) += buflen;

  (*trailer_checksum) = ipmi_checksum_incremental (buf, buflen, (*trailer_checksum));

  return (0);

}

int
ipmi_sdr_cache_create (ipmi_sdr_ctx_t ctx,
                       ipmi_ctx_t ipmi_ctx,
                       const char *filename,
                       int cache_create_flags,
                       Ipmi_Sdr_Cache_Create_Callback create_callback,
                       void *create_callback_data)
{
  int open_flags;
  uint8_t sdr_version;
  uint16_t record_count, reservation_id, record_id, next_record_id;
  uint32_t most_recent_addition_timestamp, most_recent_erase_timestamp;
  unsigned int record_count_written = 0;
  unsigned int total_bytes_written = 0;
  uint16_t *record_ids = NULL;
  unsigned int record_ids_count = 0;
  unsigned int cache_create_flags_mask = (IPMI_SDR_CACHE_CREATE_FLAGS_OVERWRITE
					  | IPMI_SDR_CACHE_CREATE_FLAGS_DUPLICATE_RECORD_ID
					  | IPMI_SDR_CACHE_CREATE_FLAGS_ASSUME_MAX_SDR_RECORD_COUNT);
  uint8_t trailer_checksum = 0;
  int fd = -1;
  int rv = -1;

  if (!ctx || ctx->magic != IPMI_SDR_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_sdr_ctx_errormsg (ctx), ipmi_sdr_ctx_errnum (ctx));
      return (-1);
    }

  /* Version cannot be 0h according to the IPMI spec */
  if (!ipmi_ctx
      || !filename
      || (strlen (filename) > MAXPATHLEN)
      || (cache_create_flags & ~cache_create_flags_mask))
    {
      SDR_SET_ERRNUM (ctx, IPMI_SDR_ERR_PARAMETERS);
      return (-1);
    }

  if (ctx->operation != IPMI_SDR_OPERATION_UNINITIALIZED)
    {
      if (ctx->operation == IPMI_SDR_OPERATION_READ_CACHE)
        SDR_SET_ERRNUM (ctx, IPMI_SDR_ERR_CONTEXT_PERFORMING_OTHER_OPERATION);
      else
        SDR_SET_ERRNUM (ctx, IPMI_SDR_ERR_INTERNAL_ERROR);
      return (-1);
    }
  
  ctx->operation = IPMI_SDR_OPERATION_CREATE_CACHE;
  
  if (cache_create_flags & IPMI_SDR_CACHE_CREATE_FLAGS_OVERWRITE)
    open_flags = O_CREAT | O_TRUNC | O_WRONLY;
  else
    open_flags = O_CREAT | O_EXCL | O_WRONLY;
  
  if ((fd = open (filename, open_flags, 0644)) < 0)
    {
      if (!(cache_create_flags & IPMI_SDR_CACHE_CREATE_FLAGS_OVERWRITE)
          && errno == EEXIST)
        SDR_SET_ERRNUM (ctx, IPMI_SDR_ERR_CACHE_CREATE_CACHE_EXISTS);
      else if (errno == EPERM
               || errno == EACCES
               || errno == EISDIR
               || errno == EROFS)
        SDR_SET_ERRNUM (ctx, IPMI_SDR_ERR_PERMISSION);
      else if (errno == ENAMETOOLONG
               || errno == ENOENT
               || errno == ELOOP)
        SDR_SET_ERRNUM (ctx, IPMI_SDR_ERR_FILENAME_INVALID);
      else if (errno == ENOSPC
               || errno == EMFILE
               || errno == ENFILE)
        SDR_SET_ERRNUM (ctx, IPMI_SDR_ERR_FILESYSTEM);
      else
        SDR_SET_ERRNUM (ctx, IPMI_SDR_ERR_SYSTEM_ERROR);
      goto cleanup;
    }

  if (sdr_info (ctx,
		ipmi_ctx,
		&sdr_version,
		&record_count,
		&most_recent_addition_timestamp,
		&most_recent_erase_timestamp) < 0)
    goto cleanup;

  if (!record_count)
    {
      SDR_SET_ERRNUM (ctx, IPMI_SDR_ERR_CACHE_CREATE_INVALID_RECORD_COUNT);
      goto cleanup;
    }

  if (_sdr_cache_header_write (ctx,
                               ipmi_ctx,
                               fd,
                               &total_bytes_written,
                               sdr_version,
                               record_count,
                               most_recent_addition_timestamp,
                               most_recent_erase_timestamp) < 0)
    goto cleanup;

  /* Version cannot be 0h according to the IPMI spec, but we accept it regardless */
  ctx->sdr_version = sdr_version;
  ctx->record_count = record_count;
  ctx->most_recent_addition_timestamp = most_recent_addition_timestamp;
  ctx->most_recent_erase_timestamp = most_recent_erase_timestamp;

  if (cache_create_flags & IPMI_SDR_CACHE_CREATE_FLAGS_DUPLICATE_RECORD_ID)
    {
      if (!(record_ids = (uint16_t *)malloc (ctx->record_count * sizeof (uint16_t))))
        {
          SDR_SET_ERRNUM (ctx, IPMI_SDR_ERR_OUT_OF_MEMORY);
          goto cleanup;
        }
      record_ids_count = 0;
    }

  if (_sdr_cache_reservation_id (ctx,
                                 ipmi_ctx,
                                 &reservation_id) < 0)
    goto cleanup;

  next_record_id = IPMI_SDR_RECORD_ID_FIRST;
  while (next_record_id != IPMI_SDR_RECORD_ID_LAST)
    {
      uint8_t record_buf[IPMI_SDR_MAX_RECORD_LENGTH];
      int record_len;

      if (record_count_written >= ctx->record_count)
        {
	  /* IPMI Workaround
	   *
	   * Discovered on unspecified Inspur motherboard
	   *
	   * SDR record reading is broken, the IPMI_SDR_RECORD_ID_LAST
	   * record id never occurs.  So this workaround allows the
	   * user to not error out and avoids this loop from looping
	   * infinitely.
	   *
	   */

	  if (cache_create_flags & IPMI_SDR_CACHE_CREATE_FLAGS_ASSUME_MAX_SDR_RECORD_COUNT)
	    break;

          SDR_SET_ERRNUM (ctx, IPMI_SDR_ERR_CACHE_CREATE_INVALID_RECORD_COUNT);
          goto cleanup;
        }

      record_id = next_record_id;
      if ((record_len = _sdr_cache_get_record (ctx,
                                               ipmi_ctx,
                                               record_id,
                                               record_buf,
                                               IPMI_SDR_MAX_RECORD_LENGTH,
                                               &reservation_id,
                                               &next_record_id)) < 0)
        goto cleanup;

      if (record_len)
        {
          if (ctx->flags & IPMI_SDR_FLAGS_DEBUG_DUMP)
            {
              const char *record_str;

              if ((record_str = sdr_record_type_str (ctx,
						     record_buf,
						     record_len)))
                {
                  char hdrbuf[IPMI_SDR_CACHE_DEBUG_BUFLEN];

                  debug_hdr_str (DEBUG_UTIL_TYPE_NONE,
                                 DEBUG_UTIL_DIRECTION_NONE,
				 DEBUG_UTIL_FLAGS_DEFAULT,
                                 record_str,
                                 hdrbuf,
                                 IPMI_SDR_CACHE_DEBUG_BUFLEN);

                  ipmi_dump_sdr_record (STDERR_FILENO,
                                        ctx->debug_prefix,
                                        hdrbuf,
                                        NULL,
                                        record_buf,
                                        record_len);
                }
            }

          if (_sdr_cache_record_write (ctx,
                                       fd,
                                       &total_bytes_written,
                                       record_ids,
                                       &record_ids_count,
                                       record_buf,
                                       record_len,
				       &trailer_checksum) < 0)
            goto cleanup;

          record_count_written++;

          if (create_callback)
            (*create_callback)(ctx->sdr_version,
                               ctx->record_count,
                               ctx->most_recent_addition_timestamp,
                               ctx->most_recent_erase_timestamp,
                               record_id,
                               create_callback_data);
        }
    }

  if (record_count_written != ctx->record_count)
    {
      /*
       * IPMI Workaround (achu)
       *
       * Discovered on Fujitsu RX 100
       * Discovered on Fujitsu RX300/200-S8
       *
       * The record_count listed from the Get SDR Repository Info command
       * is not consistent with the length of SDR records stored.
       *
       * We will assume that if we reached the end of the SDR record
       * list (i.e. next_record_id == 0xFFFF), a non-zero number of
       * records were written, it's ok and we can continue on.
       */
      if (next_record_id == IPMI_SDR_RECORD_ID_LAST
          && record_count_written)
        {
          unsigned int total_bytes_written_temp = 0;

          ctx->record_count = record_count_written;
          
          /* need to seek back to the beginning of the file and
           * re-write the header info with the correct number of
           * records
           */

          if (lseek (fd, 0, SEEK_SET) < 0)
            {
              SDR_SET_ERRNUM (ctx, IPMI_SDR_ERR_SYSTEM_ERROR);
              goto cleanup;
            }

          if (_sdr_cache_header_write (ctx,
                                       ipmi_ctx,
                                       fd,
                                       &total_bytes_written_temp,
                                       ctx->sdr_version,
                                       ctx->record_count,
                                       ctx->most_recent_addition_timestamp,
                                       ctx->most_recent_erase_timestamp) < 0)
            goto cleanup;

	  /* need to seek back to the end of the file to write the
	   * trailer below
	   */
          if (lseek (fd, 0, SEEK_END) < 0)
            {
              SDR_SET_ERRNUM (ctx, IPMI_SDR_ERR_SYSTEM_ERROR);
              goto cleanup;
            }
        }
      else
        {
          SDR_SET_ERRNUM (ctx, IPMI_SDR_ERR_CACHE_CREATE_INVALID_RECORD_COUNT);
          goto cleanup;
        }
    }

  if (_sdr_cache_trailer_write (ctx,
				ipmi_ctx,
				fd,
				total_bytes_written,
				trailer_checksum) < 0)
            goto cleanup;

  if (fsync (fd) < 0)
    {
      SDR_ERRNO_TO_SDR_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (close (fd) < 0)
    {
      SDR_ERRNO_TO_SDR_ERRNUM (ctx, errno);
      goto cleanup;
    }
  fd = -1;

  rv = 0;
  ctx->errnum = IPMI_SDR_ERR_SUCCESS;
 cleanup:
  ctx->operation = IPMI_SDR_OPERATION_UNINITIALIZED;
  if (fd >= 0)
    {
      /* If the cache create never completed, try to remove the file */
      /* ignore potential error, cleanup path */
      unlink (filename);
      /* ignore potential error, cleanup path */
      close (fd);
    }
  free (record_ids);
  sdr_init_ctx (ctx);
  return (rv);
}
