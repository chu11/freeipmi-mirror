/*****************************************************************************\
 *  $Id: ipmi-sel-parse.c,v 1.1.2.5 2008-12-22 23:04:19 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2007-2008 Lawrence Livermore National Security, LLC.
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
 *  Free Software Foundation; either version 2 of the License, or (at your
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
#include <assert.h>
#include <errno.h>

#include "freeipmi/sel-parse/ipmi-sel-parse.h"

#include "freeipmi/api/ipmi-sel-cmds-api.h"
#include "freeipmi/cmds/ipmi-sel-cmds.h"
#include "freeipmi/debug/ipmi-debug.h"
#include "freeipmi/record-format/ipmi-sel-record-format.h"
#include "freeipmi/spec/ipmi-comp-code-spec.h"
#include "freeipmi/util/ipmi-sensor-and-event-code-tables-util.h"
#include "freeipmi/util/ipmi-util.h"

#include "ipmi-sel-parse-defs.h"

#include "libcommon/ipmi-err-wrappers.h"
#include "libcommon/ipmi-fiid-wrappers.h"

#include "freeipmi-portability.h"
#include "debug-util.h"

static char *ipmi_sel_parse_errmsgs[] =
  {
    "success",
    "context null",
    "context invalid",
    "invalid parameters",
    "out of memory",
    "sdr cache permission error",
    "sdr cache filesystem error",
    "sdr cache error",
    "no sel entries available",
    "not found",
    "callback error",
    "internal IPMI error",
    "internal system error",
    "buffer overflow",
    "internal error",
    "errnum out of range",
    NULL
  };

ipmi_sel_parse_ctx_t
ipmi_sel_parse_ctx_create(ipmi_ctx_t ipmi_ctx, ipmi_sdr_cache_ctx_t sdr_cache_ctx)
{
  struct ipmi_sel_parse_ctx *ctx = NULL;

  ERR_CLEANUP((ctx = (ipmi_sel_parse_ctx_t)malloc(sizeof(struct ipmi_sel_parse_ctx))));
  memset(ctx, '\0', sizeof(struct ipmi_sel_parse_ctx));
  ctx->magic = IPMI_SEL_PARSE_MAGIC;
  ctx->flags = IPMI_SEL_PARSE_FLAGS_DEFAULT;
  ctx->debug_prefix = NULL;

  ctx->ipmi_ctx = ipmi_ctx;
  ctx->sdr_cache_ctx = sdr_cache_ctx;

  if (!(ctx->sel_entries = list_create((ListDelF)free)))
    goto cleanup;

  return ctx;

 cleanup:
  if (ctx)
    free(ctx);
  return NULL;
}

void
ipmi_sel_parse_ctx_destroy(ipmi_sel_parse_ctx_t ctx)
{
  if (!ctx || ctx->magic != IPMI_SEL_PARSE_MAGIC)
    return;

  ctx->magic = ~IPMI_SEL_PARSE_MAGIC;
  if (ctx->debug_prefix)
    free(ctx->debug_prefix);
  free(ctx);
}

int 
ipmi_sel_parse_ctx_errnum(ipmi_sel_parse_ctx_t ctx)
{
  if (!ctx)
    return IPMI_SEL_PARSE_CTX_ERR_CONTEXT_NULL;
  else if (ctx->magic != IPMI_SEL_PARSE_MAGIC)
    return IPMI_SEL_PARSE_CTX_ERR_CONTEXT_INVALID;
  else
    return ctx->errnum;
}

char *
ipmi_sel_parse_ctx_strerror(int errnum)
{
  if (errnum >= IPMI_SEL_PARSE_CTX_ERR_SUCCESS && errnum <= IPMI_SEL_PARSE_CTX_ERR_ERRNUMRANGE)
    return ipmi_sel_parse_errmsgs[errnum];
  else
    return ipmi_sel_parse_errmsgs[IPMI_SEL_PARSE_CTX_ERR_ERRNUMRANGE];
}

int
ipmi_sel_parse_ctx_get_flags(ipmi_sel_parse_ctx_t ctx, unsigned int *flags)
{
  ERR(ctx && ctx->magic == IPMI_SEL_PARSE_MAGIC);

  SEL_PARSE_ERR_PARAMETERS(flags);

  *flags = ctx->flags;
  return 0;
}

int
ipmi_sel_parse_ctx_set_flags(ipmi_sel_parse_ctx_t ctx, unsigned int flags)
{
  ERR(ctx && ctx->magic == IPMI_SEL_PARSE_MAGIC);

  SEL_PARSE_ERR_PARAMETERS(!(flags & ~IPMI_SEL_PARSE_FLAGS_DEBUG_DUMP));

  ctx->flags = flags;
  return 0;
}

char *
ipmi_sel_parse_ctx_get_debug_prefix(ipmi_sel_parse_ctx_t ctx)
{
  ERR_NULL_RETURN(ctx && ctx->magic == IPMI_SEL_PARSE_MAGIC);

  return ctx->debug_prefix;
}

int
ipmi_sel_parse_ctx_set_debug_prefix(ipmi_sel_parse_ctx_t ctx, const char *prefix)
{
  ERR(ctx && ctx->magic == IPMI_SEL_PARSE_MAGIC);

  if (ctx->debug_prefix)
    {
      free(ctx->debug_prefix);
      ctx->debug_prefix = NULL;
    }

  if (prefix)
    SEL_PARSE_ERR_OUT_OF_MEMORY((ctx->debug_prefix = strdup(prefix)));

  return 0;
}

static int
_sel_reservation_id(ipmi_sel_parse_ctx_t ctx,
                    uint16_t *reservation_id)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  int rv = -1;

  assert(ctx);
  assert(ctx->magic == IPMI_SEL_PARSE_MAGIC);
  assert(reservation_id);

  SEL_PARSE_FIID_OBJ_CREATE_CLEANUP(obj_cmd_rs, tmpl_cmd_reserve_sel_rs);

  if (ipmi_cmd_reserve_sel (ctx->ipmi_ctx, obj_cmd_rs) < 0)
    {
      /* achu:
       *
       * IPMI spec states reservation ID 0000h should be used if Reserve SEL 
       * is not supported.  
       */
      if (ipmi_ctx_errnum(ctx->ipmi_ctx) == IPMI_ERR_BAD_COMPLETION_CODE_INVALID_COMMAND
          && ipmi_check_completion_code(obj_cmd_rs,
                                        IPMI_COMP_CODE_COMMAND_INVALID) == 1)
        {
          *reservation_id = 0;
          goto out;
        }
      
      SEL_PARSE_ERRNUM_SET(IPMI_SEL_PARSE_CTX_ERR_IPMI_ERROR);
      goto cleanup;
    }

  SEL_PARSE_FIID_OBJ_GET_CLEANUP(obj_cmd_rs,
                                 "reservation_id",
                                 &val);
  *reservation_id = val;

 out:
  rv = 0;
 cleanup:
  SEL_PARSE_FIID_OBJ_DESTROY(obj_cmd_rs);
  return rv;
}

static int
_sel_entries_delete_all(void *x, void *y)
{
  return 1;
}

static void
_sel_entries_clear(ipmi_sel_parse_ctx_t ctx)
{
  assert(ctx);
  assert(ctx->magic == IPMI_SEL_PARSE_MAGIC);

  list_delete_all(ctx->sel_entries, _sel_entries_delete_all, "dummyvalue");

  if (ctx->sel_entries_itr)
    {
      list_iterator_destroy(ctx->sel_entries_itr);
      ctx->sel_entries_itr = NULL;
    }

  ctx->current_sel_entry = NULL;
  ctx->callback_sel_entry = NULL;
}

/* 
 * XXX: Handle if sel_event_record not long enough and data not set
 * XXX: Handle if sel_event_record not long enough and data not set
 * XXX: Handle if sel_event_record not long enough and data not set
 */

static int
_sel_entry_record_id(ipmi_sel_parse_ctx_t ctx,
                     struct ipmi_sel_parse_entry *sel_parse_entry,
                     uint16_t *record_id)
{
  fiid_obj_t obj_sel_record_header = NULL;
  uint64_t val;
  int rv = -1;
  
  assert(ctx);
  assert(ctx->magic == IPMI_SEL_PARSE_MAGIC);
  assert(sel_parse_entry);
  assert(record_id);
  
  SEL_PARSE_FIID_OBJ_CREATE_CLEANUP(obj_sel_record_header, tmpl_sel_record_header);
  
  SEL_PARSE_FIID_OBJ_SET_ALL_CLEANUP(obj_sel_record_header, 
                                     sel_parse_entry->sel_event_record,
                                     sel_parse_entry->sel_event_record_len);
  
  SEL_PARSE_FIID_OBJ_GET_CLEANUP(obj_sel_record_header,
                                 "record_id",
                                 &val);
  (*record_id) = val;
  
  rv = 0;
 cleanup:
  SEL_PARSE_FIID_OBJ_DESTROY(obj_sel_record_header);
  return rv;
}

static int
_sel_entry_record_type(ipmi_sel_parse_ctx_t ctx,
                       struct ipmi_sel_parse_entry *sel_parse_entry,
                       uint8_t *record_type)
{
  fiid_obj_t obj_sel_record_header = NULL;
  uint64_t val;
  int rv = -1;

  assert(ctx);
  assert(ctx->magic == IPMI_SEL_PARSE_MAGIC);
  assert(sel_parse_entry);
  assert(record_type);

  SEL_PARSE_FIID_OBJ_CREATE_CLEANUP(obj_sel_record_header, tmpl_sel_record_header);

  SEL_PARSE_FIID_OBJ_SET_ALL_CLEANUP(obj_sel_record_header, 
                                     sel_parse_entry->sel_event_record,
                                     sel_parse_entry->sel_event_record_len);
  
  SEL_PARSE_FIID_OBJ_GET_CLEANUP(obj_sel_record_header,
                                 "record_type",
                                 &val);
  (*record_type) = val;
  
  rv = 0;
 cleanup:
  SEL_PARSE_FIID_OBJ_DESTROY(obj_sel_record_header);
  return rv;
}

static int
_sel_entry_record_type_class(uint8_t record_type)
{
  if (IPMI_SEL_RECORD_TYPE_IS_EVENT(record_type))
    return IPMI_SEL_RECORD_TYPE_CLASS_SYSTEM_EVENT_RECORD;

  if (IPMI_SEL_RECORD_TYPE_IS_TIMESTAMPED_OEM(record_type))
    return IPMI_SEL_RECORD_TYPE_CLASS_TIMESTAMPED_OEM_RECORD;

  if (IPMI_SEL_RECORD_TYPE_IS_NON_TIMESTAMPED_OEM(record_type))
    return IPMI_SEL_RECORD_TYPE_CLASS_NON_TIMESTAMPED_OEM_RECORD;

  return IPMI_SEL_RECORD_TYPE_CLASS_UNKNOWN_RECORD;
}

static int
_sel_entry_system_event_record(ipmi_sel_parse_ctx_t ctx,
                               struct ipmi_sel_parse_entry *sel_parse_entry,
                               uint32_t *timestamp,
                               uint8_t *generator_id,
                               uint8_t *ipmb_device_lun,
                               uint8_t *channel_number,
                               uint8_t *event_message_format_version,
                               uint8_t *sensor_type,
                               uint8_t *sensor_number,
                               uint8_t *event_type_code,
                               uint8_t *event_direction,
                               uint8_t *offset_from_event_reading_type_code,
                               uint8_t *event_data2_flag,
                               uint8_t *event_data3_flag,
                               uint8_t *event_data2,
                               uint8_t *event_data3)
{
  fiid_obj_t obj_sel_system_event_record = NULL;
  int record_type_class;
  uint8_t record_type;
  uint8_t generator_id_type;
  uint8_t generator_id_address;
  uint64_t val;
  int rv = -1;

  assert(ctx);
  assert(ctx->magic == IPMI_SEL_PARSE_MAGIC);
  assert(sel_parse_entry);

  if (_sel_entry_record_type(ctx, sel_parse_entry, &record_type) < 0)
    goto cleanup;

  record_type_class = _sel_entry_record_type_class(record_type);

  if (record_type_class != IPMI_SEL_RECORD_TYPE_CLASS_SYSTEM_EVENT_RECORD)
    {
      SEL_PARSE_ERRNUM_SET(IPMI_SEL_PARSE_CTX_ERR_INTERNAL_ERROR);
      goto cleanup;
    }

  SEL_PARSE_FIID_OBJ_CREATE_CLEANUP(obj_sel_system_event_record,
                                    tmpl_sel_system_event_record);

  if (timestamp)
    {
      SEL_PARSE_FIID_OBJ_GET_CLEANUP (obj_sel_system_event_record, "timestamp", &val);
      *timestamp = val;
    }

  if (generator_id)
    {
      SEL_PARSE_FIID_OBJ_GET_CLEANUP (obj_sel_system_event_record, "generator_id.id_type", &val);
      generator_id_type = val;
      
      SEL_PARSE_FIID_OBJ_GET_CLEANUP (obj_sel_system_event_record, "generator_id.id", &val);
      generator_id_address = val;
      
      *generator_id = ((generator_id_address << 1) | generator_id_type);
    }

  if (ipmb_device_lun)
    {
      SEL_PARSE_FIID_OBJ_GET_CLEANUP (obj_sel_system_event_record, "ipmb_device_lun", &val);
      *ipmb_device_lun = val;
    }

  if (channel_number)
    {
      SEL_PARSE_FIID_OBJ_GET_CLEANUP (obj_sel_system_event_record, "channel_number", &val);
      *channel_number = val;
    }

  if (event_message_format_version)
    {
      SEL_PARSE_FIID_OBJ_GET_CLEANUP (obj_sel_system_event_record, "event_message_format_version", &val);
      *event_message_format_version = val;
    }

  if (sensor_type)
    {
      SEL_PARSE_FIID_OBJ_GET_CLEANUP (obj_sel_system_event_record, "sensor_type", &val);
      *sensor_type = val;
    }

  if (sensor_number)
    {
      SEL_PARSE_FIID_OBJ_GET_CLEANUP (obj_sel_system_event_record, "sensor_number", &val);
      *sensor_number = val;
    }

  if (event_type_code)
    {
      SEL_PARSE_FIID_OBJ_GET_CLEANUP (obj_sel_system_event_record, "event_type_code", &val);
      *event_type_code = val;
    }

  if (event_direction)
    {
      SEL_PARSE_FIID_OBJ_GET_CLEANUP (obj_sel_system_event_record, "event_dir", &val);
      *event_direction = val;
    }

  if (offset_from_event_reading_type_code)
    {
      SEL_PARSE_FIID_OBJ_GET_CLEANUP (obj_sel_system_event_record, "offset_from_event_reading_type_code", &val);
      *offset_from_event_reading_type_code = val;
    }

  if (event_data2_flag)
    {
      SEL_PARSE_FIID_OBJ_GET_CLEANUP (obj_sel_system_event_record, "event_data2_flag", &val);
      *event_data2_flag = val;
    }

  if (event_data3_flag)
    {
      SEL_PARSE_FIID_OBJ_GET_CLEANUP (obj_sel_system_event_record, "event_data3_flag", &val);
      *event_data3_flag = val;
    }

  if (event_data2)
    {
      SEL_PARSE_FIID_OBJ_GET_CLEANUP (obj_sel_system_event_record, "event_data2", &val);
      *event_data2 = val;
    }

  if (event_data3)
    {
      SEL_PARSE_FIID_OBJ_GET_CLEANUP (obj_sel_system_event_record, "event_data3", &val);
      *event_data3 = val;
    }

  rv = 0;
 cleanup:
  SEL_PARSE_FIID_OBJ_DESTROY(obj_sel_system_event_record);
  return rv;
}

static void
_sel_entry_dump(ipmi_sel_parse_ctx_t ctx, struct ipmi_sel_parse_entry *sel_parse_entry)
{
  fiid_obj_t obj_sel_record = NULL;
  char hdrbuf[DEBUG_UTIL_HDR_BUFLEN];
  uint8_t record_type;
  int record_type_class;

  assert(ctx);
  assert(ctx->magic == IPMI_SEL_PARSE_MAGIC);
  assert(sel_parse_entry);

  if (!(ctx->flags & IPMI_SEL_PARSE_FLAGS_DEBUG_DUMP))
    return;

  if (_sel_entry_record_type(ctx, sel_parse_entry, &record_type) < 0)
    goto cleanup;

  record_type_class = _sel_entry_record_type_class(record_type);

  if (record_type_class == IPMI_SEL_RECORD_TYPE_CLASS_SYSTEM_EVENT_RECORD)
    {
      uint8_t event_type_code;
      uint8_t event_data2_flag;
      int event_type_code_class;

      if (_sel_entry_system_event_record(ctx,
                                         sel_parse_entry,
                                         NULL,
                                         NULL,
                                         NULL,
                                         NULL,
                                         NULL,
                                         NULL,
                                         NULL,
                                         &event_type_code,
                                         NULL,
                                         NULL,
                                         &event_data2_flag,
                                         NULL,
                                         NULL,
                                         NULL) < 0)
        goto cleanup;

      event_type_code_class = ipmi_event_reading_type_code_class(event_type_code);

      if (event_type_code_class == IPMI_EVENT_READING_TYPE_CODE_CLASS_THRESHOLD)
        SEL_PARSE_FIID_OBJ_CREATE_CLEANUP(obj_sel_record, tmpl_sel_system_event_record_event_fields);
      else if (event_type_code_class == IPMI_EVENT_READING_TYPE_CODE_CLASS_GENERIC_DISCRETE
               || event_type_code_class == IPMI_EVENT_READING_TYPE_CODE_CLASS_SENSOR_SPECIFIC_DISCRETE)
        {
          if (event_data2_flag == IPMI_SEL_EVENT_DATA_PREVIOUS_STATE_OR_SEVERITY)
            SEL_PARSE_FIID_OBJ_CREATE_CLEANUP(obj_sel_record, tmpl_sel_system_event_record_discrete_previous_state_severity);
          else
            SEL_PARSE_FIID_OBJ_CREATE_CLEANUP(obj_sel_record, tmpl_sel_system_event_record_event_fields);
        }
      else
        SEL_PARSE_FIID_OBJ_CREATE_CLEANUP(obj_sel_record, tmpl_sel_system_event_record);

    }
  else if (record_type_class == IPMI_SEL_RECORD_TYPE_CLASS_TIMESTAMPED_OEM_RECORD)
    SEL_PARSE_FIID_OBJ_CREATE_CLEANUP(obj_sel_record, tmpl_sel_timestamped_oem_record);
  else if (record_type_class == IPMI_SEL_RECORD_TYPE_CLASS_NON_TIMESTAMPED_OEM_RECORD)
    SEL_PARSE_FIID_OBJ_CREATE_CLEANUP(obj_sel_record, tmpl_sel_non_timestamped_oem_record);
  else /* record_type_class == SEL_RECORD_TYPE_UNKNOWN_RECORD */
    SEL_PARSE_FIID_OBJ_CREATE_CLEANUP(obj_sel_record, tmpl_sel_system_event_record);
 
  debug_hdr_str(DEBUG_UTIL_TYPE_NONE,
                DEBUG_UTIL_DIRECTION_NONE,
                "SEL Event Record",
                hdrbuf,
                DEBUG_UTIL_HDR_BUFLEN);
  
  ipmi_obj_dump (STDERR_FILENO,
                 ctx->debug_prefix,
                 hdrbuf,
                 NULL,
                 obj_sel_record);

 cleanup:
  SEL_PARSE_FIID_OBJ_DESTROY(obj_sel_record);
}

int 
ipmi_sel_parse(ipmi_sel_parse_ctx_t ctx,
               Ipmi_Sel_Parse_Callback callback,
               void *callback_data)
{
  uint16_t reservation_id = 0;
  uint16_t record_id = 0;
  uint16_t next_record_id = 0;
  fiid_obj_t obj_cmd_rs = NULL;
  int rv = -1;

  ERR(ctx && ctx->magic == IPMI_SEL_PARSE_MAGIC);

  _sel_entries_clear(ctx);

  SEL_PARSE_FIID_OBJ_CREATE(obj_cmd_rs, tmpl_cmd_get_sel_entry_rs);

  for (record_id = IPMI_SEL_GET_RECORD_ID_FIRST_ENTRY;
       record_id != IPMI_SEL_GET_RECORD_ID_LAST_ENTRY;
       record_id = next_record_id)
    {
      struct ipmi_sel_parse_entry *sel_parse_entry = NULL;
      unsigned int reservation_id_retry_count = 0;
      unsigned int reservation_canceled = 0;
      uint64_t val;
      int len;
      /*
       *
       * IPMI spec states in section 31.4.1:
       *
       * "A Requester must issue a 'Reserve SEL' command prior to issuing
       * any of the following SEL commands. Note that the 'Reserve SEL'
       * command only needs to be reissued if the reservation is
       * canceled. ... Get SEL Entry command (if 'get' is from an offset
       * other than 00h)".
       *
       * Since we always use an offset of 00h, presumably we should never
       * need reserve the SEL before the get_sel_entry call.
       *
       * However, some machines may need it due to compliance issues.
       * I don't think using a reservation ID all of the time hurts
       * anything, so we'll just use it all of the time.
       */
      
      while (1)
        {
          if (record_id == IPMI_SEL_GET_RECORD_ID_FIRST_ENTRY || reservation_canceled)
            {
              if (_sel_reservation_id(ctx, &reservation_id) < 0)
                goto cleanup;
            }

          if (ipmi_cmd_get_sel_entry (ctx->ipmi_ctx,
                                      reservation_id,
                                      record_id,
                                      0,
                                      IPMI_SEL_READ_ENTIRE_RECORD_BYTES_TO_READ,
                                      obj_cmd_rs) < 0)
            {
              if (ipmi_ctx_errnum(ctx->ipmi_ctx) == IPMI_ERR_BAD_COMPLETION_CODE
                  && ipmi_check_completion_code(obj_cmd_rs,
                                                IPMI_COMP_CODE_RESERVATION_CANCELLED) == 1)
                {
                  reservation_id_retry_count++;
                  reservation_canceled++;
                  
                  if (reservation_id_retry_count > IPMI_SEL_PARSE_RESERVATION_ID_RETRY)
                    {
                      SEL_PARSE_ERRNUM_SET(IPMI_SEL_PARSE_CTX_ERR_IPMI_ERROR);
                      goto cleanup;
                    }
                  
                  if (_sel_reservation_id(ctx, &reservation_id) < 0)
                    goto cleanup;
                  
                  continue;
                }
              else if (record_id == IPMI_SEL_GET_RECORD_ID_FIRST_ENTRY
                       && ipmi_ctx_errnum(ctx->ipmi_ctx) == IPMI_ERR_BAD_COMPLETION_CODE_REQUEST_DATA_INVALID
                       && ipmi_check_completion_code(obj_cmd_rs,
                                                     IPMI_COMP_CODE_REQUEST_SENSOR_DATA_OR_RECORD_NOT_PRESENT) == 1)
                {
                  /* If the sel is empty, it's not really an error */
                  goto out;
                }
              else
                {
                  SEL_PARSE_ERRNUM_SET(IPMI_SEL_PARSE_CTX_ERR_IPMI_ERROR);
                  goto cleanup;
                }
            }

          SEL_PARSE_FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "next_record_id", &val);
          next_record_id = val;
    
          SEL_PARSE_ERR_OUT_OF_MEMORY_CLEANUP ((sel_parse_entry = (struct ipmi_sel_parse_entry *)malloc(sizeof(struct ipmi_sel_parse_entry))));

          SEL_PARSE_FIID_OBJ_GET_DATA_LEN_CLEANUP (len,
                                                   obj_cmd_rs,
                                                   "record_data", 
                                                   sel_parse_entry->sel_event_record,
                                                   IPMI_SEL_RECORD_LENGTH); 
          sel_parse_entry->sel_event_record_len = len;

          _sel_entry_dump(ctx, sel_parse_entry);

          /* achu: should come before list_append to avoid having a freed entry on the list */
          if (callback)
            {
              ctx->callback_sel_entry = sel_parse_entry;
              if ((*callback)(ctx, callback_data) < 0)
                {
                  ctx->errnum = IPMI_SEL_PARSE_CTX_ERR_CALLBACK_ERROR;
                  goto cleanup;
                }
            }
          
          if (!list_append(ctx->sel_entries, sel_parse_entry))
            {
              SEL_PARSE_ERRNUM_SET(IPMI_SEL_PARSE_CTX_ERR_INTERNAL_ERROR);
              goto cleanup;
            }
          
          break;
        }
    }

 out:

  if ((rv = list_count(ctx->sel_entries)) > 0)
    {
      if (!(ctx->sel_entries_itr = list_iterator_create(ctx->sel_entries)))
        {
          SEL_PARSE_ERRNUM_SET(IPMI_SEL_PARSE_CTX_ERR_INTERNAL_ERROR);
          goto cleanup;
        }
      ctx->current_sel_entry = list_next(ctx->sel_entries_itr);
    }
  
  ctx->errnum = IPMI_SEL_PARSE_CTX_ERR_SUCCESS;
 cleanup:
  ctx->callback_sel_entry = NULL;
  SEL_PARSE_FIID_OBJ_DESTROY(obj_cmd_rs);
  return rv;
}

int 
ipmi_sel_parse_clear_sel(ipmi_sel_parse_ctx_t ctx)
{
  unsigned int reservation_id_retry_count = 0;
  uint16_t reservation_id;
  fiid_obj_t obj_cmd_rs = NULL;
  int rv = -1;

  ERR(ctx && ctx->magic == IPMI_SEL_PARSE_MAGIC);

  SEL_PARSE_FIID_OBJ_CREATE(obj_cmd_rs, tmpl_cmd_clear_sel_rs);

  while (1)
    {
      if (_sel_reservation_id(ctx, &reservation_id) < 0)
        goto cleanup;

      if (ipmi_cmd_clear_sel (ctx->ipmi_ctx,
                              reservation_id,
                              IPMI_SEL_CLEAR_OPERATION_INITIATE_ERASE,
                              obj_cmd_rs) < 0)
        {
          if (ipmi_ctx_errnum(ctx->ipmi_ctx) == IPMI_ERR_BAD_COMPLETION_CODE
              && ipmi_check_completion_code(obj_cmd_rs,
                                            IPMI_COMP_CODE_RESERVATION_CANCELLED) == 1)
            {
              reservation_id_retry_count++;
              
              if (reservation_id_retry_count > IPMI_SEL_PARSE_RESERVATION_ID_RETRY)
                {
                  SEL_PARSE_ERRNUM_SET(IPMI_SEL_PARSE_CTX_ERR_IPMI_ERROR);
                  goto cleanup;
                }

              if (_sel_reservation_id(ctx, &reservation_id) < 0)
                goto cleanup;

              continue;
            }
          else
            {
              SEL_PARSE_ERRNUM_SET(IPMI_SEL_PARSE_CTX_ERR_IPMI_ERROR);
              goto cleanup;
            }
        }

      break;
    }

  rv = 0;
  ctx->errnum = IPMI_SEL_PARSE_CTX_ERR_SUCCESS;
 cleanup:
  SEL_PARSE_FIID_OBJ_DESTROY(obj_cmd_rs);
  return rv;
}  

int 
ipmi_sel_parse_delete_sel_entry(ipmi_sel_parse_ctx_t ctx, uint16_t record_id)
{
  unsigned int reservation_id_retry_count = 0;
  uint16_t reservation_id;
  fiid_obj_t obj_cmd_rs = NULL;
  int rv = -1;

  ERR(ctx && ctx->magic == IPMI_SEL_PARSE_MAGIC);

  SEL_PARSE_ERR_PARAMETERS(record_id > IPMI_SEL_GET_RECORD_ID_FIRST_ENTRY
                           && record_id < IPMI_SEL_GET_RECORD_ID_LAST_ENTRY);

  SEL_PARSE_FIID_OBJ_CREATE(obj_cmd_rs, tmpl_cmd_delete_sel_entry_rs);

  while (1)
    {
      if (_sel_reservation_id(ctx, &reservation_id) < 0)
        goto cleanup;

      if (ipmi_cmd_delete_sel_entry (ctx->ipmi_ctx,
                                     reservation_id,
                                     record_id,
                                     obj_cmd_rs) < 0)
        {
          if (ipmi_ctx_errnum(ctx->ipmi_ctx) == IPMI_ERR_BAD_COMPLETION_CODE_REQUEST_DATA_INVALID
              && (ipmi_check_completion_code(obj_cmd_rs, IPMI_COMP_CODE_REQUEST_SENSOR_DATA_OR_RECORD_NOT_PRESENT) == 1
                  || ipmi_check_completion_code(obj_cmd_rs, IPMI_COMP_CODE_REQUEST_INVALID_DATA_FIELD) == 1))
            {
              SEL_PARSE_ERRNUM_SET(IPMI_SEL_PARSE_CTX_ERR_NOT_FOUND);
              goto cleanup;
            }
          else if (ipmi_ctx_errnum(ctx->ipmi_ctx) == IPMI_ERR_BAD_COMPLETION_CODE
                   && ipmi_check_completion_code(obj_cmd_rs,
                                                 IPMI_COMP_CODE_RESERVATION_CANCELLED) == 1)
            {
              reservation_id_retry_count++;
              
              if (reservation_id_retry_count > IPMI_SEL_PARSE_RESERVATION_ID_RETRY)
                {
                  SEL_PARSE_ERRNUM_SET(IPMI_SEL_PARSE_CTX_ERR_IPMI_ERROR);
                  goto cleanup;
                }

              if (_sel_reservation_id(ctx, &reservation_id) < 0)
                goto cleanup;
              
              continue;
            }
          else
            {
              SEL_PARSE_ERRNUM_SET(IPMI_SEL_PARSE_CTX_ERR_IPMI_ERROR);
              goto cleanup;
            }
        }

      break;
    }

  rv = 0;
  ctx->errnum = IPMI_SEL_PARSE_CTX_ERR_SUCCESS;
 cleanup:
  SEL_PARSE_FIID_OBJ_DESTROY(obj_cmd_rs);
  return rv;
}
