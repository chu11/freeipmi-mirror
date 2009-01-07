/*****************************************************************************\
 *  $Id: ipmi-sel-parse.c,v 1.2 2009-01-07 17:43:55 chu11 Exp $
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
#if HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */
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
#include "ipmi-sel-parse-common.h"
#include "ipmi-sel-parse-string.h"

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
    "sdr cache error",
    "no sel entries available",
    "invalid sel entry",
    "end of sel entries list reached",
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
  
  ERR_EINVAL_NULL_RETURN(ipmi_ctx);

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

void
ipmi_sel_parse_ctx_destroy(ipmi_sel_parse_ctx_t ctx)
{
  if (!ctx || ctx->magic != IPMI_SEL_PARSE_MAGIC)
    return;

  if (ctx->debug_prefix)
    free(ctx->debug_prefix);
  _sel_entries_clear(ctx);
  list_destroy(ctx->sel_entries);
  ctx->magic = ~IPMI_SEL_PARSE_MAGIC;
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

  SEL_PARSE_ERR_PARAMETERS(!(flags & ~IPMI_SEL_PARSE_FLAGS_MASK));

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

  if (sel_parse_get_record_header_info(ctx, 
                                       sel_parse_entry, 
                                       NULL, 
                                       &record_type) < 0)
    goto cleanup;

  record_type_class = ipmi_sel_record_type_class(record_type);

  if (record_type_class == IPMI_SEL_RECORD_TYPE_CLASS_SYSTEM_EVENT_RECORD)
    {
      struct ipmi_sel_system_event_record_data system_event_record_data;
      int event_type_code_class;

      if (sel_parse_get_system_event_record(ctx,
                                            sel_parse_entry,
                                            &system_event_record_data) < 0)
        {
          /* output whatever you can */
          if (ctx->errnum == IPMI_SEL_PARSE_CTX_ERR_INVALID_SEL_ENTRY)
            {
              SEL_PARSE_FIID_OBJ_CREATE_CLEANUP(obj_sel_record, tmpl_sel_system_event_record);
              goto output;
            }
          goto cleanup;
        }
      
      event_type_code_class = ipmi_event_reading_type_code_class(system_event_record_data.event_type_code);
      
      if (event_type_code_class == IPMI_EVENT_READING_TYPE_CODE_CLASS_THRESHOLD)
        SEL_PARSE_FIID_OBJ_CREATE_CLEANUP(obj_sel_record, tmpl_sel_system_event_record_event_fields);
      else if (event_type_code_class == IPMI_EVENT_READING_TYPE_CODE_CLASS_GENERIC_DISCRETE
               || event_type_code_class == IPMI_EVENT_READING_TYPE_CODE_CLASS_SENSOR_SPECIFIC_DISCRETE)
        {
          if (system_event_record_data.event_data2_flag == IPMI_SEL_EVENT_DATA_PREVIOUS_STATE_OR_SEVERITY)
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
  else /* record_type_class == SEL_RECORD_TYPE_UNKNOWN */
    SEL_PARSE_FIID_OBJ_CREATE_CLEANUP(obj_sel_record, tmpl_sel_system_event_record);
 
 output:
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
              if (sel_parse_get_reservation_id(ctx, &reservation_id) < 0)
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
                  
                  if (sel_parse_get_reservation_id(ctx, &reservation_id) < 0)
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
ipmi_sel_parse_first(ipmi_sel_parse_ctx_t ctx)
{
  ERR(ctx && ctx->magic == IPMI_SEL_PARSE_MAGIC);

  if (!ctx->sel_entries_itr)
    {
      ctx->errnum = IPMI_SEL_PARSE_CTX_ERR_NO_SEL_ENTRIES;
      return -1;
    }

  list_iterator_reset(ctx->sel_entries_itr);
  ctx->current_sel_entry = list_next(ctx->sel_entries_itr);
  return 0;
}

int
ipmi_sel_parse_next(ipmi_sel_parse_ctx_t ctx)
{
  ERR(ctx && ctx->magic == IPMI_SEL_PARSE_MAGIC);

  if (!ctx->sel_entries_itr)
    {
      ctx->errnum = IPMI_SEL_PARSE_CTX_ERR_NO_SEL_ENTRIES;
      return -1;
    }

  ctx->current_sel_entry = list_next(ctx->sel_entries_itr);
  return ((ctx->current_sel_entry) ? 1 : 0);
}

int
ipmi_sel_parse_sel_entry_count(ipmi_sel_parse_ctx_t ctx)
{
  ERR(ctx && ctx->magic == IPMI_SEL_PARSE_MAGIC);

  if (!ctx->sel_entries_itr)
    {
      ctx->errnum = IPMI_SEL_PARSE_CTX_ERR_NO_SEL_ENTRIES;
      return -1;
    }

  return list_count(ctx->sel_entries);
}

static int
_ipmi_sel_parse_find_record_id(ipmi_sel_parse_ctx_t ctx, 
                               uint16_t record_id,
                               unsigned int exact_match_flag)
{
  struct ipmi_sel_parse_entry *sel_parse_entry;
  int rv = -1;

  ERR(ctx && ctx->magic == IPMI_SEL_PARSE_MAGIC);

  if (!ctx->sel_entries_itr)
    {
      ctx->errnum = IPMI_SEL_PARSE_CTX_ERR_NO_SEL_ENTRIES;
      return -1;
    }

  list_iterator_reset(ctx->sel_entries_itr);

  while ((sel_parse_entry = list_next(ctx->sel_entries_itr)))
    {
      uint16_t current_record_id;

      if (sel_parse_get_record_header_info(ctx, 
                                           sel_parse_entry, 
                                           &record_id, 
                                           NULL) < 0)
        {
          /* if it was an invalid SEL entry, continue on */
          if (ctx->errnum == IPMI_SEL_PARSE_CTX_ERR_INVALID_SEL_ENTRY)
            continue;
          goto cleanup;
        }

      if ((exact_match_flag 
           && current_record_id == record_id)
          || (!exact_match_flag
              && current_record_id >= record_id))
        {
          rv = 0;
          ctx->errnum = IPMI_SEL_PARSE_CTX_ERR_SUCCESS;
          ctx->current_sel_entry = sel_parse_entry;
          goto cleanup;
        }
    }

  ctx->errnum = IPMI_SEL_PARSE_CTX_ERR_NOT_FOUND;
  list_iterator_reset(ctx->sel_entries_itr);
 cleanup:
  return rv;
}

int
ipmi_sel_parse_seek_record_id(ipmi_sel_parse_ctx_t ctx, uint16_t record_id)
{
  return _ipmi_sel_parse_find_record_id(ctx,
                                        record_id,
                                        0);
}

int
ipmi_sel_parse_search_record_id(ipmi_sel_parse_ctx_t ctx, uint16_t record_id)
{
  return _ipmi_sel_parse_find_record_id(ctx,
                                        record_id,
                                        1);
}

int
_parse_read_common(ipmi_sel_parse_ctx_t ctx, struct ipmi_sel_parse_entry **sel_parse_entry)
{
  assert(ctx);
  assert(ctx->magic == IPMI_SEL_PARSE_MAGIC);
  assert(sel_parse_entry);

  if (ctx->callback_sel_entry)
    *sel_parse_entry = ctx->callback_sel_entry;
  else
    {
      if (!ctx->sel_entries_itr)
        {
          ctx->errnum = IPMI_SEL_PARSE_CTX_ERR_NO_SEL_ENTRIES;
          return -1;
        }

      *sel_parse_entry = ctx->current_sel_entry;
    }

  if (!sel_parse_entry)
    {
      ctx->errnum = IPMI_SEL_PARSE_CTX_ERR_SEL_ENTRIES_LIST_END;
      return -1;
    }

  return 0;
}

int 
ipmi_sel_parse_read_record_id(ipmi_sel_parse_ctx_t ctx, uint16_t *record_id)
{
  struct ipmi_sel_parse_entry *sel_parse_entry = NULL;

  ERR(ctx && ctx->magic == IPMI_SEL_PARSE_MAGIC);

  SEL_PARSE_ERR_PARAMETERS(record_id);

  if (_parse_read_common(ctx, &sel_parse_entry) < 0)
    return -1;

  if (sel_parse_get_record_header_info(ctx, 
                                       sel_parse_entry, 
                                       record_id, 
                                       NULL) < 0)
    return -1;

  ctx->errnum = IPMI_SEL_PARSE_CTX_ERR_SUCCESS;
  return 0;
}

int
ipmi_sel_parse_read_record_type(ipmi_sel_parse_ctx_t ctx, uint8_t *record_type)
{
  struct ipmi_sel_parse_entry *sel_parse_entry = NULL;

  ERR(ctx && ctx->magic == IPMI_SEL_PARSE_MAGIC);

  SEL_PARSE_ERR_PARAMETERS(record_type);

  if (_parse_read_common(ctx, &sel_parse_entry) < 0)
    return -1;
  
  if (sel_parse_get_record_header_info(ctx, 
                                       sel_parse_entry, 
                                       NULL,
                                       record_type) < 0)
    return -1;

  ctx->errnum = IPMI_SEL_PARSE_CTX_ERR_SUCCESS;
  return 0;
}

int
ipmi_sel_parse_read_timestamp(ipmi_sel_parse_ctx_t ctx, uint32_t *timestamp)
{
  struct ipmi_sel_parse_entry *sel_parse_entry = NULL;

  ERR(ctx && ctx->magic == IPMI_SEL_PARSE_MAGIC);
  
  SEL_PARSE_ERR_PARAMETERS(timestamp);
  
  if (_parse_read_common(ctx, &sel_parse_entry) < 0)
    return -1;
  
  if (sel_parse_get_timestamp(ctx,
                              sel_parse_entry,
                              timestamp) < 0)
    return -1;

  ctx->errnum = IPMI_SEL_PARSE_CTX_ERR_SUCCESS;
  return 0;
}

int
_parse_system_event_common(ipmi_sel_parse_ctx_t ctx,
                           struct ipmi_sel_parse_entry **sel_parse_entry,
                           struct ipmi_sel_system_event_record_data *system_event_record_data)
{
  assert(ctx);
  assert(ctx->magic == IPMI_SEL_PARSE_MAGIC);
  assert(sel_parse_entry);
  assert(system_event_record_data);

  if (_parse_read_common(ctx, sel_parse_entry) < 0)
    return -1;

  if (sel_parse_get_system_event_record(ctx,
                                        *sel_parse_entry,
                                        system_event_record_data) < 0)
    return -1;

  return 0;
}

int 
ipmi_sel_parse_read_generator_id(ipmi_sel_parse_ctx_t ctx, uint8_t *generator_id)
{
  struct ipmi_sel_system_event_record_data system_event_record_data;
  struct ipmi_sel_parse_entry *sel_parse_entry = NULL;

  ERR(ctx && ctx->magic == IPMI_SEL_PARSE_MAGIC);
  
  SEL_PARSE_ERR_PARAMETERS(generator_id);
  
  if (_parse_system_event_common(ctx, &sel_parse_entry, &system_event_record_data) < 0)
    return -1;
  
  *generator_id = system_event_record_data.generator_id;

  ctx->errnum = IPMI_SEL_PARSE_CTX_ERR_SUCCESS;
  return 0;
}

int
ipmi_sel_parse_read_ipmb_device_lun(ipmi_sel_parse_ctx_t ctx, uint8_t *ipmb_device_lun)
{
  struct ipmi_sel_system_event_record_data system_event_record_data;
  struct ipmi_sel_parse_entry *sel_parse_entry = NULL;

  ERR(ctx && ctx->magic == IPMI_SEL_PARSE_MAGIC);
  
  SEL_PARSE_ERR_PARAMETERS(ipmb_device_lun);
  
  if (_parse_system_event_common(ctx, &sel_parse_entry, &system_event_record_data) < 0)
    return -1;
  
  *ipmb_device_lun = system_event_record_data.ipmb_device_lun;

  ctx->errnum = IPMI_SEL_PARSE_CTX_ERR_SUCCESS;
  return 0;
}

int
ipmi_sel_parse_read_channel_number(ipmi_sel_parse_ctx_t ctx, uint8_t *channel_number)
{
  struct ipmi_sel_system_event_record_data system_event_record_data;
  struct ipmi_sel_parse_entry *sel_parse_entry = NULL;

  ERR(ctx && ctx->magic == IPMI_SEL_PARSE_MAGIC);
  
  SEL_PARSE_ERR_PARAMETERS(channel_number);
  
  if (_parse_system_event_common(ctx, &sel_parse_entry, &system_event_record_data) < 0)
    return -1;
  
  *channel_number = system_event_record_data.channel_number;

  ctx->errnum = IPMI_SEL_PARSE_CTX_ERR_SUCCESS;
  return 0;
}

int
ipmi_sel_parse_read_event_message_format_version(ipmi_sel_parse_ctx_t ctx, uint8_t *event_message_format_version)
{
  struct ipmi_sel_system_event_record_data system_event_record_data;
  struct ipmi_sel_parse_entry *sel_parse_entry = NULL;

  ERR(ctx && ctx->magic == IPMI_SEL_PARSE_MAGIC);
  
  SEL_PARSE_ERR_PARAMETERS(event_message_format_version);
  
  if (_parse_system_event_common(ctx, &sel_parse_entry, &system_event_record_data) < 0)
    return -1;
  
  *event_message_format_version = system_event_record_data.event_message_format_version;

  ctx->errnum = IPMI_SEL_PARSE_CTX_ERR_SUCCESS;
  return 0;
}

int
ipmi_sel_parse_read_sensor_type(ipmi_sel_parse_ctx_t ctx, uint8_t *sensor_type)
{
  struct ipmi_sel_system_event_record_data system_event_record_data;
  struct ipmi_sel_parse_entry *sel_parse_entry = NULL;

  ERR(ctx && ctx->magic == IPMI_SEL_PARSE_MAGIC);
  
  SEL_PARSE_ERR_PARAMETERS(sensor_type);
  
  if (_parse_system_event_common(ctx, &sel_parse_entry, &system_event_record_data) < 0)
    return -1;
  
  *sensor_type = system_event_record_data.sensor_type;

  ctx->errnum = IPMI_SEL_PARSE_CTX_ERR_SUCCESS;
  return 0;
}

int
ipmi_sel_parse_read_sensor_number(ipmi_sel_parse_ctx_t ctx, uint8_t *sensor_number)
{
  struct ipmi_sel_system_event_record_data system_event_record_data;
  struct ipmi_sel_parse_entry *sel_parse_entry = NULL;

  ERR(ctx && ctx->magic == IPMI_SEL_PARSE_MAGIC);
  
  SEL_PARSE_ERR_PARAMETERS(sensor_number);
  
  if (_parse_system_event_common(ctx, &sel_parse_entry, &system_event_record_data) < 0)
    return -1;
  
  *sensor_number = system_event_record_data.sensor_number;

  ctx->errnum = IPMI_SEL_PARSE_CTX_ERR_SUCCESS;
  return 0;
}

int
ipmi_sel_parse_read_event_direction(ipmi_sel_parse_ctx_t ctx, uint8_t *event_direction)
{
  struct ipmi_sel_system_event_record_data system_event_record_data;
  struct ipmi_sel_parse_entry *sel_parse_entry = NULL;

  ERR(ctx && ctx->magic == IPMI_SEL_PARSE_MAGIC);
  
  SEL_PARSE_ERR_PARAMETERS(event_direction);
  
  if (_parse_system_event_common(ctx, &sel_parse_entry, &system_event_record_data) < 0)
    return -1;
  
  *event_direction = system_event_record_data.event_direction;

  ctx->errnum = IPMI_SEL_PARSE_CTX_ERR_SUCCESS;
  return 0;
}

int
ipmi_sel_parse_read_event_type_code(ipmi_sel_parse_ctx_t ctx, uint8_t *event_type_code)
{
  struct ipmi_sel_system_event_record_data system_event_record_data;
  struct ipmi_sel_parse_entry *sel_parse_entry = NULL;

  ERR(ctx && ctx->magic == IPMI_SEL_PARSE_MAGIC);
  
  SEL_PARSE_ERR_PARAMETERS(event_type_code);
  
  if (_parse_system_event_common(ctx, &sel_parse_entry, &system_event_record_data) < 0)
    return -1;
  
  *event_type_code = system_event_record_data.event_type_code;

  ctx->errnum = IPMI_SEL_PARSE_CTX_ERR_SUCCESS;
  return 0;
}

int
ipmi_sel_parse_read_event_data1(ipmi_sel_parse_ctx_t ctx, uint8_t *event_data1)
{
  struct ipmi_sel_system_event_record_data system_event_record_data;
  struct ipmi_sel_parse_entry *sel_parse_entry = NULL;

  ERR(ctx && ctx->magic == IPMI_SEL_PARSE_MAGIC);
  
  SEL_PARSE_ERR_PARAMETERS(event_data1);
  
  if (_parse_system_event_common(ctx, &sel_parse_entry, &system_event_record_data) < 0)
    return -1;
  
  /* note: not a mistake, event_data3 is << 2 and event_data2 is << 4 */
  *event_data1 = (system_event_record_data.offset_from_event_reading_type_code
                  | system_event_record_data.event_data3_flag << 2
                  | system_event_record_data.event_data2_flag << 4);
  
  ctx->errnum = IPMI_SEL_PARSE_CTX_ERR_SUCCESS;
  return 0;
}

int
ipmi_sel_parse_read_event_data1_offset_from_event_reading_type_code(ipmi_sel_parse_ctx_t ctx,
                                                                    uint8_t *event_data1_offset)
{
  struct ipmi_sel_system_event_record_data system_event_record_data;
  struct ipmi_sel_parse_entry *sel_parse_entry = NULL;

  ERR(ctx && ctx->magic == IPMI_SEL_PARSE_MAGIC);
  
  SEL_PARSE_ERR_PARAMETERS(event_data1_offset);
  
  if (_parse_system_event_common(ctx, &sel_parse_entry, &system_event_record_data) < 0)
    return -1;
  
  *event_data1_offset = system_event_record_data.offset_from_event_reading_type_code;
  
  ctx->errnum = IPMI_SEL_PARSE_CTX_ERR_SUCCESS;
  return 0;
}

int
ipmi_sel_parse_read_event_data1_event_data2_flag(ipmi_sel_parse_ctx_t ctx,
                                                 uint8_t *event_data2_flag)
{
  struct ipmi_sel_system_event_record_data system_event_record_data;
  struct ipmi_sel_parse_entry *sel_parse_entry = NULL;

  ERR(ctx && ctx->magic == IPMI_SEL_PARSE_MAGIC);
  
  SEL_PARSE_ERR_PARAMETERS(event_data2_flag);
  
  if (_parse_system_event_common(ctx, &sel_parse_entry, &system_event_record_data) < 0)
    return -1;
  
  *event_data2_flag = system_event_record_data.event_data2_flag;
  
  ctx->errnum = IPMI_SEL_PARSE_CTX_ERR_SUCCESS;
  return 0;
}

int
ipmi_sel_parse_read_event_data1_event_data3_flag(ipmi_sel_parse_ctx_t ctx,
                                                 uint8_t *event_data3_flag)
{
  struct ipmi_sel_system_event_record_data system_event_record_data;
  struct ipmi_sel_parse_entry *sel_parse_entry = NULL;

  ERR(ctx && ctx->magic == IPMI_SEL_PARSE_MAGIC);
  
  SEL_PARSE_ERR_PARAMETERS(event_data3_flag);
  
  if (_parse_system_event_common(ctx, &sel_parse_entry, &system_event_record_data) < 0)
    return -1;
  
  *event_data3_flag = system_event_record_data.event_data3_flag;
  
  ctx->errnum = IPMI_SEL_PARSE_CTX_ERR_SUCCESS;
  return 0;
}

int
ipmi_sel_parse_read_event_data2(ipmi_sel_parse_ctx_t ctx, uint8_t *event_data2)
{
  struct ipmi_sel_system_event_record_data system_event_record_data;
  struct ipmi_sel_parse_entry *sel_parse_entry = NULL;

  ERR(ctx && ctx->magic == IPMI_SEL_PARSE_MAGIC);
  
  SEL_PARSE_ERR_PARAMETERS(event_data2);
  
  if (_parse_system_event_common(ctx, &sel_parse_entry, &system_event_record_data) < 0)
    return -1;
  
  *event_data2 = system_event_record_data.event_data2;

  ctx->errnum = IPMI_SEL_PARSE_CTX_ERR_SUCCESS;
  return 0;
}

int
ipmi_sel_parse_read_event_data3(ipmi_sel_parse_ctx_t ctx, uint8_t *event_data3)
{
  struct ipmi_sel_system_event_record_data system_event_record_data;
  struct ipmi_sel_parse_entry *sel_parse_entry = NULL;

  ERR(ctx && ctx->magic == IPMI_SEL_PARSE_MAGIC);
  
  SEL_PARSE_ERR_PARAMETERS(event_data3);
  
  if (_parse_system_event_common(ctx, &sel_parse_entry, &system_event_record_data) < 0)
    return -1;
  
  *event_data3 = system_event_record_data.event_data3;

  ctx->errnum = IPMI_SEL_PARSE_CTX_ERR_SUCCESS;
  return 0;
}

int
ipmi_sel_parse_read_manufacturer_id(ipmi_sel_parse_ctx_t ctx, uint32_t *manufacturer_id)
{
  struct ipmi_sel_parse_entry *sel_parse_entry = NULL;

  ERR(ctx && ctx->magic == IPMI_SEL_PARSE_MAGIC);
  
  SEL_PARSE_ERR_PARAMETERS(manufacturer_id);
  
  if (_parse_read_common(ctx, &sel_parse_entry) < 0)
    return -1;
  
  if (sel_parse_get_manufacturer_id(ctx,
                                    sel_parse_entry,
                                    manufacturer_id) < 0)
    return -1;

  ctx->errnum = IPMI_SEL_PARSE_CTX_ERR_SUCCESS;
  return 0;
}

int
ipmi_sel_parse_read_oem(ipmi_sel_parse_ctx_t ctx, uint8_t *buf, unsigned int buflen)
{
  struct ipmi_sel_parse_entry *sel_parse_entry = NULL;
  int rv = 0;

  ERR(ctx && ctx->magic == IPMI_SEL_PARSE_MAGIC);
  
  SEL_PARSE_ERR_PARAMETERS(buf && buflen);
  
  if (_parse_read_common(ctx, &sel_parse_entry) < 0)
    return -1;
  
  if ((rv = sel_parse_get_oem(ctx,
                              sel_parse_entry,
                              buf,
                              buflen)) < 0)
    return -1;
  
  ctx->errnum = IPMI_SEL_PARSE_CTX_ERR_SUCCESS;
  return rv;
}

int 
ipmi_sel_parse_read_record(ipmi_sel_parse_ctx_t ctx,
                           uint8_t *buf,
                           unsigned int buflen)
{
  struct ipmi_sel_parse_entry *sel_parse_entry = NULL;
  int rv = 0;

  ERR(ctx && ctx->magic == IPMI_SEL_PARSE_MAGIC);
  
  SEL_PARSE_ERR_PARAMETERS(buf && buflen);
  
  if (_parse_read_common(ctx, &sel_parse_entry) < 0)
    return -1;
  
  if (buflen < sel_parse_entry->sel_event_record_len)
    {
      ctx->errnum = IPMI_SEL_PARSE_CTX_ERR_OVERFLOW;
      return -1;
    }

  memcpy(buf, 
         sel_parse_entry->sel_event_record, 
         sel_parse_entry->sel_event_record_len);
  
  rv = sel_parse_entry->sel_event_record_len;
  
  ctx->errnum = IPMI_SEL_PARSE_CTX_ERR_SUCCESS;
  return rv;
}

int
ipmi_sel_parse_read_record_string(ipmi_sel_parse_ctx_t ctx,
                                  char *fmt,
                                  char *buf,
                                  unsigned int buflen,
                                  unsigned int flags)
{
  struct ipmi_sel_parse_entry *sel_parse_entry = NULL;

  ERR(ctx && ctx->magic == IPMI_SEL_PARSE_MAGIC);
  
  SEL_PARSE_ERR_PARAMETERS(fmt 
                           && buf 
                           && buflen
                           && !(flags & ~IPMI_SEL_PARSE_READ_STRING_MASK));

  if (_parse_read_common(ctx, &sel_parse_entry) < 0)
    return -1;
 
  if (sel_parse_entry->sel_event_record_len < IPMI_SEL_RECORD_LENGTH)
    {
      ctx->errnum = IPMI_SEL_PARSE_CTX_ERR_INVALID_SEL_ENTRY;
      return -1;
    }
  
  return sel_parse_format_record_string(ctx,
                                        fmt,
                                        sel_parse_entry->sel_event_record,
                                        sel_parse_entry->sel_event_record_len,
                                        buf,
                                        buflen,
                                        flags);
}
