/*****************************************************************************\
 *  $Id: ipmi-sel-parse-common.c,v 1.1.2.4 2009-01-06 22:14:57 chu11 Exp $
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
#include "freeipmi/record-format/ipmi-sel-record-format.h"
#include "freeipmi/spec/ipmi-comp-code-spec.h"
#include "freeipmi/util/ipmi-sensor-and-event-code-tables-util.h"
#include "freeipmi/util/ipmi-util.h"

#include "ipmi-sel-parse-defs.h"
#include "ipmi-sel-parse-common.h"

#include "libcommon/ipmi-err-wrappers.h"
#include "libcommon/ipmi-fiid-wrappers.h"

#include "freeipmi-portability.h"

int
sel_parse_get_reservation_id(ipmi_sel_parse_ctx_t ctx,
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

int
sel_parse_get_record_header_info(ipmi_sel_parse_ctx_t ctx,
                                 struct ipmi_sel_parse_entry *sel_parse_entry,
                                 uint16_t *record_id,
                                 uint8_t *record_type)
{
  fiid_obj_t obj_sel_record_header = NULL;
  uint64_t val;
  int rv = -1;
  
  assert(ctx);
  assert(ctx->magic == IPMI_SEL_PARSE_MAGIC);
  assert(sel_parse_entry);
  assert(record_id || record_type);
  
  if (sel_parse_entry->sel_event_record_len < IPMI_SEL_RECORD_HEADER_LENGTH)
    {
      ctx->errnum = IPMI_SEL_PARSE_CTX_ERR_INVALID_SEL_ENTRY;
      goto cleanup;
    }
  
  SEL_PARSE_FIID_OBJ_CREATE_CLEANUP(obj_sel_record_header, tmpl_sel_record_header);
  
  SEL_PARSE_FIID_OBJ_SET_ALL_CLEANUP(obj_sel_record_header, 
                                     sel_parse_entry->sel_event_record,
                                     sel_parse_entry->sel_event_record_len);
  
  if (record_id)
    {
      SEL_PARSE_FIID_OBJ_GET_CLEANUP(obj_sel_record_header,
                                     "record_id",
                                     &val);
      (*record_id) = val;
    }
  
  if (record_type)
    {
      SEL_PARSE_FIID_OBJ_GET_CLEANUP(obj_sel_record_header,
                                     "record_type",
                                     &val);
      (*record_type) = val;
    }

  rv = 0;
 cleanup:
  SEL_PARSE_FIID_OBJ_DESTROY(obj_sel_record_header);
  return rv;
}
  
int
sel_parse_get_timestamp(ipmi_sel_parse_ctx_t ctx,
                        struct ipmi_sel_parse_entry *sel_parse_entry,
                        uint32_t *timestamp)
{
  fiid_obj_t obj_sel_record = NULL;
  uint8_t record_type;
  int record_type_class;
  uint64_t val;
  int rv = -1;

  assert(ctx);
  assert(ctx->magic == IPMI_SEL_PARSE_MAGIC);
  assert(sel_parse_entry);
  assert(timestamp);

  if (sel_parse_entry->sel_event_record_len < IPMI_SEL_RECORD_LENGTH)
    {
      ctx->errnum = IPMI_SEL_PARSE_CTX_ERR_INVALID_SEL_ENTRY;
      goto cleanup;
    }

  
  if (sel_parse_get_record_header_info(ctx, 
                                       sel_parse_entry, 
                                       NULL, 
                                       &record_type) < 0)
    goto cleanup;

  record_type_class = ipmi_sel_record_type_class(record_type);

  if (record_type_class != IPMI_SEL_RECORD_TYPE_CLASS_SYSTEM_EVENT_RECORD
      && record_type_class != IPMI_SEL_RECORD_TYPE_CLASS_TIMESTAMPED_OEM_RECORD)
    {
      ctx->errnum = IPMI_SEL_PARSE_CTX_ERR_INVALID_SEL_ENTRY;
      goto cleanup;
    }
  
  if (record_type_class == IPMI_SEL_RECORD_TYPE_CLASS_SYSTEM_EVENT_RECORD)
    SEL_PARSE_FIID_OBJ_CREATE_CLEANUP(obj_sel_record, tmpl_sel_system_event_record);
  else
    SEL_PARSE_FIID_OBJ_CREATE_CLEANUP(obj_sel_record, tmpl_sel_timestamped_oem_record);
  
  SEL_PARSE_FIID_OBJ_SET_ALL_CLEANUP(obj_sel_record, 
                                     sel_parse_entry->sel_event_record,
                                     sel_parse_entry->sel_event_record_len);
  
  SEL_PARSE_FIID_OBJ_GET_CLEANUP(obj_sel_record,
                                 "timestamp",
                                 &val);
  (*timestamp) = val;
  
  rv = 0;
 cleanup:
  SEL_PARSE_FIID_OBJ_DESTROY(obj_sel_record);
  return rv;
}

int
sel_parse_get_manufacturer_id(ipmi_sel_parse_ctx_t ctx,
                              struct ipmi_sel_parse_entry *sel_parse_entry,
                              uint32_t *manufacturer_id)
{
  fiid_obj_t obj_sel_record = NULL;
  uint8_t record_type;
  int record_type_class;
  uint64_t val;
  int rv = -1;

  assert(ctx);
  assert(ctx->magic == IPMI_SEL_PARSE_MAGIC);
  assert(sel_parse_entry);
  assert(manufacturer_id);

  if (sel_parse_entry->sel_event_record_len < IPMI_SEL_RECORD_LENGTH)
    {
      ctx->errnum = IPMI_SEL_PARSE_CTX_ERR_INVALID_SEL_ENTRY;
      goto cleanup;
    }

  if (sel_parse_get_record_header_info(ctx, 
                                       sel_parse_entry, 
                                       NULL, 
                                       &record_type) < 0)
    goto cleanup;

  record_type_class = ipmi_sel_record_type_class(record_type);

  if (record_type_class != IPMI_SEL_RECORD_TYPE_CLASS_TIMESTAMPED_OEM_RECORD)
    {
      ctx->errnum = IPMI_SEL_PARSE_CTX_ERR_INVALID_SEL_ENTRY;
      goto cleanup;
    }
  
  SEL_PARSE_FIID_OBJ_CREATE_CLEANUP(obj_sel_record, tmpl_sel_timestamped_oem_record);
  
  SEL_PARSE_FIID_OBJ_SET_ALL_CLEANUP(obj_sel_record, 
                                     sel_parse_entry->sel_event_record,
                                     sel_parse_entry->sel_event_record_len);
  
  SEL_PARSE_FIID_OBJ_GET_CLEANUP(obj_sel_record,
                                 "manufacturer_id",
                                 &val);
  (*manufacturer_id) = val;
  
  rv = 0;
 cleanup:
  SEL_PARSE_FIID_OBJ_DESTROY(obj_sel_record);
  return rv;
}

int
sel_parse_get_oem(ipmi_sel_parse_ctx_t ctx,
                  struct ipmi_sel_parse_entry *sel_parse_entry,
                  uint8_t *buf,
                  unsigned int buflen)
{
  fiid_obj_t obj_sel_record = NULL;
  uint8_t record_type;
  int record_type_class;
  int32_t len;
  int rv = -1;

  assert(ctx);
  assert(ctx->magic == IPMI_SEL_PARSE_MAGIC);
  assert(sel_parse_entry);
  assert(buf);
  assert(buflen);

  if (sel_parse_entry->sel_event_record_len < IPMI_SEL_RECORD_LENGTH)
    {
      ctx->errnum = IPMI_SEL_PARSE_CTX_ERR_INVALID_SEL_ENTRY;
      goto cleanup;
    }

  if (sel_parse_get_record_header_info(ctx, 
                                       sel_parse_entry, 
                                       NULL, 
                                       &record_type) < 0)
    goto cleanup;

  record_type_class = ipmi_sel_record_type_class(record_type);

  if (record_type_class != IPMI_SEL_RECORD_TYPE_CLASS_TIMESTAMPED_OEM_RECORD
      && record_type_class != IPMI_SEL_RECORD_TYPE_CLASS_NON_TIMESTAMPED_OEM_RECORD)
    {
      ctx->errnum = IPMI_SEL_PARSE_CTX_ERR_INVALID_SEL_ENTRY;
      goto cleanup;
    }
  
  if (record_type_class == IPMI_SEL_RECORD_TYPE_CLASS_TIMESTAMPED_OEM_RECORD)
    SEL_PARSE_FIID_OBJ_CREATE_CLEANUP(obj_sel_record, tmpl_sel_timestamped_oem_record);
  else
    SEL_PARSE_FIID_OBJ_CREATE_CLEANUP(obj_sel_record, tmpl_sel_non_timestamped_oem_record);
  
  SEL_PARSE_FIID_OBJ_SET_ALL_CLEANUP(obj_sel_record, 
                                     sel_parse_entry->sel_event_record,
                                     sel_parse_entry->sel_event_record_len);
  
  SEL_PARSE_FIID_OBJ_GET_DATA_LEN_CLEANUP(len,
                                          obj_sel_record,
                                          "oem_defined",
                                          buf,
                                          buflen);
  
  rv = len;
 cleanup:
  SEL_PARSE_FIID_OBJ_DESTROY(obj_sel_record);
  return rv;
}

int
sel_parse_get_system_event_record(ipmi_sel_parse_ctx_t ctx,
                                  struct ipmi_sel_parse_entry *sel_parse_entry,
                                  struct ipmi_sel_system_event_record_data *system_event_record_data)
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
  assert(system_event_record_data);

  if (sel_parse_entry->sel_event_record_len < IPMI_SEL_RECORD_LENGTH)
    {
      ctx->errnum = IPMI_SEL_PARSE_CTX_ERR_INVALID_SEL_ENTRY;
      goto cleanup;
    }
  
  if (sel_parse_get_record_header_info(ctx, 
                                       sel_parse_entry, 
                                       NULL, 
                                       &record_type) < 0)
    goto cleanup;
  
  record_type_class = ipmi_sel_record_type_class(record_type);

  if (record_type_class != IPMI_SEL_RECORD_TYPE_CLASS_SYSTEM_EVENT_RECORD)
    {
      ctx->errnum = IPMI_SEL_PARSE_CTX_ERR_INVALID_SEL_ENTRY;
      goto cleanup;
    }

  SEL_PARSE_FIID_OBJ_CREATE_CLEANUP(obj_sel_system_event_record,
                                    tmpl_sel_system_event_record_event_fields);
  
  SEL_PARSE_FIID_OBJ_SET_ALL_CLEANUP(obj_sel_system_event_record, 
                                     sel_parse_entry->sel_event_record,
                                     sel_parse_entry->sel_event_record_len);
  
  SEL_PARSE_FIID_OBJ_GET_CLEANUP (obj_sel_system_event_record, "timestamp", &val);
  system_event_record_data->timestamp = val;

  SEL_PARSE_FIID_OBJ_GET_CLEANUP (obj_sel_system_event_record, "generator_id.id_type", &val);
  generator_id_type = val;
      
  SEL_PARSE_FIID_OBJ_GET_CLEANUP (obj_sel_system_event_record, "generator_id.id", &val);
  generator_id_address = val;
      
  system_event_record_data->generator_id = ((generator_id_address << 1) | generator_id_type);

  SEL_PARSE_FIID_OBJ_GET_CLEANUP (obj_sel_system_event_record, "ipmb_device_lun", &val);
  system_event_record_data->ipmb_device_lun = val;

  SEL_PARSE_FIID_OBJ_GET_CLEANUP (obj_sel_system_event_record, "channel_number", &val);
  system_event_record_data->channel_number = val;

  SEL_PARSE_FIID_OBJ_GET_CLEANUP (obj_sel_system_event_record, "event_message_format_version", &val);
  system_event_record_data->event_message_format_version = val;

  SEL_PARSE_FIID_OBJ_GET_CLEANUP (obj_sel_system_event_record, "sensor_type", &val);
  system_event_record_data->sensor_type = val;

  SEL_PARSE_FIID_OBJ_GET_CLEANUP (obj_sel_system_event_record, "sensor_number", &val);
  system_event_record_data->sensor_number = val;

  SEL_PARSE_FIID_OBJ_GET_CLEANUP (obj_sel_system_event_record, "event_type_code", &val);
  system_event_record_data->event_type_code = val;

  SEL_PARSE_FIID_OBJ_GET_CLEANUP (obj_sel_system_event_record, "event_dir", &val);
  system_event_record_data->event_direction = val;
  
  SEL_PARSE_FIID_OBJ_GET_CLEANUP (obj_sel_system_event_record, "offset_from_event_reading_type_code", &val);
  system_event_record_data->offset_from_event_reading_type_code = val;

  SEL_PARSE_FIID_OBJ_GET_CLEANUP (obj_sel_system_event_record, "event_data2_flag", &val);
  system_event_record_data->event_data2_flag = val;

  SEL_PARSE_FIID_OBJ_GET_CLEANUP (obj_sel_system_event_record, "event_data3_flag", &val);
  system_event_record_data->event_data3_flag = val;

  SEL_PARSE_FIID_OBJ_GET_CLEANUP (obj_sel_system_event_record, "event_data2", &val);
  system_event_record_data->event_data2 = val;

  SEL_PARSE_FIID_OBJ_GET_CLEANUP (obj_sel_system_event_record, "event_data3", &val);
  system_event_record_data->event_data3 = val;

  rv = 0;
 cleanup:
  SEL_PARSE_FIID_OBJ_DESTROY(obj_sel_system_event_record);
  return rv;
}

int
sel_parse_get_previous_state_or_severity(ipmi_sel_parse_ctx_t ctx,
                                         struct ipmi_sel_parse_entry *sel_parse_entry,
                                         uint8_t *previous_offset_from_event_reading_type_code,
                                         uint8_t *offset_from_severity_event_reading_type_code)
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
  assert(previous_offset_from_event_reading_type_code);
  assert(offset_from_severity_event_reading_type_code);

  if (sel_parse_entry->sel_event_record_len < IPMI_SEL_RECORD_LENGTH)
    {
      ctx->errnum = IPMI_SEL_PARSE_CTX_ERR_INVALID_SEL_ENTRY;
      goto cleanup;
    }
  
  if (sel_parse_get_record_header_info(ctx, 
                                       sel_parse_entry, 
                                       NULL, 
                                       &record_type) < 0)
    goto cleanup;
  
  record_type_class = ipmi_sel_record_type_class(record_type);
  
  if (record_type_class != IPMI_SEL_RECORD_TYPE_CLASS_SYSTEM_EVENT_RECORD)
    {
      ctx->errnum = IPMI_SEL_PARSE_CTX_ERR_INVALID_SEL_ENTRY;
      goto cleanup;
    }
  
  SEL_PARSE_FIID_OBJ_CREATE_CLEANUP(obj_sel_system_event_record,
                                    tmpl_sel_system_event_record_discrete_previous_state_severity);
  
  SEL_PARSE_FIID_OBJ_SET_ALL_CLEANUP(obj_sel_system_event_record, 
                                     sel_parse_entry->sel_event_record,
                                     sel_parse_entry->sel_event_record_len);
  
  SEL_PARSE_FIID_OBJ_GET_CLEANUP (obj_sel_system_event_record, "previous_offset_from_event_reading_type_code", &val);
  *previous_offset_from_event_reading_type_code = val;

  SEL_PARSE_FIID_OBJ_GET_CLEANUP (obj_sel_system_event_record, "offset_from_severity_event_reading_type_code", &val);
  *offset_from_severity_event_reading_type_code = val;

  rv = 0;
 cleanup:
  SEL_PARSE_FIID_OBJ_DESTROY(obj_sel_system_event_record);
  return rv;
}
