/*
 * Copyright (C) 2003-2012 FreeIPMI Core Team
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 * 
 */

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
#include "freeipmi/fiid/fiid.h"
#include "freeipmi/record-format/ipmi-sel-record-format.h"
#include "freeipmi/spec/ipmi-comp-code-spec.h"
#include "freeipmi/util/ipmi-sensor-and-event-code-tables-util.h"
#include "freeipmi/util/ipmi-util.h"

#include "ipmi-sel-parse-common.h"
#include "ipmi-sel-parse-defs.h"
#include "ipmi-sel-parse-trace.h"
#include "ipmi-sel-parse-util.h"

#include "libcommon/ipmi-fiid-util.h"

#include "freeipmi-portability.h"

int
sel_parse_get_reservation_id (ipmi_sel_parse_ctx_t ctx,
                              uint16_t *reservation_id,
                              unsigned int *is_insufficient_privilege_level)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  int rv = -1;

  assert (ctx);
  assert (ctx->magic == IPMI_SEL_PARSE_CTX_MAGIC);
  assert (reservation_id);

  if (is_insufficient_privilege_level)
    (*is_insufficient_privilege_level) = 0;

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_reserve_sel_rs)))
    {
      SEL_PARSE_ERRNO_TO_SEL_PARSE_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (ipmi_cmd_reserve_sel (ctx->ipmi_ctx, obj_cmd_rs) < 0)
    {
      /* achu:
       *
       * IPMI spec states reservation ID 0000h should be used if Reserve SEL
       * is not supported.
       */
      if (ipmi_ctx_errnum (ctx->ipmi_ctx) == IPMI_ERR_BAD_COMPLETION_CODE
          && ipmi_check_completion_code (obj_cmd_rs,
                                         IPMI_COMP_CODE_INVALID_COMMAND) == 1)
        {
          *reservation_id = 0;
          goto out;
        }

      /* IPMI Workaround (achu)
       *
       * Discovered on Supermicro H8QME with SIMSO daughter card.
       *
       * For some reason motherboard requires Operator privilege
       * instead of User privilege.  If we get
       * IPMI_COMP_CODE_INSUFFICIENT_PRIVILEGE_LEVEL, let caller know
       * and decide if it's ok to ignore it.  Some operations may be
       * able to live without a reservation ID.
       */
      if (is_insufficient_privilege_level
          && ipmi_ctx_errnum (ctx->ipmi_ctx) == IPMI_ERR_PRIVILEGE_LEVEL_INSUFFICIENT
          && ipmi_check_completion_code (obj_cmd_rs,
                                         IPMI_COMP_CODE_INSUFFICIENT_PRIVILEGE_LEVEL) == 1)
        (*is_insufficient_privilege_level) = 1;

      SEL_PARSE_SET_ERRNUM (ctx, IPMI_SEL_PARSE_ERR_IPMI_ERROR);
      goto cleanup;
    }

  if (FIID_OBJ_GET (obj_cmd_rs,
                    "reservation_id",
                    &val) < 0)
    {
      SEL_PARSE_FIID_OBJECT_ERROR_TO_SEL_PARSE_ERRNUM (ctx, obj_cmd_rs);
      goto cleanup;
    }
  *reservation_id = val;

 out:
  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

int
sel_parse_get_record_header_info (ipmi_sel_parse_ctx_t ctx,
                                  struct ipmi_sel_parse_entry *sel_parse_entry,
                                  uint16_t *record_id,
                                  uint8_t *record_type)
{
  fiid_obj_t obj_sel_record_header = NULL;
  uint64_t val;
  int rv = -1;

  assert (ctx);
  assert (ctx->magic == IPMI_SEL_PARSE_CTX_MAGIC);
  assert (sel_parse_entry);
  assert (record_id || record_type);

  if (sel_parse_entry->sel_event_record_len < IPMI_SEL_RECORD_HEADER_LENGTH)
    {
      ctx->errnum = IPMI_SEL_PARSE_ERR_INVALID_SEL_ENTRY;
      goto cleanup;
    }

  if (!(obj_sel_record_header = fiid_obj_create (tmpl_sel_record_header)))
    {
      SEL_PARSE_ERRNO_TO_SEL_PARSE_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (fiid_obj_set_all (obj_sel_record_header,
                        sel_parse_entry->sel_event_record,
                        sel_parse_entry->sel_event_record_len) < 0)
    {
      SEL_PARSE_FIID_OBJECT_ERROR_TO_SEL_PARSE_ERRNUM (ctx, obj_sel_record_header);
      goto cleanup;
    }

  if (record_id)
    {
      if (FIID_OBJ_GET (obj_sel_record_header,
                        "record_id",
                        &val) < 0)
        {
          SEL_PARSE_FIID_OBJECT_ERROR_TO_SEL_PARSE_ERRNUM (ctx, obj_sel_record_header);
          goto cleanup;
        }
      (*record_id) = val;
    }

  if (record_type)
    {
      if (FIID_OBJ_GET (obj_sel_record_header,
                        "record_type",
                        &val) < 0)
        {
          SEL_PARSE_FIID_OBJECT_ERROR_TO_SEL_PARSE_ERRNUM (ctx, obj_sel_record_header);
          goto cleanup;
        }
      (*record_type) = val;

      if (ctx->flags & IPMI_SEL_PARSE_FLAGS_ASSUME_SYTEM_EVENT_RECORDS
	  && !IPMI_SEL_RECORD_TYPE_VALID ((*record_type)))
	(*record_type) = IPMI_SEL_RECORD_TYPE_SYSTEM_EVENT_RECORD;
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_sel_record_header);
  return (rv);
}

int
sel_parse_get_timestamp (ipmi_sel_parse_ctx_t ctx,
                         struct ipmi_sel_parse_entry *sel_parse_entry,
                         uint32_t *timestamp)
{
  fiid_obj_t obj_sel_record = NULL;
  uint8_t record_type;
  int record_type_class;
  uint64_t val;
  int rv = -1;

  assert (ctx);
  assert (ctx->magic == IPMI_SEL_PARSE_CTX_MAGIC);
  assert (sel_parse_entry);
  assert (timestamp);

  if (sel_parse_entry->sel_event_record_len < IPMI_SEL_RECORD_LENGTH)
    {
      ctx->errnum = IPMI_SEL_PARSE_ERR_INVALID_SEL_ENTRY;
      goto cleanup;
    }

  if (sel_parse_get_record_header_info (ctx,
                                        sel_parse_entry,
                                        NULL,
                                        &record_type) < 0)
    goto cleanup;

  record_type_class = ipmi_sel_record_type_class (record_type);

  if (record_type_class != IPMI_SEL_RECORD_TYPE_CLASS_SYSTEM_EVENT_RECORD
      && record_type_class != IPMI_SEL_RECORD_TYPE_CLASS_TIMESTAMPED_OEM_RECORD)
    {
      ctx->errnum = IPMI_SEL_PARSE_ERR_INVALID_SEL_ENTRY;
      goto cleanup;
    }

  if (record_type_class == IPMI_SEL_RECORD_TYPE_CLASS_SYSTEM_EVENT_RECORD)
    {
      if (!(obj_sel_record = fiid_obj_create (tmpl_sel_system_event_record)))
        {
          SEL_PARSE_ERRNO_TO_SEL_PARSE_ERRNUM (ctx, errno);
          goto cleanup;
        }
    }
  else
    {
      if (!(obj_sel_record = fiid_obj_create (tmpl_sel_timestamped_oem_record)))
        {
          SEL_PARSE_ERRNO_TO_SEL_PARSE_ERRNUM (ctx, errno);
          goto cleanup;
        }
    }

  if (fiid_obj_set_all (obj_sel_record,
                        sel_parse_entry->sel_event_record,
                        sel_parse_entry->sel_event_record_len) < 0)
    {
      SEL_PARSE_FIID_OBJECT_ERROR_TO_SEL_PARSE_ERRNUM (ctx, obj_sel_record);
      goto cleanup;
    }

  if (FIID_OBJ_GET (obj_sel_record,
                    "timestamp",
                    &val) < 0)
    {
      SEL_PARSE_FIID_OBJECT_ERROR_TO_SEL_PARSE_ERRNUM (ctx, obj_sel_record);
      goto cleanup;
    }
  (*timestamp) = val;

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_sel_record);
  return (rv);
}

int
sel_parse_get_manufacturer_id (ipmi_sel_parse_ctx_t ctx,
                               struct ipmi_sel_parse_entry *sel_parse_entry,
                               uint32_t *manufacturer_id)
{
  fiid_obj_t obj_sel_record = NULL;
  uint8_t record_type;
  int record_type_class;
  uint64_t val;
  int rv = -1;

  assert (ctx);
  assert (ctx->magic == IPMI_SEL_PARSE_CTX_MAGIC);
  assert (sel_parse_entry);
  assert (manufacturer_id);

  if (sel_parse_entry->sel_event_record_len < IPMI_SEL_RECORD_LENGTH)
    {
      ctx->errnum = IPMI_SEL_PARSE_ERR_INVALID_SEL_ENTRY;
      goto cleanup;
    }

  if (sel_parse_get_record_header_info (ctx,
                                        sel_parse_entry,
                                        NULL,
                                        &record_type) < 0)
    goto cleanup;

  record_type_class = ipmi_sel_record_type_class (record_type);

  if (record_type_class != IPMI_SEL_RECORD_TYPE_CLASS_TIMESTAMPED_OEM_RECORD)
    {
      ctx->errnum = IPMI_SEL_PARSE_ERR_INVALID_SEL_ENTRY;
      goto cleanup;
    }

  if (!(obj_sel_record = fiid_obj_create (tmpl_sel_timestamped_oem_record)))
    {
      SEL_PARSE_ERRNO_TO_SEL_PARSE_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (fiid_obj_set_all (obj_sel_record,
                        sel_parse_entry->sel_event_record,
                        sel_parse_entry->sel_event_record_len) < 0)
    {
      SEL_PARSE_FIID_OBJECT_ERROR_TO_SEL_PARSE_ERRNUM (ctx, obj_sel_record);
      goto cleanup;
    }

  if (FIID_OBJ_GET (obj_sel_record,
                    "manufacturer_id",
                    &val) < 0)
    {
      SEL_PARSE_FIID_OBJECT_ERROR_TO_SEL_PARSE_ERRNUM (ctx, obj_sel_record);
      goto cleanup;
    }
  (*manufacturer_id) = val;

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_sel_record);
  return (rv);
}

int
sel_parse_get_oem (ipmi_sel_parse_ctx_t ctx,
                   struct ipmi_sel_parse_entry *sel_parse_entry,
                   uint8_t *buf,
                   unsigned int buflen)
{
  fiid_obj_t obj_sel_record = NULL;
  uint8_t record_type;
  int record_type_class;
  int len;
  int rv = -1;

  assert (ctx);
  assert (ctx->magic == IPMI_SEL_PARSE_CTX_MAGIC);
  assert (sel_parse_entry);
  assert (buf);
  assert (buflen);

  if (sel_parse_entry->sel_event_record_len < IPMI_SEL_RECORD_LENGTH)
    {
      ctx->errnum = IPMI_SEL_PARSE_ERR_INVALID_SEL_ENTRY;
      goto cleanup;
    }

  if (sel_parse_get_record_header_info (ctx,
                                        sel_parse_entry,
                                        NULL,
                                        &record_type) < 0)
    goto cleanup;

  record_type_class = ipmi_sel_record_type_class (record_type);

  if (record_type_class != IPMI_SEL_RECORD_TYPE_CLASS_TIMESTAMPED_OEM_RECORD
      && record_type_class != IPMI_SEL_RECORD_TYPE_CLASS_NON_TIMESTAMPED_OEM_RECORD)
    {
      ctx->errnum = IPMI_SEL_PARSE_ERR_INVALID_SEL_ENTRY;
      goto cleanup;
    }

  if (record_type_class == IPMI_SEL_RECORD_TYPE_CLASS_TIMESTAMPED_OEM_RECORD)
    {
      if (!(obj_sel_record = fiid_obj_create (tmpl_sel_timestamped_oem_record)))
        {
          SEL_PARSE_ERRNO_TO_SEL_PARSE_ERRNUM (ctx, errno);
          goto cleanup;
        }
    }
  else
    {
      if (!(obj_sel_record = fiid_obj_create (tmpl_sel_non_timestamped_oem_record)))
        {
          SEL_PARSE_ERRNO_TO_SEL_PARSE_ERRNUM (ctx, errno);
          goto cleanup;
        }
    }

  if (fiid_obj_set_all (obj_sel_record,
                        sel_parse_entry->sel_event_record,
                        sel_parse_entry->sel_event_record_len) < 0)
    {
      SEL_PARSE_FIID_OBJECT_ERROR_TO_SEL_PARSE_ERRNUM (ctx, obj_sel_record);
      goto cleanup;
    }

  if ((len = fiid_obj_get_data (obj_sel_record,
                                "oem_defined",
                                buf,
                                buflen)) < 0)
    {
      ERR_TRACE (ipmi_sel_parse_ctx_errormsg (ctx), ipmi_sel_parse_ctx_errnum (ctx));
      goto cleanup;
    }

  rv = len;
 cleanup:
  fiid_obj_destroy (obj_sel_record);
  return (rv);
}

int
sel_parse_get_system_event_record (ipmi_sel_parse_ctx_t ctx,
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

  assert (ctx);
  assert (ctx->magic == IPMI_SEL_PARSE_CTX_MAGIC);
  assert (sel_parse_entry);
  assert (system_event_record_data);

  if (sel_parse_entry->sel_event_record_len < IPMI_SEL_RECORD_LENGTH)
    {
      ctx->errnum = IPMI_SEL_PARSE_ERR_INVALID_SEL_ENTRY;
      goto cleanup;
    }

  if (sel_parse_get_record_header_info (ctx,
                                        sel_parse_entry,
                                        NULL,
                                        &record_type) < 0)
    goto cleanup;

  record_type_class = ipmi_sel_record_type_class (record_type);

  if (record_type_class != IPMI_SEL_RECORD_TYPE_CLASS_SYSTEM_EVENT_RECORD)
    {
      ctx->errnum = IPMI_SEL_PARSE_ERR_INVALID_SEL_ENTRY;
      goto cleanup;
    }

  if (!(obj_sel_system_event_record = fiid_obj_create (tmpl_sel_system_event_record_event_fields)))
    {
      SEL_PARSE_ERRNO_TO_SEL_PARSE_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (fiid_obj_set_all (obj_sel_system_event_record,
                        sel_parse_entry->sel_event_record,
                        sel_parse_entry->sel_event_record_len) < 0)
    {
      SEL_PARSE_FIID_OBJECT_ERROR_TO_SEL_PARSE_ERRNUM (ctx, obj_sel_system_event_record);
      goto cleanup;
    }

  if (FIID_OBJ_GET (obj_sel_system_event_record,
                    "timestamp",
                    &val) < 0)
    {
      SEL_PARSE_FIID_OBJECT_ERROR_TO_SEL_PARSE_ERRNUM (ctx, obj_sel_system_event_record);
      goto cleanup;
    }
  system_event_record_data->timestamp = val;

  if (FIID_OBJ_GET (obj_sel_system_event_record,
                    "generator_id.id_type",
                    &val) < 0)
    {
      SEL_PARSE_FIID_OBJECT_ERROR_TO_SEL_PARSE_ERRNUM (ctx, obj_sel_system_event_record);
      goto cleanup;
    }
  generator_id_type = val;

  if (FIID_OBJ_GET (obj_sel_system_event_record,
                    "generator_id.id",
                    &val) < 0)
    {
      SEL_PARSE_FIID_OBJECT_ERROR_TO_SEL_PARSE_ERRNUM (ctx, obj_sel_system_event_record);
      goto cleanup;
    }
  generator_id_address = val;

  system_event_record_data->generator_id = ((generator_id_address << 1) | generator_id_type);

  if (FIID_OBJ_GET (obj_sel_system_event_record,
                    "ipmb_device_lun",
                    &val) < 0)
    {
      SEL_PARSE_FIID_OBJECT_ERROR_TO_SEL_PARSE_ERRNUM (ctx, obj_sel_system_event_record);
      goto cleanup;
    }
  system_event_record_data->ipmb_device_lun = val;

  if (FIID_OBJ_GET (obj_sel_system_event_record,
                    "channel_number",
                    &val) < 0)
    {
      SEL_PARSE_FIID_OBJECT_ERROR_TO_SEL_PARSE_ERRNUM (ctx, obj_sel_system_event_record);
      goto cleanup;
    }
  system_event_record_data->channel_number = val;

  if (FIID_OBJ_GET (obj_sel_system_event_record,
                    "event_message_format_version",
                    &val) < 0)
    {
      SEL_PARSE_FIID_OBJECT_ERROR_TO_SEL_PARSE_ERRNUM (ctx, obj_sel_system_event_record);
      goto cleanup;
    }
  system_event_record_data->event_message_format_version = val;

  if (FIID_OBJ_GET (obj_sel_system_event_record,
                    "sensor_type",
                    &val) < 0)
    {
      SEL_PARSE_FIID_OBJECT_ERROR_TO_SEL_PARSE_ERRNUM (ctx, obj_sel_system_event_record);
      goto cleanup;
    }
  system_event_record_data->sensor_type = val;

  if (FIID_OBJ_GET (obj_sel_system_event_record,
                    "sensor_number",
                    &val) < 0)
    {
      SEL_PARSE_FIID_OBJECT_ERROR_TO_SEL_PARSE_ERRNUM (ctx, obj_sel_system_event_record);
      goto cleanup;
    }
  system_event_record_data->sensor_number = val;

  if (FIID_OBJ_GET (obj_sel_system_event_record,
                    "event_type_code",
                    &val) < 0)
    {
      SEL_PARSE_FIID_OBJECT_ERROR_TO_SEL_PARSE_ERRNUM (ctx, obj_sel_system_event_record);
      goto cleanup;
    }
  system_event_record_data->event_type_code = val;

  if (FIID_OBJ_GET (obj_sel_system_event_record,
                    "event_dir",
                    &val) < 0)
    {
      SEL_PARSE_FIID_OBJECT_ERROR_TO_SEL_PARSE_ERRNUM (ctx, obj_sel_system_event_record);
      goto cleanup;
    }
  system_event_record_data->event_direction = val;

  if (FIID_OBJ_GET (obj_sel_system_event_record,
                    "offset_from_event_reading_type_code",
                    &val) < 0)
    {
      SEL_PARSE_FIID_OBJECT_ERROR_TO_SEL_PARSE_ERRNUM (ctx, obj_sel_system_event_record);
      goto cleanup;
    }
  system_event_record_data->offset_from_event_reading_type_code = val;

  if (FIID_OBJ_GET (obj_sel_system_event_record,
                    "event_data2_flag",
                    &val) < 0)
    {
      SEL_PARSE_FIID_OBJECT_ERROR_TO_SEL_PARSE_ERRNUM (ctx, obj_sel_system_event_record);
      goto cleanup;
    }
  system_event_record_data->event_data2_flag = val;

  if (FIID_OBJ_GET (obj_sel_system_event_record,
                    "event_data3_flag",
                    &val) < 0)
    {
      SEL_PARSE_FIID_OBJECT_ERROR_TO_SEL_PARSE_ERRNUM (ctx, obj_sel_system_event_record);
      goto cleanup;
    }
  system_event_record_data->event_data3_flag = val;

  if (FIID_OBJ_GET (obj_sel_system_event_record,
                    "event_data2",
                    &val) < 0)
    {
      SEL_PARSE_FIID_OBJECT_ERROR_TO_SEL_PARSE_ERRNUM (ctx, obj_sel_system_event_record);
      goto cleanup;
    }
  system_event_record_data->event_data2 = val;

  if (FIID_OBJ_GET (obj_sel_system_event_record,
                    "event_data3",
                    &val) < 0)
    {
      SEL_PARSE_FIID_OBJECT_ERROR_TO_SEL_PARSE_ERRNUM (ctx, obj_sel_system_event_record);
      goto cleanup;
    }
  system_event_record_data->event_data3 = val;

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_sel_system_event_record);
  return (rv);
}

int
sel_parse_get_previous_state_or_severity (ipmi_sel_parse_ctx_t ctx,
                                          struct ipmi_sel_parse_entry *sel_parse_entry,
                                          uint8_t *previous_offset_from_event_reading_type_code,
                                          uint8_t *offset_from_severity_event_reading_type_code)
{
  fiid_obj_t obj_sel_system_event_record = NULL;
  int record_type_class;
  uint8_t record_type;
  uint64_t val;
  int rv = -1;

  assert (ctx);
  assert (ctx->magic == IPMI_SEL_PARSE_CTX_MAGIC);
  assert (sel_parse_entry);
  assert (previous_offset_from_event_reading_type_code);
  assert (offset_from_severity_event_reading_type_code);

  if (sel_parse_entry->sel_event_record_len < IPMI_SEL_RECORD_LENGTH)
    {
      ctx->errnum = IPMI_SEL_PARSE_ERR_INVALID_SEL_ENTRY;
      goto cleanup;
    }

  if (sel_parse_get_record_header_info (ctx,
                                        sel_parse_entry,
                                        NULL,
                                        &record_type) < 0)
    goto cleanup;

  record_type_class = ipmi_sel_record_type_class (record_type);

  if (record_type_class != IPMI_SEL_RECORD_TYPE_CLASS_SYSTEM_EVENT_RECORD)
    {
      ctx->errnum = IPMI_SEL_PARSE_ERR_INVALID_SEL_ENTRY;
      goto cleanup;
    }

  if (!(obj_sel_system_event_record = fiid_obj_create (tmpl_sel_system_event_record_discrete_previous_state_severity)))
    {
      SEL_PARSE_ERRNO_TO_SEL_PARSE_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (fiid_obj_set_all (obj_sel_system_event_record,
                        sel_parse_entry->sel_event_record,
                        sel_parse_entry->sel_event_record_len) < 0)
    {
      SEL_PARSE_FIID_OBJECT_ERROR_TO_SEL_PARSE_ERRNUM (ctx, obj_sel_system_event_record);
      goto cleanup;
    }

  if (FIID_OBJ_GET (obj_sel_system_event_record,
                    "previous_offset_from_event_reading_type_code",
                    &val) < 0)
    {
      SEL_PARSE_FIID_OBJECT_ERROR_TO_SEL_PARSE_ERRNUM (ctx, obj_sel_system_event_record);
      goto cleanup;
    }
  *previous_offset_from_event_reading_type_code = val;

  if (FIID_OBJ_GET (obj_sel_system_event_record,
                    "offset_from_severity_event_reading_type_code",
                    &val) < 0)
    {
      SEL_PARSE_FIID_OBJECT_ERROR_TO_SEL_PARSE_ERRNUM (ctx, obj_sel_system_event_record);
      goto cleanup;
    }
  *offset_from_severity_event_reading_type_code = val;

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_sel_system_event_record);
  return (rv);
}
