/*
 * Copyright (C) 2003-2014 FreeIPMI Core Team
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
#if HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */
#include <assert.h>
#include <errno.h>

#include "freeipmi/sdr/ipmi-sdr.h"

#include "freeipmi/fiid/fiid.h"
#include "freeipmi/record-format/ipmi-sdr-record-format.h"
#include "freeipmi/spec/ipmi-sensor-units-spec.h"
#include "freeipmi/spec/ipmi-event-reading-type-code-spec.h"
#include "freeipmi/util/ipmi-sensor-util.h"

#include "ipmi-sdr-common.h"
#include "ipmi-sdr-defs.h"
#include "ipmi-sdr-trace.h"
#include "ipmi-sdr-util.h"

#include "libcommon/ipmi-fiid-util.h"

#include "freeipmi-portability.h"

#define IPMI_SDR_PARSE_RECORD_TYPE_FULL_SENSOR_RECORD                          0x0001
#define IPMI_SDR_PARSE_RECORD_TYPE_COMPACT_SENSOR_RECORD                       0x0002
#define IPMI_SDR_PARSE_RECORD_TYPE_EVENT_ONLY_RECORD                           0x0004
#define IPMI_SDR_PARSE_RECORD_TYPE_ENTITY_ASSOCIATION_RECORD                   0x0008
#define IPMI_SDR_PARSE_RECORD_TYPE_DEVICE_RELATIVE_ENTITY_ASSOCIATION_RECORD   0x0010
#define IPMI_SDR_PARSE_RECORD_TYPE_GENERIC_DEVICE_LOCATOR_RECORD               0x0020
#define IPMI_SDR_PARSE_RECORD_TYPE_FRU_DEVICE_LOCATOR_RECORD                   0x0040
#define IPMI_SDR_PARSE_RECORD_TYPE_MANAGEMENT_CONTROLLER_DEVICE_LOCATOR_RECORD 0x0080
#define IPMI_SDR_PARSE_RECORD_TYPE_MANAGEMENT_CONTROLLER_CONFIRMATION_RECORD   0x0100
#define IPMI_SDR_PARSE_RECORD_TYPE_BMC_MESSAGE_CHANNEL_INFO_RECORD             0x0200
#define IPMI_SDR_PARSE_RECORD_TYPE_OEM_RECORD                                  0x0400

int
ipmi_sdr_parse_record_id_and_type (ipmi_sdr_ctx_t ctx,
                                   const void *sdr_record,
                                   unsigned int sdr_record_len,
                                   uint16_t *record_id,
                                   uint8_t *record_type)
{
  uint8_t sdr_record_buf[IPMI_SDR_MAX_RECORD_LENGTH];
  int sdr_record_buf_len;
  fiid_obj_t obj_sdr_record_header = NULL;
  int sdr_record_header_len;
  void *sdr_record_to_use;
  unsigned int sdr_record_len_to_use;
  uint64_t val;
  int rv = -1;

  if (!ctx || ctx->magic != IPMI_SDR_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_sdr_ctx_errormsg (ctx), ipmi_sdr_ctx_errnum (ctx));
      return (-1);
    }

  if (!sdr_record || !sdr_record_len)
    {
      if (ctx->operation == IPMI_SDR_OPERATION_READ_CACHE
	  && !sdr_record
	  && !sdr_record_len)
	{
	  if ((sdr_record_buf_len = ipmi_sdr_cache_record_read (ctx,
								sdr_record_buf,
								IPMI_SDR_MAX_RECORD_LENGTH)) < 0)
	    {
	      SDR_SET_INTERNAL_ERRNUM (ctx);
	      return (-1);
	    }
	  sdr_record_to_use = sdr_record_buf;
	  sdr_record_len_to_use = sdr_record_buf_len;
	}
      else
	{
	  SDR_SET_ERRNUM (ctx, IPMI_SDR_ERR_PARAMETERS);
	  return (-1);
	}
    }
  else
    {
      sdr_record_to_use = (void *)sdr_record;
      sdr_record_len_to_use = sdr_record_len;
    }

  if ((sdr_record_header_len = fiid_template_len_bytes (tmpl_sdr_record_header)) < 0)
    {
      SDR_ERRNO_TO_SDR_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (sdr_record_len_to_use < sdr_record_header_len)
    {
      SDR_SET_ERRNUM (ctx, IPMI_SDR_ERR_PARSE_INCOMPLETE_SDR_RECORD);
      goto cleanup;
    }

  if (!(obj_sdr_record_header = fiid_obj_create (tmpl_sdr_record_header)))
    {
      SDR_ERRNO_TO_SDR_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (fiid_obj_set_all (obj_sdr_record_header,
                        sdr_record_to_use,
                        sdr_record_header_len) < 0)
    {
      SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record_header);
      goto cleanup;
    }

  if (record_id)
    {
      if (FIID_OBJ_GET (obj_sdr_record_header,
                        "record_id",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record_header);
          goto cleanup;
        }
      *record_id = val;
    }

  if (record_type)
    {
      if (FIID_OBJ_GET (obj_sdr_record_header,
                        "record_type",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record_header);
          goto cleanup;
        }
      *record_type = val;
    }

  sdr_check_read_status (ctx);

  rv = 0;
  ctx->errnum = IPMI_SDR_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_sdr_record_header);
  return (rv);
}

static fiid_obj_t
_sdr_record_get_common (ipmi_sdr_ctx_t ctx,
                        const void *sdr_record,
                        unsigned int sdr_record_len,
                        uint32_t acceptable_record_types)
{
  uint8_t sdr_record_buf[IPMI_SDR_MAX_RECORD_LENGTH];
  int sdr_record_buf_len;
  void *sdr_record_to_use;
  unsigned int sdr_record_len_to_use;
  fiid_obj_t obj_sdr_record = NULL;
  uint8_t record_type;

  assert (acceptable_record_types);

  if (!ctx || ctx->magic != IPMI_SDR_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_sdr_ctx_errormsg (ctx), ipmi_sdr_ctx_errnum (ctx));
      goto cleanup;
    }

  if (!sdr_record || !sdr_record_len)
    {
      if (ctx->operation == IPMI_SDR_OPERATION_READ_CACHE
	  && !sdr_record
	  && !sdr_record_len)
	{
	  if ((sdr_record_buf_len = ipmi_sdr_cache_record_read (ctx,
								sdr_record_buf,
								IPMI_SDR_MAX_RECORD_LENGTH)) < 0)
	    {
	      SDR_SET_INTERNAL_ERRNUM (ctx);
	      goto cleanup;
	    }
	  sdr_record_to_use = sdr_record_buf;
	  sdr_record_len_to_use = sdr_record_buf_len;
	}
      else
	{
	  SDR_SET_ERRNUM (ctx, IPMI_SDR_ERR_PARAMETERS);
	  goto cleanup;
	}
    }
  else
    {
      sdr_record_to_use = (void *)sdr_record;
      sdr_record_len_to_use = sdr_record_len;
    }

  if (ipmi_sdr_parse_record_id_and_type (ctx,
                                         sdr_record_to_use,
                                         sdr_record_len_to_use,
                                         NULL,
                                         &record_type) < 0)
    {
      SDR_SET_INTERNAL_ERRNUM (ctx);
      goto cleanup;
    }

  if (!(((acceptable_record_types & IPMI_SDR_PARSE_RECORD_TYPE_FULL_SENSOR_RECORD)
         && record_type == IPMI_SDR_FORMAT_FULL_SENSOR_RECORD)
        || ((acceptable_record_types & IPMI_SDR_PARSE_RECORD_TYPE_COMPACT_SENSOR_RECORD)
            && record_type == IPMI_SDR_FORMAT_COMPACT_SENSOR_RECORD)
        || ((acceptable_record_types & IPMI_SDR_PARSE_RECORD_TYPE_EVENT_ONLY_RECORD)
            && record_type == IPMI_SDR_FORMAT_EVENT_ONLY_RECORD)
        || ((acceptable_record_types & IPMI_SDR_PARSE_RECORD_TYPE_ENTITY_ASSOCIATION_RECORD)
            && record_type == IPMI_SDR_FORMAT_ENTITY_ASSOCIATION_RECORD)
        || ((acceptable_record_types & IPMI_SDR_PARSE_RECORD_TYPE_DEVICE_RELATIVE_ENTITY_ASSOCIATION_RECORD)
            && record_type == IPMI_SDR_FORMAT_DEVICE_RELATIVE_ENTITY_ASSOCIATION_RECORD)

        || ((acceptable_record_types & IPMI_SDR_PARSE_RECORD_TYPE_GENERIC_DEVICE_LOCATOR_RECORD)
            && record_type == IPMI_SDR_FORMAT_GENERIC_DEVICE_LOCATOR_RECORD)
        || ((acceptable_record_types & IPMI_SDR_PARSE_RECORD_TYPE_FRU_DEVICE_LOCATOR_RECORD)
            && record_type == IPMI_SDR_FORMAT_FRU_DEVICE_LOCATOR_RECORD)
        || ((acceptable_record_types & IPMI_SDR_PARSE_RECORD_TYPE_MANAGEMENT_CONTROLLER_DEVICE_LOCATOR_RECORD)
            && record_type == IPMI_SDR_FORMAT_MANAGEMENT_CONTROLLER_DEVICE_LOCATOR_RECORD)
        || ((acceptable_record_types & IPMI_SDR_PARSE_RECORD_TYPE_MANAGEMENT_CONTROLLER_CONFIRMATION_RECORD)
            && record_type == IPMI_SDR_FORMAT_MANAGEMENT_CONTROLLER_CONFIRMATION_RECORD)
        || ((acceptable_record_types & IPMI_SDR_PARSE_RECORD_TYPE_BMC_MESSAGE_CHANNEL_INFO_RECORD)
            && record_type == IPMI_SDR_FORMAT_BMC_MESSAGE_CHANNEL_INFO_RECORD)
        || ((acceptable_record_types & IPMI_SDR_PARSE_RECORD_TYPE_OEM_RECORD)
            && record_type == IPMI_SDR_FORMAT_OEM_RECORD)
        ))
    {
      SDR_SET_ERRNUM (ctx, IPMI_SDR_ERR_PARSE_INVALID_SDR_RECORD);
      goto cleanup;
    }

  if (record_type == IPMI_SDR_FORMAT_FULL_SENSOR_RECORD)
    {
      if (!(obj_sdr_record = fiid_obj_create (tmpl_sdr_full_sensor_record)))
        {
          SDR_ERRNO_TO_SDR_ERRNUM (ctx, errno);
          goto cleanup;
        }
    }
  else if (record_type == IPMI_SDR_FORMAT_COMPACT_SENSOR_RECORD)
    {
      if (!(obj_sdr_record = fiid_obj_create (tmpl_sdr_compact_sensor_record)))
        {
          SDR_ERRNO_TO_SDR_ERRNUM (ctx, errno);
          goto cleanup;
        }
    }
  else if (record_type == IPMI_SDR_FORMAT_EVENT_ONLY_RECORD)
    {
      if (!(obj_sdr_record = fiid_obj_create (tmpl_sdr_event_only_record)))
        {
          SDR_ERRNO_TO_SDR_ERRNUM (ctx, errno);
          goto cleanup;
        }
    }
  else if (record_type == IPMI_SDR_FORMAT_ENTITY_ASSOCIATION_RECORD)
    {
      if (!(obj_sdr_record = fiid_obj_create (tmpl_sdr_entity_association_record)))
        {
          SDR_ERRNO_TO_SDR_ERRNUM (ctx, errno);
          goto cleanup;
        }
    }
  else if (record_type == IPMI_SDR_FORMAT_DEVICE_RELATIVE_ENTITY_ASSOCIATION_RECORD)
    {
      if (!(obj_sdr_record = fiid_obj_create (tmpl_sdr_device_relative_entity_association_record)))
        {
          SDR_ERRNO_TO_SDR_ERRNUM (ctx, errno);
          goto cleanup;
        }
    }
  else if (record_type == IPMI_SDR_FORMAT_GENERIC_DEVICE_LOCATOR_RECORD)
    {
      if (!(obj_sdr_record = fiid_obj_create (tmpl_sdr_generic_device_locator_record)))
        {
          SDR_ERRNO_TO_SDR_ERRNUM (ctx, errno);
          goto cleanup;
        }
    }
  else if (record_type == IPMI_SDR_FORMAT_FRU_DEVICE_LOCATOR_RECORD)
    {
      if (!(obj_sdr_record = fiid_obj_create (tmpl_sdr_fru_device_locator_record)))
        {
          SDR_ERRNO_TO_SDR_ERRNUM (ctx, errno);
          goto cleanup;
        }
    }
  else if (record_type == IPMI_SDR_FORMAT_MANAGEMENT_CONTROLLER_DEVICE_LOCATOR_RECORD)
    {
      if (!(obj_sdr_record = fiid_obj_create (tmpl_sdr_management_controller_device_locator_record)))
        {
          SDR_ERRNO_TO_SDR_ERRNUM (ctx, errno);
          goto cleanup;
        }
    }
  else if (record_type == IPMI_SDR_FORMAT_MANAGEMENT_CONTROLLER_CONFIRMATION_RECORD)
    {
      if (!(obj_sdr_record = fiid_obj_create (tmpl_sdr_management_controller_confirmation_record)))
        {
          SDR_ERRNO_TO_SDR_ERRNUM (ctx, errno);
          goto cleanup;
        }
    }
  else if (record_type == IPMI_SDR_FORMAT_BMC_MESSAGE_CHANNEL_INFO_RECORD)
    {
      if (!(obj_sdr_record = fiid_obj_create (tmpl_sdr_bmc_message_channel_info_record)))
        {
          SDR_ERRNO_TO_SDR_ERRNUM (ctx, errno);
          goto cleanup;
        }
    }
  else if (record_type == IPMI_SDR_FORMAT_OEM_RECORD)
    {
      if (!(obj_sdr_record = fiid_obj_create (tmpl_sdr_oem_record)))
        {
          SDR_ERRNO_TO_SDR_ERRNUM (ctx, errno);
          goto cleanup;
        }
    }

  if (fiid_obj_set_all (obj_sdr_record,
                        sdr_record_to_use,
                        sdr_record_len_to_use) < 0)
    {
      SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record);
      goto cleanup;
    }

  sdr_check_read_status (ctx);

  return (obj_sdr_record);

 cleanup:
  fiid_obj_destroy (obj_sdr_record);
  return (NULL);
}

int
ipmi_sdr_parse_sensor_owner_id (ipmi_sdr_ctx_t ctx,
                                const void *sdr_record,
                                unsigned int sdr_record_len,
                                uint8_t *sensor_owner_id_type,
                                uint8_t *sensor_owner_id)
{
  fiid_obj_t obj_sdr_record = NULL;
  uint32_t acceptable_record_types;
  uint64_t val;
  int rv = -1;

  acceptable_record_types = IPMI_SDR_PARSE_RECORD_TYPE_FULL_SENSOR_RECORD;
  acceptable_record_types |= IPMI_SDR_PARSE_RECORD_TYPE_COMPACT_SENSOR_RECORD;
  acceptable_record_types |= IPMI_SDR_PARSE_RECORD_TYPE_EVENT_ONLY_RECORD;

  if (!(obj_sdr_record = _sdr_record_get_common (ctx,
                                                 sdr_record,
                                                 sdr_record_len,
                                                 acceptable_record_types)))
    goto cleanup;

  if (sensor_owner_id_type)
    {
      if (FIID_OBJ_GET (obj_sdr_record,
                        "sensor_owner_id.type",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record);
          goto cleanup;
        }
      *sensor_owner_id_type = val;
    }

  if (sensor_owner_id)
    {
      if (FIID_OBJ_GET (obj_sdr_record,
                        "sensor_owner_id",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record);
          goto cleanup;
        }
      *sensor_owner_id = val;
    }

  rv = 0;
  ctx->errnum = IPMI_SDR_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_sdr_record);
  return (rv);
}

int
ipmi_sdr_parse_sensor_owner_lun (ipmi_sdr_ctx_t ctx,
                                 const void *sdr_record,
                                 unsigned int sdr_record_len,
                                 uint8_t *sensor_owner_lun,
                                 uint8_t *channel_number)
{
  fiid_obj_t obj_sdr_record = NULL;
  uint32_t acceptable_record_types;
  uint64_t val;
  int rv = -1;

  acceptable_record_types = IPMI_SDR_PARSE_RECORD_TYPE_FULL_SENSOR_RECORD;
  acceptable_record_types |= IPMI_SDR_PARSE_RECORD_TYPE_COMPACT_SENSOR_RECORD;
  acceptable_record_types |= IPMI_SDR_PARSE_RECORD_TYPE_EVENT_ONLY_RECORD;

  if (!(obj_sdr_record = _sdr_record_get_common (ctx,
                                                 sdr_record,
                                                 sdr_record_len,
                                                 acceptable_record_types)))
    goto cleanup;

  if (sensor_owner_lun)
    {
      if (FIID_OBJ_GET (obj_sdr_record,
                        "sensor_owner_lun",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record);
          goto cleanup;
        }
      *sensor_owner_lun = val;
    }

  if (channel_number)
    {
      if (FIID_OBJ_GET (obj_sdr_record,
                        "channel_number",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record);
          goto cleanup;
        }
      *channel_number = val;
    }

  rv = 0;
  ctx->errnum = IPMI_SDR_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_sdr_record);
  return (rv);
}

int
ipmi_sdr_parse_sensor_number (ipmi_sdr_ctx_t ctx,
                              const void *sdr_record,
                              unsigned int sdr_record_len,
                              uint8_t *sensor_number)
{
  fiid_obj_t obj_sdr_record = NULL;
  uint32_t acceptable_record_types;
  uint64_t val;
  int rv = -1;

  acceptable_record_types = IPMI_SDR_PARSE_RECORD_TYPE_FULL_SENSOR_RECORD;
  acceptable_record_types |= IPMI_SDR_PARSE_RECORD_TYPE_COMPACT_SENSOR_RECORD;
  acceptable_record_types |= IPMI_SDR_PARSE_RECORD_TYPE_EVENT_ONLY_RECORD;

  if (!(obj_sdr_record = _sdr_record_get_common (ctx,
                                                 sdr_record,
                                                 sdr_record_len,
                                                 acceptable_record_types)))
    goto cleanup;

  if (sensor_number)
    {
      if (FIID_OBJ_GET (obj_sdr_record,
                        "sensor_number",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record);
          goto cleanup;
        }
      *sensor_number = val;
    }

  rv = 0;
  ctx->errnum = IPMI_SDR_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_sdr_record);
  return (rv);
}

int
ipmi_sdr_parse_entity_id_instance_type (ipmi_sdr_ctx_t ctx,
                                        const void *sdr_record,
                                        unsigned int sdr_record_len,
                                        uint8_t *entity_id,
                                        uint8_t *entity_instance,
                                        uint8_t *entity_instance_type)
{
  fiid_obj_t obj_sdr_record = NULL;
  uint32_t acceptable_record_types;
  uint64_t val;
  int rv = -1;

  acceptable_record_types = IPMI_SDR_PARSE_RECORD_TYPE_FULL_SENSOR_RECORD;
  acceptable_record_types |= IPMI_SDR_PARSE_RECORD_TYPE_COMPACT_SENSOR_RECORD;
  acceptable_record_types |= IPMI_SDR_PARSE_RECORD_TYPE_EVENT_ONLY_RECORD;
  acceptable_record_types |= IPMI_SDR_PARSE_RECORD_TYPE_GENERIC_DEVICE_LOCATOR_RECORD;
  acceptable_record_types |= IPMI_SDR_PARSE_RECORD_TYPE_MANAGEMENT_CONTROLLER_DEVICE_LOCATOR_RECORD;

  if (!(obj_sdr_record = _sdr_record_get_common (ctx,
                                                 sdr_record,
                                                 sdr_record_len,
                                                 acceptable_record_types)))
    goto cleanup;

  if (entity_id)
    {
      if (FIID_OBJ_GET (obj_sdr_record,
                        "entity_id",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record);
          goto cleanup;
        }
      *entity_id = val;
    }
  if (entity_instance)
    {
      if (FIID_OBJ_GET (obj_sdr_record,
                        "entity_instance",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record);
          goto cleanup;
        }
      *entity_instance = val;
    }
  if (entity_instance_type)
    {
      if (FIID_OBJ_GET (obj_sdr_record,
                        "entity_instance.type",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record);
          goto cleanup;
        }
      *entity_instance_type = val;
    }

  rv = 0;
  ctx->errnum = IPMI_SDR_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_sdr_record);
  return (rv);
}

int
ipmi_sdr_parse_sensor_type (ipmi_sdr_ctx_t ctx,
                            const void *sdr_record,
                            unsigned int sdr_record_len,
                            uint8_t *sensor_type)
{
  fiid_obj_t obj_sdr_record = NULL;
  uint32_t acceptable_record_types;
  uint64_t val;
  int rv = -1;

  acceptable_record_types = IPMI_SDR_PARSE_RECORD_TYPE_FULL_SENSOR_RECORD;
  acceptable_record_types |= IPMI_SDR_PARSE_RECORD_TYPE_COMPACT_SENSOR_RECORD;
  acceptable_record_types |= IPMI_SDR_PARSE_RECORD_TYPE_EVENT_ONLY_RECORD;

  if (!(obj_sdr_record = _sdr_record_get_common (ctx,
                                                 sdr_record,
                                                 sdr_record_len,
                                                 acceptable_record_types)))
    goto cleanup;

  if (sensor_type)
    {
      if (FIID_OBJ_GET (obj_sdr_record,
                        "sensor_type",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record);
          goto cleanup;
        }
      *sensor_type = val;
    }

  rv = 0;
  ctx->errnum = IPMI_SDR_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_sdr_record);
  return (rv);
}

int
ipmi_sdr_parse_event_reading_type_code (ipmi_sdr_ctx_t ctx,
                                        const void *sdr_record,
                                        unsigned int sdr_record_len,
                                        uint8_t *event_reading_type_code)
{
  fiid_obj_t obj_sdr_record = NULL;
  uint32_t acceptable_record_types;
  uint64_t val;
  int rv = -1;

  acceptable_record_types = IPMI_SDR_PARSE_RECORD_TYPE_FULL_SENSOR_RECORD;
  acceptable_record_types |= IPMI_SDR_PARSE_RECORD_TYPE_COMPACT_SENSOR_RECORD;
  acceptable_record_types |= IPMI_SDR_PARSE_RECORD_TYPE_EVENT_ONLY_RECORD;

  if (!(obj_sdr_record = _sdr_record_get_common (ctx,
                                                 sdr_record,
                                                 sdr_record_len,
                                                 acceptable_record_types)))
    goto cleanup;

  if (event_reading_type_code)
    {
      if (FIID_OBJ_GET (obj_sdr_record,
                        "event_reading_type_code",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record);
          goto cleanup;
        }
      *event_reading_type_code = val;
    }

  rv = 0;
  ctx->errnum = IPMI_SDR_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_sdr_record);
  return (rv);
}

int
ipmi_sdr_parse_id_string (ipmi_sdr_ctx_t ctx,
                          const void *sdr_record,
                          unsigned int sdr_record_len,
                          char *id_string,
                          unsigned int id_string_len)
{
  fiid_obj_t obj_sdr_record = NULL;
  uint32_t acceptable_record_types;
  int len = 0;
  int rv = -1;

  acceptable_record_types = IPMI_SDR_PARSE_RECORD_TYPE_FULL_SENSOR_RECORD;
  acceptable_record_types |= IPMI_SDR_PARSE_RECORD_TYPE_COMPACT_SENSOR_RECORD;
  acceptable_record_types |= IPMI_SDR_PARSE_RECORD_TYPE_EVENT_ONLY_RECORD;

  if (!(obj_sdr_record = _sdr_record_get_common (ctx,
                                                 sdr_record,
                                                 sdr_record_len,
                                                 acceptable_record_types)))
    goto cleanup;

  if (id_string && id_string_len)
    {
      if ((len = fiid_obj_get_data (obj_sdr_record,
                                    "id_string",
                                    id_string,
                                    id_string_len)) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record);
          goto cleanup;
        }
    }

  rv = len;
  ctx->errnum = IPMI_SDR_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_sdr_record);
  return (rv);
}

int
ipmi_sdr_parse_sensor_units (ipmi_sdr_ctx_t ctx,
                             const void *sdr_record,
                             unsigned int sdr_record_len,
                             uint8_t *sensor_units_percentage,
                             uint8_t *sensor_units_modifier,
                             uint8_t *sensor_units_rate,
                             uint8_t *sensor_base_unit_type,
                             uint8_t *sensor_modifier_unit_type)
{
  fiid_obj_t obj_sdr_record = NULL;
  uint32_t acceptable_record_types;
  uint64_t val;
  int rv = -1;

  acceptable_record_types = IPMI_SDR_PARSE_RECORD_TYPE_FULL_SENSOR_RECORD;
  acceptable_record_types |= IPMI_SDR_PARSE_RECORD_TYPE_COMPACT_SENSOR_RECORD;

  if (!(obj_sdr_record = _sdr_record_get_common (ctx,
                                                 sdr_record,
                                                 sdr_record_len,
                                                 acceptable_record_types)))
    goto cleanup;

  if (sensor_units_percentage)
    {
      if (FIID_OBJ_GET (obj_sdr_record,
                        "sensor_unit1.percentage",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record);
          goto cleanup;
        }
      *sensor_units_percentage = val;
    }

  if (sensor_units_modifier)
    {
      if (FIID_OBJ_GET (obj_sdr_record,
                        "sensor_unit1.modifier_unit",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record);
          goto cleanup;
        }
      *sensor_units_modifier = val;
    }

  if (sensor_units_rate)
    {
      if (FIID_OBJ_GET (obj_sdr_record,
                        "sensor_unit1.rate_unit",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record);
          goto cleanup;
        }

      *sensor_units_rate = val;
    }

  if (sensor_base_unit_type)
    {
      if (FIID_OBJ_GET (obj_sdr_record,
                        "sensor_unit2.base_unit",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record);
          goto cleanup;
        }
      *sensor_base_unit_type = val;

      if (!IPMI_SENSOR_UNIT_VALID (*sensor_base_unit_type))
        *sensor_base_unit_type = IPMI_SENSOR_UNIT_UNSPECIFIED;
    }

  if (sensor_modifier_unit_type)
    {
      if (FIID_OBJ_GET (obj_sdr_record,
                        "sensor_unit3.modifier_unit",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record);
          goto cleanup;
        }
      *sensor_modifier_unit_type = val;

      if (!IPMI_SENSOR_UNIT_VALID (*sensor_modifier_unit_type))
        *sensor_modifier_unit_type = IPMI_SENSOR_UNIT_UNSPECIFIED;
    }

  rv = 0;
  ctx->errnum = IPMI_SDR_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_sdr_record);
  return (rv);
}

int
ipmi_sdr_parse_sensor_capabilities (ipmi_sdr_ctx_t ctx,
                                    const void *sdr_record,
                                    unsigned int sdr_record_len,
                                    uint8_t *event_message_control_support,
                                    uint8_t *threshold_access_support,
                                    uint8_t *hysteresis_support,
                                    uint8_t *auto_re_arm_support,
                                    uint8_t *entity_ignore_support)
{
  fiid_obj_t obj_sdr_record = NULL;
  uint32_t acceptable_record_types;
  uint64_t val;
  int rv = -1;

  acceptable_record_types = IPMI_SDR_PARSE_RECORD_TYPE_FULL_SENSOR_RECORD;
  acceptable_record_types |= IPMI_SDR_PARSE_RECORD_TYPE_COMPACT_SENSOR_RECORD;

  if (!(obj_sdr_record = _sdr_record_get_common (ctx,
                                                 sdr_record,
                                                 sdr_record_len,
                                                 acceptable_record_types)))
    goto cleanup;

  if (event_message_control_support)
    {
      if (FIID_OBJ_GET (obj_sdr_record,
                        "sensor_capabilities.event_message_control_support",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record);
          goto cleanup;
        }
      *event_message_control_support = val;
    }
  if (threshold_access_support)
    {
      if (FIID_OBJ_GET (obj_sdr_record,
                        "sensor_capabilities.threshold_access_support",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record);
          goto cleanup;
        }
      *threshold_access_support = val;
    }
  if (hysteresis_support)
    {
      if (FIID_OBJ_GET (obj_sdr_record,
                        "sensor_capabilities.hysteresis_support",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record);
          goto cleanup;
        }
      *hysteresis_support = val;
    }
  if (auto_re_arm_support)
    {
      if (FIID_OBJ_GET (obj_sdr_record,
                        "sensor_capabilities.auto_re_arm_support",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record);
          goto cleanup;
        }
      *auto_re_arm_support = val;
    }
  if (entity_ignore_support)
    {
      if (FIID_OBJ_GET (obj_sdr_record,
                        "sensor_capabilities.entity_ignore_support",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record);
          goto cleanup;
        }
      *entity_ignore_support = val;
    }

  rv = 0;
  ctx->errnum = IPMI_SDR_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_sdr_record);
  return (rv);
}

int
ipmi_sdr_parse_sensor_direction (ipmi_sdr_ctx_t ctx,
                                 const void *sdr_record,
                                 unsigned int sdr_record_len,
                                 uint8_t *sensor_direction)
{
  fiid_obj_t obj_sdr_record = NULL;
  uint32_t acceptable_record_types;
  uint64_t val;
  int rv = -1;

  acceptable_record_types = IPMI_SDR_PARSE_RECORD_TYPE_FULL_SENSOR_RECORD;
  acceptable_record_types |= IPMI_SDR_PARSE_RECORD_TYPE_COMPACT_SENSOR_RECORD;

  if (!(obj_sdr_record = _sdr_record_get_common (ctx,
                                                 sdr_record,
                                                 sdr_record_len,
                                                 acceptable_record_types)))
    goto cleanup;

  if (sensor_direction)
    {
      if (FIID_OBJ_GET (obj_sdr_record,
                        "sensor_direction",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record);
          goto cleanup;
        }
      *sensor_direction = val;
    }

  rv = 0;
  ctx->errnum = IPMI_SDR_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_sdr_record);
  return (rv);
}

int
ipmi_sdr_parse_assertion_supported (ipmi_sdr_ctx_t ctx,
                                    const void *sdr_record,
                                    unsigned int sdr_record_len,
                                    uint8_t *event_state_0,
                                    uint8_t *event_state_1,
                                    uint8_t *event_state_2,
                                    uint8_t *event_state_3,
                                    uint8_t *event_state_4,
                                    uint8_t *event_state_5,
                                    uint8_t *event_state_6,
                                    uint8_t *event_state_7,
                                    uint8_t *event_state_8,
                                    uint8_t *event_state_9,
                                    uint8_t *event_state_10,
                                    uint8_t *event_state_11,
                                    uint8_t *event_state_12,
                                    uint8_t *event_state_13,
                                    uint8_t *event_state_14)
{
  fiid_obj_t obj_sdr_record = NULL;
  fiid_obj_t obj_sdr_record_discrete = NULL;
  uint32_t acceptable_record_types;
  uint8_t record_type;
  uint8_t event_reading_type_code;
  uint64_t val;
  int rv = -1;

  acceptable_record_types = IPMI_SDR_PARSE_RECORD_TYPE_FULL_SENSOR_RECORD;
  acceptable_record_types |= IPMI_SDR_PARSE_RECORD_TYPE_COMPACT_SENSOR_RECORD;

  if (!(obj_sdr_record = _sdr_record_get_common (ctx,
                                                 sdr_record,
                                                 sdr_record_len,
                                                 acceptable_record_types)))
    goto cleanup;

  if (ipmi_sdr_parse_event_reading_type_code (ctx,
                                              sdr_record,
                                              sdr_record_len,
                                              &event_reading_type_code) < 0)
    goto cleanup;

  if (!IPMI_EVENT_READING_TYPE_CODE_IS_GENERIC (event_reading_type_code)
      && !IPMI_EVENT_READING_TYPE_CODE_IS_SENSOR_SPECIFIC (event_reading_type_code))
    {
      SDR_SET_ERRNUM (ctx, IPMI_SDR_ERR_PARSE_INVALID_SDR_RECORD);
      goto cleanup;
    }

  /* convert obj_sdr_record to appropriate format we care about */
  if (ipmi_sdr_parse_record_id_and_type (ctx,
                                         sdr_record,
                                         sdr_record_len,
                                         NULL,
                                         &record_type) < 0)
    goto cleanup;

  if (record_type == IPMI_SDR_FORMAT_FULL_SENSOR_RECORD)
    {
      if (!(obj_sdr_record_discrete = fiid_obj_copy (obj_sdr_record,
                                                     tmpl_sdr_full_sensor_record_non_threshold_based_sensors)))
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record);
          goto cleanup;
        }
    }
  else
    {
      if (!(obj_sdr_record_discrete = fiid_obj_copy (obj_sdr_record,
                                                     tmpl_sdr_compact_sensor_record_non_threshold_based_sensors)))
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record);
          goto cleanup;
        }
    }

  if (event_state_0)
    {
      if (FIID_OBJ_GET (obj_sdr_record_discrete,
                        "assertion_event_mask.event_offset_0",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record_discrete);
          goto cleanup;
        }
      *event_state_0 = val;
    }

  if (event_state_1)
    {
      if (FIID_OBJ_GET (obj_sdr_record_discrete,
                        "assertion_event_mask.event_offset_1",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record_discrete);
          goto cleanup;
        }
      *event_state_1 = val;
    }

  if (event_state_2)
    {
      if (FIID_OBJ_GET (obj_sdr_record_discrete,
                        "assertion_event_mask.event_offset_2",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record_discrete);
          goto cleanup;
        }
      *event_state_2 = val;
    }

  if (event_state_3)
    {
      if (FIID_OBJ_GET (obj_sdr_record_discrete,
                        "assertion_event_mask.event_offset_3",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record_discrete);
          goto cleanup;
        }
      *event_state_3 = val;
    }

  if (event_state_4)
    {
      if (FIID_OBJ_GET (obj_sdr_record_discrete,
                        "assertion_event_mask.event_offset_4",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record_discrete);
          goto cleanup;
        }
      *event_state_4 = val;
    }

  if (event_state_5)
    {
      if (FIID_OBJ_GET (obj_sdr_record_discrete,
                        "assertion_event_mask.event_offset_5",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record_discrete);
          goto cleanup;
        }
      *event_state_5 = val;
    }

  if (event_state_6)
    {
      if (FIID_OBJ_GET (obj_sdr_record_discrete,
                        "assertion_event_mask.event_offset_6",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record_discrete);
          goto cleanup;
        }
      *event_state_6 = val;
    }

  if (event_state_7)
    {
      if (FIID_OBJ_GET (obj_sdr_record_discrete,
                        "assertion_event_mask.event_offset_7",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record_discrete);
          goto cleanup;
        }
      *event_state_7 = val;
    }

  if (event_state_8)
    {
      if (FIID_OBJ_GET (obj_sdr_record_discrete,
                        "assertion_event_mask.event_offset_8",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record_discrete);
          goto cleanup;
        }
      *event_state_8 = val;
    }

  if (event_state_9)
    {
      if (FIID_OBJ_GET (obj_sdr_record_discrete,
                        "assertion_event_mask.event_offset_9",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record_discrete);
          goto cleanup;
        }
      *event_state_9 = val;
    }

  if (event_state_10)
    {
      if (FIID_OBJ_GET (obj_sdr_record_discrete,
                        "assertion_event_mask.event_offset_10",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record_discrete);
          goto cleanup;
        }
      *event_state_10 = val;
    }

  if (event_state_11)
    {
      if (FIID_OBJ_GET (obj_sdr_record_discrete,
                        "assertion_event_mask.event_offset_11",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record_discrete);
          goto cleanup;
        }
      *event_state_11 = val;
    }

  if (event_state_12)
    {
      if (FIID_OBJ_GET (obj_sdr_record_discrete,
                        "assertion_event_mask.event_offset_12",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record_discrete);
          goto cleanup;
        }
      *event_state_12 = val;
    }

  if (event_state_13)
    {
      if (FIID_OBJ_GET (obj_sdr_record_discrete,
                        "assertion_event_mask.event_offset_13",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record_discrete);
          goto cleanup;
        }
      *event_state_13 = val;
    }

  if (event_state_14)
    {
      if (FIID_OBJ_GET (obj_sdr_record_discrete,
                        "assertion_event_mask.event_offset_14",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record_discrete);
          goto cleanup;
        }
      *event_state_14 = val;
    }

  rv = 0;
  ctx->errnum = IPMI_SDR_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_sdr_record);
  fiid_obj_destroy (obj_sdr_record_discrete);
  return (rv);
}

int
ipmi_sdr_parse_deassertion_supported (ipmi_sdr_ctx_t ctx,
                                      const void *sdr_record,
                                      unsigned int sdr_record_len,
                                      uint8_t *event_state_0,
                                      uint8_t *event_state_1,
                                      uint8_t *event_state_2,
                                      uint8_t *event_state_3,
                                      uint8_t *event_state_4,
                                      uint8_t *event_state_5,
                                      uint8_t *event_state_6,
                                      uint8_t *event_state_7,
                                      uint8_t *event_state_8,
                                      uint8_t *event_state_9,
                                      uint8_t *event_state_10,
                                      uint8_t *event_state_11,
                                      uint8_t *event_state_12,
                                      uint8_t *event_state_13,
                                      uint8_t *event_state_14)
{
  fiid_obj_t obj_sdr_record = NULL;
  fiid_obj_t obj_sdr_record_discrete = NULL;
  uint32_t acceptable_record_types;
  uint8_t record_type;
  uint8_t event_reading_type_code;
  uint64_t val;
  int rv = -1;

  acceptable_record_types = IPMI_SDR_PARSE_RECORD_TYPE_FULL_SENSOR_RECORD;
  acceptable_record_types |= IPMI_SDR_PARSE_RECORD_TYPE_COMPACT_SENSOR_RECORD;

  if (!(obj_sdr_record = _sdr_record_get_common (ctx,
                                                 sdr_record,
                                                 sdr_record_len,
                                                 acceptable_record_types)))
    goto cleanup;

  if (ipmi_sdr_parse_event_reading_type_code (ctx,
                                              sdr_record,
                                              sdr_record_len,
                                              &event_reading_type_code) < 0)
    goto cleanup;

  if (!IPMI_EVENT_READING_TYPE_CODE_IS_GENERIC (event_reading_type_code)
      && !IPMI_EVENT_READING_TYPE_CODE_IS_SENSOR_SPECIFIC (event_reading_type_code))
    {
      SDR_SET_ERRNUM (ctx, IPMI_SDR_ERR_PARSE_INVALID_SDR_RECORD);
      goto cleanup;
    }

  /* convert obj_sdr_record to appropriate format we care about */
  if (ipmi_sdr_parse_record_id_and_type (ctx,
                                         sdr_record,
                                         sdr_record_len,
                                         NULL,
                                         &record_type) < 0)
    goto cleanup;

  if (record_type == IPMI_SDR_FORMAT_FULL_SENSOR_RECORD)
    {
      if (!(obj_sdr_record_discrete = fiid_obj_copy (obj_sdr_record,
                                                     tmpl_sdr_full_sensor_record_non_threshold_based_sensors)))
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record);
          goto cleanup;
        }
    }
  else
    {
      if (!(obj_sdr_record_discrete = fiid_obj_copy (obj_sdr_record,
                                                     tmpl_sdr_compact_sensor_record_non_threshold_based_sensors)))
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record);
          goto cleanup;
        }
    }

  if (event_state_0)
    {
      if (FIID_OBJ_GET (obj_sdr_record_discrete,
                        "deassertion_event_mask.event_offset_0",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record_discrete);
          goto cleanup;
        }
      *event_state_0 = val;
    }

  if (event_state_1)
    {
      if (FIID_OBJ_GET (obj_sdr_record_discrete,
                        "deassertion_event_mask.event_offset_1",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record_discrete);
          goto cleanup;
        }
      *event_state_1 = val;
    }

  if (event_state_2)
    {
      if (FIID_OBJ_GET (obj_sdr_record_discrete,
                        "deassertion_event_mask.event_offset_2",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record_discrete);
          goto cleanup;
        }
      *event_state_2 = val;
    }

  if (event_state_3)
    {
      if (FIID_OBJ_GET (obj_sdr_record_discrete,
                        "deassertion_event_mask.event_offset_3",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record_discrete);
          goto cleanup;
        }
      *event_state_3 = val;
    }

  if (event_state_4)
    {
      if (FIID_OBJ_GET (obj_sdr_record_discrete,
                        "deassertion_event_mask.event_offset_4",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record_discrete);
          goto cleanup;
        }
      *event_state_4 = val;
    }

  if (event_state_5)
    {
      if (FIID_OBJ_GET (obj_sdr_record_discrete,
                        "deassertion_event_mask.event_offset_5",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record_discrete);
          goto cleanup;
        }
      *event_state_5 = val;
    }

  if (event_state_6)
    {
      if (FIID_OBJ_GET (obj_sdr_record_discrete,
                        "deassertion_event_mask.event_offset_6",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record_discrete);
          goto cleanup;
        }
      *event_state_6 = val;
    }

  if (event_state_7)
    {
      if (FIID_OBJ_GET (obj_sdr_record_discrete,
                        "deassertion_event_mask.event_offset_7",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record_discrete);
          goto cleanup;
        }
      *event_state_7 = val;
    }

  if (event_state_8)
    {
      if (FIID_OBJ_GET (obj_sdr_record_discrete,
                        "deassertion_event_mask.event_offset_8",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record_discrete);
          goto cleanup;
        }
      *event_state_8 = val;
    }

  if (event_state_9)
    {
      if (FIID_OBJ_GET (obj_sdr_record_discrete,
                        "deassertion_event_mask.event_offset_9",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record_discrete);
          goto cleanup;
        }
      *event_state_9 = val;
    }

  if (event_state_10)
    {
      if (FIID_OBJ_GET (obj_sdr_record_discrete,
                        "deassertion_event_mask.event_offset_10",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record_discrete);
          goto cleanup;
        }
      *event_state_10 = val;
    }

  if (event_state_11)
    {
      if (FIID_OBJ_GET (obj_sdr_record_discrete,
                        "deassertion_event_mask.event_offset_11",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record_discrete);
          goto cleanup;
        }
      *event_state_11 = val;
    }

  if (event_state_12)
    {
      if (FIID_OBJ_GET (obj_sdr_record_discrete,
                        "deassertion_event_mask.event_offset_12",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record_discrete);
          goto cleanup;
        }
      *event_state_12 = val;
    }

  if (event_state_13)
    {
      if (FIID_OBJ_GET (obj_sdr_record_discrete,
                        "deassertion_event_mask.event_offset_13",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record_discrete);
          goto cleanup;
        }
      *event_state_13 = val;
    }

  if (event_state_14)
    {
      if (FIID_OBJ_GET (obj_sdr_record_discrete,
                        "deassertion_event_mask.event_offset_14",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record_discrete);
          goto cleanup;
        }
      *event_state_14 = val;
    }

  rv = 0;
  ctx->errnum = IPMI_SDR_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_sdr_record);
  fiid_obj_destroy (obj_sdr_record_discrete);
  return (rv);
}

int
ipmi_sdr_parse_threshold_assertion_supported (ipmi_sdr_ctx_t ctx,
                                              const void *sdr_record,
                                              unsigned int sdr_record_len,
                                              uint8_t *lower_non_critical_going_low,
                                              uint8_t *lower_non_critical_going_high,
                                              uint8_t *lower_critical_going_low,
                                              uint8_t *lower_critical_going_high,
                                              uint8_t *lower_non_recoverable_going_low,
                                              uint8_t *lower_non_recoverable_going_high,
                                              uint8_t *upper_non_critical_going_low,
                                              uint8_t *upper_non_critical_going_high,
                                              uint8_t *upper_critical_going_low,
                                              uint8_t *upper_critical_going_high,
                                              uint8_t *upper_non_recoverable_going_low,
                                              uint8_t *upper_non_recoverable_going_high)
{
  fiid_obj_t obj_sdr_record = NULL;
  fiid_obj_t obj_sdr_record_threshold = NULL;
  uint32_t acceptable_record_types;
  uint8_t event_reading_type_code;
  uint64_t val;
  int rv = -1;

  /* achu:
   *
   * Technically, the IPMI spec lists that compact record formats also
   * support settable thresholds.  However, since compact records
   * don't contain any information for interpreting threshold sensors
   * (e.g. R exponent) I don't know how they could be of any use.  No
   * vendor that I know of supports threshold sensors via a compact
   * record (excluding possible OEM ones).
   *
   * There's a part of me that believes the readable/setting
   * threshold masks for compact sensor records is a cut and paste
   * typo.  It shouldn't be there.
   */

  acceptable_record_types = IPMI_SDR_PARSE_RECORD_TYPE_FULL_SENSOR_RECORD;

  if (!(obj_sdr_record = _sdr_record_get_common (ctx,
                                                 sdr_record,
                                                 sdr_record_len,
                                                 acceptable_record_types)))
    goto cleanup;

  /* We don't want the generic sdr full record, we need the special
   * threshold one.
   */

  if (ipmi_sdr_parse_event_reading_type_code (ctx,
                                              sdr_record,
                                              sdr_record_len,
                                              &event_reading_type_code) < 0)
    goto cleanup;

  if (!IPMI_EVENT_READING_TYPE_CODE_IS_THRESHOLD (event_reading_type_code))
    {
      SDR_SET_ERRNUM (ctx, IPMI_SDR_ERR_PARSE_INVALID_SDR_RECORD);
      goto cleanup;
    }

  if (!(obj_sdr_record_threshold = fiid_obj_copy (obj_sdr_record,
                                                  tmpl_sdr_full_sensor_record_threshold_based_sensors)))
    {
      SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record);
      goto cleanup;
    }

  if (lower_non_critical_going_low)
    {
      if (FIID_OBJ_GET (obj_sdr_record_threshold,
                        "threshold_assertion_event_mask.lower_non_critical_going_low_supported",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record_threshold);
          goto cleanup;
        }
      *lower_non_critical_going_low = val;
    }

  if (lower_non_critical_going_high)
    {
      if (FIID_OBJ_GET (obj_sdr_record_threshold,
                        "threshold_assertion_event_mask.lower_non_critical_going_high_supported",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record_threshold);
          goto cleanup;
        }
      *lower_non_critical_going_high = val;
    }

  if (lower_critical_going_low)
    {
      if (FIID_OBJ_GET (obj_sdr_record_threshold,
                        "threshold_assertion_event_mask.lower_critical_going_low_supported",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record_threshold);
          goto cleanup;
        }
      *lower_critical_going_low = val;
    }

  if (lower_critical_going_high)
    {
      if (FIID_OBJ_GET (obj_sdr_record_threshold,
                        "threshold_assertion_event_mask.lower_critical_going_high_supported",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record_threshold);
          goto cleanup;
        }
      *lower_critical_going_high = val;
    }

  if (lower_non_recoverable_going_low)
    {
      if (FIID_OBJ_GET (obj_sdr_record_threshold,
                        "threshold_assertion_event_mask.lower_non_recoverable_going_low_supported",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record_threshold);
          goto cleanup;
        }
      *lower_non_recoverable_going_low = val;
    }

  if (lower_non_recoverable_going_high)
    {
      if (FIID_OBJ_GET (obj_sdr_record_threshold,
                        "threshold_assertion_event_mask.lower_non_recoverable_going_high_supported",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record_threshold);
          goto cleanup;
        }
      *lower_non_recoverable_going_high = val;
    }

  if (upper_non_critical_going_low)
    {
      if (FIID_OBJ_GET (obj_sdr_record_threshold,
                        "threshold_assertion_event_mask.upper_non_critical_going_low_supported",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record_threshold);
          goto cleanup;
        }
      *upper_non_critical_going_low = val;
    }

  if (upper_non_critical_going_high)
    {
      if (FIID_OBJ_GET (obj_sdr_record_threshold,
                        "threshold_assertion_event_mask.upper_non_critical_going_high_supported",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record_threshold);
          goto cleanup;
        }
      *upper_non_critical_going_high = val;
    }

  if (upper_critical_going_low)
    {
      if (FIID_OBJ_GET (obj_sdr_record_threshold,
                        "threshold_assertion_event_mask.upper_critical_going_low_supported",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record_threshold);
          goto cleanup;
        }
      *upper_critical_going_low = val;
    }

  if (upper_critical_going_high)
    {
      if (FIID_OBJ_GET (obj_sdr_record_threshold,
                        "threshold_assertion_event_mask.upper_critical_going_high_supported",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record_threshold);
          goto cleanup;
        }
      *upper_critical_going_high = val;
    }

  if (upper_non_recoverable_going_low)
    {
      if (FIID_OBJ_GET (obj_sdr_record_threshold,
                        "threshold_assertion_event_mask.upper_non_recoverable_going_low_supported",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record_threshold);
          goto cleanup;
        }
      *upper_non_recoverable_going_low = val;
    }

  if (upper_non_recoverable_going_high)
    {
      if (FIID_OBJ_GET (obj_sdr_record_threshold,
                        "threshold_assertion_event_mask.upper_non_recoverable_going_high_supported",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record_threshold);
          goto cleanup;
        }
      *upper_non_recoverable_going_high = val;
    }

  rv = 0;
  ctx->errnum = IPMI_SDR_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_sdr_record);
  fiid_obj_destroy (obj_sdr_record_threshold);
  return (rv);
}

int
ipmi_sdr_parse_threshold_deassertion_supported (ipmi_sdr_ctx_t ctx,
                                                const void *sdr_record,
                                                unsigned int sdr_record_len,
                                                uint8_t *lower_non_critical_going_low,
                                                uint8_t *lower_non_critical_going_high,
                                                uint8_t *lower_critical_going_low,
                                                uint8_t *lower_critical_going_high,
                                                uint8_t *lower_non_recoverable_going_low,
                                                uint8_t *lower_non_recoverable_going_high,
                                                uint8_t *upper_non_critical_going_low,
                                                uint8_t *upper_non_critical_going_high,
                                                uint8_t *upper_critical_going_low,
                                                uint8_t *upper_critical_going_high,
                                                uint8_t *upper_non_recoverable_going_low,
                                                uint8_t *upper_non_recoverable_going_high)
{
  fiid_obj_t obj_sdr_record = NULL;
  fiid_obj_t obj_sdr_record_threshold = NULL;
  uint32_t acceptable_record_types;
  uint8_t event_reading_type_code;
  uint64_t val;
  int rv = -1;

  /* achu:
   *
   * Technically, the IPMI spec lists that compact record formats also
   * support settable thresholds.  However, since compact records
   * don't contain any information for interpreting threshold sensors
   * (e.g. R exponent) I don't know how they could be of any use.  No
   * vendor that I know of supports threshold sensors via a compact
   * record (excluding possible OEM ones).
   *
   * There's a part of me that believes the readable/setting
   * threshold masks for compact sensor records is a cut and paste
   * typo.  It shouldn't be there.
   */

  acceptable_record_types = IPMI_SDR_PARSE_RECORD_TYPE_FULL_SENSOR_RECORD;

  if (!(obj_sdr_record = _sdr_record_get_common (ctx,
                                                 sdr_record,
                                                 sdr_record_len,
                                                 acceptable_record_types)))
    goto cleanup;

  /* We don't want the generic sdr full record, we need the special
   * threshold one.
   */

  if (ipmi_sdr_parse_event_reading_type_code (ctx,
                                              sdr_record,
                                              sdr_record_len,
                                              &event_reading_type_code) < 0)
    goto cleanup;

  if (!IPMI_EVENT_READING_TYPE_CODE_IS_THRESHOLD (event_reading_type_code))
    {
      SDR_SET_ERRNUM (ctx, IPMI_SDR_ERR_PARSE_INVALID_SDR_RECORD);
      goto cleanup;
    }

  if (!(obj_sdr_record_threshold = fiid_obj_copy (obj_sdr_record,
                                                  tmpl_sdr_full_sensor_record_threshold_based_sensors)))
    {
      SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record);
      goto cleanup;
    }

  if (lower_non_critical_going_low)
    {
      if (FIID_OBJ_GET (obj_sdr_record_threshold,
                        "threshold_deassertion_event_mask.lower_non_critical_going_low_supported",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record_threshold);
          goto cleanup;
        }
      *lower_non_critical_going_low = val;
    }

  if (lower_non_critical_going_high)
    {
      if (FIID_OBJ_GET (obj_sdr_record_threshold,
                        "threshold_deassertion_event_mask.lower_non_critical_going_high_supported",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record_threshold);
          goto cleanup;
        }
      *lower_non_critical_going_high = val;
    }

  if (lower_critical_going_low)
    {
      if (FIID_OBJ_GET (obj_sdr_record_threshold,
                        "threshold_deassertion_event_mask.lower_critical_going_low_supported",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record_threshold);
          goto cleanup;
        }
      *lower_critical_going_low = val;
    }

  if (lower_critical_going_high)
    {
      if (FIID_OBJ_GET (obj_sdr_record_threshold,
                        "threshold_deassertion_event_mask.lower_critical_going_high_supported",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record_threshold);
          goto cleanup;
        }
      *lower_critical_going_high = val;
    }

  if (lower_non_recoverable_going_low)
    {
      if (FIID_OBJ_GET (obj_sdr_record_threshold,
                        "threshold_deassertion_event_mask.lower_non_recoverable_going_low_supported",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record_threshold);
          goto cleanup;
        }
      *lower_non_recoverable_going_low = val;
    }

  if (lower_non_recoverable_going_high)
    {
      if (FIID_OBJ_GET (obj_sdr_record_threshold,
                        "threshold_deassertion_event_mask.lower_non_recoverable_going_high_supported",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record_threshold);
          goto cleanup;
        }
      *lower_non_recoverable_going_high = val;
    }

  if (upper_non_critical_going_low)
    {
      if (FIID_OBJ_GET (obj_sdr_record_threshold,
                        "threshold_deassertion_event_mask.upper_non_critical_going_low_supported",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record_threshold);
          goto cleanup;
        }
      *upper_non_critical_going_low = val;
    }

  if (upper_non_critical_going_high)
    {
      if (FIID_OBJ_GET (obj_sdr_record_threshold,
                        "threshold_deassertion_event_mask.upper_non_critical_going_high_supported",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record_threshold);
          goto cleanup;
        }
      *upper_non_critical_going_high = val;
    }

  if (upper_critical_going_low)
    {
      if (FIID_OBJ_GET (obj_sdr_record_threshold,
                        "threshold_deassertion_event_mask.upper_critical_going_low_supported",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record_threshold);
          goto cleanup;
        }
      *upper_critical_going_low = val;
    }

  if (upper_critical_going_high)
    {
      if (FIID_OBJ_GET (obj_sdr_record_threshold,
                        "threshold_deassertion_event_mask.upper_critical_going_high_supported",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record_threshold);
          goto cleanup;
        }
      *upper_critical_going_high = val;
    }

  if (upper_non_recoverable_going_low)
    {
      if (FIID_OBJ_GET (obj_sdr_record_threshold,
                        "threshold_deassertion_event_mask.upper_non_recoverable_going_low_supported",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record_threshold);
          goto cleanup;
        }
      *upper_non_recoverable_going_low = val;
    }

  if (upper_non_recoverable_going_high)
    {
      if (FIID_OBJ_GET (obj_sdr_record_threshold,
                        "threshold_deassertion_event_mask.upper_non_recoverable_going_high_supported",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record_threshold);
          goto cleanup;
        }
      *upper_non_recoverable_going_high = val;
    }

  rv = 0;
  ctx->errnum = IPMI_SDR_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_sdr_record);
  fiid_obj_destroy (obj_sdr_record_threshold);
  return (rv);
}

int
ipmi_sdr_parse_threshold_readable (ipmi_sdr_ctx_t ctx,
                                   const void *sdr_record,
                                   unsigned int sdr_record_len,
                                   uint8_t *lower_non_critical_threshold,
                                   uint8_t *lower_critical_threshold,
                                   uint8_t *lower_non_recoverable_threshold,
                                   uint8_t *upper_non_critical_threshold,
                                   uint8_t *upper_critical_threshold,
                                   uint8_t *upper_non_recoverable_threshold)
{
  fiid_obj_t obj_sdr_record = NULL;
  fiid_obj_t obj_sdr_record_threshold = NULL;
  uint32_t acceptable_record_types;
  uint8_t event_reading_type_code;
  uint64_t val;
  int rv = -1;

  /* achu:
   *
   * Technically, the IPMI spec lists that compact record formats also
   * support settable thresholds.  However, since compact records
   * don't contain any information for interpreting threshold sensors
   * (e.g. R exponent) I don't know how they could be of any use.  No
   * vendor that I know of supports threshold sensors via a compact
   * record (excluding possible OEM ones).
   *
   * There's a part of me that believes the readable/setting
   * threshold masks for compact sensor records is a cut and paste
   * typo.  It shouldn't be there.
   */

  acceptable_record_types = IPMI_SDR_PARSE_RECORD_TYPE_FULL_SENSOR_RECORD;

  if (!(obj_sdr_record = _sdr_record_get_common (ctx,
                                                 sdr_record,
                                                 sdr_record_len,
                                                 acceptable_record_types)))
    goto cleanup;

  /* We don't want the generic sdr full record, we need the special
   * threshold one.
   */

  if (ipmi_sdr_parse_event_reading_type_code (ctx,
                                              sdr_record,
                                              sdr_record_len,
                                              &event_reading_type_code) < 0)
    goto cleanup;

  if (!IPMI_EVENT_READING_TYPE_CODE_IS_THRESHOLD (event_reading_type_code))
    {
      SDR_SET_ERRNUM (ctx, IPMI_SDR_ERR_PARSE_INVALID_SDR_RECORD);
      goto cleanup;
    }

  if (!(obj_sdr_record_threshold = fiid_obj_copy (obj_sdr_record,
                                                  tmpl_sdr_full_sensor_record_threshold_based_sensors)))
    {
      SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record);
      goto cleanup;
    }

  if (lower_non_critical_threshold)
    {
      if (FIID_OBJ_GET (obj_sdr_record_threshold,
                        "readable_threshold_mask.lower_non_critical_threshold_is_readable",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record_threshold);
          goto cleanup;
        }
      *lower_non_critical_threshold = val;
    }

  if (lower_critical_threshold)
    {
      if (FIID_OBJ_GET (obj_sdr_record_threshold,
                        "readable_threshold_mask.lower_critical_threshold_is_readable",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record_threshold);
          goto cleanup;
        }
      *lower_critical_threshold = val;
    }

  if (lower_non_recoverable_threshold)
    {
      if (FIID_OBJ_GET (obj_sdr_record_threshold,
                        "readable_threshold_mask.lower_non_recoverable_threshold_is_readable",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record_threshold);
          goto cleanup;
        }
      *lower_non_recoverable_threshold = val;
    }

  if (upper_non_critical_threshold)
    {
      if (FIID_OBJ_GET (obj_sdr_record_threshold,
                        "readable_threshold_mask.upper_non_critical_threshold_is_readable",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record_threshold);
          goto cleanup;
        }
      *upper_non_critical_threshold = val;
    }

  if (upper_critical_threshold)
    {
      if (FIID_OBJ_GET (obj_sdr_record_threshold,
                        "readable_threshold_mask.upper_critical_threshold_is_readable",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record_threshold);
          goto cleanup;
        }
      *upper_critical_threshold = val;
    }

  if (upper_non_recoverable_threshold)
    {
      if (FIID_OBJ_GET (obj_sdr_record_threshold,
                        "readable_threshold_mask.upper_non_recoverable_threshold_is_readable",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record_threshold);
          goto cleanup;
        }
      *upper_non_recoverable_threshold = val;
    }

  rv = 0;
  ctx->errnum = IPMI_SDR_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_sdr_record);
  fiid_obj_destroy (obj_sdr_record_threshold);
  return (rv);
}

int
ipmi_sdr_parse_threshold_settable (ipmi_sdr_ctx_t ctx,
                                   const void *sdr_record,
                                   unsigned int sdr_record_len,
                                   uint8_t *lower_non_critical_threshold,
                                   uint8_t *lower_critical_threshold,
                                   uint8_t *lower_non_recoverable_threshold,
                                   uint8_t *upper_non_critical_threshold,
                                   uint8_t *upper_critical_threshold,
                                   uint8_t *upper_non_recoverable_threshold)
{
  fiid_obj_t obj_sdr_record = NULL;
  fiid_obj_t obj_sdr_record_threshold = NULL;
  uint32_t acceptable_record_types;
  uint8_t event_reading_type_code;
  uint64_t val;
  int rv = -1;

  /* achu:
   *
   * Technically, the IPMI spec lists that compact record formats also
   * support settable thresholds.  However, since compact records
   * don't contain any information for interpreting threshold sensors
   * (e.g. R exponent) I don't know how they could be of any use.  No
   * vendor that I know of supports threshold sensors via a compact
   * record (excluding possible OEM ones).
   *
   * There's a part of me that believes the readable/setting
   * threshold masks for compact sensor records is a cut and paste
   * typo.  It shouldn't be there.
   */

  acceptable_record_types = IPMI_SDR_PARSE_RECORD_TYPE_FULL_SENSOR_RECORD;

  if (!(obj_sdr_record = _sdr_record_get_common (ctx,
                                                 sdr_record,
                                                 sdr_record_len,
                                                 acceptable_record_types)))
    goto cleanup;

  /* We don't want the generic sdr full record, we need the special
   * threshold one.
   */

  if (ipmi_sdr_parse_event_reading_type_code (ctx,
                                              sdr_record,
                                              sdr_record_len,
                                              &event_reading_type_code) < 0)
    goto cleanup;

  if (!IPMI_EVENT_READING_TYPE_CODE_IS_THRESHOLD (event_reading_type_code))
    {
      SDR_SET_ERRNUM (ctx, IPMI_SDR_ERR_PARSE_INVALID_SDR_RECORD);
      goto cleanup;
    }

  if (!(obj_sdr_record_threshold = fiid_obj_copy (obj_sdr_record,
                                                  tmpl_sdr_full_sensor_record_threshold_based_sensors)))
    {
      SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record);
      goto cleanup;
    }

  if (lower_non_critical_threshold)
    {
      if (FIID_OBJ_GET (obj_sdr_record_threshold,
                        "settable_threshold_mask.lower_non_critical_threshold_is_settable",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record_threshold);
          goto cleanup;
        }
      *lower_non_critical_threshold = val;
    }

  if (lower_critical_threshold)
    {
      if (FIID_OBJ_GET (obj_sdr_record_threshold,
                        "settable_threshold_mask.lower_critical_threshold_is_settable",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record_threshold);
          goto cleanup;
        }
      *lower_critical_threshold = val;
    }

  if (lower_non_recoverable_threshold)
    {
      if (FIID_OBJ_GET (obj_sdr_record_threshold,
                        "settable_threshold_mask.lower_non_recoverable_threshold_is_settable",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record_threshold);
          goto cleanup;
        }
      *lower_non_recoverable_threshold = val;
    }

  if (upper_non_critical_threshold)
    {
      if (FIID_OBJ_GET (obj_sdr_record_threshold,
                        "settable_threshold_mask.upper_non_critical_threshold_is_settable",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record_threshold);
          goto cleanup;
        }
      *upper_non_critical_threshold = val;
    }

  if (upper_critical_threshold)
    {
      if (FIID_OBJ_GET (obj_sdr_record_threshold,
                        "settable_threshold_mask.upper_critical_threshold_is_settable",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record_threshold);
          goto cleanup;
        }
      *upper_critical_threshold = val;
    }

  if (upper_non_recoverable_threshold)
    {
      if (FIID_OBJ_GET (obj_sdr_record_threshold,
                        "settable_threshold_mask.upper_non_recoverable_threshold_is_settable",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record_threshold);
          goto cleanup;
        }
      *upper_non_recoverable_threshold = val;
    }

  rv = 0;
  ctx->errnum = IPMI_SDR_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_sdr_record);
  fiid_obj_destroy (obj_sdr_record_threshold);
  return (rv);
}

int
ipmi_sdr_parse_sensor_decoding_data (ipmi_sdr_ctx_t ctx,
                                     const void *sdr_record,
                                     unsigned int sdr_record_len,
                                     int8_t *r_exponent,
                                     int8_t *b_exponent,
                                     int16_t *m,
                                     int16_t *b,
                                     uint8_t *linearization,
                                     uint8_t *analog_data_format)
{
  fiid_obj_t obj_sdr_record = NULL;
  uint32_t acceptable_record_types;
  uint64_t val, val1, val2;
  int rv = -1;

  acceptable_record_types = IPMI_SDR_PARSE_RECORD_TYPE_FULL_SENSOR_RECORD;

  if (!(obj_sdr_record = _sdr_record_get_common (ctx,
                                                 sdr_record,
                                                 sdr_record_len,
                                                 acceptable_record_types)))
    goto cleanup;

  if (r_exponent)
    {
      if (FIID_OBJ_GET (obj_sdr_record,
                        "r_exponent",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record);
          goto cleanup;
        }
      *r_exponent = val;
      if (*r_exponent & 0x08)
        *r_exponent |= 0xF0;
    }

  if (b_exponent)
    {
      if (FIID_OBJ_GET (obj_sdr_record,
                        "b_exponent",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record);
          goto cleanup;
        }
      *b_exponent = val;
      if (*b_exponent & 0x08)
        *b_exponent |= 0xF0;
    }

  if (m)
    {
      if (FIID_OBJ_GET (obj_sdr_record,
                        "m_ls",
                        &val1) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record);
          goto cleanup;
        }
      if (FIID_OBJ_GET (obj_sdr_record,
                        "m_ms",
                        &val2) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record);
          goto cleanup;
        }
      *m = val1;
      *m |= (((int16_t)val2 & 0x3) << 8);
      if (*m & 0x200)
        *m |= 0xFE00;
    }

  if (b)
    {
      if (FIID_OBJ_GET (obj_sdr_record,
                        "b_ls",
                        &val1) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record);
          goto cleanup;
        }
      if (FIID_OBJ_GET (obj_sdr_record,
                        "b_ms",
                        &val2) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record);
          goto cleanup;
        }
      *b = val1;
      *b |= (((int16_t)val2 & 0x3) << 8);
      if (*b & 0x200)
        *b |= 0xFE00;
    }

  if (linearization)
    {
      if (FIID_OBJ_GET (obj_sdr_record,
                        "linearization",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record);
          goto cleanup;
        }
      (*linearization) = val;
    }

  if (analog_data_format)
    {
      if (FIID_OBJ_GET (obj_sdr_record,
                        "sensor_unit1.analog_data_format",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record);
          goto cleanup;
        }
      (*analog_data_format) = val;
    }

  rv = 0;
  ctx->errnum = IPMI_SDR_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_sdr_record);
  return (rv);
}

static int
_sensor_decode_value (ipmi_sdr_ctx_t ctx,
                      int8_t r_exponent,
                      int8_t b_exponent,
                      int16_t m,
                      int16_t b,
                      uint8_t linearization,
                      uint8_t analog_data_format,
                      uint8_t raw_data,
                      double **value_ptr)
{
  double reading;
  int rv = -1;

  assert (ctx);
  assert (ctx->magic == IPMI_SDR_CTX_MAGIC);
  assert (value_ptr);

  *value_ptr = NULL;

  if (ipmi_sensor_decode_value (r_exponent,
                                b_exponent,
                                m,
                                b,
                                linearization,
                                analog_data_format,
                                raw_data,
                                &reading) < 0)
    {
      SDR_SET_ERRNUM (ctx, IPMI_SDR_ERR_INTERNAL_ERROR);
      goto cleanup;
    }

  if (!((*value_ptr) = (double *)malloc (sizeof (double))))
    {
      SDR_SET_ERRNUM (ctx, IPMI_SDR_ERR_OUT_OF_MEMORY);
      goto cleanup;
    }
  (**value_ptr) = reading;

  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_sdr_parse_sensor_reading_ranges_specified (ipmi_sdr_ctx_t ctx,
                                                const void *sdr_record,
                                                unsigned int sdr_record_len,
                                                uint8_t *nominal_reading_specified,
                                                uint8_t *normal_maximum_specified,
                                                uint8_t *normal_minimum_specified)
{
  fiid_obj_t obj_sdr_record = NULL;
  uint32_t acceptable_record_types;
  uint64_t val;
  int rv = -1;

  acceptable_record_types = IPMI_SDR_PARSE_RECORD_TYPE_FULL_SENSOR_RECORD;

  if (!(obj_sdr_record = _sdr_record_get_common (ctx,
                                                 sdr_record,
                                                 sdr_record_len,
                                                 acceptable_record_types)))
    goto cleanup;

  if (nominal_reading_specified)
    {
      if (FIID_OBJ_GET (obj_sdr_record,
                        "analog_characteristics_flag.nominal_reading",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record);
          goto cleanup;
        }
      *nominal_reading_specified = val;
    }

  if (normal_maximum_specified)
    {
      if (FIID_OBJ_GET (obj_sdr_record,
                        "analog_characteristics_flag.normal_max",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record);
          goto cleanup;
        }
      *normal_maximum_specified = val;
    }

  if (normal_minimum_specified)
    {
      if (FIID_OBJ_GET (obj_sdr_record,
                        "analog_characteristics_flag.normal_min",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record);
          goto cleanup;
        }
      *normal_minimum_specified = val;
    }

  rv = 0;
  ctx->errnum = IPMI_SDR_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_sdr_record);
  return (rv);
}

int
ipmi_sdr_parse_sensor_reading_ranges (ipmi_sdr_ctx_t ctx,
                                      const void *sdr_record,
                                      unsigned int sdr_record_len,
                                      double **nominal_reading,
                                      double **normal_maximum,
                                      double **normal_minimum,
                                      double **sensor_maximum_reading,
                                      double **sensor_minimum_reading)
{
  fiid_obj_t obj_sdr_record = NULL;
  uint32_t acceptable_record_types;
  int8_t r_exponent, b_exponent;
  int16_t m, b;
  uint8_t reading_raw, linearization, analog_data_format;
  double *tmp_nominal_reading = NULL;
  double *tmp_normal_maximum = NULL;
  double *tmp_normal_minimum = NULL;
  double *tmp_sensor_maximum_reading = NULL;
  double *tmp_sensor_minimum_reading = NULL;
  uint64_t val;
  int rv = -1;

  acceptable_record_types = IPMI_SDR_PARSE_RECORD_TYPE_FULL_SENSOR_RECORD;

  if (!(obj_sdr_record = _sdr_record_get_common (ctx,
                                                 sdr_record,
                                                 sdr_record_len,
                                                 acceptable_record_types)))
    goto cleanup;

  if (nominal_reading)
    *nominal_reading = NULL;
  if (normal_maximum)
    *normal_maximum = NULL;
  if (normal_minimum)
    *normal_minimum = NULL;
  if (sensor_maximum_reading)
    *sensor_maximum_reading = NULL;
  if (sensor_minimum_reading)
    *sensor_minimum_reading = NULL;

  if (ipmi_sdr_parse_sensor_decoding_data (ctx,
                                           sdr_record,
                                           sdr_record_len,
                                           &r_exponent,
                                           &b_exponent,
                                           &m,
                                           &b,
                                           &linearization,
                                           &analog_data_format) < 0)
    goto cleanup;

  if (!IPMI_SDR_ANALOG_DATA_FORMAT_VALID (analog_data_format))
    {
      SDR_SET_ERRNUM (ctx, IPMI_SDR_ERR_PARSE_CANNOT_PARSE_OR_CALCULATE);
      goto cleanup;
    }

  if (!IPMI_SDR_LINEARIZATION_IS_LINEAR (linearization))
    {
      SDR_SET_ERRNUM (ctx, IPMI_SDR_ERR_PARSE_CANNOT_PARSE_OR_CALCULATE);
      goto cleanup;
    }

  if (nominal_reading)
    {
      if (FIID_OBJ_GET (obj_sdr_record,
                        "nominal_reading",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record);
          goto cleanup;
        }
      reading_raw = val;

      if (_sensor_decode_value (ctx,
                                r_exponent,
                                b_exponent,
                                m,
                                b,
                                linearization,
                                analog_data_format,
                                reading_raw,
                                &tmp_nominal_reading) < 0)
        goto cleanup;
    }
  if (normal_maximum)
    {
      if (FIID_OBJ_GET (obj_sdr_record,
                        "normal_maximum",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record);
          goto cleanup;
        }
      reading_raw = val;

      if (_sensor_decode_value (ctx,
                                r_exponent,
                                b_exponent,
                                m,
                                b,
                                linearization,
                                analog_data_format,
                                reading_raw,
                                &tmp_normal_maximum) < 0)
        goto cleanup;
    }
  if (normal_minimum)
    {
      if (FIID_OBJ_GET (obj_sdr_record,
                        "normal_minimum",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record);
          goto cleanup;
        }
      reading_raw = val;

      if (_sensor_decode_value (ctx,
                                r_exponent,
                                b_exponent,
                                m,
                                b,
                                linearization,
                                analog_data_format,
                                reading_raw,
                                &tmp_normal_minimum) < 0)
        goto cleanup;
    }
  if (sensor_maximum_reading)
    {
      if (FIID_OBJ_GET (obj_sdr_record,
                        "sensor_maximum_reading",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record);
          goto cleanup;
        }
      reading_raw = val;

      if (_sensor_decode_value (ctx,
                                r_exponent,
                                b_exponent,
                                m,
                                b,
                                linearization,
                                analog_data_format,
                                reading_raw,
                                &tmp_sensor_maximum_reading) < 0)
        goto cleanup;
    }
  if (sensor_minimum_reading)
    {
      if (FIID_OBJ_GET (obj_sdr_record,
                        "sensor_minimum_reading",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record);
          goto cleanup;
        }
      reading_raw = val;

      if (_sensor_decode_value (ctx,
                                r_exponent,
                                b_exponent,
                                m,
                                b,
                                linearization,
                                analog_data_format,
                                reading_raw,
                                &tmp_sensor_minimum_reading) < 0)
        goto cleanup;
    }

  if (nominal_reading)
    *nominal_reading = tmp_nominal_reading;
  if (normal_maximum)
    *normal_maximum = tmp_normal_maximum;
  if (normal_minimum)
    *normal_minimum = tmp_normal_minimum;
  if (sensor_maximum_reading)
    *sensor_maximum_reading = tmp_sensor_maximum_reading;
  if (sensor_minimum_reading)
    *sensor_minimum_reading = tmp_sensor_minimum_reading;

  rv = 0;
  ctx->errnum = IPMI_SDR_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_sdr_record);
  if (rv < 0)
    {
      free (tmp_nominal_reading);
      free (tmp_normal_maximum);
      free (tmp_normal_minimum);
      free (tmp_sensor_maximum_reading);
      free (tmp_sensor_minimum_reading);
    }
  return (rv);
}

int
ipmi_sdr_parse_thresholds (ipmi_sdr_ctx_t ctx,
                           const void *sdr_record,
                           unsigned int sdr_record_len,
                           double **lower_non_critical_threshold,
                           double **lower_critical_threshold,
                           double **lower_non_recoverable_threshold,
                           double **upper_non_critical_threshold,
                           double **upper_critical_threshold,
                           double **upper_non_recoverable_threshold)
{
  fiid_obj_t obj_sdr_record = NULL;
  uint32_t acceptable_record_types;
  int8_t r_exponent, b_exponent;
  int16_t m, b;
  uint8_t threshold_raw, linearization, analog_data_format;
  double *tmp_lower_non_critical_threshold = NULL;
  double *tmp_lower_critical_threshold = NULL;
  double *tmp_lower_non_recoverable_threshold = NULL;
  double *tmp_upper_non_critical_threshold = NULL;
  double *tmp_upper_critical_threshold = NULL;
  double *tmp_upper_non_recoverable_threshold = NULL;
  uint64_t val;
  int rv = -1;

  acceptable_record_types = IPMI_SDR_PARSE_RECORD_TYPE_FULL_SENSOR_RECORD;

  if (!(obj_sdr_record = _sdr_record_get_common (ctx,
                                                 sdr_record,
                                                 sdr_record_len,
                                                 acceptable_record_types)))
    goto cleanup;

  if (lower_non_critical_threshold)
    *lower_non_critical_threshold = NULL;
  if (lower_critical_threshold)
    *lower_critical_threshold = NULL;
  if (lower_non_recoverable_threshold)
    *lower_non_recoverable_threshold = NULL;
  if (upper_non_critical_threshold)
    *upper_non_critical_threshold = NULL;
  if (upper_critical_threshold)
    *upper_critical_threshold = NULL;
  if (upper_non_recoverable_threshold)
    *upper_non_recoverable_threshold = NULL;

  if (ipmi_sdr_parse_sensor_decoding_data (ctx,
                                           sdr_record,
                                           sdr_record_len,
                                           &r_exponent,
                                           &b_exponent,
                                           &m,
                                           &b,
                                           &linearization,
                                           &analog_data_format) < 0)
    goto cleanup;

  if (!IPMI_SDR_ANALOG_DATA_FORMAT_VALID (analog_data_format))
    {
      SDR_SET_ERRNUM (ctx, IPMI_SDR_ERR_PARSE_CANNOT_PARSE_OR_CALCULATE);
      goto cleanup;
    }

  if (!IPMI_SDR_LINEARIZATION_IS_LINEAR (linearization))
    {
      SDR_SET_ERRNUM (ctx, IPMI_SDR_ERR_PARSE_CANNOT_PARSE_OR_CALCULATE);
      goto cleanup;
    }

  if (lower_non_critical_threshold)
    {
      if (FIID_OBJ_GET (obj_sdr_record,
                        "lower_non_critical_threshold",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record);
          goto cleanup;
        }
      threshold_raw = val;

      if (_sensor_decode_value (ctx,
                                r_exponent,
                                b_exponent,
                                m,
                                b,
                                linearization,
                                analog_data_format,
                                threshold_raw,
                                &tmp_lower_non_critical_threshold) < 0)
        goto cleanup;
    }
  if (lower_critical_threshold)
    {
      if (FIID_OBJ_GET (obj_sdr_record,
                        "lower_critical_threshold",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record);
          goto cleanup;
        }
      threshold_raw = val;

      if (_sensor_decode_value (ctx,
                                r_exponent,
                                b_exponent,
                                m,
                                b,
                                linearization,
                                analog_data_format,
                                threshold_raw,
                                &tmp_lower_critical_threshold) < 0)
        goto cleanup;
    }
  if (lower_non_recoverable_threshold)
    {
      if (FIID_OBJ_GET (obj_sdr_record,
                        "lower_non_recoverable_threshold",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record);
          goto cleanup;
        }
      threshold_raw = val;

      if (_sensor_decode_value (ctx,
                                r_exponent,
                                b_exponent,
                                m,
                                b,
                                linearization,
                                analog_data_format,
                                threshold_raw,
                                &tmp_lower_non_recoverable_threshold) < 0)
        goto cleanup;
    }
  if (upper_non_critical_threshold)
    {
      if (FIID_OBJ_GET (obj_sdr_record,
                        "upper_non_critical_threshold",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record);
          goto cleanup;
        }
      threshold_raw = val;

      if (_sensor_decode_value (ctx,
                                r_exponent,
                                b_exponent,
                                m,
                                b,
                                linearization,
                                analog_data_format,
                                threshold_raw,
                                &tmp_upper_non_critical_threshold) < 0)
        goto cleanup;
    }
  if (upper_critical_threshold)
    {
      if (FIID_OBJ_GET (obj_sdr_record,
                        "upper_critical_threshold",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record);
          goto cleanup;
        }
      threshold_raw = val;

      if (_sensor_decode_value (ctx,
                                r_exponent,
                                b_exponent,
                                m,
                                b,
                                linearization,
                                analog_data_format,
                                threshold_raw,
                                &tmp_upper_critical_threshold) < 0)
        goto cleanup;
    }
  if (upper_non_recoverable_threshold)
    {
      if (FIID_OBJ_GET (obj_sdr_record,
                        "upper_non_recoverable_threshold",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record);
          goto cleanup;
        }
      threshold_raw = val;

      if (_sensor_decode_value (ctx,
                                r_exponent,
                                b_exponent,
                                m,
                                b,
                                linearization,
                                analog_data_format,
                                threshold_raw,
                                &tmp_upper_non_recoverable_threshold) < 0)
        goto cleanup;
    }

  if (lower_non_critical_threshold)
    *lower_non_critical_threshold = tmp_lower_non_critical_threshold;
  if (lower_critical_threshold)
    *lower_critical_threshold = tmp_lower_critical_threshold;
  if (lower_non_recoverable_threshold)
    *lower_non_recoverable_threshold = tmp_lower_non_recoverable_threshold;
  if (upper_non_critical_threshold)
    *upper_non_critical_threshold = tmp_upper_non_critical_threshold;
  if (upper_critical_threshold)
    *upper_critical_threshold = tmp_upper_critical_threshold;
  if (upper_non_recoverable_threshold)
    *upper_non_recoverable_threshold = tmp_upper_non_recoverable_threshold;

  rv = 0;
  ctx->errnum = IPMI_SDR_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_sdr_record);
  if (rv < 0)
    {
      free (tmp_lower_non_critical_threshold);
      free (tmp_lower_critical_threshold);
      free (tmp_lower_non_recoverable_threshold);
      free (tmp_upper_non_critical_threshold);
      free (tmp_upper_critical_threshold);
      free (tmp_upper_non_recoverable_threshold);
    }
  return (rv);
}

int
ipmi_sdr_parse_thresholds_raw (ipmi_sdr_ctx_t ctx,
                               const void *sdr_record,
                               unsigned int sdr_record_len,
                               uint8_t *lower_non_critical_threshold,
                               uint8_t *lower_critical_threshold,
                               uint8_t *lower_non_recoverable_threshold,
                               uint8_t *upper_non_critical_threshold,
                               uint8_t *upper_critical_threshold,
                               uint8_t *upper_non_recoverable_threshold)
{
  fiid_obj_t obj_sdr_record = NULL;
  fiid_obj_t obj_sdr_record_threshold = NULL;
  uint32_t acceptable_record_types;
  uint8_t event_reading_type_code;
  uint64_t val;
  int rv = -1;

  acceptable_record_types = IPMI_SDR_PARSE_RECORD_TYPE_FULL_SENSOR_RECORD;

  if (!(obj_sdr_record = _sdr_record_get_common (ctx,
                                                 sdr_record,
                                                 sdr_record_len,
                                                 acceptable_record_types)))
    goto cleanup;

  /* We don't want the generic sdr full record, we need the special
   * threshold one.
   */

  if (ipmi_sdr_parse_event_reading_type_code (ctx,
                                              sdr_record,
                                              sdr_record_len,
                                              &event_reading_type_code) < 0)
    goto cleanup;

  if (!IPMI_EVENT_READING_TYPE_CODE_IS_THRESHOLD (event_reading_type_code))
    {
      SDR_SET_ERRNUM (ctx, IPMI_SDR_ERR_PARSE_INVALID_SDR_RECORD);
      goto cleanup;
    }

  if (!(obj_sdr_record_threshold = fiid_obj_copy (obj_sdr_record,
                                                  tmpl_sdr_full_sensor_record_threshold_based_sensors)))
    {
      SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record);
      goto cleanup;
    }

  if (lower_non_critical_threshold)
    {
      if (FIID_OBJ_GET (obj_sdr_record_threshold,
                        "lower_non_critical_threshold",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record_threshold);
          goto cleanup;
        }
      *lower_non_critical_threshold = val;
    }

  if (lower_critical_threshold)
    {
      if (FIID_OBJ_GET (obj_sdr_record_threshold,
                        "lower_critical_threshold",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record_threshold);
          goto cleanup;
        }
      *lower_critical_threshold = val;
    }

  if (lower_non_recoverable_threshold)
    {
      if (FIID_OBJ_GET (obj_sdr_record_threshold,
                        "lower_non_recoverable_threshold",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record_threshold);
          goto cleanup;
        }
      *lower_non_recoverable_threshold = val;
    }

  if (upper_non_critical_threshold)
    {
      if (FIID_OBJ_GET (obj_sdr_record_threshold,
                        "upper_non_critical_threshold",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record_threshold);
          goto cleanup;
        }
      *upper_non_critical_threshold = val;
    }

  if (upper_critical_threshold)
    {
      if (FIID_OBJ_GET (obj_sdr_record_threshold,
                        "upper_critical_threshold",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record_threshold);
          goto cleanup;
        }
      *upper_critical_threshold = val;
    }

  if (upper_non_recoverable_threshold)
    {
      if (FIID_OBJ_GET (obj_sdr_record_threshold,
                        "upper_non_recoverable_threshold",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record_threshold);
          goto cleanup;
        }
      *upper_non_recoverable_threshold = val;
    }

  rv = 0;
  ctx->errnum = IPMI_SDR_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_sdr_record);
  fiid_obj_destroy (obj_sdr_record_threshold);
  return (rv);
}

int
ipmi_sdr_parse_tolerance (ipmi_sdr_ctx_t ctx,
                          const void *sdr_record,
                          unsigned int sdr_record_len,
                          double **tolerance)
{
  fiid_obj_t obj_sdr_record = NULL;
  uint32_t acceptable_record_types;
  int8_t r_exponent;
  int16_t m;
  uint8_t tolerance_raw, linearization;
  double *tmp_tolerance = NULL;
  uint64_t val;
  int rv = -1;

  acceptable_record_types = IPMI_SDR_PARSE_RECORD_TYPE_FULL_SENSOR_RECORD;

  if (!(obj_sdr_record = _sdr_record_get_common (ctx,
                                                 sdr_record,
                                                 sdr_record_len,
                                                 acceptable_record_types)))
    goto cleanup;

  if (tolerance)
    *tolerance = NULL;

  if (ipmi_sdr_parse_sensor_decoding_data (ctx,
                                           sdr_record,
                                           sdr_record_len,
                                           &r_exponent,
                                           NULL,
                                           &m,
                                           NULL,
                                           &linearization,
                                           NULL) < 0)
    goto cleanup;

  if (!IPMI_SDR_LINEARIZATION_IS_LINEAR (linearization))
    {
      SDR_SET_ERRNUM (ctx, IPMI_SDR_ERR_PARSE_CANNOT_PARSE_OR_CALCULATE);
      goto cleanup;
    }

  if (tolerance)
    {
      double reading;

      if (FIID_OBJ_GET (obj_sdr_record,
                        "tolerance",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record);
          goto cleanup;
        }
      tolerance_raw = val;

      if (ipmi_sensor_decode_tolerance (r_exponent,
                                        m,
                                        linearization,
                                        tolerance_raw,
                                        &reading) < 0)
        {
          SDR_SET_ERRNUM (ctx, IPMI_SDR_ERR_INTERNAL_ERROR);
          goto cleanup;
        }
      
      if (!(tmp_tolerance = (double *)malloc (sizeof (double))))
        {
          SDR_SET_ERRNUM (ctx, IPMI_SDR_ERR_OUT_OF_MEMORY);
          goto cleanup;
        }
      (*tmp_tolerance) = reading;
    }

  if (tolerance)
    *tolerance = tmp_tolerance;

  rv = 0;
  ctx->errnum = IPMI_SDR_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_sdr_record);
  if (rv < 0)
    free (tmp_tolerance);
  return (rv);
}

int
ipmi_sdr_parse_accuracy (ipmi_sdr_ctx_t ctx,
			 const void *sdr_record,
			 unsigned int sdr_record_len,
			 double **accuracy)
{
  fiid_obj_t obj_sdr_record = NULL;
  uint32_t acceptable_record_types;
  uint16_t accuracy_raw;
  uint8_t accuracy_ls, accuracy_ms, accuracy_exp;
  double *tmp_accuracy = NULL;
  uint64_t val;
  int rv = -1;

  acceptable_record_types = IPMI_SDR_PARSE_RECORD_TYPE_FULL_SENSOR_RECORD;

  if (!(obj_sdr_record = _sdr_record_get_common (ctx,
                                                 sdr_record,
                                                 sdr_record_len,
                                                 acceptable_record_types)))
    goto cleanup;

  if (accuracy)
    *accuracy = NULL;

  if (accuracy)
    {
      double reading;

      if (FIID_OBJ_GET (obj_sdr_record,
                        "accuracy_ls",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record);
          goto cleanup;
        }
      accuracy_ls = val;

      if (FIID_OBJ_GET (obj_sdr_record,
                        "accuracy_ms",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record);
          goto cleanup;
        }
      accuracy_ms = val;

      /* accuracy is unsigned, no need to sign extend */
      accuracy_raw = accuracy_ls | (((uint16_t)accuracy_ms) << 6);

      if (FIID_OBJ_GET (obj_sdr_record,
                        "accuracy_exp",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record);
          goto cleanup;
        }
      accuracy_exp = val;

      if (ipmi_sensor_decode_accuracy (accuracy_raw,
                                       accuracy_exp,
                                       &reading) < 0)
        {
          SDR_SET_ERRNUM (ctx, IPMI_SDR_ERR_INTERNAL_ERROR);
          goto cleanup;
        }
      
      if (!(tmp_accuracy = (double *)malloc (sizeof (double))))
        {
          SDR_SET_ERRNUM (ctx, IPMI_SDR_ERR_OUT_OF_MEMORY);
          goto cleanup;
        }
      (*tmp_accuracy) = reading;
    }

  if (accuracy)
    *accuracy = tmp_accuracy;

  rv = 0;
  ctx->errnum = IPMI_SDR_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_sdr_record);
  if (rv < 0)
    free (tmp_accuracy);
  return (rv);
}

int
ipmi_sdr_parse_hysteresis (ipmi_sdr_ctx_t ctx,
                           const void *sdr_record,
                           unsigned int sdr_record_len,
                           uint8_t *positive_going_threshold_hysteresis,
                           uint8_t *negative_going_threshold_hysteresis)
{
  fiid_obj_t obj_sdr_record = NULL;
  uint32_t acceptable_record_types;
  uint64_t val;
  int rv = -1;

  acceptable_record_types = IPMI_SDR_PARSE_RECORD_TYPE_FULL_SENSOR_RECORD;
  acceptable_record_types |= IPMI_SDR_PARSE_RECORD_TYPE_COMPACT_SENSOR_RECORD;

  if (!(obj_sdr_record = _sdr_record_get_common (ctx,
                                                 sdr_record,
                                                 sdr_record_len,
                                                 acceptable_record_types)))
    goto cleanup;

  if (positive_going_threshold_hysteresis)
    {
      if (FIID_OBJ_GET (obj_sdr_record,
                        "positive_going_threshold_hysteresis",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record);
          goto cleanup;
        }
      *positive_going_threshold_hysteresis = val;
    }
  if (negative_going_threshold_hysteresis)
    {
      if (FIID_OBJ_GET (obj_sdr_record,
                        "negative_going_threshold_hysteresis",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record);
          goto cleanup;
        }
      *negative_going_threshold_hysteresis = val;
    }
  rv = 0;
  ctx->errnum = IPMI_SDR_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_sdr_record);
  return (rv);
}

int
ipmi_sdr_parse_sensor_record_sharing (ipmi_sdr_ctx_t ctx,
                                      const void *sdr_record,
                                      unsigned int sdr_record_len,
                                      uint8_t *share_count,
                                      uint8_t *id_string_instance_modifier_type,
                                      uint8_t *id_string_instance_modifier_offset,
                                      uint8_t *entity_instance_sharing)
{
  fiid_obj_t obj_sdr_record = NULL;
  uint32_t acceptable_record_types;
  uint64_t val;
  int rv = -1;

  acceptable_record_types = IPMI_SDR_PARSE_RECORD_TYPE_COMPACT_SENSOR_RECORD;
  acceptable_record_types |= IPMI_SDR_PARSE_RECORD_TYPE_EVENT_ONLY_RECORD;

  if (!(obj_sdr_record = _sdr_record_get_common (ctx,
                                                 sdr_record,
                                                 sdr_record_len,
                                                 acceptable_record_types)))
    goto cleanup;

  if (share_count)
    {
      if (FIID_OBJ_GET (obj_sdr_record,
                        "share_count",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record);
          goto cleanup;
        }
      *share_count = val;
    }

  if (id_string_instance_modifier_type)
    {
      if (FIID_OBJ_GET (obj_sdr_record,
                        "id_string_instance_modifier_type",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record);
          goto cleanup;
        }
      *id_string_instance_modifier_type = val;
    }

  if (id_string_instance_modifier_offset)
    {
      if (FIID_OBJ_GET (obj_sdr_record,
                        "id_string_instance_modifier_offset",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record);
          goto cleanup;
        }
      *id_string_instance_modifier_offset = val;
    }

  if (entity_instance_sharing)
    {
      if (FIID_OBJ_GET (obj_sdr_record,
                        "entity_instance_sharing",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record);
          goto cleanup;
        }
      *entity_instance_sharing = val;
    }

  rv = 0;
  ctx->errnum = IPMI_SDR_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_sdr_record);
  return (rv);
}

int
ipmi_sdr_parse_container_entity (ipmi_sdr_ctx_t ctx,
                                 const void *sdr_record,
                                 unsigned int sdr_record_len,
                                 uint8_t *container_entity_id,
                                 uint8_t *container_entity_instance)
{
  fiid_obj_t obj_sdr_record = NULL;
  uint32_t acceptable_record_types;
  uint64_t val;
  int rv = -1;

  acceptable_record_types = IPMI_SDR_PARSE_RECORD_TYPE_ENTITY_ASSOCIATION_RECORD;
  acceptable_record_types |= IPMI_SDR_PARSE_RECORD_TYPE_DEVICE_RELATIVE_ENTITY_ASSOCIATION_RECORD;

  if (!(obj_sdr_record = _sdr_record_get_common (ctx,
                                                 sdr_record,
                                                 sdr_record_len,
                                                 acceptable_record_types)))
    goto cleanup;

  if (container_entity_id)
    {
      if (FIID_OBJ_GET (obj_sdr_record,
                        "container_entity_id",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record);
          goto cleanup;
        }
      *container_entity_id = val;
    }

  if (container_entity_instance)
    {
      if (FIID_OBJ_GET (obj_sdr_record,
                        "container_entity_instance",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record);
          goto cleanup;
        }
      *container_entity_instance = val;
    }

  rv = 0;
  ctx->errnum = IPMI_SDR_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_sdr_record);
  return (rv);
}

int
ipmi_sdr_parse_device_id_string (ipmi_sdr_ctx_t ctx,
                                 const void *sdr_record,
                                 unsigned int sdr_record_len,
                                 char *device_id_string,
                                 unsigned int device_id_string_len)
{
  fiid_obj_t obj_sdr_record = NULL;
  uint32_t acceptable_record_types;
  int len = 0;
  int rv = -1;

  acceptable_record_types = IPMI_SDR_PARSE_RECORD_TYPE_GENERIC_DEVICE_LOCATOR_RECORD;
  acceptable_record_types |= IPMI_SDR_PARSE_RECORD_TYPE_FRU_DEVICE_LOCATOR_RECORD;
  acceptable_record_types |= IPMI_SDR_PARSE_RECORD_TYPE_MANAGEMENT_CONTROLLER_DEVICE_LOCATOR_RECORD;

  if (!(obj_sdr_record = _sdr_record_get_common (ctx,
                                                 sdr_record,
                                                 sdr_record_len,
                                                 acceptable_record_types)))
    goto cleanup;

  if (device_id_string && device_id_string_len)
    {
      if ((len = fiid_obj_get_data (obj_sdr_record,
                                    "device_id_string",
                                    device_id_string,
                                    device_id_string_len)) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record);
          goto cleanup;
        }
    }

  rv = len;
  ctx->errnum = IPMI_SDR_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_sdr_record);
  return (rv);
}

int
ipmi_sdr_parse_device_type (ipmi_sdr_ctx_t ctx,
                            const void *sdr_record,
                            unsigned int sdr_record_len,
                            uint8_t *device_type,
                            uint8_t *device_type_modifier)
{
  fiid_obj_t obj_sdr_record = NULL;
  uint32_t acceptable_record_types;
  uint64_t val;
  int rv = -1;

  acceptable_record_types = IPMI_SDR_PARSE_RECORD_TYPE_GENERIC_DEVICE_LOCATOR_RECORD;
  acceptable_record_types |= IPMI_SDR_PARSE_RECORD_TYPE_FRU_DEVICE_LOCATOR_RECORD;

  if (!(obj_sdr_record = _sdr_record_get_common (ctx,
                                                 sdr_record,
                                                 sdr_record_len,
                                                 acceptable_record_types)))
    goto cleanup;

  if (device_type)
    {
      if (FIID_OBJ_GET (obj_sdr_record,
                        "device_type",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record);
          goto cleanup;
        }
      *device_type = val;
    }
  if (device_type_modifier)
    {
      if (FIID_OBJ_GET (obj_sdr_record,
                        "device_type_modifier",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record);
          goto cleanup;
        }
      *device_type_modifier = val;
    }

  rv = 0;
  ctx->errnum = IPMI_SDR_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_sdr_record);
  return (rv);
}

int
ipmi_sdr_parse_generic_device_locator_parameters (ipmi_sdr_ctx_t ctx,
                                                  const void *sdr_record,
                                                  unsigned int sdr_record_len,
                                                  uint8_t *device_access_address,
                                                  uint8_t *channel_number,
                                                  uint8_t *device_slave_address,
                                                  uint8_t *private_bus_id,
                                                  uint8_t *lun_for_master_write_read_command,
                                                  uint8_t *address_span,
                                                  uint8_t *oem)
{
  fiid_obj_t obj_sdr_record = NULL;
  uint32_t acceptable_record_types;
  uint64_t val1, val2;
  uint64_t val;
  int rv = -1;

  acceptable_record_types = IPMI_SDR_PARSE_RECORD_TYPE_GENERIC_DEVICE_LOCATOR_RECORD;

  if (!(obj_sdr_record = _sdr_record_get_common (ctx,
                                                 sdr_record,
                                                 sdr_record_len,
                                                 acceptable_record_types)))
    goto cleanup;

  if (device_access_address)
    {
      if (FIID_OBJ_GET (obj_sdr_record,
                        "device_access_address",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record);
          goto cleanup;
        }
      *device_access_address = val;
    }
  if (channel_number)
    {
      if (FIID_OBJ_GET (obj_sdr_record,
                        "channel_number_ls",
                        &val1) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record);
          goto cleanup;
        }
      if (FIID_OBJ_GET (obj_sdr_record,
                        "channel_number_ms",
                        &val2) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record);
          goto cleanup;
        }
      *channel_number = ((uint8_t)val1 << 3) | (uint8_t)val2;
    }
  if (device_slave_address)
    {
      if (FIID_OBJ_GET (obj_sdr_record,
                        "device_slave_address",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record);
          goto cleanup;
        }
      *device_slave_address = val;
    }
  if (private_bus_id)
    {
      if (FIID_OBJ_GET (obj_sdr_record,
                        "private_bus_id",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record);
          goto cleanup;
        }
      *private_bus_id = val;
    }
  if (lun_for_master_write_read_command)
    {
      if (FIID_OBJ_GET (obj_sdr_record,
                        "lun_for_master_write_read_command",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record);
          goto cleanup;
        }
      *lun_for_master_write_read_command = val;
    }
  if (address_span)
    {
      if (FIID_OBJ_GET (obj_sdr_record,
                        "address_span",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record);
          goto cleanup;
        }
      *address_span = val;
    }
  if (oem)
    {
      if (FIID_OBJ_GET (obj_sdr_record,
                        "oem",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record);
          goto cleanup;
        }
      *oem = val;
    }

  rv = 0;
  ctx->errnum = IPMI_SDR_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_sdr_record);
  return (rv);
}

int
ipmi_sdr_parse_fru_device_locator_parameters (ipmi_sdr_ctx_t ctx,
                                              const void *sdr_record,
                                              unsigned int sdr_record_len,
                                              uint8_t *device_access_address,
                                              uint8_t *logical_fru_device_device_slave_address,
                                              uint8_t *private_bus_id,
                                              uint8_t *lun_for_master_write_read_fru_command,
                                              uint8_t *logical_physical_fru_device,
                                              uint8_t *channel_number)
{
  fiid_obj_t obj_sdr_record = NULL;
  uint32_t acceptable_record_types;
  uint64_t val;
  int rv = -1;

  acceptable_record_types = IPMI_SDR_PARSE_RECORD_TYPE_FRU_DEVICE_LOCATOR_RECORD;

  if (!(obj_sdr_record = _sdr_record_get_common (ctx,
                                                 sdr_record,
                                                 sdr_record_len,
                                                 acceptable_record_types)))
    goto cleanup;

  if (device_access_address)
    {
      if (FIID_OBJ_GET (obj_sdr_record,
                        "device_access_address",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record);
          goto cleanup;
        }
      *device_access_address = val;
    }
  if (logical_fru_device_device_slave_address)
    {
      if (FIID_OBJ_GET (obj_sdr_record,
                        "logical_fru_device_device_slave_address",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record);
          goto cleanup;
        }
      *logical_fru_device_device_slave_address = val;
    }
  if (private_bus_id)
    {
      if (FIID_OBJ_GET (obj_sdr_record,
                        "private_bus_id",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record);
          goto cleanup;
        }
      *private_bus_id = val;
    }
  if (lun_for_master_write_read_fru_command)
    {
      if (FIID_OBJ_GET (obj_sdr_record,
                        "lun_for_master_write_read_fru_command",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record);
          goto cleanup;
        }
      *lun_for_master_write_read_fru_command = val;
    }
  if (logical_physical_fru_device)
    {
      if (FIID_OBJ_GET (obj_sdr_record,
                        "logical_physical_fru_device",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record);
          goto cleanup;
        }
      *logical_physical_fru_device = val;
    }
  if (channel_number)
    {
      if (FIID_OBJ_GET (obj_sdr_record,
                        "channel_number",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record);
          goto cleanup;
        }
      *channel_number = val;
    }

  rv = 0;
  ctx->errnum = IPMI_SDR_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_sdr_record);
  return (rv);
}

int
ipmi_sdr_parse_fru_entity_id_and_instance (ipmi_sdr_ctx_t ctx,
                                           const void *sdr_record,
                                           unsigned int sdr_record_len,
                                           uint8_t *fru_entity_id,
                                           uint8_t *fru_entity_instance)
{
  fiid_obj_t obj_sdr_record = NULL;
  uint32_t acceptable_record_types;
  uint64_t val;
  int rv = -1;

  acceptable_record_types = IPMI_SDR_PARSE_RECORD_TYPE_FRU_DEVICE_LOCATOR_RECORD;

  if (!(obj_sdr_record = _sdr_record_get_common (ctx,
                                                 sdr_record,
                                                 sdr_record_len,
                                                 acceptable_record_types)))
    goto cleanup;

  if (fru_entity_id)
    {
      if (FIID_OBJ_GET (obj_sdr_record,
                        "fru_entity_id",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record);
          goto cleanup;
        }
      *fru_entity_id = val;
    }
  if (fru_entity_instance)
    {
      if (FIID_OBJ_GET (obj_sdr_record,
                        "fru_entity_instance",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record);
          goto cleanup;
        }
      *fru_entity_instance = val;
    }

  rv = 0;
  ctx->errnum = IPMI_SDR_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_sdr_record);
  return (rv);
}

int
ipmi_sdr_parse_management_controller_device_locator_parameters (ipmi_sdr_ctx_t ctx,
                                                                const void *sdr_record,
                                                                unsigned int sdr_record_len,
                                                                uint8_t *device_slave_address,
                                                                uint8_t *channel_number,
								uint8_t *global_initialization_event_message_generation,
								uint8_t *global_initialization_log_initialization_agent_errors,
								uint8_t *global_initialization_controller_logs_initialization_agent_errors,
								uint8_t *power_state_notification_controller,
								uint8_t *power_state_notification_acpi_device_power_state_notification,
								uint8_t *power_state_notification_acpi_system_power_state_notification,
								uint8_t *device_capabilities_sensor_device,
								uint8_t *device_capabilities_sdr_repository_device,
								uint8_t *device_capabilities_sel_device,
								uint8_t *device_capabilities_fru_inventory_device,
								uint8_t *device_capabilities_ipmb_event_receiver,
								uint8_t *device_capabilities_ipmb_event_generator,
								uint8_t *device_capabilities_bridge,
								uint8_t *device_capabilities_chassis_device)
{
  fiid_obj_t obj_sdr_record = NULL;
  uint32_t acceptable_record_types;
  uint64_t val;
  int rv = -1;

  acceptable_record_types = IPMI_SDR_PARSE_RECORD_TYPE_MANAGEMENT_CONTROLLER_DEVICE_LOCATOR_RECORD;

  if (!(obj_sdr_record = _sdr_record_get_common (ctx,
                                                 sdr_record,
                                                 sdr_record_len,
                                                 acceptable_record_types)))
    goto cleanup;

  if (device_slave_address)
    {
      if (FIID_OBJ_GET (obj_sdr_record,
                        "device_slave_address",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record);
          goto cleanup;
        }
      *device_slave_address = val;
    }
  if (channel_number)
    {
      if (FIID_OBJ_GET (obj_sdr_record,
                        "channel_number",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record);
          goto cleanup;
        }
      *channel_number = val;
    }
  if (global_initialization_event_message_generation)
    {
      if (FIID_OBJ_GET (obj_sdr_record,
                        "global_initialization.event_message_generation",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record);
          goto cleanup;
        }
      *global_initialization_event_message_generation = val;
    }
  if (global_initialization_log_initialization_agent_errors)
    {
      if (FIID_OBJ_GET (obj_sdr_record,
                        "global_initialization.log_initialization_agent_errors",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record);
          goto cleanup;
        }
      *global_initialization_log_initialization_agent_errors = val;
    }
  if (global_initialization_controller_logs_initialization_agent_errors)
    {
      if (FIID_OBJ_GET (obj_sdr_record,
                        "global_initialization.controller_logs_initialization_agent_errors",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record);
          goto cleanup;
        }
      *global_initialization_controller_logs_initialization_agent_errors = val;
    }
  if (power_state_notification_controller)
    {
      if (FIID_OBJ_GET (obj_sdr_record,
                        "power_state_notification.controller",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record);
          goto cleanup;
        }
      *power_state_notification_controller = val;
    }
  if (power_state_notification_acpi_device_power_state_notification)
    {
      if (FIID_OBJ_GET (obj_sdr_record,
                        "power_state_notification.acpi_device_power_state_notification",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record);
          goto cleanup;
        }
      *power_state_notification_acpi_device_power_state_notification = val;
    }
  if (power_state_notification_acpi_system_power_state_notification)
    {
      if (FIID_OBJ_GET (obj_sdr_record,
                        "power_state_notification.acpi_system_power_state_notification",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record);
          goto cleanup;
        }
      *power_state_notification_acpi_system_power_state_notification = val;
    }
  if (device_capabilities_sensor_device)
    {
      if (FIID_OBJ_GET (obj_sdr_record,
                        "device_capabilities.sensor_device",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record);
          goto cleanup;
        }
      *device_capabilities_sensor_device = val;
    }
  if (device_capabilities_sdr_repository_device)
    {
      if (FIID_OBJ_GET (obj_sdr_record,
                        "device_capabilities.sdr_repository_device",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record);
          goto cleanup;
        }
      *device_capabilities_sdr_repository_device = val;
    }
  if (device_capabilities_sel_device)
    {
      if (FIID_OBJ_GET (obj_sdr_record,
                        "device_capabilities.sel_device",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record);
          goto cleanup;
        }
      *device_capabilities_sel_device = val;
    }
  if (device_capabilities_fru_inventory_device)
    {
      if (FIID_OBJ_GET (obj_sdr_record,
                        "device_capabilities.fru_inventory_device",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record);
          goto cleanup;
        }
      *device_capabilities_fru_inventory_device = val;
    }
  if (device_capabilities_ipmb_event_receiver)
    {
      if (FIID_OBJ_GET (obj_sdr_record,
                        "device_capabilities.ipmb_event_receiver",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record);
          goto cleanup;
        }
      *device_capabilities_ipmb_event_receiver = val;
    }
  if (device_capabilities_ipmb_event_generator)
    {
      if (FIID_OBJ_GET (obj_sdr_record,
                        "device_capabilities.ipmb_event_generator",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record);
          goto cleanup;
        }
      *device_capabilities_ipmb_event_generator = val;
    }
  if (device_capabilities_bridge)
    {
      if (FIID_OBJ_GET (obj_sdr_record,
                        "device_capabilities.bridge",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record);
          goto cleanup;
        }
      *device_capabilities_bridge = val;
    }
  if (device_capabilities_chassis_device)
    {
      if (FIID_OBJ_GET (obj_sdr_record,
                        "device_capabilities.chassis_device",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record);
          goto cleanup;
        }
      *device_capabilities_chassis_device = val;
    }

  rv = 0;
  ctx->errnum = IPMI_SDR_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_sdr_record);
  return (rv);
}

int
ipmi_sdr_parse_manufacturer_id (ipmi_sdr_ctx_t ctx,
                                const void *sdr_record,
                                unsigned int sdr_record_len,
                                uint32_t *manufacturer_id)
{
  fiid_obj_t obj_sdr_record = NULL;
  uint32_t acceptable_record_types;
  uint64_t val;
  int rv = -1;

  acceptable_record_types = IPMI_SDR_PARSE_RECORD_TYPE_MANAGEMENT_CONTROLLER_CONFIRMATION_RECORD;
  acceptable_record_types |= IPMI_SDR_PARSE_RECORD_TYPE_OEM_RECORD;

  if (!(obj_sdr_record = _sdr_record_get_common (ctx,
                                                 sdr_record,
                                                 sdr_record_len,
                                                 acceptable_record_types)))
    goto cleanup;

  if (manufacturer_id)
    {
      if (FIID_OBJ_GET (obj_sdr_record,
                        "manufacturer_id",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record);
          goto cleanup;
        }
      *manufacturer_id = val;
    }

  rv = 0;
  ctx->errnum = IPMI_SDR_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_sdr_record);
  return (rv);
}

int
ipmi_sdr_parse_product_id (ipmi_sdr_ctx_t ctx,
                           const void *sdr_record,
                           unsigned int sdr_record_len,
                           uint16_t *product_id)
{
  fiid_obj_t obj_sdr_record = NULL;
  uint32_t acceptable_record_types;
  uint64_t val;
  int rv = -1;

  acceptable_record_types = IPMI_SDR_PARSE_RECORD_TYPE_MANAGEMENT_CONTROLLER_CONFIRMATION_RECORD;

  if (!(obj_sdr_record = _sdr_record_get_common (ctx,
                                                 sdr_record,
                                                 sdr_record_len,
                                                 acceptable_record_types)))
    goto cleanup;

  if (product_id)
    {
      if (FIID_OBJ_GET (obj_sdr_record,
                        "product_id",
                        &val) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record);
          goto cleanup;
        }
      *product_id = val;
    }

  rv = 0;
  ctx->errnum = IPMI_SDR_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_sdr_record);
  return (rv);
}

int
ipmi_sdr_parse_oem_data (ipmi_sdr_ctx_t ctx,
                         const void *sdr_record,
                         unsigned int sdr_record_len,
                         void *oem_data,
                         unsigned int oem_data_len)
{
  fiid_obj_t obj_sdr_record = NULL;
  uint32_t acceptable_record_types;
  int len = 0;
  int rv = -1;

  acceptable_record_types = IPMI_SDR_PARSE_RECORD_TYPE_OEM_RECORD;

  if (!(obj_sdr_record = _sdr_record_get_common (ctx,
                                                 sdr_record,
                                                 sdr_record_len,
                                                 acceptable_record_types)))
    goto cleanup;

  if (oem_data && oem_data_len)
    {
      if ((len = fiid_obj_get_data (obj_sdr_record,
                                    "oem_data",
                                    oem_data,
                                    oem_data_len)) < 0)
        {
          SDR_FIID_OBJECT_ERROR_TO_SDR_ERRNUM (ctx, obj_sdr_record);
          goto cleanup;
        }
    }

  rv = len;
  ctx->errnum = IPMI_SDR_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_sdr_record);
  return (rv);
}
