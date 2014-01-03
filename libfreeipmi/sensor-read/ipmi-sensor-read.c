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

#include "freeipmi/sensor-read/ipmi-sensor-read.h"

#include "freeipmi/api/ipmi-sensor-cmds-api.h"
#include "freeipmi/cmds/ipmi-sensor-cmds.h"
#include "freeipmi/debug/ipmi-debug.h"
#include "freeipmi/fiid/fiid.h"
#include "freeipmi/record-format/ipmi-sdr-record-format.h"
#include "freeipmi/spec/ipmi-channel-spec.h"
#include "freeipmi/spec/ipmi-comp-code-spec.h"
#include "freeipmi/spec/ipmi-slave-address-spec.h"
#include "freeipmi/spec/ipmi-sensor-units-spec.h"
#include "freeipmi/util/ipmi-sensor-and-event-code-tables-util.h"
#include "freeipmi/util/ipmi-sensor-util.h"
#include "freeipmi/util/ipmi-util.h"

#include "ipmi-sensor-read-defs.h"
#include "ipmi-sensor-read-trace.h"
#include "ipmi-sensor-read-util.h"

#include "libcommon/ipmi-fiid-util.h"

#include "freeipmi-portability.h"
#include "debug-util.h"

static char *ipmi_sensor_read_errmsgs[] =
  {
    "success",
    "context null",
    "context invalid",
    "invalid parameters",
    "out of memory",
    "sensor reading unavailable",
    "sensor scanning disabled",
    "sensor non-analog",
    "sensor non-linear",
    "sensor not owned by BMC",
    "sensor is system software sensor",
    "sensor cannot be bridged",
    "sensor reading cannot be obtained",
    "node busy",
    "invalid sdr record type",
    "sdr entry error",
    "internal IPMI error",
    "internal system error",
    "buffer overflow",
    "internal error",
    "errnum out of range",
    NULL
  };

ipmi_sensor_read_ctx_t
ipmi_sensor_read_ctx_create (ipmi_ctx_t ipmi_ctx)
{
  struct ipmi_sensor_read_ctx *ctx = NULL;

  if (!ipmi_ctx)
    {
      SET_ERRNO (EINVAL);
      return (NULL);
    }

  /* check that ipmi_ctx is open for use */
  if (ipmi_ctx_get_target (ipmi_ctx, NULL, NULL) < 0)
    {
      SET_ERRNO (EINVAL);
      return (NULL);
    }

  if (!(ctx = (ipmi_sensor_read_ctx_t)malloc (sizeof (struct ipmi_sensor_read_ctx))))
    {
      ERRNO_TRACE (errno);
      return (NULL);
    }
  memset (ctx, '\0', sizeof (struct ipmi_sensor_read_ctx));
  ctx->magic = IPMI_SENSOR_READ_CTX_MAGIC;
  ctx->flags = IPMI_SENSOR_READ_FLAGS_DEFAULT;

  ctx->ipmi_ctx = ipmi_ctx;

  if (!(ctx->sdr_ctx = ipmi_sdr_ctx_create ()))
    {
      ERRNO_TRACE (errno);
      goto cleanup;
    }

  return (ctx);

 cleanup:
  if (ctx)
    {
      ipmi_sdr_ctx_destroy (ctx->sdr_ctx);
      free (ctx);
    }
  return (NULL);
}

void
ipmi_sensor_read_ctx_destroy (ipmi_sensor_read_ctx_t ctx)
{
  if (!ctx || ctx->magic != IPMI_SENSOR_READ_CTX_MAGIC)
    return;

  ctx->magic = ~IPMI_SENSOR_READ_CTX_MAGIC;
  ipmi_sdr_ctx_destroy (ctx->sdr_ctx);
  free (ctx);
}

int
ipmi_sensor_read_ctx_errnum (ipmi_sensor_read_ctx_t ctx)
{
  if (!ctx)
    return (IPMI_SENSOR_READ_ERR_CONTEXT_NULL);
  else if (ctx->magic != IPMI_SENSOR_READ_CTX_MAGIC)
    return (IPMI_SENSOR_READ_ERR_CONTEXT_INVALID);
  else
    return (ctx->errnum);
}

char *
ipmi_sensor_read_ctx_strerror (int errnum)
{
  if (errnum >= IPMI_SENSOR_READ_ERR_SUCCESS && errnum <= IPMI_SENSOR_READ_ERR_ERRNUMRANGE)
    return (ipmi_sensor_read_errmsgs[errnum]);
  else
    return (ipmi_sensor_read_errmsgs[IPMI_SENSOR_READ_ERR_ERRNUMRANGE]);
}

char *
ipmi_sensor_read_ctx_errormsg (ipmi_sensor_read_ctx_t ctx)
{
  return (ipmi_sensor_read_ctx_strerror (ipmi_sensor_read_ctx_errnum (ctx)));
}

int
ipmi_sensor_read_ctx_get_flags (ipmi_sensor_read_ctx_t ctx, unsigned int *flags)
{
  if (!ctx || ctx->magic != IPMI_SENSOR_READ_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_sensor_read_ctx_errormsg (ctx), ipmi_sensor_read_ctx_errnum (ctx));
      return (-1);
    }

  if (!flags)
    {
      SENSOR_READ_SET_ERRNUM (ctx, IPMI_SENSOR_READ_ERR_PARAMETERS);
      return (-1);
    }

  *flags = ctx->flags;
  ctx->errnum = IPMI_SENSOR_READ_ERR_SUCCESS;
  return (0);
}

int
ipmi_sensor_read_ctx_set_flags (ipmi_sensor_read_ctx_t ctx, unsigned int flags)
{
  if (!ctx || ctx->magic != IPMI_SENSOR_READ_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_sensor_read_ctx_errormsg (ctx), ipmi_sensor_read_ctx_errnum (ctx));
      return (-1);
    }

  if (flags & ~IPMI_SENSOR_READ_FLAGS_MASK)
    {
      SENSOR_READ_SET_ERRNUM (ctx, IPMI_SENSOR_READ_ERR_PARAMETERS);
      return (-1);
    }

  ctx->flags = flags;
  ctx->errnum = IPMI_SENSOR_READ_ERR_SUCCESS;
  return (0);
}

int
_sensor_reading_corner_case_checks (ipmi_sensor_read_ctx_t ctx,
                                    fiid_obj_t obj_cmd_rs)
{
  assert (ctx);
  assert (ctx->magic == IPMI_SENSOR_READ_CTX_MAGIC);
  assert (obj_cmd_rs);

  if (ipmi_check_completion_code (obj_cmd_rs,
                                  IPMI_COMP_CODE_NODE_BUSY) == 1)
    {
      SENSOR_READ_SET_ERRNUM (ctx, IPMI_SENSOR_READ_ERR_NODE_BUSY);
      return (-1);
    }
  /* IPMI Workaround
   *
   * achu: Error codes found on motherboards that strongly suggest
   * "sensor not available".  See comments in
   * freeipmi-bugs-issues-and-workarounds.txt for why I think this is
   * a workaround and not an "ok" set of errors.
   */
  else if ((ipmi_check_completion_code (obj_cmd_rs,
                                        IPMI_COMP_CODE_REQUESTED_SENSOR_DATA_OR_RECORD_NOT_PRESENT) == 1)
	   || (ipmi_check_completion_code (obj_cmd_rs,
					   IPMI_COMP_CODE_COMMAND_ILLEGAL_FOR_SENSOR_OR_RECORD_TYPE) == 1)
           || (ipmi_check_completion_code (obj_cmd_rs,
                                           IPMI_COMP_CODE_PARAMETER_OUT_OF_RANGE) == 1)
           || (ipmi_check_completion_code (obj_cmd_rs,
                                           IPMI_COMP_CODE_INVALID_DATA_FIELD_IN_REQUEST) == 1)
	   || (ipmi_check_completion_code (obj_cmd_rs, 
					   IPMI_COMP_CODE_COMMAND_RESPONSE_COULD_NOT_BE_PROVIDED) == 1)
	   || (ipmi_check_completion_code (obj_cmd_rs,
                                           IPMI_COMP_CODE_REQUEST_PARAMETER_NOT_SUPPORTED) == 1)
	   || (ipmi_check_completion_code (obj_cmd_rs,
					   IPMI_COMP_CODE_DESTINATION_UNAVAILABLE) == 1))
    {
      SENSOR_READ_SET_ERRNUM (ctx, IPMI_SENSOR_READ_ERR_SENSOR_READING_UNAVAILABLE);
      return (-1);
    }
  else if ((ipmi_check_completion_code (obj_cmd_rs,
					IPMI_COMP_CODE_UNSPECIFIED_ERROR) == 1))
    {
      uint64_t val; 
      int flag;

      /* IPMI Workaround
       *
       * Discovered on Sun Blade x6250
       *
       * For some reason, some sensors can return this error code
       * (0xFF).  However, it appears to be an invalid error response
       * b/c the sensor properly reports that the sensor reading is
       * not available or that scanning is diabled.  So if the sensor
       * reports that it is unavailable, we'll report an error code
       * slightly more appropriate.
       *
       * Note, I do not handle this identically to the corner case
       * checks above.  Those completion codes strongly suggest that a
       * sensor is not available.  The error code "Unspecified Error"
       * could mean anything.
       */
      
      if ((flag = fiid_obj_get (obj_cmd_rs,
				"reading_state",
				&val)) < 0)
	{
	  SENSOR_READ_FIID_OBJECT_ERROR_TO_SENSOR_READ_ERRNUM (ctx, obj_cmd_rs);
	  return (-1);
	}
      
      if (flag && val == IPMI_SENSOR_READING_STATE_UNAVAILABLE)
	{
	  SENSOR_READ_SET_ERRNUM (ctx, IPMI_SENSOR_READ_ERR_SENSOR_READING_UNAVAILABLE);
	  return (-1);
	}
      
      if (!(ctx->flags & IPMI_SENSOR_READ_FLAGS_IGNORE_SCANNING_DISABLED))
	{
	  if ((flag = fiid_obj_get (obj_cmd_rs,
				    "sensor_scanning",
				    &val)) < 0)
	    {
	      SENSOR_READ_FIID_OBJECT_ERROR_TO_SENSOR_READ_ERRNUM (ctx, obj_cmd_rs);
	      return (-1);
	    }
	  
	  if (flag && val == IPMI_SENSOR_SCANNING_ON_THIS_SENSOR_DISABLE)
	    {
	      SENSOR_READ_SET_ERRNUM (ctx, IPMI_SENSOR_READ_ERR_SENSOR_SCANNING_DISABLED);
	      return (-1);
	    }
	}

      /* else fall through like normal */
    }

  return (0);
}

int
_get_sensor_reading (ipmi_sensor_read_ctx_t ctx,
                     uint8_t sensor_number,
                     fiid_obj_t obj_cmd_rs)
{
  int rv = -1;

  assert (ctx);
  assert (ctx->magic == IPMI_SENSOR_READ_CTX_MAGIC);
  assert (obj_cmd_rs);

  if (ipmi_cmd_get_sensor_reading (ctx->ipmi_ctx,
                                   sensor_number,
                                   obj_cmd_rs) < 0)
    {
      if (_sensor_reading_corner_case_checks (ctx, obj_cmd_rs) < 0)
        goto cleanup;
      SENSOR_READ_SET_ERRNUM (ctx, IPMI_SENSOR_READ_ERR_IPMI_ERROR);
      goto cleanup;
    }

  rv = 0;
 cleanup:
  return (rv);
}

int
_get_sensor_reading_ipmb (ipmi_sensor_read_ctx_t ctx,
                          uint8_t slave_address,
                          uint8_t lun,
                          uint8_t channel_number,
                          uint8_t sensor_number,
                          fiid_obj_t obj_cmd_rs)
{
  int rv = -1;

  assert (ctx);
  assert (ctx->magic == IPMI_SENSOR_READ_CTX_MAGIC);
  assert (obj_cmd_rs);

  if (ctx->flags & IPMI_SENSOR_READ_FLAGS_BRIDGE_SENSORS)
    {
      if (ipmi_cmd_get_sensor_reading_ipmb (ctx->ipmi_ctx,
                                            channel_number,
                                            slave_address,
                                            lun,
                                            sensor_number,
                                            obj_cmd_rs) < 0)
        {
          if (ipmi_ctx_errnum (ctx->ipmi_ctx) == IPMI_ERR_COMMAND_INVALID_FOR_SELECTED_INTERFACE)
            {
              SENSOR_READ_SET_ERRNUM (ctx, IPMI_SENSOR_READ_ERR_SENSOR_CANNOT_BE_BRIDGED);
              goto cleanup;
            }
          else if (ipmi_ctx_errnum (ctx->ipmi_ctx) == IPMI_ERR_MESSAGE_TIMEOUT)
            {
              SENSOR_READ_SET_ERRNUM (ctx, IPMI_SENSOR_READ_ERR_SENSOR_READING_CANNOT_BE_OBTAINED);
              goto cleanup;
            }
          else if (_sensor_reading_corner_case_checks (ctx, obj_cmd_rs) < 0)
            goto cleanup;
          else
            SENSOR_READ_SET_ERRNUM (ctx, IPMI_SENSOR_READ_ERR_IPMI_ERROR);
          goto cleanup;
        }
    }
  else
    {
      SENSOR_READ_SET_ERRNUM (ctx, IPMI_SENSOR_READ_ERR_SENSOR_NOT_OWNED_BY_BMC);
      goto cleanup;
    }

  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_sensor_read (ipmi_sensor_read_ctx_t ctx,
                  const void *sdr_record,
                  unsigned int sdr_record_len,
                  uint8_t shared_sensor_number_offset,
                  uint8_t *sensor_reading_raw,
                  double **sensor_reading,
                  uint16_t *sensor_event_bitmask)
{
  double *tmp_sensor_reading = NULL;
  uint64_t val;
  int rv = -1;
  fiid_obj_t obj_cmd_rs = NULL;
  uint8_t sensor_event_bitmask1 = 0;
  uint8_t sensor_event_bitmask2 = 0;
  int sensor_event_bitmask1_flag = 0;
  int sensor_event_bitmask2_flag = 0;
  uint16_t record_id = 0;
  uint8_t record_type = 0;
  uint8_t sensor_number = 0;
  uint8_t event_reading_type_code = 0;
  uint8_t sensor_owner_id_type = 0;
  uint8_t sensor_owner_id = 0;
  uint8_t sensor_owner_lun = 0;
  uint8_t channel_number = 0;
  uint8_t slave_address = 0;
  uint8_t reading_state, sensor_scanning;
  uint8_t local_sensor_reading_raw;
  unsigned int ctx_flags_orig;
  int event_reading_type_code_class = 0;

  if (!ctx || ctx->magic != IPMI_SENSOR_READ_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_sensor_read_ctx_errormsg (ctx), ipmi_sensor_read_ctx_errnum (ctx));
      return (-1);
    }

  if (!sdr_record
      || !sdr_record_len
      || !sensor_reading
      || !sensor_event_bitmask)
    {
      SENSOR_READ_SET_ERRNUM (ctx, IPMI_SENSOR_READ_ERR_PARAMETERS);
      return (-1);
    }

  *sensor_reading = NULL;
  *sensor_event_bitmask = 0;

  if (ipmi_sdr_parse_record_id_and_type (ctx->sdr_ctx,
                                         sdr_record,
                                         sdr_record_len,
                                         &record_id,
                                         &record_type) < 0)
    {
      SENSOR_READ_SET_ERRNUM (ctx, IPMI_SENSOR_READ_ERR_SDR_ENTRY_ERROR);
      goto cleanup;
    }

  if (record_type != IPMI_SDR_FORMAT_FULL_SENSOR_RECORD
      && record_type != IPMI_SDR_FORMAT_COMPACT_SENSOR_RECORD)
    {
      SENSOR_READ_SET_ERRNUM (ctx, IPMI_SENSOR_READ_ERR_INVALID_SDR_RECORD_TYPE);
      goto cleanup;
    }

  if (ipmi_sdr_parse_sensor_owner_id (ctx->sdr_ctx,
                                      sdr_record,
                                      sdr_record_len,
                                      &sensor_owner_id_type,
                                      &sensor_owner_id) < 0)
    {
      SENSOR_READ_SET_ERRNUM (ctx, IPMI_SENSOR_READ_ERR_SDR_ENTRY_ERROR);
      goto cleanup;
    }

  if (ipmi_sdr_parse_sensor_owner_lun (ctx->sdr_ctx,
                                       sdr_record,
                                       sdr_record_len,
                                       &sensor_owner_lun,
                                       &channel_number) < 0)
    {
      SENSOR_READ_SET_ERRNUM (ctx, IPMI_SENSOR_READ_ERR_SDR_ENTRY_ERROR);
      goto cleanup;
    }

  if (ipmi_sdr_parse_sensor_number (ctx->sdr_ctx,
                                    sdr_record,
                                    sdr_record_len,
                                    &sensor_number) < 0)
    {
      SENSOR_READ_SET_ERRNUM (ctx, IPMI_SENSOR_READ_ERR_SDR_ENTRY_ERROR);
      goto cleanup;
    }

  if (shared_sensor_number_offset)
    {
      uint8_t share_count;

      if (record_type != IPMI_SDR_FORMAT_COMPACT_SENSOR_RECORD)
        {
          SENSOR_READ_SET_ERRNUM (ctx, IPMI_SENSOR_READ_ERR_INVALID_SDR_RECORD_TYPE);
          goto cleanup;
        }

      if (ipmi_sdr_parse_sensor_record_sharing (ctx->sdr_ctx,
                                                sdr_record,
                                                sdr_record_len,
                                                &share_count,
                                                NULL,
                                                NULL,
                                                NULL) < 0)
        {
          SENSOR_READ_SET_ERRNUM (ctx, IPMI_SENSOR_READ_ERR_SDR_ENTRY_ERROR);
          goto cleanup;
        }

      if (share_count <= 1)
        {
          SENSOR_READ_SET_ERRNUM (ctx, IPMI_SENSOR_READ_ERR_INVALID_SDR_RECORD_TYPE);
          goto cleanup;
        }

      if ((sensor_number + share_count) < (sensor_number + shared_sensor_number_offset))
        {
          SENSOR_READ_SET_ERRNUM (ctx, IPMI_SENSOR_READ_ERR_PARAMETERS);
          goto cleanup;
        }

      sensor_number += shared_sensor_number_offset;
    }

  if (ipmi_sdr_parse_event_reading_type_code (ctx->sdr_ctx,
                                              sdr_record,
                                              sdr_record_len,
                                              &event_reading_type_code) < 0)
    {
      SENSOR_READ_SET_ERRNUM (ctx, IPMI_SENSOR_READ_ERR_SDR_ENTRY_ERROR);
      goto cleanup;
    }

  if (sensor_owner_id_type == IPMI_SDR_SENSOR_OWNER_ID_TYPE_SYSTEM_SOFTWARE_ID)
    {
      SENSOR_READ_SET_ERRNUM (ctx, IPMI_SENSOR_READ_ERR_SENSOR_IS_SYSTEM_SOFTWARE);
      goto cleanup;
    }

  slave_address = (sensor_owner_id << 1) | sensor_owner_id_type;

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_get_sensor_reading_rs)))
    {
      SENSOR_READ_ERRNO_TO_SENSOR_READ_ERRNUM (ctx, errno);
      goto cleanup;
    }

  /* 
   * IPMI Workaround (achu)
   *
   * See comments below concerning sensor_event_bitmask.
   */

  if (ipmi_ctx_get_flags (ctx->ipmi_ctx, &ctx_flags_orig) < 0)
    {
      SENSOR_READ_SET_ERRNUM (ctx, IPMI_SENSOR_READ_ERR_INTERNAL_ERROR);
      goto cleanup;
    }

  if (ipmi_ctx_set_flags (ctx->ipmi_ctx, ctx_flags_orig | IPMI_FLAGS_NO_VALID_CHECK) < 0)
    {
      SENSOR_READ_SET_ERRNUM (ctx, IPMI_SENSOR_READ_ERR_INTERNAL_ERROR);
      goto cleanup;
    }

  /* IPMI Workaround
   *
   * Discovered on Fujitsu RX300
   * Discovered on Fujitsu RX300S2
   * 
   * On some motherboards, the sensor owner is invalid.  The sensor
   * owner as atually the BMC.
   */
  if (!(ctx->flags & IPMI_SENSOR_READ_FLAGS_ASSUME_BMC_OWNER))
    {
      if (slave_address == IPMI_SLAVE_ADDRESS_BMC)
	{
	  if (_get_sensor_reading (ctx,
				   sensor_number,
				   obj_cmd_rs) < 0)
	    goto cleanup;
	}
      else
	{
	  if (_get_sensor_reading_ipmb (ctx,
					slave_address,
					sensor_owner_lun,
					channel_number,
					sensor_number,
					obj_cmd_rs) < 0)
	    goto cleanup;
	}
    }
  else
    {
      if (_get_sensor_reading (ctx,
			       sensor_number,
			       obj_cmd_rs) < 0)
	goto cleanup;
    }

  /* 
   * IPMI Workaround (achu)
   *
   * See comments below concerning sensor_event_bitmask.
   */

  if (ipmi_ctx_set_flags (ctx->ipmi_ctx, ctx_flags_orig) < 0)
    {
      SENSOR_READ_SET_ERRNUM (ctx, IPMI_SENSOR_READ_ERR_INTERNAL_ERROR);
      goto cleanup;
    }

  if (FIID_OBJ_GET (obj_cmd_rs,
                    "reading_state",
                    &val) < 0)
    {
      SENSOR_READ_FIID_OBJECT_ERROR_TO_SENSOR_READ_ERRNUM (ctx, obj_cmd_rs);
      goto cleanup;
    }
  reading_state = val;

  if (reading_state == IPMI_SENSOR_READING_STATE_UNAVAILABLE)
    {
      SENSOR_READ_SET_ERRNUM (ctx, IPMI_SENSOR_READ_ERR_SENSOR_READING_UNAVAILABLE);
      goto cleanup;
    }

  /* IPMI Workaround
   *
   * Discovered on Dell Poweredge 2900
   * Discovered on Dell Poweredge 2950
   * Discovered on Dell Poweredge R410
   * Discovered on Dell Poweredge R610
   * 
   * On some motherboards, the sensor scanning bit is invalid for sensors. 
   */
  if (!(ctx->flags & IPMI_SENSOR_READ_FLAGS_IGNORE_SCANNING_DISABLED))
    {
      if (FIID_OBJ_GET (obj_cmd_rs,
			"sensor_scanning",
			&val) < 0)
	{
	  SENSOR_READ_FIID_OBJECT_ERROR_TO_SENSOR_READ_ERRNUM (ctx, obj_cmd_rs);
	  goto cleanup;
	}
      sensor_scanning = val;
      
      if (sensor_scanning == IPMI_SENSOR_SCANNING_ON_THIS_SENSOR_DISABLE)
	{
	  SENSOR_READ_SET_ERRNUM (ctx, IPMI_SENSOR_READ_ERR_SENSOR_SCANNING_DISABLED);
	  goto cleanup;
	}
    }

  /* achu:
   *
   * Note: I don't bother checking the "all_event_messages" flag from
   * the get_sensor_reading response.  If that stuff is turned off,
   * the bitmasks should be zeroed out.
   *
   * Hopefully this doesn't bite me later on.
   *
   * Call the normal fiid_obj_get instead of the wrapper, if the field
   * isn't set, we want to know and not error out.
   */

  if ((sensor_event_bitmask1_flag = fiid_obj_get (obj_cmd_rs,
                                                  "sensor_event_bitmask1",
                                                  &val)) < 0)
    {
      SENSOR_READ_FIID_OBJECT_ERROR_TO_SENSOR_READ_ERRNUM (ctx, obj_cmd_rs);
      goto cleanup;
    }
  sensor_event_bitmask1 = val;

  if ((sensor_event_bitmask2_flag = fiid_obj_get (obj_cmd_rs,
                                                  "sensor_event_bitmask2",
                                                  &val)) < 0)
    {
      SENSOR_READ_FIID_OBJECT_ERROR_TO_SENSOR_READ_ERRNUM (ctx, obj_cmd_rs);
      goto cleanup;
    }
  sensor_event_bitmask2 = val;

  /*
   * IPMI Workaround (achu)
   *
   * Discovered on Dell 2950.
   *
   * It seems the sensor_event_bitmask (16 bits) may not be returned
   * by the server at all for some sensors, despite a minimum of 8
   * bits being required.  Under this situation, there's not much that
   * can be done.  Since there is no sensor_event_bitmask, we just
   * assume that no states have been asserted and the
   * sensor_event_bitmask = 0;
   */

  if (!sensor_event_bitmask1_flag && !sensor_event_bitmask2_flag)
    (*sensor_event_bitmask) = 0;
  else if (sensor_event_bitmask1_flag && sensor_event_bitmask2_flag)
    (*sensor_event_bitmask) = sensor_event_bitmask1 | (sensor_event_bitmask2 << 8);
  else if (sensor_event_bitmask1_flag && !sensor_event_bitmask2_flag)
    (*sensor_event_bitmask) = sensor_event_bitmask1;
  else
    {
      SENSOR_READ_SET_ERRNUM (ctx, IPMI_SENSOR_READ_ERR_IPMI_ERROR);
      goto cleanup;
    }

  if (FIID_OBJ_GET (obj_cmd_rs,
                    "sensor_reading",
                    &val) < 0)
    {
      SENSOR_READ_FIID_OBJECT_ERROR_TO_SENSOR_READ_ERRNUM (ctx, obj_cmd_rs);
      goto cleanup;
    }
  local_sensor_reading_raw = val;
  
  if (sensor_reading_raw)
    (*sensor_reading_raw) = local_sensor_reading_raw;

  event_reading_type_code_class = ipmi_event_reading_type_code_class (event_reading_type_code);

  if (event_reading_type_code_class == IPMI_EVENT_READING_TYPE_CODE_CLASS_THRESHOLD)
    {
      if (record_type == IPMI_SDR_FORMAT_FULL_SENSOR_RECORD)
        {
          int8_t r_exponent, b_exponent;
          int16_t m, b;
          uint8_t linearization, analog_data_format;

          if (ipmi_sdr_parse_sensor_decoding_data (ctx->sdr_ctx,
                                                   sdr_record,
                                                   sdr_record_len,
                                                   &r_exponent,
                                                   &b_exponent,
                                                   &m,
                                                   &b,
                                                   &linearization,
                                                   &analog_data_format) < 0)
            {
              SENSOR_READ_SET_ERRNUM (ctx, IPMI_SENSOR_READ_ERR_SDR_ENTRY_ERROR);
              goto cleanup;
            }

          /* if the sensor is not analog, this is most likely a bug in the
           * SDR, since we shouldn't be decoding a non-threshold sensor.
           */
          if (!IPMI_SDR_ANALOG_DATA_FORMAT_VALID (analog_data_format))
            {
              SENSOR_READ_SET_ERRNUM (ctx, IPMI_SENSOR_READ_ERR_SENSOR_NON_ANALOG);
              rv = 0;
              goto cleanup;
            }

          /* if the sensor is non-linear, I just don't know what to do,
           * let the tool figure out what to output.
           */
          if (!IPMI_SDR_LINEARIZATION_IS_LINEAR (linearization))
            {
              SENSOR_READ_SET_ERRNUM (ctx, IPMI_SENSOR_READ_ERR_SENSOR_NON_LINEAR);
              rv = 0;
              goto cleanup;
            }

          if (!(tmp_sensor_reading = (double *)malloc (sizeof (double))))
            {
              SENSOR_READ_SET_ERRNUM (ctx, IPMI_SENSOR_READ_ERR_OUT_OF_MEMORY);
              goto cleanup;
            }

          if (ipmi_sensor_decode_value (r_exponent,
                                        b_exponent,
                                        m,
                                        b,
                                        linearization,
                                        analog_data_format,
                                        local_sensor_reading_raw,
                                        tmp_sensor_reading) < 0)
            {
              SENSOR_READ_SET_ERRNUM (ctx, IPMI_SENSOR_READ_ERR_INTERNAL_ERROR);
              goto cleanup;
            }

          *sensor_reading = tmp_sensor_reading;
        }
      rv = 1;
    }
  else if (event_reading_type_code_class == IPMI_EVENT_READING_TYPE_CODE_CLASS_GENERIC_DISCRETE
           || event_reading_type_code_class ==  IPMI_EVENT_READING_TYPE_CODE_CLASS_SENSOR_SPECIFIC_DISCRETE)
    {
      /*
       * IPMI Workaround (achu)
       *
       * Discovered on HP Proliant DL380 G7
       * Discovered on HP ProLiant ML310 G5
       *
       * SDR records for some sensors give conflicting information.  A
       * threshold based sensor lists an event/reading type code for a
       * discrete sensor.  The analog data format indicates an
       * analog/threshold based sensor, however no threshold limits
       * for the sensor are listed in the SDR.
       *
       * To deal with this, if the analog data format and units
       * strongly suggest that a reading should occur with this
       * sensor, get the reading and return it.
       *
       * Note that this can only occur for full records, since
       * decoding data does not exist in compact records.
       */
      if (ctx->flags & IPMI_SENSOR_READ_FLAGS_DISCRETE_READING
	  && record_type == IPMI_SDR_FORMAT_FULL_SENSOR_RECORD)
	{
          int8_t r_exponent, b_exponent;
          int16_t m, b;
          uint8_t linearization, analog_data_format;
	  uint8_t sensor_units_percentage;
	  uint8_t sensor_units_modifier;
	  uint8_t sensor_units_rate;
	  uint8_t sensor_base_unit_type;
	  uint8_t sensor_modifier_unit_type;

          if (ipmi_sdr_parse_sensor_decoding_data (ctx->sdr_ctx,
                                                   sdr_record,
                                                   sdr_record_len,
                                                   &r_exponent,
                                                   &b_exponent,
                                                   &m,
                                                   &b,
                                                   &linearization,
                                                   &analog_data_format) < 0)
            {
              SENSOR_READ_SET_ERRNUM (ctx, IPMI_SENSOR_READ_ERR_SDR_ENTRY_ERROR);
              goto cleanup;
            }

	  if (ipmi_sdr_parse_sensor_units (ctx->sdr_ctx,
					   sdr_record,
					   sdr_record_len,
					   &sensor_units_percentage,
					   &sensor_units_modifier,
					   &sensor_units_rate,
					   &sensor_base_unit_type,
					   &sensor_modifier_unit_type) < 0)
	    {
              SENSOR_READ_SET_ERRNUM (ctx, IPMI_SENSOR_READ_ERR_SDR_ENTRY_ERROR);
	      goto cleanup;
	    }

          /* if the sensor is not analog, this is normal expected
	   * case, fallthrough to normal expectations
           */
          if (!IPMI_SDR_ANALOG_DATA_FORMAT_VALID (analog_data_format))
            {
	      rv = 1;
	      goto cleanup;
            }

	  /* if the sensor units are not specified, this is the normal expected
	   * case, fallthrough to normal expectations
	   */
	  if (sensor_units_percentage != IPMI_SDR_PERCENTAGE_YES
	      && sensor_base_unit_type == IPMI_SENSOR_UNIT_UNSPECIFIED)
	    {
	      rv = 1;
	      goto cleanup;
	    }

          /* if the sensor is non-linear, I just don't know what to do,
           * let the tool figure out what to output.
           */
          if (!IPMI_SDR_LINEARIZATION_IS_LINEAR (linearization))
            {
              SENSOR_READ_SET_ERRNUM (ctx, IPMI_SENSOR_READ_ERR_SENSOR_NON_LINEAR);
              rv = 0;
              goto cleanup;
            }

          if (!(tmp_sensor_reading = (double *)malloc (sizeof (double))))
            {
              SENSOR_READ_SET_ERRNUM (ctx, IPMI_SENSOR_READ_ERR_OUT_OF_MEMORY);
              goto cleanup;
            }

          if (ipmi_sensor_decode_value (r_exponent,
                                        b_exponent,
                                        m,
                                        b,
                                        linearization,
                                        analog_data_format,
                                        local_sensor_reading_raw,
                                        tmp_sensor_reading) < 0)
            {
              SENSOR_READ_SET_ERRNUM (ctx, IPMI_SENSOR_READ_ERR_INTERNAL_ERROR);
              goto cleanup;
            }

          *sensor_reading = tmp_sensor_reading;
	}
      rv = 1;
    }
  else if (event_reading_type_code_class == IPMI_EVENT_READING_TYPE_CODE_CLASS_OEM)
    /* nothing to do, sensor_event_bitmask already set */
    rv = 1;
  else
    rv = 0;

 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  if (rv <= 0)
    free (tmp_sensor_reading);
  return (rv);
}
