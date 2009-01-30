/* 
   Copyright (C) 2003-2009 FreeIPMI Core Team

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software Foundation,
   Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA.  

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
#include "freeipmi/record-format/ipmi-sdr-record-format.h"
#include "freeipmi/spec/ipmi-channel-spec.h"
#include "freeipmi/spec/ipmi-comp-code-spec.h"
#include "freeipmi/spec/ipmi-slave-address-spec.h"
#include "freeipmi/util/ipmi-sensor-and-event-code-tables-util.h"
#include "freeipmi/util/ipmi-sensor-util.h"
#include "freeipmi/util/ipmi-util.h"

#include "ipmi-sensor-read-defs.h"

#include "libcommon/ipmi-err-wrappers.h"
#include "libcommon/ipmi-fiid-wrappers.h"

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
    "sdr cache error",
    "internal IPMI error",
    "internal system error",
    "buffer overflow",
    "internal error",
    "errnum out of range",
    NULL
  };

ipmi_sensor_read_ctx_t
ipmi_sensor_read_ctx_create(ipmi_ctx_t ipmi_ctx)
{
  struct ipmi_sensor_read_ctx *ctx = NULL;
  
  ERR_EINVAL_NULL_RETURN(ipmi_ctx);

  ERR_CLEANUP((ctx = (ipmi_sensor_read_ctx_t)malloc(sizeof(struct ipmi_sensor_read_ctx))));
  memset(ctx, '\0', sizeof(struct ipmi_sensor_read_ctx));
  ctx->magic = IPMI_SENSOR_READ_MAGIC;
  ctx->flags = IPMI_SENSOR_READ_FLAGS_DEFAULT;

  ctx->ipmi_ctx = ipmi_ctx;

  return ctx;

 cleanup:
  if (ctx)
    free(ctx);
  return NULL;
}

void
ipmi_sensor_read_ctx_destroy(ipmi_sensor_read_ctx_t ctx)
{
  if (!ctx || ctx->magic != IPMI_SENSOR_READ_MAGIC)
    return;

  ctx->magic = ~IPMI_SENSOR_READ_MAGIC;
  free(ctx);
}

int 
ipmi_sensor_read_ctx_errnum(ipmi_sensor_read_ctx_t ctx)
{
  if (!ctx)
    return IPMI_SENSOR_READ_CTX_ERR_CONTEXT_NULL;
  else if (ctx->magic != IPMI_SENSOR_READ_MAGIC)
    return IPMI_SENSOR_READ_CTX_ERR_CONTEXT_INVALID;
  else
    return ctx->errnum;
}

char *
ipmi_sensor_read_ctx_strerror(int errnum)
{
  if (errnum >= IPMI_SENSOR_READ_CTX_ERR_SUCCESS && errnum <= IPMI_SENSOR_READ_CTX_ERR_ERRNUMRANGE)
    return ipmi_sensor_read_errmsgs[errnum];
  else
    return ipmi_sensor_read_errmsgs[IPMI_SENSOR_READ_CTX_ERR_ERRNUMRANGE];
}

char *
ipmi_sensor_read_ctx_errormsg(ipmi_sensor_read_ctx_t ctx)
{
  return ipmi_sensor_read_ctx_strerror(ipmi_sensor_read_ctx_errnum(ctx));
}

int
ipmi_sensor_read_ctx_get_flags(ipmi_sensor_read_ctx_t ctx, unsigned int *flags)
{
  ERR(ctx && ctx->magic == IPMI_SENSOR_READ_MAGIC);

  SENSOR_READ_ERR_PARAMETERS(flags);

  *flags = ctx->flags;
  return 0;
}

int
ipmi_sensor_read_ctx_set_flags(ipmi_sensor_read_ctx_t ctx, unsigned int flags)
{
  ERR(ctx && ctx->magic == IPMI_SENSOR_READ_MAGIC);

  SENSOR_READ_ERR_PARAMETERS(!(flags & ~IPMI_SENSOR_READ_FLAGS_MASK));

  ctx->flags = flags;
  return 0;
}

int
_sensor_reading_corner_case_checks (ipmi_sensor_read_ctx_t ctx,
                                    fiid_obj_t obj_get_sensor_reading_rs)
{
  assert(ctx);
  assert(ctx->magic == IPMI_SENSOR_READ_MAGIC);
  assert(obj_get_sensor_reading_rs);

  if (ipmi_check_completion_code(obj_get_sensor_reading_rs,
                                 IPMI_COMP_CODE_NODE_BUSY) == 1)
    {
      SENSOR_READ_ERRNUM_SET(IPMI_SENSOR_READ_CTX_ERR_NODE_BUSY);
      return -1;
    }
  else if ((ipmi_check_completion_code(obj_get_sensor_reading_rs,
                                       IPMI_COMP_CODE_REQUEST_SENSOR_DATA_OR_RECORD_NOT_PRESENT) == 1)
           || (ipmi_check_completion_code(obj_get_sensor_reading_rs,
                                          IPMI_COMP_CODE_COMMAND_ILLEGAL_FOR_SENSOR_OR_RECORD_TYPE) == 1)
           || (ipmi_check_completion_code(obj_get_sensor_reading_rs,
                                          IPMI_COMP_CODE_PARAMETER_OUT_OF_RANGE) == 1)
           || (ipmi_check_completion_code(obj_get_sensor_reading_rs,
                                          IPMI_COMP_CODE_REQUEST_INVALID_DATA_FIELD) == 1))
    {
      /* A sensor listed by the SDR is not present or cannot be obtained for some reason */
      SENSOR_READ_ERRNUM_SET(IPMI_SENSOR_READ_CTX_ERR_SENSOR_READING_CANNOT_BE_OBTAINED);
      return -1;
    }

  return 0;
}

int
_get_sensor_reading (ipmi_sensor_read_ctx_t ctx,
                     uint8_t sensor_number,
                     fiid_obj_t obj_get_sensor_reading_rs)
{
  int rv = -1;

  assert(ctx);
  assert(ctx->magic == IPMI_SENSOR_READ_MAGIC);
  assert(obj_get_sensor_reading_rs);

  if (ipmi_cmd_get_sensor_reading (ctx->ipmi_ctx, 
                                   sensor_number, 
                                   obj_get_sensor_reading_rs) < 0)
    {
      if (_sensor_reading_corner_case_checks(ctx, 
                                             obj_get_sensor_reading_rs) < 0)
        goto cleanup;
      SENSOR_READ_ERRNUM_SET(IPMI_SENSOR_READ_CTX_ERR_IPMI_ERROR);
      goto cleanup;
    }
  
  rv = 0;
 cleanup:
  return rv;
}

int
_get_sensor_reading_ipmb (ipmi_sensor_read_ctx_t ctx,
                          uint8_t slave_address,
                          uint8_t lun,
                          uint8_t channel_number,
                          uint8_t sensor_number,
                          uint8_t record_id,
                          fiid_obj_t obj_get_sensor_reading_rs)
{
  int rv = -1;

  assert(ctx);
  assert(ctx->magic == IPMI_SENSOR_READ_MAGIC);
  assert(obj_get_sensor_reading_rs);

  if (ctx->flags & IPMI_SENSOR_READ_FLAGS_BRIDGE_SENSORS
      && channel_number == IPMI_CHANNEL_NUMBER_PRIMARY_IPMB)
    {
      if (ipmi_cmd_get_sensor_reading_ipmb (ctx->ipmi_ctx,
                                            slave_address,
                                            lun,
                                            sensor_number,
                                            obj_get_sensor_reading_rs) < 0)
        {
          if (ipmi_ctx_errnum (ctx->ipmi_ctx) == IPMI_ERR_COMMAND_INVALID_FOR_SELECTED_INTERFACE)
            {
              SENSOR_READ_ERRNUM_SET(IPMI_SENSOR_READ_CTX_ERR_SENSOR_CANNOT_BE_BRIDGED);
              goto cleanup;
            }
          else if (ipmi_ctx_errnum (ctx->ipmi_ctx) == IPMI_ERR_MESSAGE_TIMEOUT)
            {
              SENSOR_READ_ERRNUM_SET(IPMI_SENSOR_READ_CTX_ERR_NODE_BUSY);
              goto cleanup;
            }
          else if (_sensor_reading_corner_case_checks(ctx, 
                                                      obj_get_sensor_reading_rs) < 0)
            goto cleanup;
          else
            SENSOR_READ_ERRNUM_SET(IPMI_SENSOR_READ_CTX_ERR_IPMI_ERROR);
          goto cleanup;
        }
    }
  else
    {
      SENSOR_READ_ERRNUM_SET(IPMI_SENSOR_READ_CTX_ERR_SENSOR_NOT_OWNED_BY_BMC);
      goto cleanup;
    }

  rv = 0;
 cleanup:
  return rv;
}

/*
 * return 0 - parsed fine
 * return -1 - can't parse info/non-decodable or error
 */
static int
_get_sdr_record_header_info(ipmi_sensor_read_ctx_t ctx,
                            uint8_t *sdr_record,
                            unsigned int sdr_record_len,
                            uint16_t *record_id,
                            uint8_t *record_type)
{
  fiid_obj_t obj_sdr_record_header = NULL;
  int32_t sdr_record_header_len;
  uint64_t val;
  int rv = -1;

  assert(ctx);
  assert(ctx->magic == IPMI_SENSOR_READ_MAGIC);
  assert(sdr_record);
  assert(sdr_record_len);
  assert(record_id);
  assert(record_type);

  SENSOR_READ_FIID_TEMPLATE_LEN_BYTES(sdr_record_header_len, tmpl_sdr_record_header);

  if (sdr_record_len < sdr_record_header_len)
    {
      SENSOR_READ_ERRNUM_SET(IPMI_SENSOR_READ_CTX_ERR_INVALID_SDR_RECORD_TYPE);
      goto cleanup;
    }
  
  SENSOR_READ_FIID_OBJ_CREATE_CLEANUP(obj_sdr_record_header, tmpl_sdr_record_header);
  
  SENSOR_READ_FIID_OBJ_SET_ALL_CLEANUP(obj_sdr_record_header,
                                       sdr_record,
                                       sdr_record_header_len);

  SENSOR_READ_FIID_OBJ_GET_CLEANUP(obj_sdr_record_header, "record_id", &val);
  *record_id = val;
  
  SENSOR_READ_FIID_OBJ_GET_CLEANUP(obj_sdr_record_header, "record_type", &val);
  *record_type = val;
  
  rv = 0;
 cleanup:
  SENSOR_READ_FIID_OBJ_DESTROY(obj_sdr_record_header);
  return rv;
}

static int
_get_sdr_record_sensor_info(ipmi_sensor_read_ctx_t ctx,
                            uint8_t *sdr_record,
                            unsigned int sdr_record_len,
                            uint8_t record_type,
                            uint8_t *sensor_number,
                            uint8_t *event_reading_type_code,
                            uint8_t *sensor_owner_id_type,
                            uint8_t *sensor_owner_id,
                            uint8_t *sensor_owner_lun,
                            uint8_t *channel_number)
{
  fiid_obj_t obj_sdr_record = NULL;
  uint64_t val;
  int rv = -1;

  assert(ctx);
  assert(ctx->magic == IPMI_SENSOR_READ_MAGIC);
  assert(sdr_record);
  assert(record_type == IPMI_SDR_FORMAT_FULL_SENSOR_RECORD
         || record_type == IPMI_SDR_FORMAT_COMPACT_SENSOR_RECORD);
  assert(sensor_number);
  assert(event_reading_type_code);
  assert(sensor_owner_id_type);
  assert(sensor_owner_id);
  assert(sensor_owner_lun);
  assert(channel_number);

  if (record_type == IPMI_SDR_FORMAT_FULL_SENSOR_RECORD)
    SENSOR_READ_FIID_OBJ_CREATE_CLEANUP(obj_sdr_record, tmpl_sdr_full_sensor_record);
  else
    SENSOR_READ_FIID_OBJ_CREATE_CLEANUP(obj_sdr_record, tmpl_sdr_compact_sensor_record);

  SENSOR_READ_FIID_OBJ_SET_ALL_CLEANUP(obj_sdr_record,
                                       sdr_record,
                                       sdr_record_len);
  
  SENSOR_READ_FIID_OBJ_GET_CLEANUP(obj_sdr_record, "sensor_number", &val);
  *sensor_number = val;

  SENSOR_READ_FIID_OBJ_GET_CLEANUP(obj_sdr_record, "event_reading_type_code", &val);
  *event_reading_type_code = val;

  SENSOR_READ_FIID_OBJ_GET_CLEANUP(obj_sdr_record, "sensor_owner_id.type", &val);
  *sensor_owner_id_type = val;

  SENSOR_READ_FIID_OBJ_GET_CLEANUP(obj_sdr_record, "sensor_owner_id", &val);
  *sensor_owner_id = val;

  SENSOR_READ_FIID_OBJ_GET_CLEANUP(obj_sdr_record, "sensor_owner_lun", &val);
  *sensor_owner_lun = val;

  SENSOR_READ_FIID_OBJ_GET_CLEANUP(obj_sdr_record, "channel_number", &val);
  *channel_number = val;

  rv = 0;
 cleanup:
  SENSOR_READ_FIID_OBJ_DESTROY(obj_sdr_record);
  return rv;
}

static int
_get_sdr_senor_decoding_data(ipmi_sensor_read_ctx_t ctx,
                             uint8_t *sdr_record,
                             unsigned int sdr_record_len,
                             uint8_t record_type,
                             int8_t *r_exponent,
                             int8_t *b_exponent,
                             int16_t *m,
                             int16_t *b,
                             uint8_t *linearization,
                             uint8_t *analog_data_format)
{
  fiid_obj_t obj_sdr_record = NULL;
  uint64_t val, val1, val2;
  int rv = -1;

  assert(ctx);
  assert(ctx->magic == IPMI_SENSOR_READ_MAGIC);
  assert(sdr_record);
  assert(record_type == IPMI_SDR_FORMAT_FULL_SENSOR_RECORD);
  assert(r_exponent);
  assert(b_exponent);
  assert(m);
  assert(b);
  assert(linearization);
  assert(analog_data_format);

  SENSOR_READ_FIID_OBJ_CREATE_CLEANUP(obj_sdr_record, tmpl_sdr_full_sensor_record);

  SENSOR_READ_FIID_OBJ_SET_ALL_CLEANUP(obj_sdr_record,
                                       sdr_record,
                                       sdr_record_len);

  SENSOR_READ_FIID_OBJ_GET_CLEANUP (obj_sdr_record, 
                                    "r_exponent", 
                                    &val);
  *r_exponent = (int8_t) val;
  if (*r_exponent & 0x08)
    *r_exponent |= 0xF0;

  SENSOR_READ_FIID_OBJ_GET_CLEANUP (obj_sdr_record, 
                                    "b_exponent", 
                                    &val);
  *b_exponent = (int8_t) val;
  if (*b_exponent & 0x08)
    *b_exponent |= 0xF0;

  SENSOR_READ_FIID_OBJ_GET_CLEANUP (obj_sdr_record, 
                                    "m_ls", 
                                    &val1);
  SENSOR_READ_FIID_OBJ_GET_CLEANUP (obj_sdr_record, 
                                    "m_ms", 
                                    &val2);
  *m = (int16_t)val1;
  *m |= ((val2 & 0x3) << 8);
  if (*m & 0x200)
    *m |= 0xFE00;

  SENSOR_READ_FIID_OBJ_GET_CLEANUP (obj_sdr_record, 
                                    "b_ls", 
                                    &val1);
  SENSOR_READ_FIID_OBJ_GET_CLEANUP (obj_sdr_record, 
                                    "b_ms", 
                                    &val2);
  *b = (int16_t)val1;
  *b |= ((val2 & 0x3) << 8);
  if (*b & 0x200)
    *b |= 0xFE00;

  SENSOR_READ_FIID_OBJ_GET_CLEANUP (obj_sdr_record, 
                                    "linearization", 
                                    &val);
  *linearization = (uint8_t)val;

  SENSOR_READ_FIID_OBJ_GET_CLEANUP (obj_sdr_record, 
                                    "sensor_unit1.analog_data_format", 
                                    &val);
  *analog_data_format = (uint8_t) val;

  rv = 0;
 cleanup:
  SENSOR_READ_FIID_OBJ_DESTROY(obj_sdr_record);
  return rv;
}


int
ipmi_sensor_read(ipmi_sensor_read_ctx_t ctx,
                 uint8_t *sdr_record,
                 unsigned int sdr_record_len,
                 double **sensor_reading,
                 uint16_t *sensor_event_bitmask)
{ 
  double *tmp_sensor_reading = NULL;
  uint64_t val;
  int rv = -1;
  fiid_obj_t obj_get_sensor_reading_rs = NULL;  
  uint64_t sensor_event_bitmask1 = 0;
  uint64_t sensor_event_bitmask2 = 0;
  int8_t sensor_event_bitmask1_len = 0;
  int8_t sensor_event_bitmask2_len = 0;
  uint16_t record_id = 0;
  uint8_t record_type = 0;
  uint8_t sensor_number = 0;
  uint8_t event_reading_type_code = 0;
  uint8_t sensor_owner_id_type = 0;
  uint8_t sensor_owner_id = 0;
  uint8_t sensor_owner_lun = 0;
  uint8_t channel_number = 0;
  uint8_t slave_address = 0;
  int event_reading_type_code_class = 0;

  ERR(ctx && ctx->magic == IPMI_SENSOR_READ_MAGIC);

  SENSOR_READ_ERR_PARAMETERS(sdr_record
                             && sdr_record_len
                             && sensor_reading
                             && sensor_event_bitmask); 

  *sensor_reading = NULL;
  *sensor_event_bitmask = 0;

  if (_get_sdr_record_header_info(ctx,
                                  sdr_record,
                                  sdr_record_len,
                                  &record_id,
                                  &record_type) < 0)
    goto cleanup;

  /* can't get reading for this sdr entry. don't output an error
   * though, since this isn't really an error.  The tool will output
   * something appropriate as it sees fit.
   */
  if (record_type != IPMI_SDR_FORMAT_FULL_SENSOR_RECORD
      && record_type != IPMI_SDR_FORMAT_COMPACT_SENSOR_RECORD)
    {
      SENSOR_READ_ERRNUM_SET(IPMI_SENSOR_READ_CTX_ERR_INVALID_SDR_RECORD_TYPE);
      goto cleanup;
    }
 
  if (_get_sdr_record_sensor_info(ctx,
                                  sdr_record,
                                  sdr_record_len,
                                  record_type,
                                  &sensor_number,
                                  &event_reading_type_code,
                                  &sensor_owner_id_type,
                                  &sensor_owner_id,
                                  &sensor_owner_lun,
                                  &channel_number) < 0)
    goto cleanup;

  if (sensor_owner_id_type == IPMI_SDR_SENSOR_OWNER_ID_TYPE_SYSTEM_SOFTWARE_ID)
    {
      SENSOR_READ_ERRNUM_SET(IPMI_SENSOR_READ_CTX_ERR_SENSOR_IS_SYSTEM_SOFTWARE);
      goto cleanup;
    }

  slave_address = (sensor_owner_id << 1) | sensor_owner_id_type;

  SENSOR_READ_FIID_OBJ_CREATE_CLEANUP(obj_get_sensor_reading_rs, 
                                      tmpl_cmd_get_sensor_reading_rs);

  if (slave_address == IPMI_SLAVE_ADDRESS_BMC)
    {
      if (_get_sensor_reading (ctx,
                               sensor_number,
                               obj_get_sensor_reading_rs) < 0)
        goto cleanup;
    }
  else
    {
      if (_get_sensor_reading_ipmb (ctx,
                                    slave_address,
                                    sensor_owner_lun,
                                    channel_number,
                                    sensor_number,
                                    record_id,
                                    obj_get_sensor_reading_rs) < 0)
        goto cleanup;
    }

  SENSOR_READ_FIID_OBJ_GET_CLEANUP (obj_get_sensor_reading_rs,
                                    "reading_state",
                                    &val);
  
  if (val == IPMI_SENSOR_READING_STATE_UNAVAILABLE)
    {
      SENSOR_READ_ERRNUM_SET(IPMI_SENSOR_READ_CTX_ERR_SENSOR_READING_UNAVAILABLE);
      goto cleanup;
    }

  SENSOR_READ_FIID_OBJ_GET_CLEANUP (obj_get_sensor_reading_rs,
                                    "sensor_scanning",
                                    &val);

  if (val == IPMI_SENSOR_SCANNING_ON_THIS_SENSOR_DISABLE)
    {
      SENSOR_READ_ERRNUM_SET(IPMI_SENSOR_READ_CTX_ERR_SENSOR_SCANNING_DISABLED);
      goto cleanup;
    }

  /* achu:
   * 
   * Note: I don't bother checking the "all_event_messages" flag from
   * the get_sensor_reading response.  If that stuff is turned off,
   * the bitmasks should be zeroed out.
   *
   * Hopefully this doesn't bite me later on.
   */

  SENSOR_READ_FIID_OBJ_GET_WITH_RV_CLEANUP (sensor_event_bitmask1_len,
                                            obj_get_sensor_reading_rs,
                                            "sensor_event_bitmask1",
                                            &sensor_event_bitmask1);
  
  SENSOR_READ_FIID_OBJ_GET_WITH_RV_CLEANUP (sensor_event_bitmask2_len,
                                            obj_get_sensor_reading_rs,
                                            "sensor_event_bitmask2",
                                            &sensor_event_bitmask2);
 
  /* 
   * IPMI Workaround (achu)
   *
   * Discovered on Dell 2950.
   *
   * It seems the sensor_event_bitmask may not be returned by the server
   * at all for some sensors.  Under this situation, there's not
   * much that can be done.  Since there is no sensor_event_bitmask, we
   * just assume that no states have been asserted and the
   * sensor_event_bitmask = 0;
   */
  
  if (!sensor_event_bitmask1_len && !sensor_event_bitmask2_len)
    (*sensor_event_bitmask) = 0;
  else if (sensor_event_bitmask1_len && sensor_event_bitmask2_len)
    (*sensor_event_bitmask) = sensor_event_bitmask1 | (sensor_event_bitmask2 << 8);
  else if (sensor_event_bitmask1_len && !sensor_event_bitmask2_len)
    (*sensor_event_bitmask) = sensor_event_bitmask1;
  else
    {
      SENSOR_READ_ERRNUM_SET(IPMI_SENSOR_READ_CTX_ERR_IPMI_ERROR);
      goto cleanup;
    }
  
  event_reading_type_code_class = ipmi_event_reading_type_code_class (event_reading_type_code);

  if (event_reading_type_code_class == IPMI_EVENT_READING_TYPE_CODE_CLASS_THRESHOLD)
    {
      SENSOR_READ_FIID_OBJ_GET_CLEANUP (obj_get_sensor_reading_rs, 
                                        "sensor_reading", 
                                        &val);

      if (record_type == IPMI_SDR_FORMAT_FULL_SENSOR_RECORD)
        {
          int8_t r_exponent, b_exponent;
          int16_t m, b;
          uint8_t linearization, analog_data_format;

          if (_get_sdr_senor_decoding_data(ctx,
                                           sdr_record,
                                           sdr_record_len,
                                           record_type,
                                           &r_exponent,
                                           &b_exponent,
                                           &m,
                                           &b,
                                           &linearization,
                                           &analog_data_format) < 0)
            goto cleanup;

          /* if the sensor is not analog, this is most likely a bug in the
           * SDR, since we shouldn't be decoding a non-threshold sensor.
           */
          if (!IPMI_SDR_ANALOG_DATA_FORMAT_VALID(analog_data_format))
            {
              SENSOR_READ_ERRNUM_SET(IPMI_SENSOR_READ_CTX_ERR_SENSOR_NON_ANALOG);
              rv = 0;
              goto cleanup;
            }
          
          /* if the sensor is non-linear, I just don't know what to do, 
           * let the tool figure out what to output.
           */
          if (!IPMI_SDR_LINEARIZATION_IS_LINEAR(linearization))
            {
              SENSOR_READ_ERRNUM_SET(IPMI_SENSOR_READ_CTX_ERR_SENSOR_NON_LINEAR);
              rv = 0;
              goto cleanup;
            }
             
          if (!(tmp_sensor_reading = (double *)malloc(sizeof(double))))
            {
              SENSOR_READ_ERRNUM_SET(IPMI_SENSOR_READ_CTX_ERR_OUT_OF_MEMORY);
              goto cleanup;
            }
          
          if (ipmi_sensor_decode_value (r_exponent, 
                                        b_exponent, 
                                        m, 
                                        b, 
                                        linearization, 
                                        analog_data_format, 
                                        (uint8_t) val,
                                        tmp_sensor_reading) < 0)
            {
              SENSOR_READ_ERRNUM_SET(IPMI_SENSOR_READ_CTX_ERR_SYSTEM_ERROR);
              goto cleanup;
            }
          
          *sensor_reading = tmp_sensor_reading;
        }
      rv = 1;
    }
  else if (event_reading_type_code_class == IPMI_EVENT_READING_TYPE_CODE_CLASS_GENERIC_DISCRETE
           || event_reading_type_code_class ==  IPMI_EVENT_READING_TYPE_CODE_CLASS_SENSOR_SPECIFIC_DISCRETE
           || event_reading_type_code_class == IPMI_EVENT_READING_TYPE_CODE_CLASS_OEM)
    /* nothing to do, sensor_event_bitmask already set */
    rv = 1;
  else
    rv = 0;
  
 cleanup:
  SENSOR_READ_FIID_OBJ_DESTROY(obj_get_sensor_reading_rs);
  if (rv <= 0)
    {
      if (tmp_sensor_reading)
        free(tmp_sensor_reading);
    }
  return (rv);
}
