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
#include <config.h>
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#ifdef STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <assert.h>
#include <errno.h>

#include "freeipmi/util/ipmi-channel-util.h"
#include "freeipmi/api/ipmi-device-global-cmds-api.h"
#include "freeipmi/api/ipmi-messaging-support-cmds-api.h"
#include "freeipmi/cmds/ipmi-device-global-cmds.h"
#include "freeipmi/fiid/fiid.h"
#include "freeipmi/spec/ipmi-channel-spec.h"
#include "freeipmi/spec/ipmi-iana-enterprise-numbers-spec.h"

#include "api/ipmi-api-defs.h"
#include "api/ipmi-api-trace.h"
#include "api/ipmi-api-util.h"
#include "libcommon/ipmi-trace.h"

#include "freeipmi-portability.h"

#define IPMI_NUM_CHANNELS 8

static int
_get_channel_numbers (ipmi_ctx_t ctx,
                      uint8_t channel_medium_type,
                      uint8_t channels[IPMI_NUM_CHANNELS],
                      unsigned int *channels_found)
{
  fiid_obj_t obj_cmd_rs = NULL;
  int rv = -1;
  uint64_t val;
  int i;

  assert (ctx);
  assert (ctx->magic == IPMI_CTX_MAGIC);
  assert (channels_found);

  (*channels_found) = 0;

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_get_channel_info_rs)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  /* Channel numbers range from 0 - 7 */
  for (i = 0; i < IPMI_NUM_CHANNELS; i++)
    {
      uint8_t channel_medium_type_read;

      if (ipmi_cmd_get_channel_info (ctx, i, obj_cmd_rs) < 0)
        continue;

      if (FIID_OBJ_GET (obj_cmd_rs,
                        "channel_medium_type",
                        &val) < 0)
        {
          API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rs);
          goto cleanup;
        }
      channel_medium_type_read = val;
      
      if (channel_medium_type_read == channel_medium_type)
        {
          uint8_t actual_channel_number;
          
          if (FIID_OBJ_GET (obj_cmd_rs,
                            "actual_channel_number",
                            &val) < 0)
            {
              API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rs);
              goto cleanup;
            }
          actual_channel_number = val;

          rv = 0;
          channels[(*channels_found)] = actual_channel_number;
          (*channels_found)++;
        }
    }
  
  if (rv < 0)
    API_SET_ERRNUM (ctx, IPMI_ERR_NOT_FOUND);
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

/* returns 1 if special case handled, 0 if not, -1 on error */
static int
_get_channel_number_special (ipmi_ctx_t ctx,
                             uint8_t channel_medium_type,
                             uint8_t *channel_number)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint32_t manufacturer_id;
  uint16_t product_id;
  uint64_t val;
  int rv = -1;

  assert (ctx);
  assert (ctx->magic == IPMI_CTX_MAGIC);

  if (channel_medium_type == IPMI_CHANNEL_MEDIUM_TYPE_LAN_802_3)
    {
      if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_get_device_id_rs)))
        {
          API_ERRNO_TO_API_ERRNUM (ctx, errno);
          goto cleanup;
        }
      
      if (ipmi_cmd_get_device_id (ctx, obj_cmd_rs) < 0)
        goto cleanup;
      
      if (FIID_OBJ_GET (obj_cmd_rs,
                        "manufacturer_id.id",
                        &val) < 0)
        {
          API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rs);
          goto cleanup;
        }
      manufacturer_id = val;
      
      if (FIID_OBJ_GET (obj_cmd_rs,
                        "product_id",
                        &val) < 0)
        {
          API_FIID_OBJECT_ERROR_TO_API_ERRNUM (ctx, obj_cmd_rs);
          goto cleanup;
        }
      product_id = val;
      
      switch (manufacturer_id)
        {
        case IPMI_IANA_ENTERPRISE_ID_INTEL:
        case 0xB000157: /* Intel */
          switch (product_id)
            {
            case 0x1B:
              rv = 1;
              (*channel_number) = 7;
              goto cleanup;
            }
        }
    }

  if (rv < 0)
    rv = 0;

 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

int
ipmi_get_channel_number (ipmi_ctx_t ctx,
                         uint8_t channel_medium_type,
                         uint8_t *channel_number)
{
  uint8_t channels[IPMI_NUM_CHANNELS];
  unsigned int channels_found = 0;
  uint8_t special_channel_number = 0;
  int rv = -1;
  int ret;

  /* XXX channel medium type check? - OEM channels 0-0xFF possible, so skip */
  if (!ctx || ctx->magic != IPMI_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      return (-1);
    }

  if (!channel_number)
    {
      API_SET_ERRNUM (ctx, IPMI_ERR_PARAMETERS);
      return (-1);
    }

  if ((ret = _get_channel_number_special (ctx,
                                          channel_medium_type,
                                          &special_channel_number)) < 0)
    goto cleanup;

  if (ret)
    {
      rv = 0;
      (*channel_number) = special_channel_number;
      goto cleanup;
    }

  if (_get_channel_numbers (ctx,
                            channel_medium_type,
                            channels,
                            &channels_found) < 0)
    goto cleanup;

  if (!channels_found)
    {
      API_SET_ERRNUM (ctx, IPMI_ERR_NOT_FOUND);
      goto cleanup;
    }

  rv = 0;
  (*channel_number) = channels[0];

 cleanup:
  return (rv);
}

int
ipmi_get_channel_numbers (ipmi_ctx_t ctx,
                          uint8_t channel_medium_type,
                          uint8_t *channel_numbers,
                          unsigned int channel_numbers_len)
{ 
  uint8_t channels[IPMI_NUM_CHANNELS];
  unsigned int channels_found = 0;
  uint8_t special_channel_number = 0;
  int rv = -1;
  int ret;

  /* XXX channel medium type check? - OEM channels 0-0xFF possible, so skip */
  if (!ctx || ctx->magic != IPMI_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_ctx_errormsg (ctx), ipmi_ctx_errnum (ctx));
      return (-1);
    }

  if (!channel_numbers
      || !channel_numbers_len)
    {
      API_SET_ERRNUM (ctx, IPMI_ERR_PARAMETERS);
      return (-1);
    }

  if ((ret = _get_channel_number_special (ctx,
                                          channel_medium_type,
                                          &special_channel_number)) < 0)
    goto cleanup;

  if (ret)
    {
      rv = 1;
      channel_numbers[0] = special_channel_number;
      goto cleanup;
    }

  if (_get_channel_numbers (ctx,
                            channel_medium_type,
                            channels,
                            &channels_found) < 0)
    goto cleanup;

  if (!channels_found)
    {
      API_SET_ERRNUM (ctx, IPMI_ERR_NOT_FOUND);
      goto cleanup;
    }

  if (channels_found > channel_numbers_len)
    channels_found = channel_numbers_len;

  rv = (int)channels_found;

  memcpy (channel_numbers,
          channels,
          channels_found);

 cleanup:
  return (rv);
}
