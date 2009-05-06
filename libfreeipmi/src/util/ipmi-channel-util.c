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
#include <config.h>
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>

#include "freeipmi/util/ipmi-channel-util.h"
#include "freeipmi/api/ipmi-device-global-cmds-api.h"
#include "freeipmi/cmds/ipmi-device-global-cmds.h"
#include "freeipmi/fiid/fiid.h"
#include "freeipmi/spec/ipmi-channel-spec.h"

#include "api/ipmi-api-defs.h"
#include "api/ipmi-api-trace.h"
#include "api/ipmi-api-util.h"
#include "libcommon/ipmi-trace.h"

#include "freeipmi-portability.h"

int
ipmi_get_channel_number (ipmi_ctx_t ctx,
                         uint8_t channel_medium_type,
                         uint8_t *channel_number)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint32_t manufacturer_id;
  uint16_t product_id;
  int rv = -1;
  uint64_t val;
  int i;

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
        case IPMI_MANUFACTURER_ID_INTEL:
        case 0xB000157: /* Intel */
          switch (product_id)
            {
            case IPMI_PRODUCT_ID_SE7501WV2:
              rv = 7;
              goto cleanup;
            }
        }

      fiid_obj_destroy (obj_cmd_rs);
    }

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_get_channel_info_rs)))
    {
      API_ERRNO_TO_API_ERRNUM (ctx, errno);
      goto cleanup;
    }

  /* Channel numbers range from 0 - 7 */
  for (i = 0; i < 8; i++)
    {
      uint8_t channel_medium_type_read;

      if (ipmi_cmd_get_channel_info (ctx, i, obj_cmd_rs) != 0)
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
          (*channel_number) = actual_channel_number;
          break;
        }
    }
  
  if (rv < 0)
    API_SET_ERRNUM (ctx, IPMI_ERR_NOT_FOUND);
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}
