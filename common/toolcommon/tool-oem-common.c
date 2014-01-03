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

#if HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <freeipmi/freeipmi.h>
#include <assert.h>
#include <errno.h>

#include "tool-oem-common.h"

#include "freeipmi-portability.h"
#include "pstdout.h"

int
ipmi_get_oem_data (pstdout_state_t pstate,
                   ipmi_ctx_t ipmi_ctx,
                   struct ipmi_oem_data *oem_data)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  int rv = -1;

  assert (ipmi_ctx);
  assert (oem_data);
  
  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_get_device_id_rs)))
    {
      PSTDOUT_FPRINTF (pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if (ipmi_cmd_get_device_id (ipmi_ctx, obj_cmd_rs) < 0)
    {
      PSTDOUT_FPRINTF (pstate,
                       stderr,
                       "ipmi_cmd_get_device_id: %s\n",
                       ipmi_ctx_errormsg (ipmi_ctx));
      goto cleanup;
    }

  if (FIID_OBJ_GET (obj_cmd_rs, "manufacturer_id.id", &val) < 0)
    {
      PSTDOUT_FPRINTF (pstate,
                       stderr,
                       "fiid_obj_get: 'manufacturer_id.id': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  oem_data->manufacturer_id = val;

  if (FIID_OBJ_GET (obj_cmd_rs, "product_id", &val) < 0)
    {
      PSTDOUT_FPRINTF (pstate,
                       stderr,
                       "fiid_obj_get: 'product_id': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  oem_data->product_id = val;

  if (FIID_OBJ_GET (obj_cmd_rs, "ipmi_version_major", &val) < 0)
    {
      PSTDOUT_FPRINTF (pstate,
                       stderr,
                       "fiid_obj_get: 'ipmi_version_major': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  oem_data->ipmi_version_major = val;

  if (FIID_OBJ_GET (obj_cmd_rs, "ipmi_version_minor", &val) < 0)
    {
      PSTDOUT_FPRINTF (pstate,
                       stderr,
                       "fiid_obj_get: 'ipmi_version_minor': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  oem_data->ipmi_version_minor = val;

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}
