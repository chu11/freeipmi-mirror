/*
  Copyright (C) 2008-2009 FreeIPMI Core Team

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2, or (at your option)
  any later version.

  This program is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA
*/

#if HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <assert.h>

#include <freeipmi/freeipmi.h>

#include "ipmi-oem.h"
#include "ipmi-oem-argp.h"
#include "ipmi-oem-common.h"
#include "ipmi-oem-dell.h"

#include "freeipmi-portability.h"
#include "pstdout.h"

/* 256 b/c length is 8 bit field */
#define IPMI_OEM_DELL_MAX_BYTES 256

static int
_get_dell_system_info (ipmi_oem_state_data_t *state_data,
                       uint8_t parameter_selector,
                       char *string,
                       unsigned int string_len)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint8_t configuration_parameter_data[IPMI_OEM_MAX_BYTES];
  int len;
  int rv = -1;

  assert (state_data);
  assert (string);
  assert (string_len);

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_get_system_info_parameters_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if (ipmi_cmd_get_system_info_parameters (state_data->ipmi_ctx,
                                           IPMI_GET_SYSTEM_INFO_PARAMETER,
                                           parameter_selector,
                                           0,
                                           0,
                                           obj_cmd_rs) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_cmd_get_system_info_parameters: %s\n",
                       ipmi_ctx_errormsg (state_data->ipmi_ctx));
      goto cleanup;
    }

  if ((len = fiid_obj_get_data (obj_cmd_rs,
                                "configuration_parameter_data",
                                configuration_parameter_data,
                                IPMI_OEM_MAX_BYTES)) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get_data: 'configuration_parameter_data': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }

  /*
   * configuration_parameter_data[0] - string length
   * configuration_parameter_data[1-n] - string
   *
   * We ignore the first byte, assume its correct.
   */

  if ((len - 1) > 0)
    {
      if ((len - 1) > string_len)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "internal buffer overflow\n");
          goto cleanup;
        }

      memcpy (string, &(configuration_parameter_data[1]), (len - 1));
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

int
ipmi_oem_dell_get_asset_tag (ipmi_oem_state_data_t *state_data)
{
  char asset_tag[IPMI_OEM_DELL_MAX_BYTES+1];
  int rv = -1;

  assert (state_data);
  assert (!state_data->prog_data->args->oem_options_count);

  /* Dell OEM
   *
   * From http://linux.dell.com/files/openipmi/ipmitool/
   *
   * Uses Get System Info command, OEM parameter number 0xC4.
   *
   * Parameter data response formatted:
   *
   * 1 byte = length
   * ? bytes = string
   */

  memset (asset_tag, '\0', IPMI_OEM_DELL_MAX_BYTES + 1);

  if (_get_dell_system_info (state_data,
                             0xC4,
                             asset_tag,
                             IPMI_OEM_DELL_MAX_BYTES) < 0)
    goto cleanup;
 
  pstdout_printf (state_data->pstate,
                  "Asset Tag: %s\n",
                  asset_tag);
 
  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_oem_dell_get_service_tag (ipmi_oem_state_data_t *state_data)
{
  char service_tag[IPMI_OEM_DELL_MAX_BYTES+1];
  int rv = -1;

  assert (state_data);
  assert (!state_data->prog_data->args->oem_options_count);

  /* Dell OEM
   *
   * From http://linux.dell.com/files/openipmi/ipmitool/
   *
   * Uses Get System Info command, OEM parameter number 0xC5.
   *
   * Parameter data response formatted:
   *
   * 1 byte = length
   * ? bytes = string
   */

  memset (service_tag, '\0', IPMI_OEM_DELL_MAX_BYTES + 1);

  if (_get_dell_system_info (state_data,
                             0xC5,
                             service_tag,
                             IPMI_OEM_DELL_MAX_BYTES) < 0)
    goto cleanup;
 
  pstdout_printf (state_data->pstate,
                  "Service Tag: %s\n",
                  service_tag);
 
  rv = 0;
 cleanup:
  return (rv);
}
