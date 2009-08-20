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
#include "ipmi-oem-supermicro.h"

#include "freeipmi-portability.h"
#include "pstdout.h"

#define IPMI_OEM_SUPERMICRO_STRING_MAX 128

int
ipmi_oem_supermicro_extra_firmware_info (ipmi_oem_state_data_t *state_data)
{
  uint8_t bytes_rq[IPMI_OEM_MAX_BYTES];
  uint8_t bytes_rs[IPMI_OEM_MAX_BYTES];
  uint32_t firmware_major_version;
  uint32_t firmware_minor_version;
  uint32_t firmware_sub_version;
  uint32_t firmware_build_number;
  uint8_t firmware_hardware_id;
  char firmware_tag[IPMI_OEM_SUPERMICRO_STRING_MAX+1];
  int rs_len;
  int rv = -1;

  assert (state_data);
  assert (!state_data->prog_data->args->oem_options_count);

  memset (firmware_tag, '\0', IPMI_OEM_SUPERMICRO_STRING_MAX + 1);

  /* Supermicro OEM
   *
   * From post by ipmitool developer.
   *
   * http://sourceforge.net/mailarchive/message.php?msg_name=49ABCCC3.4040004%40cern.ch
   *
   * Request
   *
   * 0x3C - OEM network function
   * 0x20 - OEM cmd
   *
   * Response
   * 0x20 - OEM cmd
   * 0x?? - Completion Code
   * 4 bytes - firmware major version (LSB first)
   * 4 bytes - firmware minor version (LSB first)
   * 4 bytes - firmware sub version (LSB first)
   * 4 bytes - firmware build number (LSB first)
   * 1 byte - hardware ID
   * ? bytes - firmware tag, null terminated string
   */

  bytes_rq[0] = 0x20;

  if ((rs_len = ipmi_cmd_raw (state_data->ipmi_ctx,
                              0, /* lun */
                              0x3C, /* network function */
                              bytes_rq, /* data */
                              1, /* num bytes */
                              bytes_rs,
                              IPMI_OEM_MAX_BYTES)) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_cmd_raw: %s\n",
                       ipmi_ctx_strerror (ipmi_ctx_errnum (state_data->ipmi_ctx)));
      goto cleanup;
    }

  if (ipmi_oem_check_response_and_completion_code (state_data,
                                                   bytes_rs,
                                                   rs_len,
                                                   19,
                                                   0x20,
                                                   0x3C) < 0)
    goto cleanup;

  firmware_major_version = bytes_rs[2];
  firmware_major_version |= (bytes_rs[3] << 8);
  firmware_major_version |= (bytes_rs[4] << 16);
  firmware_major_version |= (bytes_rs[5] << 24);

  firmware_minor_version = bytes_rs[6];
  firmware_minor_version |= (bytes_rs[7] << 8);
  firmware_minor_version |= (bytes_rs[8] << 16);
  firmware_minor_version |= (bytes_rs[9] << 24);

  firmware_sub_version = bytes_rs[10];
  firmware_sub_version |= (bytes_rs[11] << 8);
  firmware_sub_version |= (bytes_rs[12] << 16);
  firmware_sub_version |= (bytes_rs[13] << 24);

  firmware_build_number = bytes_rs[14];
  firmware_build_number |= (bytes_rs[15] << 8);
  firmware_build_number |= (bytes_rs[16] << 16);
  firmware_build_number |= (bytes_rs[17] << 24);

  firmware_hardware_id = bytes_rs[18];
  
  if (rs_len > 19)
    memcpy (firmware_tag, &bytes_rs[19], rs_len - 19);

  /* assume minor version is BCD, just like in Get Device ID command */
  /* assume sub version is also BCD */ 
  pstdout_printf (state_data->pstate,
                  "Firmware Version      : %u.%02x.%02x\n",
                  firmware_major_version,
                  firmware_minor_version,
                  firmware_sub_version);

  pstdout_printf (state_data->pstate,
                  "Firmware Build Number : %u\n",
                  firmware_build_number);

  pstdout_printf (state_data->pstate,
                  "Firmware Hardware ID  : %u\n",
                  firmware_hardware_id);

  pstdout_printf (state_data->pstate,
                  "Firmware Tag          : %s\n",
                  firmware_tag);

  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_oem_supermicro_reset_intrusion (ipmi_oem_state_data_t *state_data)
{
  uint8_t bytes_rq[IPMI_OEM_MAX_BYTES];
  uint8_t bytes_rs[IPMI_OEM_MAX_BYTES];
  int32_t rs_len;
  int rv = -1;

  assert (state_data);
  assert (!state_data->prog_data->args->oem_options_count);

  /* Supermicro OEM
   *
   * 0x30 - OEM network function
   * 0x03 - OEM cmd
   */

  bytes_rq[0] = 0x03;

  if ((rs_len = ipmi_cmd_raw (state_data->ipmi_ctx,
                              0, /* lun */
                              0x30, /* network function */
                              bytes_rq, /* data */
                              1, /* num bytes */
                              bytes_rs,
                              IPMI_OEM_MAX_BYTES)) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_cmd_raw: %s\n",
                       ipmi_ctx_strerror (ipmi_ctx_errnum (state_data->ipmi_ctx)));
      goto cleanup;
    }

  if (ipmi_oem_check_response_and_completion_code (state_data,
                                                   bytes_rs,
                                                   rs_len,
                                                   2,
                                                   0x03,
                                                   0x30) < 0)
    goto cleanup;

  rv = 0;
 cleanup:
  return (rv);
}
