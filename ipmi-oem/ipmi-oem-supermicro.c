/*
 * Copyright (C) 2008-2014 FreeIPMI Core Team
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

  bytes_rq[0] = IPMI_CMD_OEM_SUPERMICRO_EXTRA_FIRMWARE_INFO;

  if ((rs_len = ipmi_cmd_raw (state_data->ipmi_ctx,
                              0, /* lun */
                              IPMI_NET_FN_OEM_SUPERMICRO_PEPPERCON_RQ, /* network function */
                              bytes_rq, /* data */
                              1, /* num bytes */
                              bytes_rs,
                              IPMI_OEM_MAX_BYTES)) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_cmd_raw: %s\n",
                       ipmi_ctx_errormsg (state_data->ipmi_ctx));
      goto cleanup;
    }

  if (ipmi_oem_check_response_and_completion_code (state_data,
                                                   bytes_rs,
                                                   rs_len,
                                                   19,
                                                   IPMI_CMD_OEM_SUPERMICRO_EXTRA_FIRMWARE_INFO,
                                                   IPMI_NET_FN_OEM_SUPERMICRO_PEPPERCON_RS,
                                                   NULL) < 0)
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
  int rs_len;
  int rv = -1;

  assert (state_data);
  assert (!state_data->prog_data->args->oem_options_count);

  /* Supermicro OEM
   *
   * 0x30 - OEM network function
   * 0x03 - OEM cmd
   */

  bytes_rq[0] = IPMI_CMD_OEM_SUPERMICRO_RESET_INTRUSION;

  if ((rs_len = ipmi_cmd_raw (state_data->ipmi_ctx,
                              0, /* lun */
                              IPMI_NET_FN_OEM_SUPERMICRO_GENERIC_RQ, /* network function */
                              bytes_rq, /* data */
                              1, /* num bytes */
                              bytes_rs,
                              IPMI_OEM_MAX_BYTES)) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_cmd_raw: %s\n",
                       ipmi_ctx_errormsg (state_data->ipmi_ctx));
      goto cleanup;
    }

  if (ipmi_oem_check_response_and_completion_code (state_data,
                                                   bytes_rs,
                                                   rs_len,
                                                   2,
                                                   IPMI_CMD_OEM_SUPERMICRO_RESET_INTRUSION,
                                                   IPMI_NET_FN_OEM_SUPERMICRO_GENERIC_RS,
                                                   NULL) < 0)
    goto cleanup;

  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_oem_supermicro_get_bmc_services_status (ipmi_oem_state_data_t *state_data)
{
  uint8_t bytes_rq[IPMI_OEM_MAX_BYTES];
  uint8_t bytes_rs[IPMI_OEM_MAX_BYTES];
  int rs_len;
  int rv = -1;

  assert (state_data);
  assert (!state_data->prog_data->args->oem_options_count);

  /* Supermicro OEM
   *
   * Request 
   *
   * 0x30 - OEM network function
   * 0x70 - OEM cmd
   * 0xF0 - Sub-command
   * 0x?? - action
   *      - 0x00 - disable
   *      - 0x01 - enable
   *      - 0x02 - status
   *
   * Response 
   *
   * 0x70 - OEM cmd
   * 0x?? - Completion Code
   * 0x?? - if action == status
   *      - 0x00 - disabled
   *      - 0x01 - enabled
   */

  bytes_rq[0] = IPMI_CMD_OEM_SUPERMICRO_GENERIC_EXTENSION;
  bytes_rq[1] = IPMI_OEM_SUPERMICRO_SUB_COMMAND_BMC_SERVICES;
  bytes_rq[2] = IPMI_OEM_SUPERMICRO_BMC_SERVICES_ACTION_STATUS;

  if ((rs_len = ipmi_cmd_raw (state_data->ipmi_ctx,
                              0, /* lun */
                              IPMI_NET_FN_OEM_SUPERMICRO_GENERIC_RQ, /* network function */
                              bytes_rq, /* data */
                              3, /* num bytes */
                              bytes_rs,
                              IPMI_OEM_MAX_BYTES)) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_cmd_raw: %s\n",
                       ipmi_ctx_errormsg (state_data->ipmi_ctx));
      goto cleanup;
    }
  
  if (ipmi_oem_check_response_and_completion_code (state_data,
                                                   bytes_rs,
                                                   rs_len,
                                                   3,
                                                   IPMI_CMD_OEM_SUPERMICRO_GENERIC_EXTENSION,
                                                   IPMI_NET_FN_OEM_SUPERMICRO_GENERIC_RS,
                                                   NULL) < 0)
    goto cleanup;

  if (bytes_rs[2] == IPMI_OEM_SUPERMICRO_BMC_SERVICES_STATUS_DISABLED)
    pstdout_printf (state_data->pstate, "disabled\n");
  else if (bytes_rs[2] == IPMI_OEM_SUPERMICRO_BMC_SERVICES_STATUS_ENABLED)
    pstdout_printf (state_data->pstate, "enabled\n");
  else
    pstdout_fprintf (state_data->pstate,
		     stderr,
		     "Unknown Non-IPMI Ports Status\n");
  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_oem_supermicro_set_bmc_services_status (ipmi_oem_state_data_t *state_data)
{
  uint8_t bytes_rq[IPMI_OEM_MAX_BYTES];
  uint8_t bytes_rs[IPMI_OEM_MAX_BYTES];
  int rs_len;
  int rv = -1;

  assert (state_data);
  assert (state_data->prog_data->args->oem_options_count == 1);

  if (strcasecmp (state_data->prog_data->args->oem_options[0], "enable")
      && strcasecmp (state_data->prog_data->args->oem_options[0], "disable"))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "%s:%s invalid OEM option argument '%s'\n",
                       state_data->prog_data->args->oem_id,
                       state_data->prog_data->args->oem_command,
                       state_data->prog_data->args->oem_options[0]);
      goto cleanup;
    }

  /* Supermicro OEM
   *
   * Request 
   *
   * 0x30 - OEM network function
   * 0x70 - OEM cmd
   * 0xF0 - Sub-command
   * 0x?? - action
   *      - 0x00 - disable
   *      - 0x01 - enable
   *      - 0x02 - status
   *
   * Response 
   *
   * 0x70 - OEM cmd
   * 0x?? - Completion Code
   * 0x?? - if action == status
   *      - 0x00 - disabled
   *      - 0x01 - enabled
   */

  bytes_rq[0] = IPMI_CMD_OEM_SUPERMICRO_GENERIC_EXTENSION;
  bytes_rq[1] = IPMI_OEM_SUPERMICRO_SUB_COMMAND_BMC_SERVICES;

  if (!strcasecmp (state_data->prog_data->args->oem_options[0], "enable"))
    bytes_rq[2] = IPMI_OEM_SUPERMICRO_BMC_SERVICES_ACTION_ENABLE;
  else /* !strcasecmp (state_data->prog_data->args->oem_options[0], "disable") */
    bytes_rq[2] = IPMI_OEM_SUPERMICRO_BMC_SERVICES_ACTION_DISABLE;

  if ((rs_len = ipmi_cmd_raw (state_data->ipmi_ctx,
                              0, /* lun */
                              IPMI_NET_FN_OEM_SUPERMICRO_GENERIC_RQ, /* network function */
                              bytes_rq, /* data */
                              3, /* num bytes */
                              bytes_rs,
                              IPMI_OEM_MAX_BYTES)) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_cmd_raw: %s\n",
                       ipmi_ctx_errormsg (state_data->ipmi_ctx));
      goto cleanup;
    }

  if (ipmi_oem_check_response_and_completion_code (state_data,
                                                   bytes_rs,
                                                   rs_len,
                                                   2,
                                                   IPMI_CMD_OEM_SUPERMICRO_GENERIC_EXTENSION,
                                                   IPMI_NET_FN_OEM_SUPERMICRO_GENERIC_RS,
                                                   NULL) < 0)
    goto cleanup;
  
  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_oem_supermicro_get_power_supply_status (ipmi_oem_state_data_t *state_data)
{
  uint8_t bytes_rq[IPMI_OEM_MAX_BYTES];
  uint8_t bytes_rs[IPMI_OEM_MAX_BYTES];
  int rs_len;
  int rv = -1;

  assert (state_data);
  assert (state_data->prog_data->args->oem_options_count == 1);

  if (strcasecmp (state_data->prog_data->args->oem_options[0], "1")
      && strcasecmp (state_data->prog_data->args->oem_options[0], "2")
      && strcasecmp (state_data->prog_data->args->oem_options[0], "3"))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "%s:%s invalid OEM option argument '%s'\n",
                       state_data->prog_data->args->oem_id,
                       state_data->prog_data->args->oem_command,
                       state_data->prog_data->args->oem_options[0]);
      goto cleanup;
    }

  /* Supermicro OEM
   * From Supermicro Engineer 
   *
   * Request 
   *
   * 0x06 - network function
   * 0x52 - cmd (master read/write)
   * 0x07 - (channel = 0, bus id = 3, bus type = private) 
   * 0x?? - slave address
   *      - 0x70 - ps 1 
   *      - 0x72 - ps 2 
   *      - 0x74 - ps 3 
   * 0x01 - read count
   * 0x0c - data to write ... no idea why 0x0c
   *
   * Response 
   *
   * 0x52 - cmd
   * 0x?? - Completion Code
   * 0x?? - 0x01 - good
   *      - 0x00 - bad
   */
  
  bytes_rq[0] = IPMI_CMD_MASTER_WRITE_READ;
  bytes_rq[1] = IPMI_OEM_SUPERMICRO_GET_POWER_SUPPLY_STATUS_CHANNEL;
  if (!strcasecmp (state_data->prog_data->args->oem_options[0], "1"))
    bytes_rq[2] = IPMI_OEM_SUPERMICRO_GET_POWER_SUPPLY_STATUS_PS1;
  else if (!strcasecmp (state_data->prog_data->args->oem_options[0], "2"))
    bytes_rq[2] = IPMI_OEM_SUPERMICRO_GET_POWER_SUPPLY_STATUS_PS2;
  else /* !strcasecmp (state_data->prog_data->args->oem_options[0], "3") */
    bytes_rq[2] = IPMI_OEM_SUPERMICRO_GET_POWER_SUPPLY_STATUS_PS3;
  bytes_rq[3] = 1;
  bytes_rq[4] = IPMI_OEM_SUPERMICRO_GET_POWER_SUPPLY_STATUS_MAGIC;

  if ((rs_len = ipmi_cmd_raw (state_data->ipmi_ctx,
                              0, /* lun */
			      IPMI_NET_FN_APP_RQ,
                              bytes_rq, /* data */
                              5, /* num bytes */
                              bytes_rs,
                              IPMI_OEM_MAX_BYTES)) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_cmd_raw: %s\n",
                       ipmi_ctx_errormsg (state_data->ipmi_ctx));
      goto cleanup;
    }

  if (ipmi_oem_check_response_and_completion_code (state_data,
                                                   bytes_rs,
                                                   rs_len,
						   3,
                                                   IPMI_CMD_MASTER_WRITE_READ,
                                                   IPMI_NET_FN_APP_RQ,
                                                   NULL) < 0)
    goto cleanup;

  if (bytes_rs[2] == IPMI_OEM_SUPERMICRO_GET_POWER_SUPPLY_STATUS_GOOD)
    pstdout_printf (state_data->pstate, "good\n");
  else if (bytes_rs[2] == IPMI_OEM_SUPERMICRO_GET_POWER_SUPPLY_STATUS_BAD)
    pstdout_printf (state_data->pstate, "bad\n");
  else
    pstdout_printf (state_data->pstate, "unknown\n");
  
  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_oem_supermicro_get_power_supply_status2 (ipmi_oem_state_data_t *state_data)
{
  uint8_t bytes_rq[IPMI_OEM_MAX_BYTES];
  uint8_t bytes_rs[IPMI_OEM_MAX_BYTES];
  int rs_len;
  int rv = -1;

  assert (state_data);
  assert (state_data->prog_data->args->oem_options_count == 1);

  if (strcasecmp (state_data->prog_data->args->oem_options[0], "1")
      && strcasecmp (state_data->prog_data->args->oem_options[0], "2"))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "%s:%s invalid OEM option argument '%s'\n",
                       state_data->prog_data->args->oem_id,
                       state_data->prog_data->args->oem_command,
                       state_data->prog_data->args->oem_options[0]);
      goto cleanup;
    }

  /* Supermicro OEM
   * From Supermicro Engineer 
   *
   * Request 
   *
   * 0x06 - network function
   * 0x52 - cmd (master read/write)
   * 0x07 - (channel = 0, bus id = 3, bus type = private) 
   * 0x?? - slave address
   *      - 0xb0 - ps 1 
   *      - 0xb2 - ps 2 
   * 0x02 - read count
   * 0x79 - data to write ... no idea why 0x0c
   *
   * Response 
   *
   * 0x52 - cmd
   * 0x?? - Completion Code
   * 0x?? - 0x00 or 0x02 good, anything else bad
   */
  
  bytes_rq[0] = IPMI_CMD_MASTER_WRITE_READ;
  bytes_rq[1] = IPMI_OEM_SUPERMICRO_GET_POWER_SUPPLY_STATUS2_CHANNEL;
  if (!strcasecmp (state_data->prog_data->args->oem_options[0], "1"))
    bytes_rq[2] = IPMI_OEM_SUPERMICRO_GET_POWER_SUPPLY_STATUS2_PS1;
  else /* (!strcasecmp (state_data->prog_data->args->oem_options[0], "2")) */
    bytes_rq[2] = IPMI_OEM_SUPERMICRO_GET_POWER_SUPPLY_STATUS2_PS2;
  bytes_rq[3] = 1;
  bytes_rq[4] = IPMI_OEM_SUPERMICRO_GET_POWER_SUPPLY_STATUS2_MAGIC;

  if ((rs_len = ipmi_cmd_raw (state_data->ipmi_ctx,
                              0, /* lun */
			      IPMI_NET_FN_APP_RQ,
                              bytes_rq, /* data */
                              5, /* num bytes */
                              bytes_rs,
                              IPMI_OEM_MAX_BYTES)) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_cmd_raw: %s\n",
                       ipmi_ctx_errormsg (state_data->ipmi_ctx));
      goto cleanup;
    }

  if (ipmi_oem_check_response_and_completion_code (state_data,
                                                   bytes_rs,
                                                   rs_len,
						   3,
                                                   IPMI_CMD_MASTER_WRITE_READ,
                                                   IPMI_NET_FN_APP_RQ,
                                                   NULL) < 0)
    goto cleanup;

  if (bytes_rs[2] == IPMI_OEM_SUPERMICRO_GET_POWER_SUPPLY_STATUS2_GOOD1
      || bytes_rs[2] == IPMI_OEM_SUPERMICRO_GET_POWER_SUPPLY_STATUS2_GOOD2)
    pstdout_printf (state_data->pstate, "good\n");
  else 
    pstdout_printf (state_data->pstate, "bad\n");
  
  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_oem_supermicro_get_pmbus_power_supply_status (ipmi_oem_state_data_t *state_data)
{
  uint8_t bytes_rq[IPMI_OEM_MAX_BYTES];
  uint8_t bytes_rs[IPMI_OEM_MAX_BYTES];
  int rs_len;
  int rv = -1;

  assert (state_data);
  assert (state_data->prog_data->args->oem_options_count == 1);

  if (strcasecmp (state_data->prog_data->args->oem_options[0], "1")
      && strcasecmp (state_data->prog_data->args->oem_options[0], "2")
      && strcasecmp (state_data->prog_data->args->oem_options[0], "3"))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "%s:%s invalid OEM option argument '%s'\n",
                       state_data->prog_data->args->oem_id,
                       state_data->prog_data->args->oem_command,
                       state_data->prog_data->args->oem_options[0]);
      goto cleanup;
    }

  /* Supermicro OEM
   * From Supermicro Engineer 
   *
   * Request 
   *
   * 0x06 - network function
   * 0x52 - cmd (master read/write)
   * 0x07 - (channel = 0, bus id = 3, bus type = private) 
   * 0x?? - slave address
   *      - 0x78 - ps 1 
   *      - 0x7a - ps 2 
   *      - 0x7c - ps 3 
   * 0x01 - read count
   * 0x78 - data to write ... no idea why 0x78
   *
   * Response 
   *
   * 0x52 - cmd
   * 0x?? - Completion Code
   * 0x?? - 0x01 - good
   *      - 0x00 - bad
   */
  
  bytes_rq[0] = IPMI_CMD_MASTER_WRITE_READ;
  bytes_rq[1] = IPMI_OEM_SUPERMICRO_GET_PMBUS_POWER_SUPPLY_STATUS_CHANNEL;
  if (!strcasecmp (state_data->prog_data->args->oem_options[0], "1"))
    bytes_rq[2] = IPMI_OEM_SUPERMICRO_GET_PMBUS_POWER_SUPPLY_STATUS_PS1;
  else if (!strcasecmp (state_data->prog_data->args->oem_options[0], "2"))
    bytes_rq[2] = IPMI_OEM_SUPERMICRO_GET_PMBUS_POWER_SUPPLY_STATUS_PS2;
  else /* !strcasecmp (state_data->prog_data->args->oem_options[0], "3") */
    bytes_rq[2] = IPMI_OEM_SUPERMICRO_GET_PMBUS_POWER_SUPPLY_STATUS_PS3;
  bytes_rq[3] = 1;
  bytes_rq[4] = IPMI_OEM_SUPERMICRO_GET_PMBUS_POWER_SUPPLY_STATUS_MAGIC;

  if ((rs_len = ipmi_cmd_raw (state_data->ipmi_ctx,
                              0, /* lun */
			      IPMI_NET_FN_APP_RQ,
                              bytes_rq, /* data */
                              5, /* num bytes */
                              bytes_rs,
                              IPMI_OEM_MAX_BYTES)) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_cmd_raw: %s\n",
                       ipmi_ctx_errormsg (state_data->ipmi_ctx));
      goto cleanup;
    }

  if (ipmi_oem_check_response_and_completion_code (state_data,
                                                   bytes_rs,
                                                   rs_len,
						   3,
                                                   IPMI_CMD_MASTER_WRITE_READ,
                                                   IPMI_NET_FN_APP_RQ,
                                                   NULL) < 0)
    goto cleanup;
  
  if (bytes_rs[2] == IPMI_OEM_SUPERMICRO_GET_PMBUS_POWER_SUPPLY_STATUS_GOOD)
    pstdout_printf (state_data->pstate, "good\n");
  else if (bytes_rs[2] == IPMI_OEM_SUPERMICRO_GET_PMBUS_POWER_SUPPLY_STATUS_BAD)
    pstdout_printf (state_data->pstate, "bad\n");
  else
    pstdout_printf (state_data->pstate, "unknown\n");

  rv = 0;
 cleanup:
  return (rv);
}
