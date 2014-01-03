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
#if HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */
#include <limits.h>
#include <assert.h>

#include <freeipmi/freeipmi.h>

#include "ipmi-oem.h"
#include "ipmi-oem-argp.h"
#include "ipmi-oem-common.h"
#include "ipmi-oem-quanta.h"
#include "ipmi-oem-thirdparty.h"

#include "freeipmi-portability.h"
#include "pstdout.h"

/* Quanta S99Q/Dell FS12-TY Notes
 *
 * Update Firmware Request
 *
 * 0x08 - network function
 * 0x01 - OEM cmd
 * 0x?? - Interface
 *      - 00h - system interface (e.g. kcs)
 *      - 01h - networking (e.g. tftp, ftp, http)
 *      - 02h - usb msc, legacy
 *      - 03h - USB init command
 * 0x?? - update type
 *      - [7] - force update
 *            - 0h - normal
 *            - 1h - forced
 *      - [6:3] - reserved
 *      - [2:0] - entity
 * bytes 4:15 - install options (vendor)
 *
 * Update Firmware Response
 *
 * 0x08 - network function
 * 0x01 - command
 * 0x?? - completion code
 * 0x?? - task ID
 *
 * Get Update Status Request
 *
 * 0x08 - network function
 * 0x02 - OEM cmd
 * 0x?? - task ID
 * 
 * Get Update Status Response
 *      
 * 0x08 - network function
 * 0x02 - OEM cmd
 * 0x?? - completion code
 * 0x?? - status
 *      - 00h - Transmitting Image
 *      - 01h - Validating Image
 *      - 02h - Programming
 *      - 03h - Ready to Accept Image
 *      - 04h - USB Init stage
 *      - 05h - Connecting to server
 *      - 80h - General error
 *      - 81h - Cannot establish connection
 *      - 82h - Path not found
 *      - 83h - Transmission Abort
 *      - 84h - Checksum error
 *      - 85h - Incorrect Platform
 *      - 86h - Allocate memory failed
 *      - 87h - Virtual media detach
 *      - FFh - Completed
 * 0x?? - progression indicator (optional)
 *
 * Copy Image Data Request
 *
 * 0x08 - network function
 * 0x03 - OEM cmd
 * 0x?? - In Progress
 *      - 00h - data transmission is in progress
 *      - 01h - data transmission completed
 * bytes 3:6 - image offset to be copied
 * bytes 7:N - image data to be copied
 *
 * Copy Image Data Response
 *
 * 0x03 - OEM cmd
 * 0x?? - Completion Code
 *
 * USB Firmware Update Request
 *
 * 0x08 - network function
 * 0x04 - OEM cmd
 * 0x?? - Task ID
 * 0x?? - Execution phases
 *      - 00h - phase one, emulate virtual disk
 *      - 01h - phase two, un-mount virtual disk and then start firmware update
 *
 * USB Firmware Update Response
 *
 * 0x04 - OEM cmd
 * 0x?? - Completion Code
 *
 * Get Sensor Temperature Reading Request
 *
 * 0x30 - network function
 * 0x10 - OEM cmd
 * 0x?? - sensor number
 *
 * Get Sensor Temperature Reading Response
 *
 * 0x10 - OEM cmd
 * 0x?? - Completion Code
 * 0x?? - sensor temperature reading
 *
 * Set Processor Information Request
 *
 * 0x30 - network function
 * 0x17 - OEM cmd
 * 0x?? - Processor Index, 1 based
 * 0x?? - Processor Type
 * bytes 4-5 - Processor frequency in MHZ (LSB first)
 *
 * Set Processor Information Response
 *
 * 0x17 - OEM cmd
 * 0x?? - Completion Code
 */

#define IPMI_OEM_QUANTA_MAC_ADDRESS_BYTES            6

#define IPMI_OEM_QUANTA_MAC_ADDRESS_BUS_ID           2
#define IPMI_OEM_QUANTA_MAC_ADDRESS_CHANNEL          0
#define IPMI_OEM_QUANTA_MAC_ADDRESS_SLAVE_ADDRESS    0x55
#define IPMI_OEM_QUANTA_MAC_ADDRESS_BASE_OFFSET      0x0000
#define IPMI_OEM_QUANTA_MAC_ADDRESS_DEDICATED_OFFSET 0x0000
#define IPMI_OEM_QUANTA_MAC_ADDRESS_SHARED_OFFSET    0x0006

int
ipmi_oem_quanta_get_nic_mode (ipmi_oem_state_data_t *state_data)
{
  assert (state_data);
  assert (!state_data->prog_data->args->oem_options_count);

  return (ipmi_oem_thirdparty_get_nic_mode (state_data));
}

int
ipmi_oem_quanta_set_nic_mode (ipmi_oem_state_data_t *state_data)
{
  assert (state_data);
  assert (state_data->prog_data->args->oem_options_count == 1);

  return (ipmi_oem_thirdparty_set_nic_mode (state_data));
}

int
ipmi_oem_quanta_get_bmc_services (ipmi_oem_state_data_t *state_data)
{
  assert (state_data);
  assert (!state_data->prog_data->args->oem_options_count);

  return (ipmi_oem_thirdparty_get_bmc_services_v1 (state_data));
}

int
ipmi_oem_quanta_set_bmc_services (ipmi_oem_state_data_t *state_data)
{
  assert (state_data);
  assert (state_data->prog_data->args->oem_options_count == 2);
  
  return (ipmi_oem_thirdparty_set_bmc_services_v1 (state_data));
}

int
ipmi_oem_quanta_get_account_status (ipmi_oem_state_data_t *state_data)
{
  assert (state_data);
  assert (!state_data->prog_data->args->oem_options_count);

  return (ipmi_oem_thirdparty_get_account_status (state_data));
}

int
ipmi_oem_quanta_get_dns_config (ipmi_oem_state_data_t *state_data)
{
  assert (state_data);
  assert (!state_data->prog_data->args->oem_options_count);

  return (ipmi_oem_thirdparty_get_dns_config_v1 (state_data));
}

int
ipmi_oem_quanta_set_dns_config (ipmi_oem_state_data_t *state_data)
{
  assert (state_data);

  return (ipmi_oem_thirdparty_set_dns_config_v1 (state_data));
}

int
ipmi_oem_quanta_get_web_server_config (ipmi_oem_state_data_t *state_data)
{
  assert (state_data);
  assert (!state_data->prog_data->args->oem_options_count);

  return (ipmi_oem_thirdparty_get_web_server_config_v1 (state_data));
}

int
ipmi_oem_quanta_set_web_server_config (ipmi_oem_state_data_t *state_data)
{
  assert (state_data);

  return (ipmi_oem_thirdparty_set_web_server_config_v1 (state_data));
}

int
ipmi_oem_quanta_get_power_management_config (ipmi_oem_state_data_t *state_data)
{
  assert (state_data);
  assert (!state_data->prog_data->args->oem_options_count);

  return (ipmi_oem_thirdparty_get_power_management_config_v1 (state_data));
}

int
ipmi_oem_quanta_set_power_management_config (ipmi_oem_state_data_t *state_data)
{
  assert (state_data);

  return (ipmi_oem_thirdparty_set_power_management_config_v1 (state_data));
}

int
ipmi_oem_quanta_get_sol_idle_timeout (ipmi_oem_state_data_t *state_data)
{
  assert (state_data);
  assert (!state_data->prog_data->args->oem_options_count);

  return (ipmi_oem_thirdparty_get_sol_idle_timeout (state_data));
}

int
ipmi_oem_quanta_set_sol_idle_timeout (ipmi_oem_state_data_t *state_data)
{
  assert (state_data);
  assert (state_data->prog_data->args->oem_options_count == 1);

  return (ipmi_oem_thirdparty_set_sol_idle_timeout (state_data));
}

int
ipmi_oem_quanta_get_telnet_ssh_redirect_status (ipmi_oem_state_data_t *state_data)
{
  assert (state_data);
  assert (!state_data->prog_data->args->oem_options_count);

  return (ipmi_oem_thirdparty_get_telnet_ssh_redirect_status (state_data));
}

int
ipmi_oem_quanta_set_telnet_ssh_redirect_status (ipmi_oem_state_data_t *state_data)
{
  assert (state_data);
  assert (state_data->prog_data->args->oem_options_count == 1);

  return (ipmi_oem_thirdparty_set_telnet_ssh_redirect_status (state_data));
}

int
ipmi_oem_quanta_reset_to_defaults (ipmi_oem_state_data_t *state_data)
{
  uint8_t bytes_rq[IPMI_OEM_MAX_BYTES];
  uint8_t bytes_rs[IPMI_OEM_MAX_BYTES];
  uint8_t task_id;
  int rs_len;
  int rv = -1;

  assert (state_data);
  assert (state_data->prog_data->args->oem_options_count == 1);

  if (strcasecmp (state_data->prog_data->args->oem_options[0], "all")
      && strcasecmp (state_data->prog_data->args->oem_options[0], "user")
      && strcasecmp (state_data->prog_data->args->oem_options[0], "lan")
      && strcasecmp (state_data->prog_data->args->oem_options[0], "sol")
      && strcasecmp (state_data->prog_data->args->oem_options[0], "serial")
      && strcasecmp (state_data->prog_data->args->oem_options[0], "pef"))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "%s:%s invalid OEM option argument '%s'\n",
                       state_data->prog_data->args->oem_id,
                       state_data->prog_data->args->oem_command,
                       state_data->prog_data->args->oem_options[0]);
      goto cleanup;
    }

  /* Quanta S99Q/Dell FS12-TY OEM
   *
   * Request Reset To Defaults
   *
   * 0x30 - OEM network function
   * 0x04 - OEM cmd
   * 0x?? - bitmask
   *      [7:5] = 111b = restore parameters not included below
   *            = 000b = remaining parameters stay what it is
   *      [4] = 1b = restore PEFs to default
   *      [3] = 1b = restore serial configuration parameters to default
   *      [2] = 1b = restore SOL configuration parameters to default
   *      [1] = 1b = restore LAN configuration parameters to default
   *      [0] = 1b = restore user accounts to default
   *
   * Response Reset To Defaults
   *
   * 0x04 - OEM cmd
   * 0x?? - Completion Code
   *      - 0xCC - one or more configs not supported
   * 0x?? - Task ID - used to get the restore status.  Invalid after
   *        120 seconds.  00h = reserved.
   *
   * Request Get Restore Status
   *
   * 0x30 - OEM network function
   * 0x05 - OEM cmd
   * 0x?? - Task ID
   *
   * Response Get Restore Status
   *
   * 0x05 - OEM cmd
   * 0x?? - Completion Code
   * 0x?? - restore status
   *      - 00h = restore in progress
   *      - 01h = restore complete
   */

  bytes_rq[0] = IPMI_CMD_OEM_QUANTA_RESET_TO_DEFAULTS;

  bytes_rq[1] = IPMI_OEM_QUANTA_RESET_TO_DEFAULTS_RESTORE_FLAG_REMAINING_PARAMETERS_STAY_WHAT_IT_IS;
  bytes_rq[1] <<= IPMI_OEM_QUANTA_RESET_TO_DEFAULTS_RESTORE_FLAG_SHIFT;

  if (!strcasecmp (state_data->prog_data->args->oem_options[0], "all"))
    {
      bytes_rq[1] = IPMI_OEM_QUANTA_RESET_TO_DEFAULTS_RESTORE_FLAG_RESTORE_PARAMETERS_NOT_INCLUDED_BELOW;
      bytes_rq[1] <<= IPMI_OEM_QUANTA_RESET_TO_DEFAULTS_RESTORE_FLAG_SHIFT;
      bytes_rq[1] |= IPMI_OEM_QUANTA_RESET_TO_DEFAULTS_USER_ACCOUNTS_BITMASK;
      bytes_rq[1] |= IPMI_OEM_QUANTA_RESET_TO_DEFAULTS_LAN_CONFIGURATION_BITMASK;
      bytes_rq[1] |= IPMI_OEM_QUANTA_RESET_TO_DEFAULTS_SOL_CONFIGURATION_BITMASK;
      bytes_rq[1] |= IPMI_OEM_QUANTA_RESET_TO_DEFAULTS_SERIAL_CONFIGURATION_BITMASK;
      bytes_rq[1] |= IPMI_OEM_QUANTA_RESET_TO_DEFAULTS_PEF_BITMASK;
    }
  else if (!strcasecmp (state_data->prog_data->args->oem_options[0], "user"))
    bytes_rq[1] |= IPMI_OEM_QUANTA_RESET_TO_DEFAULTS_USER_ACCOUNTS_BITMASK;
  else if (!strcasecmp (state_data->prog_data->args->oem_options[0], "lan"))
    bytes_rq[1] |= IPMI_OEM_QUANTA_RESET_TO_DEFAULTS_LAN_CONFIGURATION_BITMASK;
  else if (!strcasecmp (state_data->prog_data->args->oem_options[0], "sol"))
    bytes_rq[1] |= IPMI_OEM_QUANTA_RESET_TO_DEFAULTS_SOL_CONFIGURATION_BITMASK;
  else if (!strcasecmp (state_data->prog_data->args->oem_options[0], "serial"))
    bytes_rq[1] |= IPMI_OEM_QUANTA_RESET_TO_DEFAULTS_SERIAL_CONFIGURATION_BITMASK;
  else  /* !strcasecmp (state_data->prog_data->args->oem_options[0], "pef" */
    bytes_rq[1] |= IPMI_OEM_QUANTA_RESET_TO_DEFAULTS_PEF_BITMASK;

  if ((rs_len = ipmi_cmd_raw (state_data->ipmi_ctx,
                              0, /* lun */
                              IPMI_NET_FN_OEM_QUANTA_GENERIC_RQ, /* network function */
                              bytes_rq, /* data */
                              2, /* num bytes */
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
                                                   IPMI_CMD_OEM_QUANTA_RESET_TO_DEFAULTS,
                                                   IPMI_NET_FN_OEM_QUANTA_GENERIC_RS,
                                                   NULL) < 0)
    goto cleanup;

  task_id = bytes_rs[2];

  /* don't quit until it is done */
  while (1)
    {
      bytes_rq[0] = IPMI_CMD_OEM_QUANTA_GET_RESTORE_STATUS;
      bytes_rq[1] = task_id;

      if ((rs_len = ipmi_cmd_raw (state_data->ipmi_ctx,
                                  0, /* lun */
                                  IPMI_NET_FN_OEM_QUANTA_GENERIC_RQ, /* network function */
                                  bytes_rq, /* data */
                                  2, /* num bytes */
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
                                                       IPMI_CMD_OEM_QUANTA_GET_RESTORE_STATUS,
                                                       IPMI_NET_FN_OEM_QUANTA_GENERIC_RS,
                                                       NULL) < 0)
        goto cleanup;

      if (bytes_rs[2] == IPMI_OEM_QUANTA_GET_RESTORE_STATUS_RESTORE_COMPLETE)
        break;

      sleep (1);
    }

  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_oem_quanta_get_processor_information (ipmi_oem_state_data_t *state_data)
{
  uint8_t bytes_rq[IPMI_OEM_MAX_BYTES];
  uint8_t bytes_rs[IPMI_OEM_MAX_BYTES];
  uint8_t processor_index = 0;
  uint8_t processor_index_init = 1;
  uint8_t processor_index_max = 0xFF;
  uint8_t processor_type;
  char *processor_type_str = NULL;
  uint16_t processor_mhz;
  int rs_len;
  int rv = -1;

  /* Quanta S99Q/Dell FS12-TY OEM
   *
   * Request Get Processor Information
   *
   * 0x30 - OEM network function
   * 0x18 - OEM cmd
   * 0x?? - Processor Index, 1 based
   * 
   * Response Get Processor Information
   *
   * 0x18 - OEM cmd
   * 0x?? - Completion Code
   * 0x?? - Processor Type
   * bytes 3-4 - Processor frequency in MHZ (LSB first)
   */

  assert (state_data);
  
  if (state_data->prog_data->args->oem_options_count > 1)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "%s:%s invalid number of options specified\n");
      goto cleanup;
    }

  if (state_data->prog_data->args->oem_options_count)
    {
      char *endptr = NULL;
      unsigned int temp;

      errno = 0;
      temp = strtoul (state_data->prog_data->args->oem_options[0], &endptr, 10);
      if (errno
          || endptr[0] != '\0'
          || temp > UCHAR_MAX
          || !temp)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "%s:%s invalid OEM option argument '%s'\n",
                           state_data->prog_data->args->oem_id,
                           state_data->prog_data->args->oem_command,
                           state_data->prog_data->args->oem_options[0]);
          goto cleanup;
        }
      
      processor_index_init = temp;

      processor_index_max = processor_index_init;
    }

  for (processor_index = processor_index_init;
       processor_index <= processor_index_max;
       processor_index++)
    {
      bytes_rq[0] = IPMI_CMD_OEM_QUANTA_GET_PROCESSOR_INFORMATION;
      bytes_rq[1] = processor_index;

      if ((rs_len = ipmi_cmd_raw (state_data->ipmi_ctx,
                                  0, /* lun */
                                  IPMI_NET_FN_OEM_QUANTA_GENERIC_RQ, /* network function */
                                  bytes_rq, /* data */
                                  2, /* num bytes */
                                  bytes_rs,
                                  IPMI_OEM_MAX_BYTES)) < 0)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "ipmi_cmd_raw: %s\n",
                           ipmi_ctx_errormsg (state_data->ipmi_ctx));
          goto cleanup;
        }

      /* If processor index 1 fails or user input a processor index,
       * fall through to normal error output below.
       */
      if (rs_len >= 2
	  && (bytes_rs[1] == IPMI_COMP_CODE_PARAMETER_OUT_OF_RANGE
	      || bytes_rs[1] == IPMI_COMP_CODE_INVALID_DATA_FIELD_IN_REQUEST)
          && processor_index_init != processor_index_max
          && processor_index != 1)
        break;

      if (ipmi_oem_check_response_and_completion_code (state_data,
                                                       bytes_rs,
                                                       rs_len,
                                                       5,
                                                       IPMI_CMD_OEM_QUANTA_GET_PROCESSOR_INFORMATION,
                                                       IPMI_NET_FN_OEM_QUANTA_GENERIC_RS,
                                                       NULL) < 0)
        goto cleanup;

      processor_type = bytes_rs[2];

      processor_mhz = (bytes_rs[3] | (bytes_rs[4] << 8));

      if (processor_type == IPMI_OEM_QUANTA_PROCESSOR_TYPE_NO_CPU_PRESENT)
	pstdout_printf (state_data->pstate,
			"Processor %u: No CPU Present\n",
                        processor_index);
      else
	{
	  switch (processor_type)
	    {
	    case IPMI_OEM_QUANTA_PROCESSOR_TYPE_CELERON:
	      processor_type_str = "Celeron";
	      break;
	    case IPMI_OEM_QUANTA_PROCESSOR_TYPE_PENTIUM_3:
	      processor_type_str = "Pentium 3";
	      break;
	    case IPMI_OEM_QUANTA_PROCESSOR_TYPE_PENTIUM_4:
	      processor_type_str = "Pentium 4";
	      break;
	    case IPMI_OEM_QUANTA_PROCESSOR_TYPE_XEON:
	      processor_type_str = "Xeon";
	      break;
	    case IPMI_OEM_QUANTA_PROCESSOR_TYPE_PRESTONIA:
	      processor_type_str = "Prestonia";
	      break;
	    case IPMI_OEM_QUANTA_PROCESSOR_TYPE_NOCONA:
	      processor_type_str = "Nocona";
	      break;
	    case IPMI_OEM_QUANTA_PROCESSOR_TYPE_OPTERON:
	      processor_type_str = "Opteron";
	      break;
	    case IPMI_OEM_QUANTA_PROCESSOR_TYPE_DEMPSEY:
	      processor_type_str = "Dempsey";
	      break;
	    case IPMI_OEM_QUANTA_PROCESSOR_TYPE_CLOVERTOWN:
	      processor_type_str = "Clovertown";
	      break;
	    case IPMI_OEM_QUANTA_PROCESSOR_TYPE_TIGERTON:
	      processor_type_str = "Tigerton";
	      break;
	    case IPMI_OEM_QUANTA_PROCESSOR_TYPE_DUNNINGTON:
	      processor_type_str = "Dunnington";
	      break;
	    case IPMI_OEM_QUANTA_PROCESSOR_TYPE_HARPERTOWN:
	      processor_type_str = "Harpertown";
	      break;
	    case IPMI_OEM_QUANTA_PROCESSOR_TYPE_WOLFDALE_DP:
	      /* achu: listed as "WolfDale-Dp" in spec, I don't like that output */
	      processor_type_str = "Wolfdale-DP";
	      break;
	    case IPMI_OEM_QUANTA_PROCESSOR_TYPE_NEHALEM_EP:
	      processor_type_str = "Nehalem-EP";
	      break;
	    case IPMI_OEM_QUANTA_PROCESSOR_TYPE_WESTMERE_EP:
	      processor_type_str = "Westmere-EP";
	      break;
	    default:
	      processor_type_str = "Unknown Processor";
	    }
	  
	  pstdout_printf (state_data->pstate,
			  "Processor %u: %s %.2f Ghz\n",
			  processor_index,
			  processor_type_str,
			  (double)processor_mhz / 1000);
	}
    }

  rv = 0;
 cleanup:
  return (rv);
}

static int
_ipmi_oem_quanta_read_mac_address_s99q (ipmi_oem_state_data_t *state_data)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint8_t data_buf[IPMI_OEM_MAX_BYTES];
  uint8_t data_rq[IPMI_OEM_MAX_BYTES];
  int data_len;
  int rv = -1;

  assert (state_data);
  assert (state_data->prog_data->args->oem_options_count == 2);

  if (strcasecmp (state_data->prog_data->args->oem_options[1], "shared")
      && strcasecmp (state_data->prog_data->args->oem_options[1], "dedicated"))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "%s:%s invalid OEM option argument '%s'\n",
                       state_data->prog_data->args->oem_id,
                       state_data->prog_data->args->oem_command,
                       state_data->prog_data->args->oem_options[1]);
      goto cleanup;
    }

  /* Uses Master-Read Write Command
   *
   * Most addresses provided directly from Dell.
   *
   * byte 1 = 5 (channel = 0, bus id = 2, bus-type = 1 = private)
   * byte 2 = 0xAA (slave address 7 bit = 0x55, lowest bit for r/w, 0b = read, 1b = write)
   * byte 3 = 0x0C - read count
   * byte 4/5 - 0x0000 - address to read, msb first
   * 
   * response
   *
   * byte 1 = comp-code
   * byte 2-7 = dedicated MAC
   * byte 8-13 = shared MAC
   */

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_master_write_read_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if (fiid_obj_clear (obj_cmd_rs) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_clear: %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }

  data_rq[0] = (IPMI_OEM_QUANTA_MAC_ADDRESS_BASE_OFFSET & 0xFF00) >> 8;
  data_rq[1] = (IPMI_OEM_QUANTA_MAC_ADDRESS_BASE_OFFSET & 0x00FF);

  if (ipmi_cmd_master_write_read (state_data->ipmi_ctx,
                                  IPMI_BUS_TYPE_PRIVATE,
                                  IPMI_OEM_QUANTA_MAC_ADDRESS_BUS_ID,
                                  IPMI_OEM_QUANTA_MAC_ADDRESS_CHANNEL,
                                  IPMI_OEM_QUANTA_MAC_ADDRESS_SLAVE_ADDRESS,
                                  IPMI_OEM_QUANTA_MAC_ADDRESS_BYTES * 2, /* returns two MACs */
                                  data_rq,
                                  2,
                                  obj_cmd_rs) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_cmd_master_write_read: %s\n",
                       ipmi_ctx_errormsg (state_data->ipmi_ctx));
      goto cleanup;
    }

  if ((data_len = fiid_obj_get_data (obj_cmd_rs,
                                     "data",
                                     data_buf,
                                     IPMI_OEM_MAX_BYTES)) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get_data: 'data': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }

  if (data_len != (IPMI_OEM_QUANTA_MAC_ADDRESS_BYTES * 2))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "invalid bytes returned: %u\n",
                       data_len);
      goto cleanup;
    }

  if (!strcasecmp (state_data->prog_data->args->oem_options[1], "dedicated"))
    pstdout_printf (state_data->pstate,
                    "%02X:%02X:%02X:%02X:%02X:%02X\n",
                    data_buf[0],
                    data_buf[1],
                    data_buf[2],
                    data_buf[3],
                    data_buf[4],
                    data_buf[5]);
  else
    pstdout_printf (state_data->pstate,
                    "%02X:%02X:%02X:%02X:%02X:%02X\n",
                    data_buf[6],
                    data_buf[7],
                    data_buf[8],
                    data_buf[9],
                    data_buf[10],
                    data_buf[11]);

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

int
ipmi_oem_quanta_read_mac_address (ipmi_oem_state_data_t *state_data)
{
  assert (state_data);
  assert (state_data->prog_data->args->oem_options_count == 2);

  if (!strcasecmp (state_data->prog_data->args->oem_options[0], "s99q"))
    return _ipmi_oem_quanta_read_mac_address_s99q (state_data);
  
  pstdout_fprintf (state_data->pstate,
                   stderr,
                   "%s:%s invalid OEM option argument '%s'\n",
                   state_data->prog_data->args->oem_id,
                   state_data->prog_data->args->oem_command,
                   state_data->prog_data->args->oem_options[0]);
  return (-1);
}

static int
_ipmi_oem_quanta_write_mac_address_s99q (ipmi_oem_state_data_t *state_data)
{
  fiid_obj_t obj_cmd_rs = NULL;
  unsigned int b1, b2, b3, b4, b5, b6;
  uint8_t data_rq[IPMI_OEM_MAX_BYTES];
  int rv = -1;

  assert (state_data);
  assert (state_data->prog_data->args->oem_options_count == 3);

  if (strcasecmp (state_data->prog_data->args->oem_options[1], "shared")
      && strcasecmp (state_data->prog_data->args->oem_options[1], "dedicated"))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "%s:%s invalid OEM option argument '%s'\n",
                       state_data->prog_data->args->oem_id,
                       state_data->prog_data->args->oem_command,
                       state_data->prog_data->args->oem_options[1]);
      goto cleanup;
    }
  
  if (sscanf (state_data->prog_data->args->oem_options[2],
              "%02x:%02x:%02x:%02x:%02x:%02x",
              &b1,
              &b2,
              &b3,
              &b4,
              &b5,
              &b6) != 6)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "%s:%s invalid OEM option argument '%s'\n",
                       state_data->prog_data->args->oem_id,
                       state_data->prog_data->args->oem_command,
                       state_data->prog_data->args->oem_options[2]);
      goto cleanup;
    }

  /* Uses Master-Read Write Command
   *
   * Most addresses provided directly from Dell.
   *
   * byte 1 = 5 (channel = 0, bus id = 2, bus-type = 1 = private)
   * byte 2 = 0xAA (slave address 7 bit = 0x55, lowest bit for r/w, 0b = read, 1b = write)
   * byte 3 = 0x00 - read count
   * byte 4/5 - 0x0000 | 0x0006 - address to read, msb first
   *          - 0x0000 for dedicated
   *          - 0x0006 for shared
   * 
   * response
   *
   * byte 1 = comp-code
   */

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_master_write_read_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if (fiid_obj_clear (obj_cmd_rs) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_clear: %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }

  if (!strcasecmp (state_data->prog_data->args->oem_options[1], "dedicated"))
    {
      data_rq[0] = (IPMI_OEM_QUANTA_MAC_ADDRESS_DEDICATED_OFFSET & 0xFF00) >> 8;
      data_rq[1] = (IPMI_OEM_QUANTA_MAC_ADDRESS_DEDICATED_OFFSET & 0x00FF);
    }
  else
    {
      data_rq[0] = (IPMI_OEM_QUANTA_MAC_ADDRESS_SHARED_OFFSET & 0xFF00) >> 8;
      data_rq[1] = (IPMI_OEM_QUANTA_MAC_ADDRESS_SHARED_OFFSET & 0x00FF);
    }

  data_rq[2] = b1;
  data_rq[3] = b2;
  data_rq[4] = b3;
  data_rq[5] = b4;
  data_rq[6] = b5;
  data_rq[7] = b6;

  if (ipmi_cmd_master_write_read (state_data->ipmi_ctx,
                                  IPMI_BUS_TYPE_PRIVATE,
                                  IPMI_OEM_QUANTA_MAC_ADDRESS_BUS_ID,
                                  IPMI_OEM_QUANTA_MAC_ADDRESS_CHANNEL,
                                  IPMI_OEM_QUANTA_MAC_ADDRESS_SLAVE_ADDRESS,
                                  0,
                                  data_rq,
                                  8,
                                  obj_cmd_rs) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_cmd_master_write_read: %s\n",
                       ipmi_ctx_errormsg (state_data->ipmi_ctx));
      goto cleanup;
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

int
ipmi_oem_quanta_write_mac_address (ipmi_oem_state_data_t *state_data)
{
  assert (state_data);
  assert (state_data->prog_data->args->oem_options_count == 3);

  if (!strcasecmp (state_data->prog_data->args->oem_options[0], "s99q"))
    return _ipmi_oem_quanta_write_mac_address_s99q (state_data);
  
  pstdout_fprintf (state_data->pstate,
                   stderr,
                   "%s:%s invalid OEM option argument '%s'\n",
                   state_data->prog_data->args->oem_id,
                   state_data->prog_data->args->oem_command,
                   state_data->prog_data->args->oem_options[0]);
  return (-1);
}
