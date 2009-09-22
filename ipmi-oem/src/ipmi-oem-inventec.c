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
#include <ctype.h>
#endif /* STDC_HEADERS */
#if HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */
#include <assert.h>

#include <freeipmi/freeipmi.h>

#include "ipmi-oem.h"
#include "ipmi-oem-argp.h"
#include "ipmi-oem-common.h"
#include "ipmi-oem-inventec.h"

#include "freeipmi-portability.h"
#include "pstdout.h"

/* Inventec 5441 Notes
 *
 * Three firmware update commands exist for firmware updates, but
 * given that we need firmware images, install options (even
 * documented that there are OEM specific options involved), just
 * document these.
 *
 * Update Firmware Request
 *
 * 0x08 - network function
 * 0x01 - OEM cmd
 * 0x?? - interface used
 *      - 00h - system interface (i.e. KCS)
 *      - 01h - networking (i.e. tftp, ftp, http)
 *      - 02h - USB MSC
 * 0x?? - update type
 *      [7]   - force update
 *            - 0h - normal update, an update operation will occur only when the
 *              BMC validate (sic) target board, target product and
 *              version number.
 *            - 1h - forced update, make the BMC update the image without
 *              validate (sic) target board, target product and
 *              version number.
 *      [6:0] - reserved
 * bytes 3 - 14: install options (OEM specific)
 *
 * Update Firmware Response
 *
 * 0x01 - OEM cmd
 * 0x?? - Completion Code
 * 0x?? - Task ID
 *
 * Get Update Status Request
 *
 * 0x08 - network function
 * 0x02 - OEM cmd
 * 0x?? - Task ID
 *
 * Get Update Status Response
 *
 * 0x02 - OEM cmd
 * 0x?? - Completion Code
 * 0x?? - Status
 *      - 00h = transmitting image
 *      - 01h = validating image
 *      - 02h = programming
 *      - 03h = ready to accept image
 *      - 80h = general error
 *      - 81h = cannot establish connection
 *      - 82h = path not found
 *      - 83h = transmission abort
 *      - 84h = checkusm error
 *      - 85h = incorrect platform
 *      - FFh = completed
 * 0x?? - Progression Indicator
 *      - "This field is optional, if present its value indicates the
 *        current progress of the status specified in Status byte"
 *
 * Copy Image Data Request
 *
 * 0x08 - network function
 * 0x03 - OEM cmd
 * 0x?? - In Progress
 *      - 00h - data transmission is in progress
 *      - 01h = data transmission completed
 * bytes 3:6 - image offset to be copied
 * bytes 7:N - image data to be copied
 *
 * Copy Image Data Response
 *
 * 0x03 - OEM cmd
 * 0x?? - Completion Code
 */

/* achu: all named from doc except 'lan' configuration id, which I assumed names */

#define IPMI_OEM_EXTENDED_CONFIGURATION_ID_LAN                      0x02
#define IPMI_OEM_EXTENDED_CONFIGURATION_ID_SOL                      0x03
#define IPMI_OEM_EXTENDED_CONFIGURATION_ID_SECURITY                 0x04
#define IPMI_OEM_EXTENDED_CONFIGURATION_ID_WEB_SERVER_CONFIGURATION 0x0C
#define IPMI_OEM_EXTENDED_CONFIGURATION_ID_FIRMWARE_LOG             0x0E
#define IPMI_OEM_EXTENDED_CONFIGURATION_ID_FIRMWARE_INFORMATION     0x0F
#define IPMI_OEM_EXTENDED_CONFIGURATION_ID_FIRMWARE_UPDATE          0x10
#define IPMI_OEM_EXTENDED_CONFIGURATION_ID_POWER_MANAGEMENT         0x11

/* nic status - is implemented, see below
 */
#define IPMI_OEM_EXTENDED_ATTRIBUTE_ID_LAN_NIC_STATUS 0x01

/* sol idle timeout - 2 bytes, ls byte first, 0h = no timeout, default = 01h
 *
 * telnet/ssh redirect enable - 1 byte, 0 = disable, 1 = enabled 
 */
#define IPMI_OEM_EXTENDED_ATTRIBUTE_ID_SOL_SOL_IDLE_TIMEOUT           0x01
#define IPMI_OEM_EXTENDED_ATTRIBUTE_ID_SOL_TELNET_SSH_REDIRECT_ENABLE 0x02

/* service disabled - is implemented, see below
 *
 * max authentication failures - 1 byte, 0 = disable
 *
 * lockout window - 2 bytes, in seconds, 0 = disable, default = 180
 *
 * lockout time - 2 bytes, in seconds, 0 = disable, default = 3600
 */
#define IPMI_OEM_EXTENDED_ATTRIBUTE_ID_SECURITY_SERVICE_DISABLED            0x01
#define IPMI_OEM_EXTENDED_ATTRIBUTE_ID_SECURITY_MAX_AUTHENTICATION_FAILURES 0x02
#define IPMI_OEM_EXTENDED_ATTRIBUTE_ID_SECURITY_LOCKOUT_WINDOW              0x03
#define IPMI_OEM_EXTENDED_ATTRIBUTE_ID_SECURITY_LOCKOUT_TIME                0x04

/* web server enabled - 1 byte, 0 = false, 1 = true (default)
 *
 * max web sessions - 1 byte, read only
 *
 * active web sessions - 1 byte, read only
 *
 * web server timeout - 4 bytes, in seconds, 0 = disable, range 60-1920, default 300
 *
 * http port num - 2 bytes, default 80
 *
 * https port num - 2 bytes, default 443
 */
/* Dell Xanadu2 OEM
 *
 * achu: Dell appears to have also implemented an additional OEM
 * command that duplicates this configuration.  Currently, we do not
 * implement the Dell equivalent in ipmi-oem.
 *
 * achu: The document states "web port" and "http port".  That
 * probably means "http" vs. "https" port.  The below documents this
 * typo.
 *
 * Set Web Port Num Request
 *
 * 0x34 - OEM network function
 * 0x02 - OEM cmd
 * 0x?? - web port num (LSB)
 * 0x?? - web port num (MSB)
 * 0x?? - http num (LSB)
 * 0x?? - http num (MSB)
 *
 * Set Web Port Num Response
 *
 * 0x02 - OEM cmd
 * 0x?? - Completion Code
 *
 * Get Web Port Num Request
 *
 * 0x34 - OEM network function
 * 0x03 - OEM cmd
 *
 * Get Web Port Num Response
 *
 * 0x03 - OEM cmd
 * 0x?? - Completion Code
 * 0x?? - web port num (LSB)
 * 0x?? - web port num (MSB)
 * 0x?? - http num (LSB)
 * 0x?? - http num (MSB)
 */
#define IPMI_OEM_EXTENDED_ATTRIBUTE_ID_WEB_SERVER_CONFIGURATION_WEB_SERVER_ENABLED  0x01
#define IPMI_OEM_EXTENDED_ATTRIBUTE_ID_WEB_SERVER_CONFIGURATION_MAX_WEB_SESSIONS    0x02
#define IPMI_OEM_EXTENDED_ATTRIBUTE_ID_WEB_SERVER_CONFIGURATION_ACTIVE_WEB_SESSIONS 0x03
#define IPMI_OEM_EXTENDED_ATTRIBUTE_ID_WEB_SERVER_CONFIGURATION_WEB_SERVER_TIMEOUT  0x04
#define IPMI_OEM_EXTENDED_ATTRIBUTE_ID_WEB_SERVER_CONFIGURATION_HTTP_PORT_NUM       0x05
#define IPMI_OEM_EXTENDED_ATTRIBUTE_ID_WEB_SERVER_CONFIGURATION_HTTPS_PORT_NUM      0x06

/* entity - 1 byte (read only)
 *
 * firmware version - 1-16 bytes (read only)
 *
 * branch - 1-16 bytes (read only)
 *
 * build information - 1-16 bytes (read only)
 *
 * update date / time - 3 bytes, from 0:00 1/1/08, lsbyte first
 */
#define IPMI_OEM_EXTENDED_ATTRIBUTE_ID_FIRMWARE_LOG_ENTITY            0x01
#define IPMI_OEM_EXTENDED_ATTRIBUTE_ID_FIRMWARE_LOG_FIRMWARE_VERSION  0x02
#define IPMI_OEM_EXTENDED_ATTRIBUTE_ID_FIRMWARE_LOG_BRANCH            0x03
#define IPMI_OEM_EXTENDED_ATTRIBUTE_ID_FIRMWARE_LOG_BUILD_INFORMATION 0x04
#define IPMI_OEM_EXTENDED_ATTRIBUTE_ID_FIRMWARE_LOG_UPDATE_DATE_TIME  0x05

/* name - 1-16 bytes (read only)
 *
 * description - 1-256 bytes (read only)
 *
 * entity - 1 byte (read only)
 *
 * product info - 1-64 bytes (read only)
 *
 * firmware version - 1-16 bytes (read only)
 *
 * branch - 1-16 bytes (read only)
 *
 * build information - 1-16 bytes (read only) 
 *
 */
#define IPMI_OEM_EXTENDED_ATTRIBUTE_ID_FIRMWARE_INFORMATION_NAME              0x01
#define IPMI_OEM_EXTENDED_ATTRIBUTE_ID_FIRMWARE_INFORMATION_DESCRIPTION       0x02
#define IPMI_OEM_EXTENDED_ATTRIBUTE_ID_FIRMWARE_INFORMATION_ENTITY            0x03
#define IPMI_OEM_EXTENDED_ATTRIBUTE_ID_FIRMWARE_INFORMATION_PRODUCT_INFO      0x04
#define IPMI_OEM_EXTENDED_ATTRIBUTE_ID_FIRMWARE_INFORMATION_FIRMWARE_VERSION  0x05
#define IPMI_OEM_EXTENDED_ATTRIBUTE_ID_FIRMWARE_INFORMATION_BRANCH            0x06
#define IPMI_OEM_EXTENDED_ATTRIBUTE_ID_FIRMWARE_INFORMATION_BUILD_INFORMATION 0x07

/* remote update enable - 1 byte, ??
 *
 * protocol - 1 byte, bitmask, 7:3 reserved, 2 : http, 1: ftp, 0: tftp (read only)
 *
 * url - 1-256 bytes
 *
 * connection retry - 1 byte, 0 = no retries
 *
 * retry interval - 1 byte, in 5 second increments
 *
 * delay time - 1 byte, in seconds, 0h = immediate, ffh = random between 5 and 10
 */
#define IPMI_OEM_EXTENDED_ATTRIBUTE_ID_FIRMWARE_UPDATE_REMOTE_UPDATE_ENABLE 0x01
#define IPMI_OEM_EXTENDED_ATTRIBUTE_ID_FIRMWARE_UPDATE_PROTOCOL             0x02
#define IPMI_OEM_EXTENDED_ATTRIBUTE_ID_FIRMWARE_UPDATE_URL                  0x03
#define IPMI_OEM_EXTENDED_ATTRIBUTE_ID_FIRMWARE_UPDATE_CONNECTION_RETRY     0x04
#define IPMI_OEM_EXTENDED_ATTRIBUTE_ID_FIRMWARE_UPDATE_RETRY_INTERVAL       0x05
#define IPMI_OEM_EXTENDED_ATTRIBUTE_ID_FIRMWARE_UPDATE_DELAY_TIME           0x06

/* power management enable - 1 byte, bit 7: 0 = disable, 1 = enable, reset reserved
 *
 * power staggery ac recovery - 1 byte, 0x00 = immediate power on
 * (default), 0x01 = auto, random between min and max below, 0x02 =
 * user defined, must be between min and max below
 *
 * power on delay - 2 bytes
 *
 * minimum power on delay - 2 bytes (read only)
 *
 * maximum power on delay - 2 bytes
 */
#define IPMI_OEM_EXTENDED_ATTRIBUTE_ID_POWER_MANAGEMENT_POWER_MANAGEMENT_ENABLE      0x01
#define IPMI_OEM_EXTENDED_ATTRIBUTE_ID_POWER_MANAGEMENT_POWER_STAGGERING_AC_RECOVERY 0x02
#define IPMI_OEM_EXTENDED_ATTRIBUTE_ID_POWER_MANAGEMENT_POWER_ON_DELAY               0x03
#define IPMI_OEM_EXTENDED_ATTRIBUTE_ID_POWER_MANAGEMENT_MINIMUM_POWER_ON_DELAY       0x04
#define IPMI_OEM_EXTENDED_ATTRIBUTE_ID_POWER_MANAGEMENT_MAXIMUM_POWER_ON_DELAY       0x05

#define IPMI_OEM_SET_SELECTOR   0x0
#define IPMI_OEM_BLOCK_SELECTOR 0x0

#define IPMI_OEM_MAX_MACADDRLEN 24

#define IPMI_OEM_EXTENDED_CONFIG_READ_ALL_BYTES           0xFF

#define IPMI_OEM_EXTENDED_CONFIG_LAN_NIC_STATUS_SHARED    0x00
#define IPMI_OEM_EXTENDED_CONFIG_LAN_NIC_STATUS_DEDICATED 0x01

#define IPMI_OEM_EXTENDED_CONFIG_SECURITY_SERVICES_DISABLED_ENABLE_ALL   0x00
#define IPMI_OEM_EXTENDED_CONFIG_SECURITY_SERVICES_DISABLED_BITMASK_ALL  0x01
#define IPMI_OEM_EXTENDED_CONFIG_SECURITY_SERVICES_DISABLED_BITMASK_KVM  0x02
#define IPMI_OEM_EXTENDED_CONFIG_SECURITY_SERVICES_DISABLED_BITMASK_HTTP 0x04
#define IPMI_OEM_EXTENDED_CONFIG_SECURITY_SERVICES_DISABLED_BITMASK_SSH  0x08

#define IPMI_OEM_INVENTEC_RESTORE_TO_DEFAULTS_RESTORE_FLAG_RESTORE_PARAMETERS_NOT_INCLUDED_BELOW 0x0
#define IPMI_OEM_INVENTEC_RESTORE_TO_DEFAULTS_RESTORE_FLAG_REMAINING_PARAMETERS_STAY_WHAT_IT_IS  0x7
#define IPMI_OEM_INVENTEC_RESTORE_TO_DEFAULTS_RESTORE_FLAG_SHIFT                                 5

#define IPMI_OEM_INVENTEC_RESTORE_TO_DEFAULTS_PEF_BITMASK                  0x10
#define IPMI_OEM_INVENTEC_RESTORE_TO_DEFAULTS_SERIAL_CONFIGURATION_BITMASK 0x08
#define IPMI_OEM_INVENTEC_RESTORE_TO_DEFAULTS_SOL_CONFIGURATION_BITMASK    0x04
#define IPMI_OEM_INVENTEC_RESTORE_TO_DEFAULTS_LAN_CONFIGURATION_BITMASK    0x02
#define IPMI_OEM_INVENTEC_RESTORE_TO_DEFAULTS_USER_ACCOUNTS_BITMASK        0x01

#define IPMI_OEM_INVENTEC_GET_RESTORE_STATUS_RESTORE_IN_PROGRESS 0x00
#define IPMI_OEM_INVENTEC_GET_RESTORE_STATUS_RESTORE_COMPLETE    0x01

#define IPMI_OEM_EEPROM_AT24C256N_SLAVE_ADDRESS 0x53
#define IPMI_OEM_EEPROM_AT24C256N_BUS_ID        2
#define IPMI_OEM_EEPROM_AT24C256N_ADDRESS_MIN   0x0000
#define IPMI_OEM_EEPROM_AT24C256N_ADDRESS_MAX   0x7FFF
#define IPMI_OEM_EEPROM_AT24C256N_CLEAR_BYTE    0xFF

#define IPMI_OEM_BUFLEN 1024

static int
_inventec_get_reservation (ipmi_oem_state_data_t *state_data,
                           uint8_t *reservation_id)
{
  uint8_t bytes_rq[IPMI_OEM_MAX_BYTES];
  uint8_t bytes_rs[IPMI_OEM_MAX_BYTES];
  int rs_len;
  int rv = -1;

  /* Inventec OEM
   *
   * Get Reservation Request
   *
   * 0x30 - OEM network function
   * 0x01 - OEM cmd
   *
   * Get Reservation Response 
   *
   * 0x01 - OEM cmd
   * 0x?? - Completion Code
   * 0x?? - Reservation ID
   */

  assert (state_data);
  assert (reservation_id);

  bytes_rq[0] = IPMI_CMD_OEM_INVENTEC_RESERVED_EXTENDED_CONFIGUATION;

  if ((rs_len = ipmi_cmd_raw (state_data->ipmi_ctx,
                              0, /* lun */
                              IPMI_NET_FN_OEM_INVENTEC_GENERIC_RQ, /* network function */
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
                                                   3,
                                                   IPMI_CMD_OEM_INVENTEC_RESERVED_EXTENDED_CONFIGUATION,
                                                   IPMI_NET_FN_OEM_INVENTEC_GENERIC_RS) < 0)
    goto cleanup;
  
  (*reservation_id) = bytes_rs[2];

  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_oem_inventec_get_nic_status (ipmi_oem_state_data_t *state_data)
{
  uint8_t bytes_rq[IPMI_OEM_MAX_BYTES];
  uint8_t bytes_rs[IPMI_OEM_MAX_BYTES];
  int rs_len;
  uint8_t reservation_id;
  int rv = -1;

  assert (state_data);
  assert (!state_data->prog_data->args->oem_options_count);

  /* Inventec OEM
   *
   * Get NIC Status Request
   *
   * 0x30 - OEM network function
   * 0x02 - OEM cmd
   * 0x?? - Reservation ID
   * 0x02 - Configuration ID (0x02 = ??)
   * 0x01 - Attribute ID (0x01 = ??)
   * 0x00 - Index (unused here??)
   * 0x00 - Data Offset - LSB (unused here??)
   * 0x00 = Data Offset - MSB (unused here??)
   * 0xFF - Bytes to read (0xFF = all)
   * 
   * Get NIC Status Response
   *
   * 0x02 - OEM cmd
   * 0x?? - Completion Code
   * 0x02 - Configuration ID (0x02 = ??)
   * 0x01 - Attribute ID (0x01 = ??)
   * 0x00 - Index (unused here??)
   * 0x01 - number of bytes returned
   * 0x00 | 0x01 - 0x00 = shared, 0x01 = dedicated
   */

  /* Dell Xanadu2 OEM
   *
   * achu: Dell appears to have also implemented an additional OEM
   * command that duplicates this behavior.  Currently, we do not
   * implement the Dell equivalent in ipmi-oem.
   *
   * Get LAN Source Request
   *
   * 0x34 - OEM network function
   * 0x14 - OEM cmd
   *
   * Get LAN Source Response
   *
   * 0x14 - OEM cmd
   * 0x?? - Completion Code
   * 0x?? - LAN Source Setting
   *      - 00h = shared, 01h = dedicated
   */

  if (_inventec_get_reservation (state_data,
                                 &reservation_id) < 0)
    goto cleanup;

  bytes_rq[0] = IPMI_CMD_OEM_INVENTEC_GET_EXTENDED_CONFIGURATION;
  bytes_rq[1] = reservation_id;
  bytes_rq[2] = IPMI_OEM_EXTENDED_CONFIGURATION_ID_LAN;
  bytes_rq[3] = IPMI_OEM_EXTENDED_ATTRIBUTE_ID_LAN_NIC_STATUS;
  bytes_rq[4] = 0x00;
  bytes_rq[5] = 0x00;
  bytes_rq[6] = 0x00;
  bytes_rq[7] = IPMI_OEM_EXTENDED_CONFIG_READ_ALL_BYTES;

  if ((rs_len = ipmi_cmd_raw (state_data->ipmi_ctx,
                              0, /* lun */
                              IPMI_NET_FN_OEM_INVENTEC_GENERIC_RQ, /* network function */
                              bytes_rq, /* data */
                              8, /* num bytes */
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
                                                   7,
                                                   IPMI_CMD_OEM_INVENTEC_GET_EXTENDED_CONFIGURATION,
                                                   IPMI_NET_FN_OEM_INVENTEC_GENERIC_RS) < 0)
    goto cleanup;

  switch (bytes_rs[6])
    {
    case IPMI_OEM_EXTENDED_CONFIG_LAN_NIC_STATUS_SHARED:
      pstdout_printf (state_data->pstate, "shared\n");
      break;
    case IPMI_OEM_EXTENDED_CONFIG_LAN_NIC_STATUS_DEDICATED:
      pstdout_printf (state_data->pstate, "dedicated\n");
      break;
    default:
      pstdout_printf (state_data->pstate, "unknown NIC status: %Xh\n", bytes_rs[7]);
      break;
    }

  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_oem_inventec_set_nic_status (ipmi_oem_state_data_t *state_data)
{
  uint8_t bytes_rq[IPMI_OEM_MAX_BYTES];
  uint8_t bytes_rs[IPMI_OEM_MAX_BYTES];
  int rs_len;
  uint8_t reservation_id;
  int rv = -1;

  assert (state_data);
  assert (state_data->prog_data->args->oem_options_count == 1);

  if (strcasecmp (state_data->prog_data->args->oem_options[0], "shared")
      && strcasecmp (state_data->prog_data->args->oem_options[0], "dedicated"))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "%s:%s invalid OEM option argument '%s'\n",
                       state_data->prog_data->args->oem_id,
                       state_data->prog_data->args->oem_command,
                       state_data->prog_data->args->oem_options[0]);
      goto cleanup;
    }

  /* Inventec OEM
   *
   * Set NIC Status Request
   *
   * 0x30 - OEM network function
   * 0x03 - OEM cmd
   * 0x?? - Reservation ID
   * 0x02 - Configuration ID (0x02 = ??)
   * 0x01 - Attribute ID (0x01 = ??)
   * 0x00 - Index (unused here??)
   * 0x00 - Data Offset - LSB (unused here??)
   * 0x00 = Data Offset - MSB (unused here??)
   * 0x01 - In progress bit (0x00 in progress, 0x01 - last config in this request)
   * 0x00 | 0x01 - 0x00 = shared, 0x01 = dedicated
   * 
   * Set NIC Status Response
   *
   * 0x03 - OEM cmd
   * 0x?? - Completion Code
   * 0x?? - bytes written
   */

  /* Dell Xanadu2 OEM
   *
   * achu: Dell appears to have also implemented an additional OEM
   * command that duplicates this behavior.  Currently, we do not
   * implement the Dell equivalent in ipmi-oem.
   *
   * Set LAN Source Request
   *
   * 0x34 - OEM network function
   * 0x13 - OEM cmd
   * 0x?? - LAN Source
   *      - 00h = shared, 01h = dedicated
   *
   * Set LAN Source Response
   *
   * 0x13 - OEM cmd
   * 0x?? - Completion Code
   * 0x?? - LAN Source Setting
   */

  if (_inventec_get_reservation (state_data,
                                 &reservation_id) < 0)
    goto cleanup;

  bytes_rq[0] = IPMI_CMD_OEM_INVENTEC_SET_EXTENDED_CONFIGURATION;
  bytes_rq[1] = reservation_id;
  bytes_rq[2] = IPMI_OEM_EXTENDED_CONFIGURATION_ID_LAN;
  bytes_rq[3] = IPMI_OEM_EXTENDED_ATTRIBUTE_ID_LAN_NIC_STATUS;
  bytes_rq[4] = 0x00;
  bytes_rq[5] = 0x00;
  bytes_rq[6] = 0x00;
  bytes_rq[7] = 0x01;

  if (!strcasecmp (state_data->prog_data->args->oem_options[0], "shared"))
    bytes_rq[8] = IPMI_OEM_EXTENDED_CONFIG_LAN_NIC_STATUS_SHARED;
  else
    bytes_rq[8] = IPMI_OEM_EXTENDED_CONFIG_LAN_NIC_STATUS_DEDICATED;

  if ((rs_len = ipmi_cmd_raw (state_data->ipmi_ctx,
                              0, /* lun */
                              IPMI_NET_FN_OEM_INVENTEC_GENERIC_RQ, /* network function */
                              bytes_rq, /* data */
                              9, /* num bytes */
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
                                                   2, /* don't care about the 3rd byte, don't know what it is used for */
                                                   IPMI_CMD_OEM_INVENTEC_SET_EXTENDED_CONFIGURATION,
                                                   IPMI_NET_FN_OEM_INVENTEC_GENERIC_RS) < 0)
    goto cleanup;

  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_oem_inventec_get_mac_address (ipmi_oem_state_data_t *state_data)
{
  fiid_obj_t obj_cmd_rs = NULL;
  char mac_address_str[IPMI_OEM_MAX_MACADDRLEN+1];
  uint8_t mac_address_bytes[6];
  uint8_t lan_channel_number;
  int rv = -1;

  assert (state_data);
  assert (!state_data->prog_data->args->oem_options_count);

  /* Use normal IPMI to get the MAC address.  This is offered more as
   * a convenience to the user rather, so there is always a "get" and
   * a "set" command in ipmi-oem.
   */

  if (ipmi_get_channel_number (state_data->ipmi_ctx,
                               IPMI_CHANNEL_MEDIUM_TYPE_LAN_802_3,
                               &lan_channel_number) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_get_channel_number: %s\n",
                       ipmi_ctx_errormsg (state_data->ipmi_ctx));
      goto cleanup;
    }

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_get_lan_configuration_parameters_mac_address_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }
  
  if (ipmi_cmd_get_lan_configuration_parameters_mac_address (state_data->ipmi_ctx,
                                                             lan_channel_number,
                                                             IPMI_GET_LAN_PARAMETER,
                                                             IPMI_OEM_SET_SELECTOR,
                                                             IPMI_OEM_BLOCK_SELECTOR,
                                                             obj_cmd_rs) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_cmd_get_lan_configuration_parameters_mac_address: %s\n",
                       ipmi_ctx_errormsg (state_data->ipmi_ctx));
      goto cleanup;
    }

  if (fiid_obj_get_data (obj_cmd_rs,
                         "mac_address",
                         mac_address_bytes,
                         6) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get_data: 'mac_address': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  
  memset (mac_address_str, '\0', IPMI_OEM_MAX_MACADDRLEN+1);
  snprintf (mac_address_str,
            IPMI_OEM_MAX_MACADDRLEN,
            "%02X:%02X:%02X:%02X:%02X:%02X",
            mac_address_bytes[0],
            mac_address_bytes[1],
            mac_address_bytes[2],
            mac_address_bytes[3],
            mac_address_bytes[4],
            mac_address_bytes[5]);

  pstdout_printf (state_data->pstate,
                  "%s\n",
                  mac_address_str);

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

int
ipmi_oem_inventec_set_mac_address (ipmi_oem_state_data_t *state_data)
{
  uint8_t bytes_rq[IPMI_OEM_MAX_BYTES];
  uint8_t bytes_rs[IPMI_OEM_MAX_BYTES];
  int rs_len;
  unsigned int tmp;
  uint8_t cmd;
  int rv = -1;

  assert (state_data);
  assert (state_data->prog_data->args->oem_options_count == 2);

  if (strcasecmp (state_data->prog_data->args->oem_options[0], "shared")
      && strcasecmp (state_data->prog_data->args->oem_options[0], "dedicated"))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "%s:%s invalid OEM option argument '%s'\n",
                       state_data->prog_data->args->oem_id,
                       state_data->prog_data->args->oem_command,
                       state_data->prog_data->args->oem_options[0]);
      goto cleanup;
    }

  if (sscanf (state_data->prog_data->args->oem_options[1],
              "%02x:%02x:%02x:%02x:%02x:%02x",
              &tmp,
              &tmp,
              &tmp,
              &tmp,
              &tmp,
              &tmp) != 6)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "%s:%s invalid OEM option argument '%s'\n",
                       state_data->prog_data->args->oem_id,
                       state_data->prog_data->args->oem_command,
                       state_data->prog_data->args->oem_options[1]);
      goto cleanup;
    }

  /* Inventec OEM
   *
   * Set MAC Address Request
   *
   * 0x2E - OEM network function (is IPMI_NET_FN_OEM_GROUP_RQ)
   * 0x21 | 0x23 - OEM cmd - 0x21 = dedicated, 0x23 = shared
   * bytes 1-17: MAC address in ASCII (including semicolons)
   * 0x00 - sentinel value 0x00
   * 
   * Set MAC Address Response
   *
   * 0x21 | 0x23 - OEM cmd - 0x21 = dedicated, 0x23 = shared
   * 0x?? - Completion Code
   */

  if (!strcasecmp (state_data->prog_data->args->oem_options[0], "dedicated"))
    cmd = IPMI_CMD_OEM_INVENTEC_SET_DEDICATED_MAC_ADDRESS;
  else
    cmd = IPMI_CMD_OEM_INVENTEC_SET_SHARED_MAC_ADDRESS;
  
  bytes_rq[0] = cmd;
  memcpy (&bytes_rq[1],
          state_data->prog_data->args->oem_options[1],
          17);
  bytes_rq[18] = 0x00;
  
  if ((rs_len = ipmi_cmd_raw (state_data->ipmi_ctx,
                              0, /* lun */
                              IPMI_NET_FN_OEM_GROUP_RQ, /* network function */
                              bytes_rq, /* data */
                              19, /* num bytes */
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
                                                   cmd,
                                                   IPMI_NET_FN_OEM_GROUP_RQ) < 0)
    goto cleanup;
  
  rv = 0;
 cleanup:
  return (rv);
}

static int
_inventec_get_bmc_services (ipmi_oem_state_data_t *state_data,
                            uint8_t *services)
{
  uint8_t bytes_rq[IPMI_OEM_MAX_BYTES];
  uint8_t bytes_rs[IPMI_OEM_MAX_BYTES];
  int rs_len;
  uint8_t reservation_id;
  int rv = -1;

  assert (state_data);
  assert (services);

  /* Inventec OEM
   *
   * Get BMC Services Request
   *
   * 0x30 - OEM network function
   * 0x02 - OEM cmd
   * 0x?? - Reservation ID
   * 0x04 - Configuration ID (0x04 = Security)
   * 0x01 - Attribute ID (0x01 = Service Disabled)
   * 0x00 - Index (unused here)
   * 0x00 - Data Offset - LSB (unused here)
   * 0x00 = Data Offset - MSB (unused here)
   * 0xFF - Bytes to read (0xFF = all)
   * 
   * Get BMC Services Response
   *
   * 0x03 - OEM cmd
   * 0x?? - Completion Code
   * 0x04 - Configuration ID (0x04 = Security)
   * 0x01 - Attribute ID (0x01 = Service Disabled)
   * 0x00 - Index (unused here)
   * 0x01 - number of bytes returned
   * 0xXX - services
   *
   * services bit 0 : All services except IPMI disabled
   * services bit 1 : KVM/Virtual Storage disabled
   * services bit 2 : HTTP/HTTPS disabled
   * services bit 3 : SSH/Telnet disabled
   */

  if (_inventec_get_reservation (state_data,
                                 &reservation_id) < 0)
    goto cleanup;

  bytes_rq[0] = IPMI_CMD_OEM_INVENTEC_GET_EXTENDED_CONFIGURATION;
  bytes_rq[1] = reservation_id;
  bytes_rq[2] = IPMI_OEM_EXTENDED_CONFIGURATION_ID_SECURITY;
  bytes_rq[3] = IPMI_OEM_EXTENDED_ATTRIBUTE_ID_SECURITY_SERVICE_DISABLED;
  bytes_rq[4] = 0x00;
  bytes_rq[5] = 0x00;
  bytes_rq[6] = 0x00;
  bytes_rq[7] = IPMI_OEM_EXTENDED_CONFIG_READ_ALL_BYTES;

  if ((rs_len = ipmi_cmd_raw (state_data->ipmi_ctx,
                              0, /* lun */
                              IPMI_NET_FN_OEM_INVENTEC_GENERIC_RQ, /* network function */
                              bytes_rq, /* data */
                              8, /* num bytes */
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
                                                   7,
                                                   IPMI_CMD_OEM_INVENTEC_GET_EXTENDED_CONFIGURATION,
                                                   IPMI_NET_FN_OEM_INVENTEC_GENERIC_RS) < 0)
    goto cleanup;

  (*services) = bytes_rs[6];
  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_oem_inventec_get_bmc_services (ipmi_oem_state_data_t *state_data)
{
  uint8_t services = 0;
  int rv = -1;

  assert (state_data);
  assert (!state_data->prog_data->args->oem_options_count);

  if (_inventec_get_bmc_services (state_data, &services) < 0)
    goto cleanup;

  if (services)
    {
      /* achu: it is not clear if only one bit or multiple bits can be
       * set.  I'm assuming if the "all" bit is set, there is no need
       * to output anything else.
       */
      if (services & IPMI_OEM_EXTENDED_CONFIG_SECURITY_SERVICES_DISABLED_BITMASK_ALL)
        {
          pstdout_printf (state_data->pstate, "All services except IPMI disabled\n");
          goto out;
        }
      if (services & IPMI_OEM_EXTENDED_CONFIG_SECURITY_SERVICES_DISABLED_BITMASK_KVM)
        pstdout_printf (state_data->pstate, "KVM/Virtual Storage disabled\n");
      if (services & IPMI_OEM_EXTENDED_CONFIG_SECURITY_SERVICES_DISABLED_BITMASK_HTTP)
        pstdout_printf (state_data->pstate, "HTTP/HTTPS disabled\n");
      if (services & IPMI_OEM_EXTENDED_CONFIG_SECURITY_SERVICES_DISABLED_BITMASK_SSH)
        pstdout_printf (state_data->pstate, "SSH/Telnet disabled\n");
    }
  else
    pstdout_printf (state_data->pstate, "All services enabled\n");

 out:
  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_oem_inventec_set_bmc_services (ipmi_oem_state_data_t *state_data)
{
  uint8_t bytes_rq[IPMI_OEM_MAX_BYTES];
  uint8_t bytes_rs[IPMI_OEM_MAX_BYTES];
  int enable = 0;
  int rs_len;
  uint8_t reservation_id;
  int rv = -1;

  assert (state_data);
  assert (state_data->prog_data->args->oem_options_count == 2);

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

  if (strcasecmp (state_data->prog_data->args->oem_options[1], "all")
      && strcasecmp (state_data->prog_data->args->oem_options[1], "kvm")
      && strcasecmp (state_data->prog_data->args->oem_options[1], "http")
      && strcasecmp (state_data->prog_data->args->oem_options[1], "ssh"))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "%s:%s invalid OEM option argument '%s'\n",
                       state_data->prog_data->args->oem_id,
                       state_data->prog_data->args->oem_command,
                       state_data->prog_data->args->oem_options[1]);
      goto cleanup;
    }

  /* Inventec OEM
   *
   * Disable/Enable BMC Services Request
   *
   * 0x30 - OEM network function
   * 0x03 - OEM cmd
   * 0x?? - Reservation ID
   * 0x04 - Configuration ID (0x04 = Security)
   * 0x01 - Attribute ID (0x01 = Service Disabled)
   * 0x00 - Index (unused here)
   * 0x00 - Data Offset - LSB (unused here)
   * 0x00 = Data Offset - MSB (unused here)
   * 0x01 - Bytes to read
   * 0xXX - 0x00 - enable all
   *        0x01 - disable all except IPMI
   *        0x02 - disable KVM/Virtual Storage
   *        0x04 - disable HTTP/HTTPS
   *        0x08 - disable SSH/Telent
   *
   * Disable BMC Services Response
   *
   * 0x03 - OEM cmd
   * 0x?? - Completion Code
   */

  /* achu: do bytes_rq[8] first, b/c we may call
   * _inventec_get_bmc_services, which does a get reservation id call
   * too.
   */

  if (!strcasecmp (state_data->prog_data->args->oem_options[0], "enable"))
    enable = 1;
        
  /* if all, it's an easy special case */
  if (!strcasecmp (state_data->prog_data->args->oem_options[1], "all"))
    {

      if (enable)
        bytes_rq[8] = IPMI_OEM_EXTENDED_CONFIG_SECURITY_SERVICES_DISABLED_ENABLE_ALL;
      else
        bytes_rq[8] = IPMI_OEM_EXTENDED_CONFIG_SECURITY_SERVICES_DISABLED_BITMASK_ALL;
    }
  else
    {
      uint8_t services = 0;

      if (_inventec_get_bmc_services (state_data, &services) < 0)
        goto cleanup;

      if (enable && (services & IPMI_OEM_EXTENDED_CONFIG_SECURITY_SERVICES_DISABLED_BITMASK_ALL))
        {
          /* clear out "all" bit, and replace with remaining bits */
          services &= (~IPMI_OEM_EXTENDED_CONFIG_SECURITY_SERVICES_DISABLED_BITMASK_ALL);
          services |= IPMI_OEM_EXTENDED_CONFIG_SECURITY_SERVICES_DISABLED_BITMASK_KVM;
          services |= IPMI_OEM_EXTENDED_CONFIG_SECURITY_SERVICES_DISABLED_BITMASK_HTTP;
          services |= IPMI_OEM_EXTENDED_CONFIG_SECURITY_SERVICES_DISABLED_BITMASK_SSH;
        }

      if (!strcasecmp (state_data->prog_data->args->oem_options[1], "kvm"))
        {
          if (enable)
            services &= (~IPMI_OEM_EXTENDED_CONFIG_SECURITY_SERVICES_DISABLED_BITMASK_KVM);
          else
            services |= IPMI_OEM_EXTENDED_CONFIG_SECURITY_SERVICES_DISABLED_BITMASK_KVM;
        }
      else if (!strcasecmp (state_data->prog_data->args->oem_options[1], "http"))
        {
          if (enable)
            services &= (~IPMI_OEM_EXTENDED_CONFIG_SECURITY_SERVICES_DISABLED_BITMASK_HTTP);
          else
            services |= IPMI_OEM_EXTENDED_CONFIG_SECURITY_SERVICES_DISABLED_BITMASK_HTTP;
        }
      else /* !strcasecmp (state_data->prog_data->args->oem_options[1], "ssh") */
        {
          if (enable)
            services &= (~IPMI_OEM_EXTENDED_CONFIG_SECURITY_SERVICES_DISABLED_BITMASK_SSH);
          else
            services |= IPMI_OEM_EXTENDED_CONFIG_SECURITY_SERVICES_DISABLED_BITMASK_SSH;
        }

      bytes_rq[8] = services;
    }

  if (_inventec_get_reservation (state_data,
                                 &reservation_id) < 0)
    goto cleanup;

  bytes_rq[0] = IPMI_CMD_OEM_INVENTEC_SET_EXTENDED_CONFIGURATION;
  bytes_rq[1] = reservation_id;
  bytes_rq[2] = IPMI_OEM_EXTENDED_CONFIGURATION_ID_SECURITY;
  bytes_rq[3] = IPMI_OEM_EXTENDED_ATTRIBUTE_ID_SECURITY_SERVICE_DISABLED;
  bytes_rq[4] = 0x00;
  bytes_rq[5] = 0x00;
  bytes_rq[6] = 0x00;
  bytes_rq[7] = 0x01;

  if ((rs_len = ipmi_cmd_raw (state_data->ipmi_ctx,
                              0, /* lun */
                              IPMI_NET_FN_OEM_INVENTEC_GENERIC_RQ, /* network function */
                              bytes_rq, /* data */
                              9, /* num bytes */
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
                                                   IPMI_CMD_OEM_INVENTEC_SET_EXTENDED_CONFIGURATION,
                                                   IPMI_NET_FN_OEM_INVENTEC_GENERIC_RS) < 0)
    goto cleanup;
  
  rv = 0;
 cleanup:
  return (rv);
}

#if 0
/* cannot verify */
int
ipmi_oem_inventec_restore_to_defaults (ipmi_oem_state_data_t *state_data)
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
  
  /* Inventec OEM
   *
   * Request Restore To Defaults
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
   * Response Restore To Defaults
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

  bytes_rq[0] = IPMI_CMD_OEM_INVENTEC_RESTORE_TO_DEFAULTS;
  
  bytes_rq[1] = IPMI_OEM_INVENTEC_RESTORE_TO_DEFAULTS_RESTORE_FLAG_RESTORE_PARAMETERS_NOT_INCLUDED_BELOW;
  bytes_rq[1] <<= IPMI_OEM_INVENTEC_RESTORE_TO_DEFAULTS_RESTORE_FLAG_SHIFT;
  
  if (!strcasecmp (state_data->prog_data->args->oem_options[0], "all"))
    {
      bytes_rq[1] |= IPMI_OEM_INVENTEC_RESTORE_TO_DEFAULTS_USER_ACCOUNTS_BITMASK;
      bytes_rq[1] |= IPMI_OEM_INVENTEC_RESTORE_TO_DEFAULTS_LAN_CONFIGURATION_BITMASK;
      bytes_rq[1] |= IPMI_OEM_INVENTEC_RESTORE_TO_DEFAULTS_SOL_CONFIGURATION_BITMASK;
      bytes_rq[1] |= IPMI_OEM_INVENTEC_RESTORE_TO_DEFAULTS_SERIAL_CONFIGURATION_BITMASK;
      bytes_rq[1] |= IPMI_OEM_INVENTEC_RESTORE_TO_DEFAULTS_PEF_BITMASK;
    }
  else if (!strcasecmp (state_data->prog_data->args->oem_options[0], "user"))
    bytes_rq[1] |= IPMI_OEM_INVENTEC_RESTORE_TO_DEFAULTS_USER_ACCOUNTS_BITMASK;
  else if (!strcasecmp (state_data->prog_data->args->oem_options[0], "lan"))
    bytes_rq[1] |= IPMI_OEM_INVENTEC_RESTORE_TO_DEFAULTS_LAN_CONFIGURATION_BITMASK;
  else if (!strcasecmp (state_data->prog_data->args->oem_options[0], "sol"))
    bytes_rq[1] |= IPMI_OEM_INVENTEC_RESTORE_TO_DEFAULTS_SOL_CONFIGURATION_BITMASK;
  else if (!strcasecmp (state_data->prog_data->args->oem_options[0], "serial"))
    bytes_rq[1] |= IPMI_OEM_INVENTEC_RESTORE_TO_DEFAULTS_SERIAL_CONFIGURATION_BITMASK;
  else  /* !strcasecmp (state_data->prog_data->args->oem_options[0], "pef" */
    bytes_rq[1] |= IPMI_OEM_INVENTEC_RESTORE_TO_DEFAULTS_PEF_BITMASK;
           
  if ((rs_len = ipmi_cmd_raw (state_data->ipmi_ctx,
                              0, /* lun */
                              IPMI_NET_FN_OEM_INVENTEC_GENERIC_RQ, /* network function */
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
                                                   IPMI_CMD_OEM_INVENTEC_RESTORE_TO_DEFAULTS,
                                                   IPMI_NET_FN_OEM_INVENTEC_GENERIC_RS) < 0)
    goto cleanup;
  
  task_id = bytes_rs[2];
  
  /* don't quit until it is done */
  while (1)
    {
      bytes_rq[0] = IPMI_CMD_OEM_INVENTEC_GET_RESTORE_STATUS;
      bytes_rq[1] = task_id;
      
      if ((rs_len = ipmi_cmd_raw (state_data->ipmi_ctx,
				  0, /* lun */
				  IPMI_NET_FN_OEM_INVENTEC_GENERIC_RQ, /* network function */
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
						       IPMI_CMD_OEM_INVENTEC_RESTORE_TO_DEFAULTS,
						       IPMI_NET_FN_OEM_INVENTEC_GENERIC_RS) < 0)
	goto cleanup;

      if (bytes_rs[2] == IPMI_OEM_INVENTEC_GET_RESTORE_STATUS_RESTORE_COMPLETE)
	break;

      sleep (1);
    }
  
  rv = 0;
 cleanup:
  return (rv);
}
#endif

#if 0
/* cannot verify */
int
ipmi_oem_inventec_set_system_guid (ipmi_oem_state_data_t *state_data)
{
  uint8_t bytes_rq[IPMI_OEM_MAX_BYTES];
  uint8_t bytes_rs[IPMI_OEM_MAX_BYTES];
  int rs_len;
  int rv = -1;
  int i;

  assert (state_data);
  assert (state_data->prog_data->args->oem_options_count == 1);

  if (strlen (state_data->prog_data->args->oem_options[0]) != (IPMI_SYSTEM_GUID_LENGTH * 2))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "%s:%s OEM option argument '%s' invalid length, must be %u long\n",
                       state_data->prog_data->args->oem_id,
                       state_data->prog_data->args->oem_command,
                       state_data->prog_data->args->oem_options[0],
                       (IPMI_SYSTEM_GUID_LENGTH * 2));
      goto cleanup;
    }
  
  for (i = 0; i < (IPMI_SYSTEM_GUID_LENGTH * 2); i++)
    {
      if (!isxdigit (state_data->prog_data->args->oem_options[0][i]))
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "%s:%s OEM option argument '%s' contains invalid hex code\n",
                           state_data->prog_data->args->oem_id,
                           state_data->prog_data->args->oem_command,
                           state_data->prog_data->args->oem_options[0]);
          goto cleanup;
        }
    }

  /* Inventec OEM
   *
   * Set System GUID Request
   *
   * 0x30 - OEM network function
   * 0xB3 - OEM cmd
   * bytes 1-16: System GUID
   *
   * Set System GUID Response
   *
   * 0xB3 - OEM cmd
   * 0x?? - Completion Code
   */

  bytes_rq[0] = IPMI_CMD_OEM_INVENTEC_SET_SYSTEM_GUID;
  for (i = 0; i < IPMI_SYSTEM_GUID_LENGTH; i++)
    {
      char strbuf[IPMI_OEM_BUFLEN];
      long val;
      
      /* achu: there *must* be something faster than this, I just
       * can't find the magic lib call to do 1-char to 1-hex.  All the
       * strxxx() functions take NUL terminated strings.
       */

      memset (strbuf, '\0', IPMI_OEM_BUFLEN);
      strbuf[0] = state_data->prog_data->args->oem_options[0][i];
      val = strtol (strbuf, NULL, IPMI_OEM_HEX_BASE);
      bytes_rq[1 + i] |= (val & 0x0F);

      memset (strbuf, '\0', IPMI_OEM_BUFLEN);
      strbuf[0] = state_data->prog_data->args->oem_options[0][i + 1];
      val = strtol (strbuf, NULL, IPMI_OEM_HEX_BASE);
      bytes_rq[1 + i] |= ((val << 4) & 0xF0);
    }

  if ((rs_len = ipmi_cmd_raw (state_data->ipmi_ctx,
                              0, /* lun */
                              IPMI_NET_FN_OEM_INVENTEC_GENERIC_RQ, /* network function */
                              bytes_rq, /* data */
                              17, /* num bytes */
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
                                                   IPMI_CMD_OEM_INVENTEC_SET_SYSTEM_GUID,
                                                   IPMI_NET_FN_OEM_INVENTEC_GENERIC_RS) < 0)
    goto cleanup;
  
  rv = 0;
 cleanup:
  return (rv);
}
#endif

static int
_ipmi_oem_inventec_read_eeprom_at24c256n (ipmi_oem_state_data_t *state_data)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint8_t data_rq[IPMI_OEM_MAX_BYTES];
  uint8_t data_rs[IPMI_OEM_MAX_BYTES];
  unsigned int read_count = 0;
  int len;
  int rv = -1;

  assert (state_data);
  assert (state_data->prog_data->args->oem_options_count == 1);

  /* Uses Master-Read Write Command
   *
   * Most addresses provided directly from Dell.
   *
   * byte 1 = 5 (channel = 0, bus id = 2, bus-type = 1 = private)
   * byte 2 = 0xA6 (slave address 7 bit = 0x53, lowest bit for r/w, 0b = read, 1b = write)
   * byte 3 = read count, we'll use 1
   * byte 4/5 - address to read, msb first
   * 
   * response
   *
   * byte 1 = comp-code
   * byte N = read data
   *
   * address ranges from 0x0000 - 0x7fff
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
  
  while (read_count <= IPMI_OEM_EEPROM_AT24C256N_ADDRESS_MAX)
    {
      data_rq[0] = (read_count & 0xFF00) >> 8;
      data_rq[1] = (read_count & 0x00FF);
      
      if (ipmi_cmd_master_write_read (state_data->ipmi_ctx,
                                      IPMI_BUS_TYPE_PRIVATE,
                                      IPMI_OEM_EEPROM_AT24C256N_BUS_ID,
                                      0,
                                      IPMI_OEM_EEPROM_AT24C256N_SLAVE_ADDRESS,
                                      1,
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

      if ((len = fiid_obj_get_data (obj_cmd_rs,
                                    "data",
                                    data_rs,
                                    IPMI_OEM_MAX_BYTES)) < 0)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "fiid_obj_get_data: %s\n",
                           fiid_obj_errormsg (obj_cmd_rs));
          goto cleanup;
        }
      
      if (len)
        {
          int i;
          
          for (i = read_count; i < (read_count + len); i++)
            {
              if (i && (i % 8) == 0)
                pstdout_printf (state_data->pstate, "\n");
              
              pstdout_printf (state_data->pstate, "0x%02X ", data_rs[i - read_count]);
            }
          
          read_count += len;
        }
    }

  pstdout_printf (state_data->pstate, "\n");
  
  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

int
ipmi_oem_inventec_read_eeprom (ipmi_oem_state_data_t *state_data)
{
  assert (state_data);
  assert (state_data->prog_data->args->oem_options_count == 1);

  if (!strcasecmp (state_data->prog_data->args->oem_options[0], "at24c256n"))
    return _ipmi_oem_inventec_read_eeprom_at24c256n (state_data);
  
  pstdout_fprintf (state_data->pstate,
                   stderr,
                   "%s:%s invalid OEM option argument '%s'\n",
                   state_data->prog_data->args->oem_id,
                   state_data->prog_data->args->oem_command,
                   state_data->prog_data->args->oem_options[0]);
  return (-1);
}

static int
_ipmi_oem_inventec_clear_eeprom_at24c256n (ipmi_oem_state_data_t *state_data)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint8_t data_rq[IPMI_OEM_MAX_BYTES];
  unsigned int count = 0;
  unsigned int percent = 0;
  int rv = -1;

  assert (state_data);
  assert (state_data->prog_data->args->oem_options_count == 1);

  /* Uses Master-Read Write Command
   *
   * Most addresses provided directly from Dell.
   *
   * byte 1 = 5 (channel = 0, bus id = 2, bus-type = 1 = private)
   * byte 2 = 0xA6 (slave address 7 bit = 0x53, lowest bit for r/w, 0b = read, 1b = write)
   * byte 3 = read count, 0 to write
   * byte 4/5 - address to read, msb first
   * byte 6 - data to write
   * 
   * response
   *
   * byte 1 = comp-code
   * byte N = read data
   *
   * address ranges from 0x0000 - 0x7fff
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

  if (state_data->prog_data->args->verbose_count)
    fprintf (stderr, "%u%%\r", percent);

  while (count <= IPMI_OEM_EEPROM_AT24C256N_ADDRESS_MAX)
    {
      data_rq[0] = (count & 0xFF00) >> 8;
      data_rq[1] = (count & 0x00FF);
      data_rq[2] = IPMI_OEM_EEPROM_AT24C256N_CLEAR_BYTE;

      if (ipmi_cmd_master_write_read (state_data->ipmi_ctx,
                                      IPMI_BUS_TYPE_PRIVATE,
                                      IPMI_OEM_EEPROM_AT24C256N_BUS_ID,
                                      0,
                                      IPMI_OEM_EEPROM_AT24C256N_SLAVE_ADDRESS,
                                      0,
                                      data_rq,
                                      3,
                                      obj_cmd_rs) < 0)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "ipmi_cmd_master_write_read: %s\n",
                           ipmi_ctx_errormsg (state_data->ipmi_ctx));
          goto cleanup;
        }

      if (state_data->prog_data->args->verbose_count)
        {
          if ((unsigned int)(((double)count/IPMI_OEM_EEPROM_AT24C256N_ADDRESS_MAX) * 100) > percent)
            {
              fprintf (stderr, "%u%%\r", percent);
              percent++;
            }
        }

      count++;
    }

  if (state_data->prog_data->args->verbose_count)
    fprintf (stderr, "100%%\r\n");

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

int
ipmi_oem_inventec_clear_eeprom (ipmi_oem_state_data_t *state_data)
{
  assert (state_data);
  assert (state_data->prog_data->args->oem_options_count == 1);

  if (!strcasecmp (state_data->prog_data->args->oem_options[0], "at24c256n"))
    return _ipmi_oem_inventec_clear_eeprom_at24c256n (state_data);
  
  pstdout_fprintf (state_data->pstate,
                   stderr,
                   "%s:%s invalid OEM option argument '%s'\n",
                   state_data->prog_data->args->oem_id,
                   state_data->prog_data->args->oem_command,
                   state_data->prog_data->args->oem_options[0]);
  return (-1);
}
