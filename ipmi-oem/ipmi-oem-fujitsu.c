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
#include <ctype.h>
#endif /* STDC_HEADERS */
#if HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */
#if TIME_WITH_SYS_TIME
#include <sys/time.h>
#include <time.h>
#else  /* !TIME_WITH_SYS_TIME */
#if HAVE_SYS_TIME_H
#include <sys/time.h>
#else /* !HAVE_SYS_TIME_H */
#include <time.h>
#endif  /* !HAVE_SYS_TIME_H */
#endif /* !TIME_WITH_SYS_TIME */
#include <limits.h>
#include <assert.h>

#include <freeipmi/freeipmi.h>

#include "ipmi-oem.h"
#include "ipmi-oem-argp.h"
#include "ipmi-oem-common.h"
#include "ipmi-oem-fujitsu.h"

#include "tool-oem-common.h"
#include "tool-util-common.h"

#include "freeipmi-portability.h"
#include "pstdout.h"

static int
_ipmi_oem_get_power_source (ipmi_oem_state_data_t *state_data,
                            uint8_t command_specifier,
                            uint8_t *source)
{
  uint8_t bytes_rq[IPMI_OEM_MAX_BYTES];
  uint8_t bytes_rs[IPMI_OEM_MAX_BYTES];
  int rs_len;
  int rv = -1;

  assert (state_data);
  assert (command_specifier == IPMI_OEM_FUJITSU_COMMAND_SPECIFIER_GET_POWER_ON_SOURCE
          || command_specifier == IPMI_OEM_FUJITSU_COMMAND_SPECIFIER_GET_POWER_OFF_SOURCE);
  assert (source);

  /* Fujitsu OEM
   * 
   * http://manuals.ts.fujitsu.com/file/4390/irmc_s2-en.pdf
   *
   * Request
   *
   * 0x2E - OEM network function
   * 0x01 - OEM cmd
   * 0x?? - Fujitsu IANA (LSB first)
   * 0x?? - Fujitsu IANA
   * 0x?? - Fujitsu IANA
   * 0x?? - Command Specifier
   *
   * Response
   *
   * 0x01 - OEM cmd
   * 0x?? - Completion code
   * 0x?? - Fujitsu IANA (LSB first)
   * 0x?? - Fujitsu IANA
   * 0x?? - Fujitsu IANA
   * 0x01 - data length
   * 0x?? - power on/off source
   */
  
  bytes_rq[0] = IPMI_CMD_OEM_FUJITSU_POWER;
  bytes_rq[1] = (IPMI_IANA_ENTERPRISE_ID_FUJITSU & 0x0000FF);
  bytes_rq[2] = (IPMI_IANA_ENTERPRISE_ID_FUJITSU & 0x00FF00) >> 8;
  bytes_rq[3] = (IPMI_IANA_ENTERPRISE_ID_FUJITSU & 0xFF0000) >> 16;
  bytes_rq[4] = command_specifier;

  if ((rs_len = ipmi_cmd_raw (state_data->ipmi_ctx,
                              0, /* lun */
                              IPMI_NET_FN_OEM_GROUP_RQ, /* network function */
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
                                                   7,
                                                   IPMI_CMD_OEM_FUJITSU_POWER,
                                                   IPMI_NET_FN_OEM_GROUP_RS,
                                                   NULL) < 0)
    goto cleanup;

  (*source) = bytes_rs[6];

  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_oem_fujitsu_get_power_on_source (ipmi_oem_state_data_t *state_data)
{
  uint8_t source = 0;
  char str[IPMI_OEM_STR_BUFLEN];
  int rv = -1;

  assert (state_data);
  assert (!state_data->prog_data->args->oem_options_count);

  if (_ipmi_oem_get_power_source (state_data,
                                  IPMI_OEM_FUJITSU_COMMAND_SPECIFIER_GET_POWER_ON_SOURCE,
                                  &source) < 0)
    goto cleanup;

  memset (str, '\0', IPMI_OEM_STR_BUFLEN);

  switch (source)
    {
    case IPMI_OEM_FUJITSU_POWER_ON_SOURCE_SOFTWARE_OR_COMMAND:
      snprintf (str, IPMI_OEM_STR_BUFLEN, "Software or command");
      break;
    case IPMI_OEM_FUJITSU_POWER_ON_SOURCE_POWER_SWITCH:
      snprintf (str, IPMI_OEM_STR_BUFLEN, "Power switch");
      break;
    case IPMI_OEM_FUJITSU_POWER_ON_SOURCE_AUTOMATIC_RESTART_AFTER_POWER_FAILURE:
      snprintf (str, IPMI_OEM_STR_BUFLEN, "Automatic restart after power failure");
      break;
    case IPMI_OEM_FUJITSU_POWER_ON_SOURCE_CLOCK_OR_TIMER:
      snprintf (str, IPMI_OEM_STR_BUFLEN, "Clock or timer");
      break;
    case IPMI_OEM_FUJITSU_POWER_ON_SOURCE_AUTOMATIC_RESTART_AFTER_FAN_FAILURE_SHUTDOWN:
      snprintf (str, IPMI_OEM_STR_BUFLEN, "Automatic restart after fan failure shutdown");
      break;
    case IPMI_OEM_FUJITSU_POWER_ON_SOURCE_AUTOMATIC_RESTART_AFTER_CRITICAL_TEMPERATURE_SHUTDOWN:
      snprintf (str, IPMI_OEM_STR_BUFLEN, "Automatic restart after critical temperature shutdown");
      break;
    case IPMI_OEM_FUJITSU_POWER_ON_SOURCE_REBOOT_AFTER_WATCHDOG_TIMEOUT:
      snprintf (str, IPMI_OEM_STR_BUFLEN, "Reboot after watchdog timeout");
      break;
    case IPMI_OEM_FUJITSU_POWER_ON_SOURCE_REMOTE_ON:
      snprintf (str, IPMI_OEM_STR_BUFLEN, "Remote on");
      break;
    case IPMI_OEM_FUJITSU_POWER_ON_SOURCE_REBOOT_AFTER_A_CPU_ERROR:
      snprintf (str, IPMI_OEM_STR_BUFLEN, "Reboot after a CPU error");
      break;
    case IPMI_OEM_FUJITSU_POWER_ON_SOURCE_REBOOT_BY_HARDWARE_RESET:
      snprintf (str, IPMI_OEM_STR_BUFLEN, "Reboot by hardware reset");
      break;
    case IPMI_OEM_FUJITSU_POWER_ON_SOURCE_REBOOT_AFTER_WARM_START:
      snprintf (str, IPMI_OEM_STR_BUFLEN, "Reboot after warm start");
      break;
    case IPMI_OEM_FUJITSU_POWER_ON_SOURCE_POWERED_ON_BY_A_PCI_BUS_POWER_MANAGEMENT_EVENT:
      snprintf (str, IPMI_OEM_STR_BUFLEN, "Powered on by a PCI Bus Power Management Event");
      break;
    case IPMI_OEM_FUJITSU_POWER_ON_SOURCE_POWERED_ON_BY_REMOTE_CONTROL_VIA_REMOTE_MANAGER:
      /* HLiebig: capitalized "remote manager" from doc */
      snprintf (str, IPMI_OEM_STR_BUFLEN, "Powered on by remote control via Remote Manager");
      break;
    case IPMI_OEM_FUJITSU_POWER_ON_SOURCE_REBOOT_RESET_BY_REMOTE_CONTROL_VIA_REMOTE_MANAGER:
      /* HLiebig: capitalized "remote manager" from doc */
      snprintf (str, IPMI_OEM_STR_BUFLEN, "Reboot/reset by remote control via Remote Manager");
      break;
    default:
      snprintf (str, IPMI_OEM_STR_BUFLEN, "Unrecognized source: %02Xh", source);
    }
  
  pstdout_printf (state_data->pstate,
                  "%s\n",
                  str);

  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_oem_fujitsu_get_power_off_source (ipmi_oem_state_data_t *state_data)
{
  uint8_t source = 0;
  char str[IPMI_OEM_STR_BUFLEN];
  int rv = -1;

  assert (state_data);
  assert (!state_data->prog_data->args->oem_options_count);

  if (_ipmi_oem_get_power_source (state_data,
                                  IPMI_OEM_FUJITSU_COMMAND_SPECIFIER_GET_POWER_OFF_SOURCE,
                                  &source) < 0)
    goto cleanup;

  memset (str, '\0', IPMI_OEM_STR_BUFLEN);

  switch (source)
    {
    case IPMI_OEM_FUJITSU_POWER_OFF_SOURCE_SOFTWARE:
      snprintf (str, IPMI_OEM_STR_BUFLEN, "Software or command");
      break;
    case IPMI_OEM_FUJITSU_POWER_OFF_SOURCE_POWER_SWITCH:
      snprintf (str, IPMI_OEM_STR_BUFLEN, "Power switch");
      break;
    case IPMI_OEM_FUJITSU_POWER_OFF_SOURCE_AC_POWER_FAIL:
      snprintf (str, IPMI_OEM_STR_BUFLEN, "AC power fail");
      break;
    case IPMI_OEM_FUJITSU_POWER_OFF_SOURCE_CLOCK_OR_TIMER:
      snprintf (str, IPMI_OEM_STR_BUFLEN, "Clock or timer");
      break;
    case IPMI_OEM_FUJITSU_POWER_OFF_SOURCE_FAN_FAILURE:
      snprintf (str, IPMI_OEM_STR_BUFLEN, "Fan failure");
      break;
    case IPMI_OEM_FUJITSU_POWER_OFF_SOURCE_CRITICAL_TEMPERATURE:
      snprintf (str, IPMI_OEM_STR_BUFLEN, "Critical temperature");
      break;
    case IPMI_OEM_FUJITSU_POWER_OFF_SOURCE_FINAL_POWER_OFF_AFTER_REPEATED_WATCHDOG_TIMEOUTS:
      snprintf (str, IPMI_OEM_STR_BUFLEN, "Final power-off after repeated watchdog timeouts");
      break;
    case IPMI_OEM_FUJITSU_POWER_OFF_SOURCE_FINAL_POWER_OFF_AFTER_REPEATED_CPU_ERRORS:
      snprintf (str, IPMI_OEM_STR_BUFLEN, "Final power-off after repeated CPU errors");
      break;
    case IPMI_OEM_FUJITSU_POWER_OFF_SOURCE_POWERED_OFF_BY_REMOTE_CONTROL_VIA_REMOTE_MANAGER:
      /* HLiebig: capitalized "remote manager" from doc */
      snprintf (str, IPMI_OEM_STR_BUFLEN, "Powered off by remote control via Remote Manager");
      break;
    default:
      snprintf (str, IPMI_OEM_STR_BUFLEN, "Unrecognized source: %02Xh", source);
    }
  
  pstdout_printf (state_data->pstate,
                  "%s\n",
                  str);

  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_oem_fujitsu_get_remote_storage_status (ipmi_oem_state_data_t *state_data)
{
  uint8_t bytes_rq[IPMI_OEM_MAX_BYTES];
  uint8_t bytes_rs[IPMI_OEM_MAX_BYTES];
  int rs_len;
  uint8_t connection;
  uint8_t storage_status;
  uint8_t storage_type;
  char *storage_status_str;
  char *storage_type_str;
  long tmp;
  char *endptr;
  int rv = -1;

  assert (state_data);
  assert (state_data->prog_data->args->oem_options_count == 1);
  
  errno = 0;

  tmp = strtol (state_data->prog_data->args->oem_options[0],
                &endptr,
                0);
  if (errno
      || endptr[0] != '\0'
      || tmp < 0
      || tmp > UCHAR_MAX)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "%s:%s invalid OEM option argument '%s'\n",
                       state_data->prog_data->args->oem_id,
                       state_data->prog_data->args->oem_command,
                       state_data->prog_data->args->oem_options[0]);
      goto cleanup;
    }

  if (!(tmp >= IPMI_OEM_FUJITSU_REMOTE_STORAGE_CONNECTION_MIN
        && tmp <= IPMI_OEM_FUJITSU_REMOTE_STORAGE_CONNECTION_MAX))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "%s:%s invalid OEM option argument '%s' : out of range\n",
                       state_data->prog_data->args->oem_id,
                       state_data->prog_data->args->oem_command,
                       state_data->prog_data->args->oem_options[0]);
      goto cleanup;
    }
  connection = tmp;

  /* Fujitsu OEM
   * 
   * http://manuals.ts.fujitsu.com/file/4390/irmc_s2-en.pdf
   *
   * Request
   *
   * 0x30 - OEM network function
   * 0x19 - OEM cmd
   * 0x?? - "Command Specifier"
   *      - 0x01 - determine if storage media are connected
   *      - 0x02 - get remote storage status
   * 0x?? - no apparent use
   * 0x?? - if command specifier == 0x01
   *        - 0x00
   *        if command specifier == 0x02
   *        - 0x00 - connection 0
   *        - 0x01 - connection 2 ("2" - is this a typo in the document??)
   *
   * Response
   *
   * 0x19 - OEM cmd
   * 0x?? - Completion code
   * 0x?? - "Command Specifier"
   * 0x?? - if command specifier == 0x01
   *        - connection status
   *        - 0x00 = no, 0x01 = yes
   *      - if command specifier == 0x02
   *        - 0x00
   * 0x?? - if command specifier == 0x01
   *        - 0x00
   *      - if command specifier == 0x02
   *        - 0x00
   * 0x?? - if command specifier == 0x01
   *        - 0x00
   *      - if command specifier == 0x02
   *        - remote storage status
   * 0x?? - if command specifier == 0x01
   *        - N/A - not returned??
   *      - if command specifier == 0x02
   *        - remote storage type
   */
  
  /* achu: we won't bother checking if there are any remote
   * connections, just check the connection indicated by the user
   */

  bytes_rq[0] = IPMI_CMD_OEM_FUJITSU_GET_REMOTE_STORAGE_CONNECTION_OR_STATUS;
  bytes_rq[1] = IPMI_OEM_FUJITSU_COMMAND_SPECIFIER_REMOTE_STORAGE_STATUS;
  bytes_rq[2] = 0x00;
  bytes_rq[3] = connection;

  if ((rs_len = ipmi_cmd_raw (state_data->ipmi_ctx,
                              0, /* lun */
                              IPMI_NET_FN_OEM_FUJITSU_GENERIC_RQ, /* network function */
                              bytes_rq, /* data */
                              4, /* num bytes */
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
                                                   6,
                                                   IPMI_CMD_OEM_FUJITSU_GET_REMOTE_STORAGE_CONNECTION_OR_STATUS,
                                                   IPMI_NET_FN_OEM_FUJITSU_GENERIC_RS,
                                                   NULL) < 0)
    goto cleanup;

  storage_status = bytes_rs[4];
  storage_type = bytes_rs[5];

  switch (storage_status)
    {
    case IPMI_OEM_FUJITSU_REMOTE_STORAGE_STATUS_INVALID_UNKNOWN:
      storage_status_str = "Invalid / unknown";
      break;
    case IPMI_OEM_FUJITSU_REMOTE_STORAGE_STATUS_IDLE:
      storage_status_str = "idle";
      break;
    case IPMI_OEM_FUJITSU_REMOTE_STORAGE_STATUS_CONNECTION_ATTEMPT_PENDING:
      storage_status_str = "Connection Attempt pending";
      break;
    case IPMI_OEM_FUJITSU_REMOTE_STORAGE_STATUS_CONNECTED:
      storage_status_str = "Connected";
      break;
    case IPMI_OEM_FUJITSU_REMOTE_STORAGE_STATUS_CONNECTION_ATTEMPTS_RETRIES_EXHAUSTED_FAILED:
      storage_status_str = "Connection Attempts retries exhausted / failed";
      break;
    case IPMI_OEM_FUJITSU_REMOTE_STORAGE_STATUS_CONNECTION_LOST:
      storage_status_str = "Connection lost";
      break;
    case IPMI_OEM_FUJITSU_REMOTE_STORAGE_STATUS_DISCONNECT_PENDING:
      storage_status_str = "Disconnect pending";
      break;
    default:
      storage_status_str = "Unknown Storage Status";
    }

  pstdout_printf (state_data->pstate,
                  "Storage Status : %s\n",
                  storage_status_str);

  switch (storage_type)
    {
    case IPMI_OEM_FUJITSU_REMOTE_STORAGE_TYPE_INVALID_UNKNOWN:
      storage_type_str = "Invalid / unknown";
      break;
    case IPMI_OEM_FUJITSU_REMOTE_STORAGE_TYPE_STORAGE_SERVER_IPMI:
      storage_type_str = "Storage Server / IPMI";
      break;
    case IPMI_OEM_FUJITSU_REMOTE_STORAGE_TYPE_APPLET:
      storage_type_str = "Applet";
      break;
    case IPMI_OEM_FUJITSU_REMOTE_STORAGE_TYPE_NONE_NOT_CONNECTED:
      storage_type_str = "None / Not connected";
      break;
    default:
      storage_type_str = "Unknown Storage Type";
    }
  
  pstdout_printf (state_data->pstate,
                  "Storage Type   : %s\n",
                  storage_type_str);

  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_oem_fujitsu_get_system_status (ipmi_oem_state_data_t *state_data)
{
  uint8_t bytes_rq[IPMI_OEM_MAX_BYTES];
  uint8_t bytes_rs[IPMI_OEM_MAX_BYTES];
  uint8_t system_status;
  uint8_t signaling;
  uint8_t post_code;
  uint8_t system_power;
  uint8_t sel_entries_available;
  uint8_t watchdog_active;
  uint8_t agent_connected;
  uint8_t post_state;
  uint8_t identify_led;            /* rename from "localize" */
  uint8_t css_led;
  uint8_t global_error_led;
  char *css_led_str = NULL;
  char *global_error_led_str = NULL;
  struct timeval t;
  int rs_len;
  int rv = -1;

  assert (state_data);
  assert (!state_data->prog_data->args->oem_options_count);

  /* Fujitsu OEM
   * 
   * http://manuals.ts.fujitsu.com/file/4390/irmc_s2-en.pdf
   *
   * Request
   *
   * 0x2E - OEM network function
   * 0xF5 - OEM cmd
   * 0x?? - Fujitsu IANA (LSB first)
   * 0x?? - Fujitsu IANA
   * 0x?? - Fujitsu IANA
   * 0x10 - Command Specifier
   * 0x?? - timestamp (LSB first)
   * 0x?? - timestamp
   * 0x?? - timestamp
   * 0x?? - timestamp
   *
   * Response
   *
   * 0xF5 - OEM cmd
   * 0x?? - Completion code
   * 0x?? - Fujitsu IANA (LSB first)
   * 0x?? - Fujitsu IANA
   * 0x?? - Fujitsu IANA
   * 0x?? - System Status
   *      bit 7 - System ON
   *      bit 6 - 
   *      bit 5 - 
   *      bit 4 - SEL entries available
   *      bit 3 - 
   *      bit 2 - Watchdog active
   *      bit 1 - Agent connected
   *      bit 0 - Post State
   * 0x?? - Signaling
   *      bit 7 - Identify LED (sometimes refered as "Localize")
   *      bit 6 - 
   *      bit 5 - 
   *      bit 4 - 
   *      bit 3 - CSS LED (Customer Self Service LED)
   *      bit 2 - CSS LED (Customer Self Service LED)
   *      bit 1 - Global Error LED
   *      bit 0 - Global Error LED
   * 0x?? - Notifications
   *      bit 7 - SEL Modified (New SEL Entry)
   *      bit 6 - SEL Modified (SEL Cleared)
   *      bit 5 - SDR Modified
   *      bit 4 - Nonvolatile IPMI Variable Modified
   *      bit 3 - ConfigSpace Modified
   *      bit 2 - 
   *      bit 1 - 
   *      bit 0 - New Output on LocalView display
   * 0x?? - Last POST Code (Port 80) (HLiebig: renamed from "Post Code" in doc)
   *
   * achu: Docs say "timestamp is only relevant for evaluating the
   * Notifications Byte".  I assume this is because the Notifications
   * timestamp indicates if things have changed in the SEL/SDR.
   */
  
  if (gettimeofday (&t, NULL) < 0)
    {
      pstdout_perror (state_data->pstate, "gettimeofday");
      goto cleanup;
    }

  bytes_rq[0] = IPMI_CMD_OEM_FUJITSU_SYSTEM;
  bytes_rq[1] = (IPMI_IANA_ENTERPRISE_ID_FUJITSU & 0x0000FF);
  bytes_rq[2] = (IPMI_IANA_ENTERPRISE_ID_FUJITSU & 0x00FF00) >> 8;
  bytes_rq[3] = (IPMI_IANA_ENTERPRISE_ID_FUJITSU & 0xFF0000) >> 16;
  bytes_rq[4] = IPMI_OEM_FUJITSU_COMMAND_SPECIFIER_GET_SYSTEM_STATUS;
  bytes_rq[5] = (t.tv_sec & 0x000000FF);
  bytes_rq[6] = (t.tv_sec & 0x0000FF00) >> 8;
  bytes_rq[7] = (t.tv_sec & 0x00FF0000) >> 16;
  bytes_rq[8] = (t.tv_sec & 0xFF000000) >> 24;

  if ((rs_len = ipmi_cmd_raw (state_data->ipmi_ctx,
                              0, /* lun */
                              IPMI_NET_FN_OEM_GROUP_RQ, /* network function */
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
                                                   9,
                                                   IPMI_CMD_OEM_FUJITSU_SYSTEM,
                                                   IPMI_NET_FN_OEM_GROUP_RS,
                                                   NULL) < 0)
    goto cleanup;

  /* achu: the "Notifications" are dependent on the timestamp input,
   * we won't bother outputting them
   */
  
  system_status = bytes_rs[5];
  signaling = bytes_rs[6];
  post_code = bytes_rs[8];

  system_power = (system_status & IPMI_OEM_FUJITSU_SYSTEM_STATUS_SYSTEM_POWER_BITMASK);
  system_power >>= IPMI_OEM_FUJITSU_SYSTEM_STATUS_SYSTEM_POWER_SHIFT;

  sel_entries_available = (system_status & IPMI_OEM_FUJITSU_SYSTEM_STATUS_SEL_ENTRIES_AVAILABLE_BITMASK);
  sel_entries_available >>= IPMI_OEM_FUJITSU_SYSTEM_STATUS_SEL_ENTRIES_AVAILABLE_SHIFT;

  watchdog_active = (system_status & IPMI_OEM_FUJITSU_SYSTEM_STATUS_WATCHDOG_ACTIVE_BITMASK);
  watchdog_active >>= IPMI_OEM_FUJITSU_SYSTEM_STATUS_WATCHDOG_ACTIVE_SHIFT;

  agent_connected = (system_status & IPMI_OEM_FUJITSU_SYSTEM_STATUS_AGENT_CONNECTED_BITMASK);
  agent_connected >>= IPMI_OEM_FUJITSU_SYSTEM_STATUS_AGENT_CONNECTED_SHIFT;

  post_state = (system_status & IPMI_OEM_FUJITSU_SYSTEM_STATUS_POST_STATE_BITMASK);
  post_state >>= IPMI_OEM_FUJITSU_SYSTEM_STATUS_POST_STATE_SHIFT;

  identify_led = (signaling & IPMI_OEM_FUJITSU_SYSTEM_STATUS_SIGNALING_LOCAL_LED_BITMASK);
  identify_led >>= IPMI_OEM_FUJITSU_SYSTEM_STATUS_SIGNALING_LOCAL_LED_SHIFT;

  css_led = (signaling & IPMI_OEM_FUJITSU_SYSTEM_STATUS_SIGNALING_CSS_LED_BITMASK);
  css_led >>= IPMI_OEM_FUJITSU_SYSTEM_STATUS_SIGNALING_CSS_LED_SHIFT;
  
  global_error_led = (signaling & IPMI_OEM_FUJITSU_SYSTEM_STATUS_SIGNALING_GLOBAL_ERROR_LED_BITMASK);
  global_error_led >>= IPMI_OEM_FUJITSU_SYSTEM_STATUS_SIGNALING_GLOBAL_ERROR_LED_SHIFT;

  pstdout_printf (state_data->pstate,
                  "System Power             : %s\n",
                  system_power ? "On" : "Off");

  pstdout_printf (state_data->pstate,
                  "SEL Entries Available    : %s\n",
                  sel_entries_available ? "Yes" : "No");

  pstdout_printf (state_data->pstate,
                  "Watchdog Active          : %s\n",
                  watchdog_active ? "Yes" : "No");

  pstdout_printf (state_data->pstate,
                  "Agent Connected          : %s\n",
                  agent_connected ? "Yes" : "No");

  /* HLiebig: renamed from "Post State" in doc */
  pstdout_printf (state_data->pstate,
                  "System in POST           : %s\n",
                  post_state ? "Yes" : "No");

  pstdout_printf (state_data->pstate,
                  "Identify LED             : %s\n",
                  identify_led ? "On" : "Off");

  switch (css_led)
    {
    case IPMI_OEM_FUJITSU_CSS_LED_OFF:
      css_led_str = "Off";
      break;
    case IPMI_OEM_FUJITSU_CSS_LED_ON:
      css_led_str = "On";
      break;
    case IPMI_OEM_FUJITSU_CSS_LED_BLINK:
      css_led_str = "Blink";
      break;
    default:
      css_led_str = "Unknown LED State";
    }

  pstdout_printf (state_data->pstate,
                  "CSS LED                  : %s\n",
                  css_led_str);
  
  switch (global_error_led)
    {
    case IPMI_OEM_FUJITSU_GLOBAL_ERROR_LED_OFF:
      global_error_led_str = "Off";
      break;
    case IPMI_OEM_FUJITSU_GLOBAL_ERROR_LED_ON:
      global_error_led_str = "On";
      break;
    case IPMI_OEM_FUJITSU_GLOBAL_ERROR_LED_BLINK:
      global_error_led_str = "Blink";
      break;
    default:
      global_error_led_str = "Unknown LED State";
    }

  pstdout_printf (state_data->pstate,
                  "Global Error LED         : %s\n",
                  global_error_led_str);

  /* HLiebig: renamed from "Post Code" in doc */
  pstdout_printf (state_data->pstate,
                  "Last POST Code (Port 80) : %02Xh\n",
                  post_code);

  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_oem_fujitsu_get_eeprom_version_info (ipmi_oem_state_data_t *state_data)
{
  uint8_t bytes_rq[IPMI_OEM_MAX_BYTES];
  uint8_t bytes_rs[IPMI_OEM_MAX_BYTES];
  int rs_len;
  uint8_t eeprom_number;
  uint8_t status;
  uint8_t major_firmware_revision;
  uint8_t minor_firmware_revision;
  uint8_t aux_firmware_revision_major;
  uint8_t aux_firmware_revision_minor;
  uint8_t aux_firmware_revision_res;
  char major_firmware_revision_char;
  uint8_t major_sdrr_revision;
  uint8_t minor_sdrr_revision;
  char major_sdrr_revision_char;
  uint16_t sdrr_id;
  uint8_t major_booter_revision;
  uint8_t minor_booter_revision;
  uint8_t aux_booter_revision_major;
  uint8_t aux_booter_revision_minor;
  long tmp;
  char *endptr;
  int rv = -1;

  assert (state_data);
  assert (state_data->prog_data->args->oem_options_count == 1);
  
  errno = 0;

  tmp = strtol (state_data->prog_data->args->oem_options[0],
                &endptr,
                0);
  if (errno
      || endptr[0] != '\0'
      || tmp < 0
      || tmp > UCHAR_MAX)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "%s:%s invalid OEM option argument '%s'\n",
                       state_data->prog_data->args->oem_id,
                       state_data->prog_data->args->oem_command,
                       state_data->prog_data->args->oem_options[0]);
      goto cleanup;
    }

  if (!(tmp >= IPMI_OEM_FUJITSU_EEPROM_NUMBER_MIN
        && tmp <= IPMI_OEM_FUJITSU_EEPROM_NUMBER_MAX))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "%s:%s invalid OEM option argument '%s' : out of range\n",
                       state_data->prog_data->args->oem_id,
                       state_data->prog_data->args->oem_command,
                       state_data->prog_data->args->oem_options[0]);
      goto cleanup;
    }
  eeprom_number = tmp;
  
  /* Fujitsu OEM
   * 
   * http://manuals.ts.fujitsu.com/file/4390/irmc_s2-en.pdf
   *
   * Request
   *
   * 0x2E - OEM network function
   * 0xF5 - OEM cmd
   * 0x?? - Fujitsu IANA (LSB first)
   * 0x?? - Fujitsu IANA
   * 0x?? - Fujitsu IANA
   * 0x12 - Command Specifier
   *
   * Response
   *
   * 0xF5 - OEM cmd
   * 0x?? - Completion code
   * 0x?? - Fujitsu IANA (LSB first)
   * 0x?? - Fujitsu IANA
   * 0x?? - Fujitsu IANA
   * 0x?? - status
   *        0x00 - checksum error
   *        0x01 - ok
   * 0x?? - major fw revision - binary coded
   * 0x?? - minor fw revision - binary - 2 char field coded (HLiebig: listed incorrectly as BCD in doc)
   * 0x?? - aux fw revision (lsb first) - binary coded, major/minor/res.
   * 0x?? - aux fw revision
   * 0x?? - aux fw revision
   * 0x?? - major fw char - ascii char
   * 0x?? - major sdrr revision - binary coded (HLiebig: listed incorrectly as BCD in doc)
   * 0x?? - minor sdrr revision - bcd coded
   * 0x?? - major sdrr char - ascii char
   * 0x?? - sdrr-ID (lsb) - hex coded (HLiebig: listed incorrectly as binary in doc)
   * 0x?? - sdrr-ID (msb) - hex coded (HLiebig: listed incorrectly as binary in doc)
   * 0x?? - major booter revision - binary coded
   * 0x?? - minor booter revision - binary - 2 char field coded (HLiebig: listed incorrectly as BCD in doc)
   * 0x?? - aux booter revision (lsb first) - binary coded, major/minor
   * 0x?? - aux booter revision
   */
     
  bytes_rq[0] = IPMI_CMD_OEM_FUJITSU_SYSTEM;
  bytes_rq[1] = (IPMI_IANA_ENTERPRISE_ID_FUJITSU & 0x0000FF);
  bytes_rq[2] = (IPMI_IANA_ENTERPRISE_ID_FUJITSU & 0x00FF00) >> 8;
  bytes_rq[3] = (IPMI_IANA_ENTERPRISE_ID_FUJITSU & 0xFF0000) >> 16;
  bytes_rq[4] = IPMI_OEM_FUJITSU_COMMAND_SPECIFIER_GET_EEPROM_VERSION_INFO;
  bytes_rq[5] = eeprom_number;

  if ((rs_len = ipmi_cmd_raw (state_data->ipmi_ctx,
                              0, /* lun */
                              IPMI_NET_FN_OEM_GROUP_RQ, /* network function */
                              bytes_rq, /* data */
                              6, /* num bytes */
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
                                                   21,
                                                   IPMI_CMD_OEM_FUJITSU_SYSTEM,
                                                   IPMI_NET_FN_OEM_GROUP_RS,
                                                   NULL) < 0)
    goto cleanup;

  status = bytes_rs[5];

  if (status != IPMI_OEM_FUJITSU_EEPROM_CHECKSUM_OK)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "Check Error Runtime\n");
      goto cleanup;
    }
  
  major_firmware_revision = bytes_rs[6];
  minor_firmware_revision = bytes_rs[7];
  aux_firmware_revision_major = bytes_rs[8];
  aux_firmware_revision_minor = bytes_rs[9];
  aux_firmware_revision_res = bytes_rs[10];
  major_firmware_revision_char = (char)bytes_rs[11];

  major_sdrr_revision = bytes_rs[12];
  minor_sdrr_revision = bytes_rs[13];
  major_sdrr_revision_char = (char)bytes_rs[14];

  sdrr_id = bytes_rs[15];
  sdrr_id |= (bytes_rs[16] << 8);

  major_booter_revision = bytes_rs[17];
  minor_booter_revision = bytes_rs[18];
  aux_booter_revision_major = bytes_rs[19];
  aux_booter_revision_minor = bytes_rs[20];
  
  /* make sure char is atleast legit */
  /* HLiebig: minor_firmware_revision listed incorrectly as BCD in doc */
  if (isalnum(major_firmware_revision_char))
    pstdout_printf (state_data->pstate,
                    "Firmware Revision : %u.%02u%c (%u.%02u)\n",
                    aux_firmware_revision_major,
                    aux_firmware_revision_minor,
                    major_firmware_revision_char,
                    major_firmware_revision,
                    minor_firmware_revision);
  else
    pstdout_printf (state_data->pstate,
                    "Firmware Revision : %u.%02u (%u.%02u)\n",
                    aux_firmware_revision_major,
                    aux_firmware_revision_minor,
                    major_firmware_revision,
                    minor_firmware_revision);

  /* HLiebig: major_sdrr_revision listed incorrectly as BCD in doc */
  if (isalnum (major_sdrr_revision_char))
    pstdout_printf (state_data->pstate,
                    "SDRR Revision     : %u.%02X%c\n",
                    major_sdrr_revision,
                    minor_sdrr_revision,
                    major_sdrr_revision_char);
  else
    pstdout_printf (state_data->pstate,
                    "SDRR Revision     : %u.%02X\n",
                    major_sdrr_revision,
                    minor_sdrr_revision);

  /* HLiebig: sdrr_id listed incorrectly as binary in doc */
  pstdout_printf (state_data->pstate,
                  "SDRR ID           : %X\n",
                  sdrr_id);

  /* HLiebig: minor_booter_revision listed incorrectly as BCD in doc */
  pstdout_printf (state_data->pstate,
                  "Booter Revision   : %u.%02u (%u.%02u)\n",
                  aux_booter_revision_major,
                  aux_booter_revision_minor,
                  major_booter_revision,
                  minor_booter_revision);
  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_oem_fujitsu_get_identify_led (ipmi_oem_state_data_t *state_data)
{
  uint8_t bytes_rq[IPMI_OEM_MAX_BYTES];
  uint8_t bytes_rs[IPMI_OEM_MAX_BYTES];
  uint8_t state;
  int rs_len;
  int rv = -1;

  assert (state_data);
  assert (!state_data->prog_data->args->oem_options_count);

  /* Fujitsu OEM
   * 
   * http://manuals.ts.fujitsu.com/file/4390/irmc_s2-en.pdf
   *
   * Request
   *
   * 0x2E - OEM network function
   * 0xF5 - OEM cmd
   * 0x?? - Fujitsu IANA (LSB first)
   * 0x?? - Fujitsu IANA
   * 0x?? - Fujitsu IANA
   * 0xB1 - Command Specifier
   *
   * Response
   *
   * 0xF5 - OEM cmd
   * 0x?? - Completion code
   * 0x?? - Fujitsu IANA (LSB first)
   * 0x?? - Fujitsu IANA
   * 0x?? - Fujitsu IANA
   * 0x?? - led on/off state (0 == off, 1 == on)
   */
  
  bytes_rq[0] = IPMI_CMD_OEM_FUJITSU_SYSTEM;
  bytes_rq[1] = (IPMI_IANA_ENTERPRISE_ID_FUJITSU & 0x0000FF);
  bytes_rq[2] = (IPMI_IANA_ENTERPRISE_ID_FUJITSU & 0x00FF00) >> 8;
  bytes_rq[3] = (IPMI_IANA_ENTERPRISE_ID_FUJITSU & 0xFF0000) >> 16;
  bytes_rq[4] = IPMI_OEM_FUJITSU_COMMAND_SPECIFIER_GET_IDENTIFY_LED;

  if ((rs_len = ipmi_cmd_raw (state_data->ipmi_ctx,
                              0, /* lun */
                              IPMI_NET_FN_OEM_GROUP_RQ, /* network function */
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
                                                   6,
                                                   IPMI_CMD_OEM_FUJITSU_SYSTEM,
                                                   IPMI_NET_FN_OEM_GROUP_RS,
                                                   NULL) < 0)
    goto cleanup;

  state = (bytes_rs[5] & IPMI_OEM_FUJITSU_IDENTIFY_LED_BITMASK);
  state >>= IPMI_OEM_FUJITSU_IDENTIFY_LED_SHIFT;

  if (state == IPMI_OEM_FUJITSU_IDENTIFY_LED_ON)
    pstdout_printf (state_data->pstate, "on\n");
  else
    pstdout_printf (state_data->pstate, "off\n");

  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_oem_fujitsu_set_identify_led (ipmi_oem_state_data_t *state_data)
{
  uint8_t bytes_rq[IPMI_OEM_MAX_BYTES];
  uint8_t bytes_rs[IPMI_OEM_MAX_BYTES];
  int rs_len;
  int rv = -1;

  assert (state_data);
  assert (state_data->prog_data->args->oem_options_count == 1);

  if (strcasecmp (state_data->prog_data->args->oem_options[0], "on")
      && strcasecmp (state_data->prog_data->args->oem_options[0], "off"))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "%s:%s invalid OEM option argument '%s'\n",
                       state_data->prog_data->args->oem_id,
                       state_data->prog_data->args->oem_command,
                       state_data->prog_data->args->oem_options[0]);
      goto cleanup;
    }

  /* Fujitsu OEM
   * 
   * http://manuals.ts.fujitsu.com/file/4390/irmc_s2-en.pdf
   *
   * Request
   *
   * 0x2E - OEM network function
   * 0xF5 - OEM cmd
   * 0x?? - Fujitsu IANA (LSB first)
   * 0x?? - Fujitsu IANA
   * 0x?? - Fujitsu IANA
   * 0xB0 - Command Specifier
   * 0x?? - led on/off (0 == off, 1 == on)
   *
   * Response
   *
   * 0xF5 - OEM cmd
   * 0x?? - Completion code
   * 0x?? - Fujitsu IANA (LSB first)
   * 0x?? - Fujitsu IANA
   * 0x?? - Fujitsu IANA
   */
  
  bytes_rq[0] = IPMI_CMD_OEM_FUJITSU_SYSTEM;
  bytes_rq[1] = (IPMI_IANA_ENTERPRISE_ID_FUJITSU & 0x0000FF);
  bytes_rq[2] = (IPMI_IANA_ENTERPRISE_ID_FUJITSU & 0x00FF00) >> 8;
  bytes_rq[3] = (IPMI_IANA_ENTERPRISE_ID_FUJITSU & 0xFF0000) >> 16;
  bytes_rq[4] = IPMI_OEM_FUJITSU_COMMAND_SPECIFIER_SET_IDENTIFY_LED;

  if (!strcasecmp (state_data->prog_data->args->oem_options[0], "on"))
    bytes_rq[5] = IPMI_OEM_FUJITSU_IDENTIFY_LED_ON;
  else
    bytes_rq[5] = IPMI_OEM_FUJITSU_IDENTIFY_LED_OFF;

  if ((rs_len = ipmi_cmd_raw (state_data->ipmi_ctx,
                              0, /* lun */
                              IPMI_NET_FN_OEM_GROUP_RQ, /* network function */
                              bytes_rq, /* data */
                              6, /* num bytes */
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
                                                   5,
                                                   IPMI_CMD_OEM_FUJITSU_SYSTEM,
                                                   IPMI_NET_FN_OEM_GROUP_RS,
                                                   NULL) < 0)
    goto cleanup;

  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_oem_fujitsu_get_error_led (ipmi_oem_state_data_t *state_data)
{
  uint8_t bytes_rq[IPMI_OEM_MAX_BYTES];
  uint8_t bytes_rs[IPMI_OEM_MAX_BYTES];
  uint8_t state;
  int rs_len;
  int rv = -1;

  assert (state_data);
  assert (!state_data->prog_data->args->oem_options_count);

  /* Fujitsu OEM
   * 
   * http://manuals.ts.fujitsu.com/file/4390/irmc_s2-en.pdf
   *
   * Request
   *
   * 0x2E - OEM network function
   * 0xF5 - OEM cmd
   * 0x?? - Fujitsu IANA (LSB first)
   * 0x?? - Fujitsu IANA
   * 0x?? - Fujitsu IANA
   * 0xB3 - Command Specifier
   *
   * Response
   *
   * 0xF5 - OEM cmd
   * 0x?? - Completion code
   * 0x?? - Fujitsu IANA (LSB first)
   * 0x?? - Fujitsu IANA
   * 0x?? - Fujitsu IANA
   * 0x?? - error led state
   * 
   * GEL - Global Error LED
   * CSS - Customer Self Service LED
   *
   * 0 - CSS off / GEL off
   * 1 - CSS off / GEL on
   * 2 - CSS off / GEL blink
   * 3 - CSS on / GEL off
   * 4 - CSS on / GEL on
   * 5 - CSS on / GEL blink
   * 6 - CSS blink / GEL off
   * 7 - CSS blink / GEL on
   * 8 - CSS blink / GEL blink
   */
  
  bytes_rq[0] = IPMI_CMD_OEM_FUJITSU_SYSTEM;
  bytes_rq[1] = (IPMI_IANA_ENTERPRISE_ID_FUJITSU & 0x0000FF);
  bytes_rq[2] = (IPMI_IANA_ENTERPRISE_ID_FUJITSU & 0x00FF00) >> 8;
  bytes_rq[3] = (IPMI_IANA_ENTERPRISE_ID_FUJITSU & 0xFF0000) >> 16;
  bytes_rq[4] = IPMI_OEM_FUJITSU_COMMAND_SPECIFIER_GET_ERROR_LED;

  if ((rs_len = ipmi_cmd_raw (state_data->ipmi_ctx,
                              0, /* lun */
                              IPMI_NET_FN_OEM_GROUP_RQ, /* network function */
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
                                                   6,
                                                   IPMI_CMD_OEM_FUJITSU_SYSTEM,
                                                   IPMI_NET_FN_OEM_GROUP_RS,
                                                   NULL) < 0)
    goto cleanup;

  state = bytes_rs[5];

  switch (state)
    {
    case IPMI_OEM_FUJITSU_ERROR_LED_CSS_OFF_GEL_OFF:
    case IPMI_OEM_FUJITSU_ERROR_LED_CSS_OFF_GEL_ON:
    case IPMI_OEM_FUJITSU_ERROR_LED_CSS_OFF_GEL_BLINK:
      pstdout_printf (state_data->pstate, "CSS LED: off\n");
      break;
    case IPMI_OEM_FUJITSU_ERROR_LED_CSS_ON_GEL_OFF:
    case IPMI_OEM_FUJITSU_ERROR_LED_CSS_ON_GEL_ON:
    case IPMI_OEM_FUJITSU_ERROR_LED_CSS_ON_GEL_BLINK:
      pstdout_printf (state_data->pstate, "CSS LED: on\n");
      break;
    case IPMI_OEM_FUJITSU_ERROR_LED_CSS_BLINK_GEL_OFF:
    case IPMI_OEM_FUJITSU_ERROR_LED_CSS_BLINK_GEL_ON:
    case IPMI_OEM_FUJITSU_ERROR_LED_CSS_BLINK_GEL_BLINK:
      pstdout_printf (state_data->pstate, "CSS LED: blink\n");
      break;
    default:
      pstdout_printf (state_data->pstate,
                      "Unrecognized LED state: %02Xh\n",
                      state);
      goto cleanup;
    }

  switch (state)
    {
    case IPMI_OEM_FUJITSU_ERROR_LED_CSS_OFF_GEL_OFF:
    case IPMI_OEM_FUJITSU_ERROR_LED_CSS_ON_GEL_OFF:
    case IPMI_OEM_FUJITSU_ERROR_LED_CSS_BLINK_GEL_OFF:
      pstdout_printf (state_data->pstate, "GEL LED: off\n");
      break;
    case IPMI_OEM_FUJITSU_ERROR_LED_CSS_OFF_GEL_ON:
    case IPMI_OEM_FUJITSU_ERROR_LED_CSS_ON_GEL_ON:
    case IPMI_OEM_FUJITSU_ERROR_LED_CSS_BLINK_GEL_ON:
      pstdout_printf (state_data->pstate, "GEL LED: on\n");
      break;
    case IPMI_OEM_FUJITSU_ERROR_LED_CSS_OFF_GEL_BLINK:
    case IPMI_OEM_FUJITSU_ERROR_LED_CSS_ON_GEL_BLINK:
    case IPMI_OEM_FUJITSU_ERROR_LED_CSS_BLINK_GEL_BLINK:
      pstdout_printf (state_data->pstate, "GEL LED: blink\n");
      break;
    default:
      pstdout_printf (state_data->pstate,
                      "Unrecognized LED state: %02Xh\n",
                      state);
      goto cleanup;
    }

  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_oem_fujitsu_get_sel_entry_long_text (ipmi_oem_state_data_t *state_data)
{
  uint8_t bytes_rq[IPMI_OEM_MAX_BYTES];
  uint8_t bytes_rs[IPMI_OEM_MAX_BYTES];
  int rs_len;
  long value;
  uint16_t sel_record_id;
  uint16_t actual_record_id = 0;
  uint32_t timestamp = 0;
  uint8_t css = 0;
  uint8_t severity = 0;
  char time_buf[IPMI_OEM_TIME_BUFLEN + 1];
  char data_buf[IPMI_OEM_FUJITSU_SEL_ENTRY_LONG_TEXT_MAX_DATA_LENGTH + 1];
  uint8_t data_length;
  uint8_t offset = 0;
  uint8_t component_length = 0;
  char *css_str = NULL;
  char *severity_str = NULL;
  char *endptr = NULL;
  int rv = -1;
  uint8_t max_read_length;
  struct ipmi_oem_data oem_data;

  assert (state_data);
  assert (state_data->prog_data->args->oem_options_count == 1);
  
  errno = 0;
  
  value = strtol (state_data->prog_data->args->oem_options[0], &endptr, 0);
  if (errno
      || endptr[0] != '\0'
      || value < IPMI_SEL_GET_RECORD_ID_FIRST_ENTRY
      || value > IPMI_SEL_GET_RECORD_ID_LAST_ENTRY)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "%s:%s invalid OEM option argument '%s'\n",
                       state_data->prog_data->args->oem_id,
                       state_data->prog_data->args->oem_command,
                       state_data->prog_data->args->oem_options[0]);
      goto cleanup;
    }

  sel_record_id = value;

  memset (data_buf, '\0', IPMI_OEM_FUJITSU_SEL_ENTRY_LONG_TEXT_MAX_DATA_LENGTH + 1);

  /* HLiebig: Note: Documentation is for iRMC S2 version */
  if ((ipmi_get_oem_data (state_data->pstate,
                          state_data->ipmi_ctx,
                          &oem_data) <= 0)
      || (IPMI_FUJITSU_PRODUCT_ID_IS_IRMC_S1 (oem_data.product_id)))
    {
      max_read_length = IPMI_OEM_FUJITSU_SEL_ENTRY_LONG_TEXT_IRMC_S1_MAX_READ_LENGTH;
      data_length = IPMI_OEM_FUJITSU_SEL_ENTRY_LONG_TEXT_IRMC_S1_MAX_DATA_LENGTH;
    }
  else 
    {
      max_read_length = IPMI_OEM_FUJITSU_SEL_ENTRY_LONG_TEXT_IRMC_S2_MAX_READ_LENGTH;
      data_length = IPMI_OEM_FUJITSU_SEL_ENTRY_LONG_TEXT_IRMC_S2_MAX_DATA_LENGTH;
    }

  /* Fujitsu OEM
   * 
   * http://manuals.ts.fujitsu.com/file/4390/irmc_s2-ug-en.pdf
   *
   * Request
   *
   * 0x2E - OEM network function
   * 0xF5 - OEM cmd
   * 0x?? - Fujitsu IANA (LSB first)
   * 0x?? - Fujitsu IANA
   * 0x?? - Fujitsu IANA
   * 0x43 - Command Specifier
   * 0x?? - Record ID (LSB first)
   * 0x?? - Record ID ; 0x0000 = "first record", 0xFFFF = "last record"
   * 0x?? - Offset (in response SEL text)
   * 0x?? - MaxResponseDataSize (size of converted SEL data 16:n in response, maximum is 100)
   *
   * Response
   *
   * 0xF5 - OEM cmd
   * 0x?? - Completion code
   * 0x?? - Fujitsu IANA (LSB first)
   * 0x?? - Fujitsu IANA
   * 0x?? - Fujitsu IANA
   * 0x?? - Next Record ID (LSB)
   * 0x?? - Next Record ID (MSB)
   * 0x?? - Actual Record ID (LSB)
   * 0x?? - Actual Record ID (MSB)
   * 0x?? - Record type
   * 0x?? - timestamp (LSB first)
   * 0x?? - timestamp
   * 0x?? - timestamp
   * 0x?? - timestamp
   * 0x?? - severity   
   *      bit 7   - CSS component
   *              - 0 - No CSS component
   *              - 1 - CSS component
   *      bit 6-4 - 000 = INFORMATIONAL
   *                001 = MINOR
   *                010 = MAJOR
   *                011 = CRITICAL
   *                1xx = unknown
   *      bit 3-0 - reserved
   * 0x?? - data length (of the whole text)
   * 0x?? - converted SEL data
   *      - requested number of bytes starting at requested offset (MaxResponseDataSize-1 bytes of data)
   * 0x00 - trailing '\0' character
   */
     
  bytes_rq[0] = IPMI_CMD_OEM_FUJITSU_SYSTEM;
  bytes_rq[1] = (IPMI_IANA_ENTERPRISE_ID_FUJITSU & 0x0000FF);
  bytes_rq[2] = (IPMI_IANA_ENTERPRISE_ID_FUJITSU & 0x00FF00) >> 8;
  bytes_rq[3] = (IPMI_IANA_ENTERPRISE_ID_FUJITSU & 0xFF0000) >> 16;
  bytes_rq[4] = IPMI_OEM_FUJITSU_COMMAND_SPECIFIER_GET_SEL_ENTRY_LONG_TEXT;
  bytes_rq[5] = (sel_record_id & 0x00FF);
  bytes_rq[6] = (sel_record_id & 0xFF00) >> 8;

  /*
   * From Holger Liebig <holger.liebig at ts.fujitsu.com>
   *
   * "Unfortunately due to memory constrains in the previous BMC
   * generation the maximum length of a single response is limited to
   * 64bytes, while on the current product line you should be able to
   * get the full 100bytes translated SEL text with a single request
   * at least over LAN. Maximum (non standard) KCS transfer size is
   * also different between current (255) and previous (64)
   * generation, so the code should compare the data received with the
   * total length reported in the response."
   */

  /* Request partial or complete string, depending on product */
  bytes_rq[8] = max_read_length;

  while (offset < data_length)
    {
      bytes_rq[7] = offset;

      /* BMC checks for boundaries, offset + len has to be <= 80 (iRMC S1) <= 100 (iRMC S2) */ 
      if (offset + bytes_rq[8] > data_length)
         bytes_rq[8] = data_length - offset;
      
      if ((rs_len = ipmi_cmd_raw (state_data->ipmi_ctx,
                                  0, /* lun */
                                  IPMI_NET_FN_OEM_GROUP_RQ, /* network function */
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
                                                       17,
                                                       IPMI_CMD_OEM_FUJITSU_SYSTEM,
                                                       IPMI_NET_FN_OEM_GROUP_RS,
                                                       NULL) < 0)
        goto cleanup;
  
      /* achu: assume a lot of this will be the same not matter how many times we loop */
      if (!offset)
        {
          actual_record_id = bytes_rs[7];
          actual_record_id |= (bytes_rs[8] << 8);

          /* use actual_record_id for subsequent requests to obtain consistent results */
          bytes_rq[5] = bytes_rs[7];
          bytes_rq[6] = bytes_rs[8];

          timestamp = bytes_rs[10];
          timestamp |= (bytes_rs[11] << 8);
          timestamp |= (bytes_rs[12] << 16);
          timestamp |= (bytes_rs[13] << 24);
          
          css = (bytes_rs[14] & IPMI_OEM_FUJITSU_CSS_BITMASK);
          css >>= IPMI_OEM_FUJITSU_CSS_SHIFT;
          
          severity = (bytes_rs[14] & IPMI_OEM_FUJITSU_SEVERITY_BITMASK);
          severity >>= IPMI_OEM_FUJITSU_SEVERITY_SHIFT;
        }
      
      data_length = bytes_rs[15];
      
      /* Every response should be NUL terminated, not just the last
       * component.
       */

      bytes_rs[rs_len - 1] = '\0'; /* just to be sure it's terminated */

      component_length = strlen ((char *)bytes_rs + 16);
  
      /* achu: truncate if there is overflow */
      if (offset + component_length > data_length)
        {
          memcpy (data_buf + offset,
                  &bytes_rs[16],
                  IPMI_OEM_FUJITSU_SEL_ENTRY_LONG_TEXT_MAX_DATA_LENGTH - offset);
          offset = data_length;
        }
      else
        {
          memcpy (data_buf + offset,
                  &bytes_rs[16],
                  component_length);
          offset += component_length;
        }
    }
  
  if (css == IPMI_OEM_FUJITSU_CSS_COMPONENT)
    css_str = "CSS Component";

  switch (severity)
    {
    case IPMI_OEM_FUJITSU_SEVERITY_INFORMATIONAL:
      severity_str = "INFORMATIONAL";
      break;
    case IPMI_OEM_FUJITSU_SEVERITY_MINOR:
      severity_str = "MINOR";
      break;
    case IPMI_OEM_FUJITSU_SEVERITY_MAJOR:
      severity_str = "MAJOR";
      break;
    case IPMI_OEM_FUJITSU_SEVERITY_CRITICAL:
      severity_str = "CRITICAL";
      break;
    default:
      severity_str = "Unknown Severity";
    }

  memset (time_buf, '\0', IPMI_OEM_TIME_BUFLEN + 1);
  
  if (ipmi_timestamp_string (timestamp,
			     state_data->prog_data->args->common_args.utc_offset,
			     get_timestamp_flags (&(state_data->prog_data->args->common_args),
						  IPMI_TIMESTAMP_FLAG_DEFAULT), 
			     "%b-%d-%Y | %H:%M:%S",
			     time_buf,
			     IPMI_OEM_TIME_BUFLEN) < 0)
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "ipmi_timestamp_string: %s\n",
		       strerror (errno));
      goto cleanup;
    }

  if (css_str)
    pstdout_printf (state_data->pstate,
                    "%u | %s | %s ; %s ; %s\n",
                    actual_record_id,
                    time_buf,
                    severity_str,
                    data_buf,
                    css_str);
  else
    pstdout_printf (state_data->pstate,
                    "%u | %s | %s ; %s\n",
                    actual_record_id,
                    time_buf,
                    severity_str,
                    data_buf);

  rv = 0;
 cleanup:
  return (rv);
}

