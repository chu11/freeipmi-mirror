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

#include "freeipmi-portability.h"
#include "pstdout.h"

/*
 * All of the below are from
 *
 * http://manuals.ts.fujitsu.com/file/4390/irmc_s2-en.pdf
 */

/* With IPMI_CMD_OEM_FUJITSU_POWER */
#define IPMI_OEM_FUJITSU_COMMAND_SPECIFIER_GET_POWER_ON_SOURCE    0x15
#define IPMI_OEM_FUJITSU_COMMAND_SPECIFIER_GET_POWER_OFF_SOURCE   0x16
#define IPMI_OEM_FUJITSU_COMMAND_SPECIFIER_SET_POWER_OFF_INHIBIT  0x1C
#define IPMI_OEM_FUJITSU_COMMAND_SPECIFIER_GET_POWER_OFF_INHIBIT  0x1D
#define IPMI_OEM_FUJITSU_COMMAND_SPECIFIER_SET_NEXT_POWER_ON_TIME 0x20

/* With IPMI_CMD_OEM_FUJITSU_COMMUNICATION */
#define IPMI_OEM_FUJITSU_COMMAND_SPECIFIER_SYSTEM_OS_SHUTDOWN_REQUEST           0x05
#define IPMI_OEM_FUJITSU_COMMAND_SPECIFIER_SYSTEM_OS_SHUTDOWN_REQUEST_AND_RESET 0x06
#define IPMI_OEM_FUJITSU_COMMAND_SPECIFIER_AGENT_CONNECT_STATUS                 0x08
#define IPMI_OEM_FUJITSU_COMMAND_SPECIFIER_SHUTDOWN_REQUEST_CANCELLED           0x09

/* With IPMI_CMD_OEM_FUJITSU_FAN_TEST */
#define IPMI_OEM_FUJITSU_COMMAND_SPECIFIER_WRITE_TO_SYSTEM_DISPLAY 0x02

/* With IPMI_CMD_OEM_FUJITSU_BIOS */
#define IPMI_OEM_FUJITSU_COMMAND_SPECIFIER_GET_BIOS_POST_STATE 0x09
#define IPMI_OEM_FUJITSU_COMMAND_SPECIFIER_GET_CPU_INFO        0x15

/* With IPMI_CMD_OEM_FUJITSU_SYSTEM */
#define IPMI_OEM_FUJITSU_COMMAND_SPECIFIER_GET_SYSTEM_STATUS                          0x10
#define IPMI_OEM_FUJITSU_COMMAND_SPECIFIER_GET_EEPROM_VERSION_INFO                    0x12
#define IPMI_OEM_FUJITSU_COMMAND_SPECIFIER_GET_SEL_ENTRY_LONG_TEXT                    0x43
#define IPMI_OEM_FUJITSU_COMMAND_SPECIFIER_GET_SEL_ENTRY_TEXT                         0x45
#define IPMI_OEM_FUJITSU_COMMAND_SPECIFIER_SET_IDENTIFY_LED                           0xB0
#define IPMI_OEM_FUJITSU_COMMAND_SPECIFIER_GET_IDENTIFY_LED                           0xB1
#define IPMI_OEM_FUJITSU_COMMAND_SPECIFIER_GET_ERROR_LED                              0xB3
#define IPMI_OEM_FUJITSU_COMMAND_SPECIFIER_RESET_NONVOLATILE_CFG_VARIABLES_TO_DEFAULT 0xDF
#define IPMI_OEM_FUJITSU_COMMAND_SPECIFIER_RESET_CONFIGSPACE_VARIABLES_TO_DEFAULT     0xE0

/* IPMI_CMD_OEM_FUJITSU_GET_REMOTE_STORAGE_CONNECTION_OR_STATUS */
/* achu: making up names, not listed in documents */
#define IPMI_OEM_FUJITSU_COMMAND_SPECIFIER_REMOTE_STORAGE_CONNECTED 0x01
#define IPMI_OEM_FUJITSU_COMMAND_SPECIFIER_REMOTE_STORAGE_STATUS    0x02

#define IPMI_OEM_FUJITSU_POWER_ON_SOURCE_SOFTWARE_OR_COMMAND                                   0x00
#define IPMI_OEM_FUJITSU_POWER_ON_SOURCE_POWER_SWITCH                                          0x01
#define IPMI_OEM_FUJITSU_POWER_ON_SOURCE_AUTOMATIC_RESTART_AFTER_POWER_FAILURE                 0x02
#define IPMI_OEM_FUJITSU_POWER_ON_SOURCE_CLOCK_OR_TIMER                                        0x03
#define IPMI_OEM_FUJITSU_POWER_ON_SOURCE_AUTOMATIC_RESTART_AFTER_FAN_FAILURE_SHUTDOWN          0x04
#define IPMI_OEM_FUJITSU_POWER_ON_SOURCE_AUTOMATIC_RESTART_AFTER_CRITICAL_TEMPERATURE_SHUTDOWN 0x05
#define IPMI_OEM_FUJITSU_POWER_ON_SOURCE_REBOOT_AFTER_WATCHDOG_TIMEOUT                         0x08
#define IPMI_OEM_FUJITSU_POWER_ON_SOURCE_REMOTE_ON                                             0x09
#define IPMI_OEM_FUJITSU_POWER_ON_SOURCE_REBOOT_AFTER_A_CPU_ERROR                              0x0C
#define IPMI_OEM_FUJITSU_POWER_ON_SOURCE_REBOOT_BY_HARDWARE_RESET                              0x15
#define IPMI_OEM_FUJITSU_POWER_ON_SOURCE_REBOOT_AFTER_WARM_START                               0x16
#define IPMI_OEM_FUJITSU_POWER_ON_SOURCE_POWERED_ON_BY_A_PCI_BUS_POWER_MANAGEMENT_EVENT        0x1A
#define IPMI_OEM_FUJITSU_POWER_ON_SOURCE_POWERED_ON_BY_REMOTE_CONTROL_VIA_REMOTE_MANAGER       0x1D
#define IPMI_OEM_FUJITSU_POWER_ON_SOURCE_REBOOT_RESET_BY_REMOTE_CONTROL_VIA_REMOTE_MANAGER     0x1E

#define IPMI_OEM_FUJITSU_POWER_OFF_SOURCE_SOFTWARE                                         0x00
#define IPMI_OEM_FUJITSU_POWER_OFF_SOURCE_POWER_SWITCH                                     0x01
#define IPMI_OEM_FUJITSU_POWER_OFF_SOURCE_AC_POWER_FAIL                                    0x02
#define IPMI_OEM_FUJITSU_POWER_OFF_SOURCE_CLOCK_OR_TIMER                                   0x03
#define IPMI_OEM_FUJITSU_POWER_OFF_SOURCE_FAN_FAILURE                                      0x04
#define IPMI_OEM_FUJITSU_POWER_OFF_SOURCE_CRITICAL_TEMPERATURE                             0x05
#define IPMI_OEM_FUJITSU_POWER_OFF_SOURCE_FINAL_POWER_OFF_AFTER_REPEATED_WATCHDOG_TIMEOUTS 0x08
#define IPMI_OEM_FUJITSU_POWER_OFF_SOURCE_FINAL_POWER_OFF_AFTER_REPEATED_CPU_ERRORS        0x0C
#define IPMI_OEM_FUJITSU_POWER_OFF_SOURCE_POWERED_OFF_BY_REMOTE_CONTROL_VIA_REMOTE_MANAGER 0x1D

#define IPMI_OEM_FUJITSU_REMOTE_STORAGE_CONNECTION_MIN 0
#define IPMI_OEM_FUJITSU_REMOTE_STORAGE_CONNECTION_MAX 1

#define IPMI_OEM_FUJITSU_REMOTE_STORAGE_CONNECTED     0x01
#define IPMI_OEM_FUJITSU_REMOTE_STORAGE_NOT_CONNECTED 0x00

#define IPMI_OEM_FUJITSU_REMOTE_STORAGE_STATUS_INVALID_UNKNOWN                              0x00
#define IPMI_OEM_FUJITSU_REMOTE_STORAGE_STATUS_IDLE                                         0x01
#define IPMI_OEM_FUJITSU_REMOTE_STORAGE_STATUS_CONNECTION_ATTEMPT_PENDING                   0x02
#define IPMI_OEM_FUJITSU_REMOTE_STORAGE_STATUS_CONNECTED                                    0x03
#define IPMI_OEM_FUJITSU_REMOTE_STORAGE_STATUS_CONNECTION_ATTEMPTS_RETRIES_EXHAUSTED_FAILED 0x04
#define IPMI_OEM_FUJITSU_REMOTE_STORAGE_STATUS_CONNECTION_LOST                              0x05
#define IPMI_OEM_FUJITSU_REMOTE_STORAGE_STATUS_DISCONNECT_PENDING                           0x06

#define IPMI_OEM_FUJITSU_REMOTE_STORAGE_TYPE_INVALID_UNKNOWN     0x00
#define IPMI_OEM_FUJITSU_REMOTE_STORAGE_TYPE_STORAGE_SERVER_IPMI 0x01
#define IPMI_OEM_FUJITSU_REMOTE_STORAGE_TYPE_APPLET              0x02
#define IPMI_OEM_FUJITSU_REMOTE_STORAGE_TYPE_NONE_NOT_CONNECTED  0x03

#define IPMI_OEM_FUJITSU_SYSTEM_STATUS_SYSTEM_POWER_BITMASK 0x80
#define IPMI_OEM_FUJITSU_SYSTEM_STATUS_SYSTEM_POWER_SHIFT   7

#define IPMI_OEM_FUJITSU_SYSTEM_STATUS_SEL_ENTRIES_AVAILABLE_BITMASK  0x10
#define IPMI_OEM_FUJITSU_SYSTEM_STATUS_SEL_ENTRIES_AVAILABLE_SHIFT    4

#define IPMI_OEM_FUJITSU_SYSTEM_STATUS_WATCHDOG_ACTIVE_BITMASK 0x04
#define IPMI_OEM_FUJITSU_SYSTEM_STATUS_WATCHDOG_ACTIVE_SHIFT   2

#define IPMI_OEM_FUJITSU_SYSTEM_STATUS_AGENT_CONNECTED_BITMASK 0x02
#define IPMI_OEM_FUJITSU_SYSTEM_STATUS_AGENT_CONNECTED_SHIFT   1

#define IPMI_OEM_FUJITSU_SYSTEM_STATUS_POST_STATE_BITMASK 0x01
#define IPMI_OEM_FUJITSU_SYSTEM_STATUS_POST_STATE_SHIFT   0

/* rename from "localize" */
#define IPMI_OEM_FUJITSU_SYSTEM_STATUS_SIGNALING_LOCAL_LED_BITMASK 0x80
#define IPMI_OEM_FUJITSU_SYSTEM_STATUS_SIGNALING_LOCAL_LED_SHIFT   7

#define IPMI_OEM_FUJITSU_SYSTEM_STATUS_SIGNALING_CSS_LED_BITMASK 0xC0
#define IPMI_OEM_FUJITSU_SYSTEM_STATUS_SIGNALING_CSS_LED_SHIFT   2

#define IPMI_OEM_FUJITSU_SYSTEM_STATUS_SIGNALING_GLOBAL_ERROR_LED_BITMASK 0x03
#define IPMI_OEM_FUJITSU_SYSTEM_STATUS_SIGNALING_GLOBAL_ERROR_LED_SHIFT   0

#define IPMI_OEM_FUJITSU_SYSTEM_STATUS_NOTIFICATIONS_SEL_MODIFIED_NEW_SEL_ENTRY_BITMASK 0x80
#define IPMI_OEM_FUJITSU_SYSTEM_STATUS_NOTIFICATIONS_SEL_MODIFIED_NEW_SEL_ENTRY_SHIFT   7

#define IPMI_OEM_FUJITSU_SYSTEM_STATUS_NOTIFICATIONS_SEL_MODIFIED_SEL_CLEARED_BITMASK 0x40
#define IPMI_OEM_FUJITSU_SYSTEM_STATUS_NOTIFICATIONS_SEL_MODIFIED_SEL_CLEARED_SHIFT   6

#define IPMI_OEM_FUJITSU_SYSTEM_STATUS_NOTIFICATIONS_SDR_MODIFIED_BITMASK  0x20
#define IPMI_OEM_FUJITSU_SYSTEM_STATUS_NOTIFICATIONS_SDR_MODIFIED_SHIFT    5

#define IPMI_OEM_FUJITSU_SYSTEM_STATUS_NOTIFICATIONS_NONVOLATILE_IPMI_VARIABLE_MODIFIED_BITMASK 0x10
#define IPMI_OEM_FUJITSU_SYSTEM_STATUS_NOTIFICATIONS_NONVOLATILE_IPMI_VARIABLE_MODIFIED_SHIFT   4

#define IPMI_OEM_FUJITSU_SYSTEM_STATUS_NOTIFICATIONS_CONFIGSPACE_MODIFIED_BITMASK 0x08
#define IPMI_OEM_FUJITSU_SYSTEM_STATUS_NOTIFICATIONS_CONFIGSPACE_MODIFIED_SHIFT   3

#define IPMI_OEM_FUJITSU_SYSTEM_STATUS_NOTIFICATIONS_NEW_OUTPUT_ON_LOCALVIEW_DISPLAY_BITMASK 0x01
#define IPMI_OEM_FUJITSU_SYSTEM_STATUS_NOTIFICATIONS_NEW_OUTPUT_ON_LOCALVIEW_DISPLAY_SHIFT   0

/* achu: not defined in Fujitsu docs.  "off" is confirmed to be
 * correct, but "on" and "blink" are being gussed based on ordering of
 * error leds in LED sections.
 */
#define IPMI_OEM_FUJITSU_CSS_LED_OFF   0x0
#define IPMI_OEM_FUJITSU_CSS_LED_ON    0x1
#define IPMI_OEM_FUJITSU_CSS_LED_BLINK 0x2

/* achu: not defined in Fujitsu docs.  "off" is confirmed to be
 * correct, but "on" and "blink" are being gussed based on ordering of
 * error leds in LED sections.
 */
#define IPMI_OEM_FUJITSU_GLOBAL_ERROR_LED_OFF   0x0
#define IPMI_OEM_FUJITSU_GLOBAL_ERROR_LED_ON    0x1
#define IPMI_OEM_FUJITSU_GLOBAL_ERROR_LED_BLINK 0x2

#define IPMI_OEM_FUJITSU_EEPROM_NUMBER_MIN 0
#define IPMI_OEM_FUJITSU_EEPROM_NUMBER_MAX 1

#define IPMI_OEM_FUJITSU_EEPROM_CHECKSUM_OK    0x01
#define IPMI_OEM_FUJITSU_EEPROM_CHECKSUM_ERROR 0x00

#define IPMI_OEM_FUJITSU_IDENTIFY_LED_ON  0x1
#define IPMI_OEM_FUJITSU_IDENTIFY_LED_OFF 0x0

#define IPMI_OEM_FUJITSU_IDENTIFY_LED_BITMASK 0x01
#define IPMI_OEM_FUJITSU_IDENTIFY_LED_SHIFT      0 

#define IPMI_OEM_FUJITSU_ERROR_LED_CSS_OFF_GEL_OFF     0
#define IPMI_OEM_FUJITSU_ERROR_LED_CSS_OFF_GEL_ON      1
#define IPMI_OEM_FUJITSU_ERROR_LED_CSS_OFF_GEL_BLINK   2
#define IPMI_OEM_FUJITSU_ERROR_LED_CSS_ON_GEL_OFF      3
#define IPMI_OEM_FUJITSU_ERROR_LED_CSS_ON_GEL_ON       4
#define IPMI_OEM_FUJITSU_ERROR_LED_CSS_ON_GEL_BLINK    5
#define IPMI_OEM_FUJITSU_ERROR_LED_CSS_BLINK_GEL_OFF   6
#define IPMI_OEM_FUJITSU_ERROR_LED_CSS_BLINK_GEL_ON    7
#define IPMI_OEM_FUJITSU_ERROR_LED_CSS_BLINK_GEL_BLINK 8

#define IPMI_IANA_ENTERPRISE_ID_FUJITSU               10368

#define IPMI_NET_FN_OEM_FUJITSU_GENERIC_RQ 0x30
#define IPMI_NET_FN_OEM_FUJITSU_GENERIC_RS 0x31

#define IPMI_NET_FN_OEM_GROUP_RQ         0x2E
#define IPMI_NET_FN_OEM_GROUP_RS         0x2F

#define IPMI_CMD_OEM_FUJITSU_POWER         0x01
#define IPMI_CMD_OEM_FUJITSU_SYSTEM        0xF5
#define IPMI_CMD_OEM_FUJITSU_GET_REMOTE_STORAGE_CONNECTION_OR_STATUS 0x19

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
                       ipmi_ctx_strerror (ipmi_ctx_errnum (state_data->ipmi_ctx)));
      goto cleanup;
    }
  
  if (ipmi_oem_check_response_and_completion_code (state_data,
                                                   bytes_rs,
                                                   rs_len,
                                                   7,
                                                   IPMI_CMD_OEM_FUJITSU_POWER,
                                                   IPMI_NET_FN_OEM_GROUP_RS) < 0)
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

  if (source == IPMI_OEM_FUJITSU_POWER_ON_SOURCE_SOFTWARE_OR_COMMAND)
    snprintf (str, IPMI_OEM_STR_BUFLEN, "Software or command");
  else if (source == IPMI_OEM_FUJITSU_POWER_ON_SOURCE_POWER_SWITCH)
    snprintf (str, IPMI_OEM_STR_BUFLEN, "Power switch");
  else if (source == IPMI_OEM_FUJITSU_POWER_ON_SOURCE_AUTOMATIC_RESTART_AFTER_POWER_FAILURE)
    snprintf (str, IPMI_OEM_STR_BUFLEN, "Automatic restart after power failure");
  else if (source == IPMI_OEM_FUJITSU_POWER_ON_SOURCE_CLOCK_OR_TIMER)
    snprintf (str, IPMI_OEM_STR_BUFLEN, "Clock or timer");
  else if (source == IPMI_OEM_FUJITSU_POWER_ON_SOURCE_AUTOMATIC_RESTART_AFTER_FAN_FAILURE_SHUTDOWN)
    snprintf (str, IPMI_OEM_STR_BUFLEN, "Automatic restart after fan failure shutdown");
  else if (source == IPMI_OEM_FUJITSU_POWER_ON_SOURCE_AUTOMATIC_RESTART_AFTER_CRITICAL_TEMPERATURE_SHUTDOWN)
    snprintf (str, IPMI_OEM_STR_BUFLEN, "Automatic restart after critical temperature shutdown");
  else if (source == IPMI_OEM_FUJITSU_POWER_ON_SOURCE_REBOOT_AFTER_WATCHDOG_TIMEOUT)
    snprintf (str, IPMI_OEM_STR_BUFLEN, "Reboot after watchdog timeout");
  else if (source == IPMI_OEM_FUJITSU_POWER_ON_SOURCE_REMOTE_ON)
    snprintf (str, IPMI_OEM_STR_BUFLEN, "Remote on");
  else if (source == IPMI_OEM_FUJITSU_POWER_ON_SOURCE_REBOOT_AFTER_A_CPU_ERROR)
    snprintf (str, IPMI_OEM_STR_BUFLEN, "Reboot after a CPU error");
  else if (source == IPMI_OEM_FUJITSU_POWER_ON_SOURCE_REBOOT_BY_HARDWARE_RESET)
    snprintf (str, IPMI_OEM_STR_BUFLEN, "Reboot by hardware reset");
  else if (source == IPMI_OEM_FUJITSU_POWER_ON_SOURCE_REBOOT_AFTER_WARM_START)
    snprintf (str, IPMI_OEM_STR_BUFLEN, "Reboot after warm start");
  else if (source == IPMI_OEM_FUJITSU_POWER_ON_SOURCE_POWERED_ON_BY_A_PCI_BUS_POWER_MANAGEMENT_EVENT)
    snprintf (str, IPMI_OEM_STR_BUFLEN, "Powered on by a PCI Bus Power Management Event");
  else if (source == IPMI_OEM_FUJITSU_POWER_ON_SOURCE_POWERED_ON_BY_REMOTE_CONTROL_VIA_REMOTE_MANAGER)
    snprintf (str, IPMI_OEM_STR_BUFLEN, "Powered on by remote control via remote manager");
  else if (source == IPMI_OEM_FUJITSU_POWER_ON_SOURCE_REBOOT_RESET_BY_REMOTE_CONTROL_VIA_REMOTE_MANAGER)
    snprintf (str, IPMI_OEM_STR_BUFLEN, "Reboot/reset by remote control via remote manager");
  else
    snprintf (str, IPMI_OEM_STR_BUFLEN, "Unrecognized source: %02Xh", source);
  
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

  if (source == IPMI_OEM_FUJITSU_POWER_OFF_SOURCE_SOFTWARE)
    snprintf (str, IPMI_OEM_STR_BUFLEN, "Software");
  else if (source == IPMI_OEM_FUJITSU_POWER_OFF_SOURCE_POWER_SWITCH)
    snprintf (str, IPMI_OEM_STR_BUFLEN, "Power switch");
  else if (source == IPMI_OEM_FUJITSU_POWER_OFF_SOURCE_AC_POWER_FAIL)
    snprintf (str, IPMI_OEM_STR_BUFLEN, "AC power fail");
  else if (source == IPMI_OEM_FUJITSU_POWER_OFF_SOURCE_CLOCK_OR_TIMER)
    snprintf (str, IPMI_OEM_STR_BUFLEN, "Clock or timer");
  else if (source == IPMI_OEM_FUJITSU_POWER_OFF_SOURCE_FAN_FAILURE)
    snprintf (str, IPMI_OEM_STR_BUFLEN, "Fan failure");
  else if (source == IPMI_OEM_FUJITSU_POWER_OFF_SOURCE_CRITICAL_TEMPERATURE)
    snprintf (str, IPMI_OEM_STR_BUFLEN, "Critical temperature");
  else if (source == IPMI_OEM_FUJITSU_POWER_OFF_SOURCE_FINAL_POWER_OFF_AFTER_REPEATED_WATCHDOG_TIMEOUTS)
    snprintf (str, IPMI_OEM_STR_BUFLEN, "Final power-off after repeated watchdog timeouts");
  else if (source == IPMI_OEM_FUJITSU_POWER_OFF_SOURCE_FINAL_POWER_OFF_AFTER_REPEATED_CPU_ERRORS)
    snprintf (str, IPMI_OEM_STR_BUFLEN, "Final power-off after repeated CPU errors");
  else if (source == IPMI_OEM_FUJITSU_POWER_OFF_SOURCE_POWERED_OFF_BY_REMOTE_CONTROL_VIA_REMOTE_MANAGER)
    snprintf (str, IPMI_OEM_STR_BUFLEN, "Powered off by remote control via remote manager");
  else
    snprintf (str, IPMI_OEM_STR_BUFLEN, "Unrecognized source: %02Xh", source);
  
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
  char *ptr;
  int rv = -1;

  assert (state_data);
  assert (state_data->prog_data->args->oem_options_count == 1);
  
  tmp = strtol (state_data->prog_data->args->oem_options[0],
                &ptr,
                0);
  if (tmp < 0
      || tmp > UCHAR_MAX
      || (*ptr) != '\0')
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
                       ipmi_ctx_strerror (ipmi_ctx_errnum (state_data->ipmi_ctx)));
      goto cleanup;
    }
  
  if (ipmi_oem_check_response_and_completion_code (state_data,
                                                   bytes_rs,
                                                   rs_len,
                                                   6,
                                                   IPMI_CMD_OEM_FUJITSU_GET_REMOTE_STORAGE_CONNECTION_OR_STATUS,
                                                   IPMI_NET_FN_OEM_FUJITSU_GENERIC_RS) < 0)
    goto cleanup;

  storage_status = bytes_rs[4];
  storage_type = bytes_rs[5];

  if (storage_status == IPMI_OEM_FUJITSU_REMOTE_STORAGE_STATUS_INVALID_UNKNOWN)
    storage_status_str = "Invalid / unknown";
  else if (storage_status == IPMI_OEM_FUJITSU_REMOTE_STORAGE_STATUS_IDLE)
    storage_status_str = "idle";
  else if (storage_status == IPMI_OEM_FUJITSU_REMOTE_STORAGE_STATUS_CONNECTION_ATTEMPT_PENDING)
    storage_status_str = "Connection Attempt pending";
  else if (storage_status == IPMI_OEM_FUJITSU_REMOTE_STORAGE_STATUS_CONNECTED)
    storage_status_str = "Connected";
  else if (storage_status == IPMI_OEM_FUJITSU_REMOTE_STORAGE_STATUS_CONNECTION_ATTEMPTS_RETRIES_EXHAUSTED_FAILED)
    storage_status_str = "Connection Attempts retries exhausted / failed";
  else if (storage_status == IPMI_OEM_FUJITSU_REMOTE_STORAGE_STATUS_CONNECTION_LOST)
    storage_status_str = "Connection lost";
  else if (storage_status == IPMI_OEM_FUJITSU_REMOTE_STORAGE_STATUS_DISCONNECT_PENDING)
    storage_status_str = "Disconnect pending";
  else
    storage_status_str = "Unknown Storage Status";

  pstdout_printf (state_data->pstate,
                  "Storage Status : %s\n",
                  storage_status_str);

  if (storage_type == IPMI_OEM_FUJITSU_REMOTE_STORAGE_TYPE_INVALID_UNKNOWN)
    storage_type_str = "Invalid / unknown";
  else if (storage_type == IPMI_OEM_FUJITSU_REMOTE_STORAGE_TYPE_STORAGE_SERVER_IPMI)
    storage_type_str = "Storage Server / IPMI";
  else if (storage_type == IPMI_OEM_FUJITSU_REMOTE_STORAGE_TYPE_APPLET)
    storage_type_str = "Applet";
  else if (storage_type == IPMI_OEM_FUJITSU_REMOTE_STORAGE_TYPE_NONE_NOT_CONNECTED)
    storage_type_str = "None / Not connected";
  else
    storage_type_str = "Unknown Storage Type";
  
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
  uint8_t local_led;            /* rename from "localize" */
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
   *      bit 7 - Localize LED (achu: is this a typo, "Local" LED?)
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
   * 0x?? - POST Code
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
                       ipmi_ctx_strerror (ipmi_ctx_errnum (state_data->ipmi_ctx)));
      goto cleanup;
    }
  
  if (ipmi_oem_check_response_and_completion_code (state_data,
                                                   bytes_rs,
                                                   rs_len,
                                                   9,
                                                   IPMI_CMD_OEM_FUJITSU_SYSTEM,
                                                   IPMI_NET_FN_OEM_GROUP_RS) < 0)
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

  local_led = (signaling & IPMI_OEM_FUJITSU_SYSTEM_STATUS_SIGNALING_LOCAL_LED_BITMASK);
  local_led >>= IPMI_OEM_FUJITSU_SYSTEM_STATUS_SIGNALING_LOCAL_LED_SHIFT;

  css_led = (signaling & IPMI_OEM_FUJITSU_SYSTEM_STATUS_SIGNALING_CSS_LED_BITMASK);
  css_led >>= IPMI_OEM_FUJITSU_SYSTEM_STATUS_SIGNALING_CSS_LED_SHIFT;
  
  global_error_led = (signaling & IPMI_OEM_FUJITSU_SYSTEM_STATUS_SIGNALING_GLOBAL_ERROR_LED_BITMASK);
  global_error_led >>= IPMI_OEM_FUJITSU_SYSTEM_STATUS_SIGNALING_GLOBAL_ERROR_LED_SHIFT;

  pstdout_printf (state_data->pstate,
                  "System Power          : %s\n",
                  system_power ? "On" : "Off");

  pstdout_printf (state_data->pstate,
                  "SEL Entries Available : %s\n",
                  sel_entries_available ? "Yes" : "No");

  pstdout_printf (state_data->pstate,
                  "Watchdog Active       : %s\n",
                  watchdog_active ? "Yes" : "No");

  pstdout_printf (state_data->pstate,
                  "Agent Connected       : %s\n",
                  agent_connected ? "Yes" : "No");

  /* achu: on vs. off?? or should be something else? */
  pstdout_printf (state_data->pstate,
                  "Post State            : %s\n",
                  post_state ? "On" : "Off");

  pstdout_printf (state_data->pstate,
                  "Local LED             : %s\n",
                  local_led ? "On" : "Off");

  if (css_led == IPMI_OEM_FUJITSU_CSS_LED_OFF)
    css_led_str = "Off";
  else if (css_led == IPMI_OEM_FUJITSU_CSS_LED_ON)
    css_led_str = "On";
  else if (css_led == IPMI_OEM_FUJITSU_CSS_LED_BLINK)
    css_led_str = "Blink";
  else
    css_led_str = "Unknown LED State";

  pstdout_printf (state_data->pstate,
                  "CSS LED               : %s\n",
                  css_led_str);
  
  if (global_error_led == IPMI_OEM_FUJITSU_GLOBAL_ERROR_LED_OFF)
    global_error_led_str = "Off";
  else if (global_error_led == IPMI_OEM_FUJITSU_GLOBAL_ERROR_LED_ON)
    global_error_led_str = "On";
  else if (global_error_led == IPMI_OEM_FUJITSU_GLOBAL_ERROR_LED_BLINK)
    global_error_led_str = "Blink";
  else
    global_error_led_str = "Unknown LED State";

  pstdout_printf (state_data->pstate,
                  "Global error LED      : %s\n",
                  global_error_led_str);

  pstdout_printf (state_data->pstate,
                  "Post Code             : %02Xh\n",
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
  char *ptr;
  int rv = -1;

  assert (state_data);
  assert (state_data->prog_data->args->oem_options_count == 1);
  
  tmp = strtol (state_data->prog_data->args->oem_options[0],
                &ptr,
                0);
  if (tmp < 0
      || tmp > UCHAR_MAX
      || (*ptr) != '\0')
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
   * 0x?? - minor fw revision - bcd coded
   * 0x?? - aux fw revision (lsb first) - binary coded, major/minor/res.
   * 0x?? - aux fw revision
   * 0x?? - aux fw revision
   * 0x?? - major fw char - ascii char
   * 0x?? - major sdrr revision - bcd coded
   * 0x?? - minor sdrr revision - bcd coded
   * 0x?? - major sdrr char - ascii char
   * 0x?? - sdrr-ID (lsb) - binary coded
   * 0x?? - sdrr-ID (msb) - binary coded
   * 0x?? - major booter revision - binary coded
   * 0x?? - minor booter revision - bcd coded
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
                       ipmi_ctx_strerror (ipmi_ctx_errnum (state_data->ipmi_ctx)));
      goto cleanup;
    }
  
  if (ipmi_oem_check_response_and_completion_code (state_data,
                                                   bytes_rs,
                                                   rs_len,
                                                   21,
                                                   IPMI_CMD_OEM_FUJITSU_SYSTEM,
                                                   IPMI_NET_FN_OEM_GROUP_RS) < 0)
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
  if (isalnum(major_firmware_revision_char))
    pstdout_printf (state_data->pstate,
                    "Firmware Revsion : %c%u.%02X.%u.%u.%u\n",
                    major_firmware_revision_char,
                    major_firmware_revision,
                    minor_firmware_revision,
                    aux_firmware_revision_major,
                    aux_firmware_revision_minor,
                    aux_firmware_revision_res);
  else
    pstdout_printf (state_data->pstate,
                    "Firmware Revsion : %u.%02X.%u.%u.%u\n",
                    major_firmware_revision,
                    minor_firmware_revision,
                    aux_firmware_revision_major,
                    aux_firmware_revision_minor,
                    aux_firmware_revision_res);

  if (isalnum (major_sdrr_revision_char))
    pstdout_printf (state_data->pstate,
                    "SDRR Revision    : %c%02X.%02X\n",
                    major_sdrr_revision_char,
                    major_sdrr_revision,
                    minor_sdrr_revision);
  else
    pstdout_printf (state_data->pstate,
                    "SDRR Revision    : %02X.%02X\n",
                    major_sdrr_revision,
                    minor_sdrr_revision);

  pstdout_printf (state_data->pstate,
                  "SDRR ID          : %u\n",
                  sdrr_id);

  pstdout_printf (state_data->pstate,
                  "Booter Revision  : %u.%02X.%u.%u\n",
                  major_booter_revision,
                  minor_booter_revision,
                  aux_booter_revision_major,
                  aux_booter_revision_minor);
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
                       ipmi_ctx_strerror (ipmi_ctx_errnum (state_data->ipmi_ctx)));
      goto cleanup;
    }
  
  if (ipmi_oem_check_response_and_completion_code (state_data,
                                                   bytes_rs,
                                                   rs_len,
                                                   6,
                                                   IPMI_CMD_OEM_FUJITSU_SYSTEM,
                                                   IPMI_NET_FN_OEM_GROUP_RS) < 0)
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
                       ipmi_ctx_strerror (ipmi_ctx_errnum (state_data->ipmi_ctx)));
      goto cleanup;
    }
  
  if (ipmi_oem_check_response_and_completion_code (state_data,
                                                   bytes_rs,
                                                   rs_len,
                                                   5,
                                                   IPMI_CMD_OEM_FUJITSU_SYSTEM,
                                                   IPMI_NET_FN_OEM_GROUP_RS) < 0)
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
                       ipmi_ctx_strerror (ipmi_ctx_errnum (state_data->ipmi_ctx)));
      goto cleanup;
    }
  
  if (ipmi_oem_check_response_and_completion_code (state_data,
                                                   bytes_rs,
                                                   rs_len,
                                                   6,
                                                   IPMI_CMD_OEM_FUJITSU_SYSTEM,
                                                   IPMI_NET_FN_OEM_GROUP_RS) < 0)
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
