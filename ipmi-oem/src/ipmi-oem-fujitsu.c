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

#define IPMI_NET_FN_OEM_GROUP_RQ         0x2E
#define IPMI_NET_FN_OEM_GROUP_RS         0x2F

#define IPMI_CMD_OEM_FUJITSU_POWER         0x01
#define IPMI_CMD_OEM_FUJITSU_SYSTEM        0xF5

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
                                                   IPMI_CMD_OEM_FUJITSU_POWER,
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
                                                   IPMI_CMD_OEM_FUJITSU_POWER,
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
                                                   IPMI_CMD_OEM_FUJITSU_POWER,
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
