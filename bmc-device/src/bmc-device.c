/*
  Copyright (C) 2008 FreeIPMI Core Team

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
#endif

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#include <ctype.h>
#endif /* STDC_HEADERS */
#if TIME_WITH_SYS_TIME
#include <sys/time.h>
#include <time.h>
#else /* !TIME_WITH_SYS_TIME */
#if HAVE_SYS_TIME_H
#include <sys/time.h>
#else /* !HAVE_SYS_TIME_H */
#include <time.h>
#endif /* !HAVE_SYS_TIME_H */
#endif /* !TIME_WITH_SYS_TIME */
#include <assert.h>
#include <errno.h>

#include <freeipmi/freeipmi.h>

#include "bmc-device.h"
#include "bmc-device-argp.h"

#include "freeipmi-portability.h"
#include "pstdout.h"
#include "tool-common.h"
#include "tool-cmdline-common.h"
#include "tool-fiid-wrappers.h"
#include "tool-hostrange-common.h"

#define BMC_DEVICE_MAX_EVENT_ARGS 9

static int
cold_reset (bmc_device_state_data_t *state_data)
{
  fiid_obj_t cmd_rs = NULL;
  int rv = -1;

  assert(state_data);

  _FIID_OBJ_CREATE(cmd_rs, tmpl_cmd_cold_reset_rs);

  if (ipmi_cmd_cold_reset (state_data->ipmi_ctx, cmd_rs) < 0)
    {
      pstdout_fprintf(state_data->pstate,
                      stderr,
                      "ipmi_cmd_cold_reset: %s\n",
                      ipmi_ctx_strerror(ipmi_ctx_errnum(state_data->ipmi_ctx)));
      goto cleanup;
    }
  
  rv = 0;
 cleanup:
  _FIID_OBJ_DESTROY(cmd_rs);
  return rv;
}

static int
warm_reset (bmc_device_state_data_t *state_data)
{
  fiid_obj_t cmd_rs = NULL;
  int rv = -1;

  assert(state_data);

  _FIID_OBJ_CREATE(cmd_rs, tmpl_cmd_warm_reset_rs);

  if (ipmi_cmd_warm_reset (state_data->ipmi_ctx, cmd_rs) < 0)
    {
      pstdout_fprintf(state_data->pstate,
                      stderr,
                      "ipmi_cmd_warm_reset: %s\n",
                      ipmi_ctx_strerror(ipmi_ctx_errnum(state_data->ipmi_ctx)));
      goto cleanup;
    }
  
  rv = 0;
 cleanup:
  _FIID_OBJ_DESTROY(cmd_rs);
  return rv;
}

static int
get_self_test_results (bmc_device_state_data_t *state_data)
{
  fiid_obj_t cmd_rs = NULL;
  uint64_t val;
  int rv = -1;

  assert(state_data);

  _FIID_OBJ_CREATE(cmd_rs, tmpl_cmd_get_self_test_results_rs);

  if (ipmi_cmd_get_self_test_results (state_data->ipmi_ctx, cmd_rs) < 0)
    {
      pstdout_fprintf(state_data->pstate,
                      stderr,
                      "ipmi_cmd_get_self_test_results: %s\n",
                      ipmi_ctx_strerror(ipmi_ctx_errnum(state_data->ipmi_ctx)));
      goto cleanup;
    }
  
  _FIID_OBJ_GET (cmd_rs,
                 "self_test_result",
                 &val);
  
  pstdout_printf(state_data->pstate,
                 "Self Test Result: ");
  if (val == IPMI_SELF_TEST_RESULT_NO_ERROR)
    pstdout_printf(state_data->pstate,
                   "No Error\n");
  else if (val == IPMI_SELF_TEST_RESULT_SELF_TEST_FUNCTION_NOT_IMPLEMENTED_IN_THIS_CONTROLLER)
    pstdout_printf(state_data->pstate,
                   "Self Test function not implemented in this controller.\n");
  else if (val == IPMI_SELF_TEST_RESULT_CORRUPTED_OR_INACCESSIBLE_DATA_OR_DEVICES)
    pstdout_printf(state_data->pstate,
                   "Corrupted or inaccessible data or devices\n");
  else if (val == IPMI_SELF_TEST_RESULT_FATAL_HARDWARE_ERROR)
    pstdout_printf(state_data->pstate,
                   "Fatal hardware error (system should consider BMC inoperative).  Controller hardware may need to be repaired or replaced.\n");
  else
    pstdout_printf(state_data->pstate,
                   "Device-specific error: 0x%X\n",
                   val);

  if (val == IPMI_SELF_TEST_RESULT_CORRUPTED_OR_INACCESSIBLE_DATA_OR_DEVICES)
    {
      _FIID_OBJ_GET (cmd_rs, 
                     "controller_operation_firmware_corrupted",
                     &val);
      if (val)
        pstdout_printf(state_data->pstate,
                       "                  [Controller operation firmware corrupted]\n");
                      
      _FIID_OBJ_GET (cmd_rs, 
                     "controller_update_boot_block_firmware_corrupted",
                     &val);
      if (val)
        pstdout_printf(state_data->pstate,
                       "                  [Controller update 'boot block' firmware corrupted]\n");

      _FIID_OBJ_GET (cmd_rs, 
                     "internal_use_area_of_bmc_fru_corrupted",
                     &val);
      if (val)
        pstdout_printf(state_data->pstate,
                       "                  [Internal Use Area of BMC FRU corrupted]\n");

      _FIID_OBJ_GET (cmd_rs, 
                     "sdr_repository_empty",
                     &val);
      if (val)
        pstdout_printf(state_data->pstate,
                       "                  [SDR Repository empty]\n");

      _FIID_OBJ_GET (cmd_rs, 
                     "ipmb_signal_lines_do_not_respond",
                     &val);
      if (val)
        pstdout_printf(state_data->pstate,
                       "                  [IPMB signal lines do not respond]\n");

      _FIID_OBJ_GET (cmd_rs, 
                     "cannot_access_bmc_fru_device",
                     &val);
      if (val)
        pstdout_printf(state_data->pstate,
                       "                  [Cannot access BMC FRU device]\n");

      _FIID_OBJ_GET (cmd_rs, 
                     "cannot_access_sdr_repository",
                     &val);
      if (val)
        pstdout_printf(state_data->pstate,
                       "                  [Cannot access SDR Repository]\n");

      _FIID_OBJ_GET (cmd_rs, 
                     "cannot_access_sel_device",
                     &val);
      if (val)
        pstdout_printf(state_data->pstate,
                       "                  [Cannot access SEL device]\n");

    }

  rv = 0;
 cleanup:
  _FIID_OBJ_DESTROY(cmd_rs);
  return rv;
}

static int
get_acpi_power_state (bmc_device_state_data_t *state_data)
{
  fiid_obj_t cmd_rs = NULL;
  uint64_t val;
  char *statestr = NULL;
  char statestrbuf[1024];
  char *verbosestr = NULL;
  char verbosestrbuf[1024];
  int rv = -1;

  assert(state_data);

  _FIID_OBJ_CREATE(cmd_rs, tmpl_cmd_get_acpi_power_state_rs);

  if (ipmi_cmd_get_acpi_power_state (state_data->ipmi_ctx, cmd_rs) < 0)
    {
      pstdout_fprintf(state_data->pstate,
                      stderr,
                      "ipmi_cmd_get_acpi_power_state: %s\n",
                      ipmi_ctx_strerror(ipmi_ctx_errnum(state_data->ipmi_ctx)));
      goto cleanup;
    }
  
  _FIID_OBJ_GET (cmd_rs,
                 "system_power_state_enumeration",
                 &val);

  switch (val) 
    {
    case IPMI_ACPI_SYSTEM_POWER_STATE_S0_G0:
      statestr = "S0/G0";
      verbosestr = "working";
      break;
    case IPMI_ACPI_SYSTEM_POWER_STATE_S1:
      statestr = "S1";
      verbosestr = "hardware context maintained, typically equates to processor/chip set clocks stopped";
      break;
    case IPMI_ACPI_SYSTEM_POWER_STATE_S2:
      statestr = "S2";
      verbosestr = "typically equates to stopped clocks with processor/cache context lost";
      break;
    case IPMI_ACPI_SYSTEM_POWER_STATE_S3:
      statestr = "S3";
      verbosestr = "typically equates to \"suspend-to-RAM\"";
      break;
    case IPMI_ACPI_SYSTEM_POWER_STATE_S4:
      statestr = "S4";
      verbosestr = "typically equates to \"suspend-to-disk\"";
      break;
    case IPMI_ACPI_SYSTEM_POWER_STATE_S5_G2:
      statestr = "S5/G2";
      verbosestr = "soft off";
      break;
    case IPMI_ACPI_SYSTEM_POWER_STATE_S4_S5:
      statestr = "S4/S5";
      verbosestr = "soft off, cannot differentiate between S4 and S5";
      break;
    case IPMI_ACPI_SYSTEM_POWER_STATE_G3:
      statestr = "G3";
      verbosestr = "mechanical off";
      break;
    case IPMI_ACPI_SYSTEM_POWER_STATE_SLEEPING:
      statestr = "SLEEPING";
      verbosestr = "sleeping - cannot differentiate between S1-S3";
      break;
    case IPMI_ACPI_SYSTEM_POWER_STATE_G1_SLEEPING:
      statestr = "G1 SLEEPING";
      verbosestr = "sleeping - cannot differentiate between S1-S4";
      break;
    case IPMI_ACPI_SYSTEM_POWER_STATE_OVERRIDE:
      statestr = "OVERRIDE";
      verbosestr = "S5 entered by override";
      break;
    case IPMI_ACPI_SYSTEM_POWER_STATE_LEGACY_ON:
      statestr = "LEGACY_ON";
      /* achu: specification text uses singular "system".  I substitute in correct english. */
      verbosestr = "Legacy On (indicates On for systems that don't support ACPI or have ACPI capabilities disabled:";
      break;
    case IPMI_ACPI_SYSTEM_POWER_STATE_LEGACY_OFF:
      statestr = "LEGACY_OFF";
      verbosestr = "Legacy Soft-Off";
      break;
    case IPMI_ACPI_SYSTEM_POWER_STATE_UNKNOWN:
      statestr = "UNKNOWN";
      verbosestr = "power state has not been initialized, or device lost track of power state";
      break;
    default:
      snprintf(statestrbuf, 1024, "UNSPECIFIED");
      statestr = statestrbuf;
      snprintf(verbosestrbuf, 1024, "0x%X", (unsigned int)val);
      verbosestr = verbosestrbuf;
      break;
    }
  
  if (state_data->prog_data->args->verbose && verbosestr)
    pstdout_printf(state_data->pstate,
                   "ACPI System Power State: %s: %s\n",
                   statestr,
                   verbosestr);
  else
    pstdout_printf(state_data->pstate,
                   "ACPI System Power State: %s\n",
                   statestr);

  statestr = NULL;
  verbosestr = NULL;

  _FIID_OBJ_GET (cmd_rs,
                 "device_power_state_enumeration",
                 &val);
  switch (val) 
    {
    case IPMI_ACPI_DEVICE_POWER_STATE_D0:
      statestr = "D0";
      break;
    case IPMI_ACPI_DEVICE_POWER_STATE_D1:
      statestr = "D1";
      break;
    case IPMI_ACPI_DEVICE_POWER_STATE_D2:
      statestr = "D2";
      break;
    case IPMI_ACPI_DEVICE_POWER_STATE_D3:
      statestr = "D3";
      break;
    case IPMI_ACPI_DEVICE_POWER_STATE_UNKNOWN:
      statestr = "UNKNOWN";
      verbosestr = "power state has not been initialized, or device lost track of power state";
      break;
    default:
      snprintf(statestrbuf, 1024, "UNSPECIFIED");
      statestr = statestrbuf;
      snprintf(verbosestrbuf, 1024, "0x%X", (unsigned int)val);
      verbosestr = verbosestrbuf;
      break;
    }

  if (state_data->prog_data->args->verbose && verbosestr)
    pstdout_printf(state_data->pstate,
                   "ACPI Device Power State: %s: %s\n",
                   statestr,
                   verbosestr);
  else
    pstdout_printf(state_data->pstate,
                   "ACPI Device Power State: %s\n",
                   statestr);
  
  rv = 0;
 cleanup:
  _FIID_OBJ_DESTROY(cmd_rs);
  return rv;
}

static int
set_acpi_power_state (bmc_device_state_data_t *state_data)
{
  fiid_obj_t cmd_rs = NULL;
  uint8_t system_power_state;
  uint8_t device_power_state;
  int rv = -1;

  assert(state_data);

  _FIID_OBJ_CREATE(cmd_rs, tmpl_cmd_set_acpi_power_state_rs);

  system_power_state = state_data->prog_data->args->set_acpi_power_state_args.system_power_state;
  device_power_state = state_data->prog_data->args->set_acpi_power_state_args.device_power_state;

  if (ipmi_cmd_set_acpi_power_state (state_data->ipmi_ctx, 
                                     system_power_state,
                                     (system_power_state == IPMI_ACPI_SYSTEM_POWER_STATE_NO_CHANGE) ? IPMI_ACPI_SET_SYSTEM_POWER_STATE_DONT_SET_SYSTEM_POWER_STATE : IPMI_ACPI_SET_SYSTEM_POWER_STATE_SET_SYSTEM_POWER_STATE,
                                     device_power_state,
                                     (device_power_state == IPMI_ACPI_DEVICE_POWER_STATE_NO_CHANGE) ? IPMI_ACPI_SET_DEVICE_POWER_STATE_DONT_SET_DEVICE_POWER_STATE : IPMI_ACPI_SET_DEVICE_POWER_STATE_SET_DEVICE_POWER_STATE,
                                     cmd_rs) < 0)
    {
      pstdout_fprintf(state_data->pstate,
                      stderr,
                      "ipmi_cmd_set_acpi_power_state: %s\n",
                      ipmi_ctx_strerror(ipmi_ctx_errnum(state_data->ipmi_ctx)));
      goto cleanup;
    }

  rv = 0;
 cleanup:
  _FIID_OBJ_DESTROY(cmd_rs);
  return rv;
}

static int
get_lan_statistics (bmc_device_state_data_t *state_data)
{
  fiid_obj_t cmd_rs = NULL;
  int8_t lan_channel_number;
  uint64_t val;
  int rv = -1;

  assert(state_data);

  _FIID_OBJ_CREATE(cmd_rs, tmpl_cmd_get_ip_udp_rmcp_statistics_rs);

  if ((lan_channel_number = ipmi_get_channel_number (state_data->ipmi_ctx,
                                                     IPMI_CHANNEL_MEDIUM_TYPE_LAN_802_3)) < 0)
    {
      pstdout_fprintf(state_data->pstate,
                      stderr,
                      "ipmi_get_channel_number: %s\n",
                      ipmi_ctx_strerror(ipmi_ctx_errnum(state_data->ipmi_ctx)));
      goto cleanup;
    }

  if (ipmi_cmd_get_ip_udp_rmcp_statistics (state_data->ipmi_ctx,
                                           lan_channel_number,
                                           IPMI_DONT_CLEAR_ALL_STATISTICS,
                                           cmd_rs) < 0)
    {
      pstdout_fprintf(state_data->pstate,
                      stderr,
                      "ipmi_cmd_get_ip_udp_rmcp_statistics: %s\n",
                      ipmi_ctx_strerror(ipmi_ctx_errnum(state_data->ipmi_ctx)));
      goto cleanup;
    }

  _FIID_OBJ_GET (cmd_rs, 
                 "ip_packets_received",
                 &val);
  pstdout_printf (state_data->pstate,
                  "IP Packets Received: %u\n",
                  (uint16_t)val);

  _FIID_OBJ_GET (cmd_rs, 
                 "received_ip_header_errors",
                 &val);
  pstdout_printf (state_data->pstate,
                  "Received IP Header Errors: %u\n",
                  (uint16_t)val);

  _FIID_OBJ_GET (cmd_rs, 
                 "received_ip_address_errors",
                 &val);
  pstdout_printf (state_data->pstate,
                  "Received IP Address Errors: %u\n",
                  (uint16_t)val);

  _FIID_OBJ_GET (cmd_rs, 
                 "fragmented_ip_packets_received",
                 &val);
  pstdout_printf (state_data->pstate,
                  "Fragmented IP Packets Received: %u\n",
                  (uint16_t)val);

  _FIID_OBJ_GET (cmd_rs, 
                 "ip_packets_transmitted",
                 &val);
  pstdout_printf (state_data->pstate,
                  "IP Packets Transmitted: %u\n",
                  (uint16_t)val);

  _FIID_OBJ_GET (cmd_rs, 
                 "udp_packets_received",
                 &val);
  pstdout_printf (state_data->pstate,
                  "UDP Packets Received: %u\n",
                  (uint16_t)val);

  _FIID_OBJ_GET (cmd_rs, 
                 "valid_rmcp_packets_received",
                 &val);
  pstdout_printf (state_data->pstate,
                  "Valid RMCP Packets Received: %u\n",
                  (uint16_t)val);

  _FIID_OBJ_GET (cmd_rs, 
                 "udp_proxy_packets_received",
                 &val);
  pstdout_printf (state_data->pstate,
                  "UDP Proxy Packets Received: %u\n",
                  (uint16_t)val);

  _FIID_OBJ_GET (cmd_rs, 
                 "udp_proxy_packets_dropped",
                 &val);
  pstdout_printf (state_data->pstate,
                  "UDP Proxy Packets Dropped: %u\n",
                  (uint16_t)val);
  
  rv = 0;
 cleanup:
  _FIID_OBJ_DESTROY(cmd_rs);
  return rv;
}

static int
clear_lan_statistics (bmc_device_state_data_t *state_data)
{
  fiid_obj_t cmd_rs = NULL;
  int8_t lan_channel_number;
  int rv = -1;

  assert(state_data);

  _FIID_OBJ_CREATE(cmd_rs, tmpl_cmd_get_ip_udp_rmcp_statistics_rs);

  if ((lan_channel_number = ipmi_get_channel_number (state_data->ipmi_ctx,
                                                     IPMI_CHANNEL_MEDIUM_TYPE_LAN_802_3)) < 0)
    {
      pstdout_fprintf(state_data->pstate,
                      stderr,
                      "ipmi_get_channel_number: %s\n",
                      ipmi_ctx_strerror(ipmi_ctx_errnum(state_data->ipmi_ctx)));
      goto cleanup;
    }

  if (ipmi_cmd_get_ip_udp_rmcp_statistics (state_data->ipmi_ctx,
                                           lan_channel_number,
                                           IPMI_CLEAR_ALL_STATISTICS,
                                           cmd_rs) < 0)
    {
      pstdout_fprintf(state_data->pstate,
                      stderr,
                      "ipmi_cmd_get_ip_udp_rmcp_statistics: %s\n",
                      ipmi_ctx_strerror(ipmi_ctx_errnum(state_data->ipmi_ctx)));
      goto cleanup;
    }
  
  rv = 0;
 cleanup:
  _FIID_OBJ_DESTROY(cmd_rs);
  return rv;
}

static int
get_sdr_repository_time (bmc_device_state_data_t *state_data)
{
  fiid_obj_t cmd_rs = NULL;
  uint64_t val;
  char str[512];
  int rv = -1;
  time_t t;
  struct tm tm;

  assert(state_data);

  _FIID_OBJ_CREATE (cmd_rs, tmpl_cmd_get_sdr_repository_time_rs);

  if (ipmi_cmd_get_sdr_repository_time (state_data->ipmi_ctx, cmd_rs) < 0)
    {
      pstdout_fprintf(state_data->pstate,
                      stderr,
                      "ipmi_cmd_get_sdr_repository_time: %s\n",
                      ipmi_ctx_strerror(ipmi_ctx_errnum(state_data->ipmi_ctx)));
      goto cleanup;
    }

  _FIID_OBJ_GET (cmd_rs, "time", &val);

  t = val;
  localtime_r (&t, &tm);
  strftime (str, sizeof (str), "%m/%d/%Y - %H:%M:%S", &tm);
  pstdout_printf (state_data->pstate,
                  "SDR Repository Time: %s\n",
                  str);
  rv = 0;
 cleanup:
  _FIID_OBJ_DESTROY(cmd_rs);
  return (rv);
}

static int
set_sdr_repository_time (bmc_device_state_data_t *state_data)
{
  struct bmc_device_arguments *args;
  fiid_obj_t cmd_rs = NULL;
  int rv = -1;
  time_t t;
  struct tm tm;

  assert(state_data);

  args = state_data->prog_data->args;

  if (!strcasecmp(args->set_sdr_repository_time_arg, "now"))
    t = time(NULL);
  else
    {
      if (!strptime(args->set_sdr_repository_time_arg, "%m/%d/%Y - %H:%M:%S", &tm))
        {
          pstdout_fprintf(state_data->pstate,
                          stderr,
                          "Invalid time specification '%s'.\n",
                          args->set_sdr_repository_time_arg);
          goto cleanup;
        }
      if ((t = mktime(&tm)) == (time_t)-1)
        {
          pstdout_fprintf(state_data->pstate,
                          stderr,
                          "Time specification '%s' cannot be represented.\n",
                          args->set_sdr_repository_time_arg);
          goto cleanup;
        }
    }

  _FIID_OBJ_CREATE (cmd_rs, tmpl_cmd_set_sdr_repository_time_rs);

  if (ipmi_cmd_set_sdr_repository_time (state_data->ipmi_ctx, t, cmd_rs) < 0)
    {
      pstdout_fprintf(state_data->pstate,
                      stderr,
                      "ipmi_cmd_set_sdr_repository_time: %s\n",
                      ipmi_ctx_strerror(ipmi_ctx_errnum(state_data->ipmi_ctx)));
      goto cleanup;
    }

  rv = 0;
 cleanup:
  _FIID_OBJ_DESTROY(cmd_rs);
  return (rv);
}

static int
get_sel_time (bmc_device_state_data_t *state_data)
{
  fiid_obj_t cmd_rs = NULL;
  uint64_t val;
  char str[512];
  int rv = -1;
  time_t t;
  struct tm tm;

  assert(state_data);

  _FIID_OBJ_CREATE (cmd_rs, tmpl_cmd_get_sel_time_rs);

  if (ipmi_cmd_get_sel_time (state_data->ipmi_ctx, cmd_rs) < 0)
    {
      pstdout_fprintf(state_data->pstate,
                      stderr,
                      "ipmi_cmd_get_sel_time: %s\n",
                      ipmi_ctx_strerror(ipmi_ctx_errnum(state_data->ipmi_ctx)));
      goto cleanup;
    }

  _FIID_OBJ_GET (cmd_rs, "time", &val);

  t = val;
  localtime_r (&t, &tm);
  strftime (str, sizeof (str), "%m/%d/%Y - %H:%M:%S", &tm);
  pstdout_printf (state_data->pstate,
                  "SEL Time: %s\n",
                  str);
  rv = 0;
 cleanup:
  _FIID_OBJ_DESTROY(cmd_rs);
  return (rv);
}

static int
set_sel_time (bmc_device_state_data_t *state_data)
{
  struct bmc_device_arguments *args;
  fiid_obj_t cmd_rs = NULL;
  int rv = -1;
  time_t t;
  struct tm tm;

  assert(state_data);

  args = state_data->prog_data->args;

  if (!strcasecmp(args->set_sel_time_arg, "now"))
    t = time(NULL);
  else
    {
      if (!strptime(args->set_sel_time_arg, "%m/%d/%Y - %H:%M:%S", &tm))
        {
          pstdout_fprintf(state_data->pstate,
                          stderr,
                          "Invalid time specification '%s'.\n",
                          args->set_sel_time_arg);
          goto cleanup;
        }
      if ((t = mktime(&tm)) == (time_t)-1)
        {
          pstdout_fprintf(state_data->pstate,
                          stderr,
                          "Time specification '%s' cannot be represented.\n",
                          args->set_sel_time_arg);
          goto cleanup;
        }
    }

  _FIID_OBJ_CREATE (cmd_rs, tmpl_cmd_set_sel_time_rs);

  if (ipmi_cmd_set_sel_time (state_data->ipmi_ctx, t, cmd_rs) < 0)
    {
      pstdout_fprintf(state_data->pstate,
                      stderr,
                      "ipmi_cmd_set_sel_time: %s\n",
                      ipmi_ctx_strerror(ipmi_ctx_errnum(state_data->ipmi_ctx)));
      goto cleanup;
    }

  rv = 0;
 cleanup:
  _FIID_OBJ_DESTROY(cmd_rs);
  return (rv);
}

static int
parse_hex_byte (bmc_device_state_data_t *state_data,
                char *from,
                uint8_t *to,
                char *str)
{
  int i;

  assert (state_data);
  assert (from);
  assert (to);
  assert (str);

  if (strlen (from) >= 2)
    {
      if (strncmp (from, "0x", 2) == 0)
        from += 2;
    }

  if (*from == '\0')
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "invalid hex byte argument for %s\n",
                       str);
      return (-1);
    }
  
  for (i = 0; from[i] != '\0'; i++)
    {
      if (i >= 2)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "invalid hex byte argument for %s\n",
                           str);
          return (-1);
        }
      
      if (!isxdigit (from[i]))
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "invalid hex byte argument for %s\n",
                           str);
          return (-1);
        }
    }

  (*to) = strtol (from, (char **) NULL, 16);
  
  return (0);
}

static int
platform_event (bmc_device_state_data_t *state_data)
{
  struct bmc_device_arguments *args;
  fiid_obj_t obj_cmd_get_channel_info_rs = NULL;
  fiid_obj_t obj_cmd_rs = NULL;
  char *platform_event_arg_cpy;
  char *str_args[BMC_DEVICE_MAX_EVENT_ARGS];
  unsigned int num_str_args = 0;
  char *str_ptr;
  char *lasts;
  unsigned int str_args_index = 0;
  uint8_t channel_medium_type;
  uint8_t generator_id;
  uint8_t *generator_id_ptr = NULL;
  uint8_t event_message_format_version;
  uint8_t sensor_type;
  uint8_t sensor_number;
  uint8_t event_type;
  uint8_t event_direction;
  uint8_t event_data1;
  uint8_t event_data2;
  uint8_t event_data3;
  uint64_t val;
  int rv = -1;

  assert (state_data);

  args = state_data->prog_data->args;

  if (!(platform_event_arg_cpy = strdup(args->platform_event_arg)))
    {
      pstdout_perror (state_data->pstate, "strdup");
      goto cleanup;
    }

  str_ptr = strtok_r (platform_event_arg_cpy, " \t\0", &lasts);
  while (str_ptr && num_str_args < BMC_DEVICE_MAX_EVENT_ARGS)
    {
      str_args[num_str_args] = str_ptr;
      num_str_args++;

      str_ptr = strtok_r (NULL, " \t\0", &lasts);
    }

  if (num_str_args != BMC_DEVICE_MAX_EVENT_ARGS
      && num_str_args != (BMC_DEVICE_MAX_EVENT_ARGS - 1))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "Invalid number of arguments specified\n");
      goto cleanup;
    }

  _FIID_OBJ_CREATE(obj_cmd_get_channel_info_rs, tmpl_cmd_get_channel_info_rs);

  if (ipmi_cmd_get_channel_info (state_data->ipmi_ctx,
                                 IPMI_CHANNEL_NUMBER_CURRENT_CHANNEL,
                                 obj_cmd_get_channel_info_rs) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_cmd_get_channel_info: %s",
                       ipmi_ctx_strerror(ipmi_ctx_errnum(state_data->ipmi_ctx)));
      goto cleanup;
    }

  _FIID_OBJ_GET (obj_cmd_get_channel_info_rs,
                 "channel_medium_type",
                 &val);
  channel_medium_type = val;
  
  if (channel_medium_type == IPMI_CHANNEL_MEDIUM_TYPE_SYS_IFACE
      && num_str_args != BMC_DEVICE_MAX_EVENT_ARGS)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "Generator ID required for given system interface\n");
      goto cleanup;
    }
  
  /* see if generator_id specified */
  if (num_str_args == BMC_DEVICE_MAX_EVENT_ARGS)
    {
      if (parse_hex_byte (state_data,
                          str_args[str_args_index],
                          &generator_id,
                          "generator id") < 0)
        goto cleanup;
      generator_id_ptr = &generator_id;
      str_args_index++;
    }
  
  if (parse_hex_byte (state_data,
                      str_args[str_args_index],
                      &event_message_format_version,
                      "event message format version") < 0)
    goto cleanup;
  str_args_index++;
  
  if (parse_hex_byte (state_data,
                      str_args[str_args_index],
                      &sensor_type,
                      "sensor type") < 0)
    goto cleanup;
  str_args_index++;

  if (parse_hex_byte (state_data,
                      str_args[str_args_index],
                      &sensor_number,
                      "sensor number") < 0)
    goto cleanup;
  str_args_index++;

  if (parse_hex_byte (state_data,
                      str_args[str_args_index],
                      &event_type,
                      "event type") < 0)
    goto cleanup;
  str_args_index++;

  if (!strcasecmp (str_args[str_args_index], "assertion"))
    event_direction = IPMI_SEL_RECORD_ASSERTION_EVENT;
  else if (!strcasecmp (str_args[str_args_index], "deassertion"))
    event_direction = IPMI_SEL_RECORD_DEASSERTION_EVENT;
  else
    {
      if (parse_hex_byte (state_data,
                          str_args[str_args_index],
                          &event_direction,
                          "event direction") < 0)
        goto cleanup;

      if (!IPMI_SEL_RECORD_EVENT_DIRECTION_VALID (event_direction))
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "invalid hex byte argument for event direction\n");
          goto cleanup;
        }
    }
  str_args_index++;
    
  if (parse_hex_byte (state_data,
                      str_args[str_args_index],
                      &event_data1,
                      "event data1") < 0)
    goto cleanup;
  str_args_index++;

  if (parse_hex_byte (state_data,
                      str_args[str_args_index],
                      &event_data2,
                      "event data2") < 0)
    goto cleanup;
  str_args_index++;

  if (parse_hex_byte (state_data,
                      str_args[str_args_index],
                      &event_data3,
                      "event data3") < 0)
    goto cleanup;
  str_args_index++;

  _FIID_OBJ_CREATE(obj_cmd_rs, tmpl_cmd_platform_event_rs);

  if (ipmi_cmd_platform_event (state_data->ipmi_ctx,
                               generator_id_ptr,
                               event_message_format_version,
                               sensor_type,
                               sensor_number,
                               event_type,
                               event_direction,
                               event_data1,
                               event_data2,
                               event_data3,
                               obj_cmd_rs) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_cmd_platform_event: %s\n",
                       ipmi_ctx_strerror(ipmi_ctx_errnum(state_data->ipmi_ctx)));
      goto cleanup;
    }

  rv = 0;
 cleanup:
  if (platform_event_arg_cpy)
    free (platform_event_arg_cpy);
  _FIID_OBJ_DESTROY (obj_cmd_get_channel_info_rs);
  _FIID_OBJ_DESTROY (obj_cmd_rs);
  return (rv);
} 

static int
get_mca_auxiliary_log_status (bmc_device_state_data_t *state_data)
{
  fiid_obj_t cmd_rs = NULL;
  fiid_obj_t mca_cmd_rs = NULL;
  uint64_t val;
  char str[512];
  int rv = -1;
  time_t t;
  struct tm tm;

  assert(state_data);

  _FIID_OBJ_CREATE(cmd_rs, tmpl_cmd_get_auxiliary_log_status_rs);

  if (ipmi_cmd_get_auxiliary_log_status (state_data->ipmi_ctx, 
                                         IPMI_AUXILIARY_LOG_TYPE_MCA,
                                         cmd_rs) < 0)
    {
      pstdout_fprintf(state_data->pstate,
                      stderr,
                      "ipmi_cmd_get_auxiliary_log_status: %s\n",
                      ipmi_ctx_strerror(ipmi_ctx_errnum(state_data->ipmi_ctx)));
      goto cleanup;
    }
  
  _FIID_OBJ_COPY(mca_cmd_rs,
                 cmd_rs,
                 tmpl_cmd_get_auxiliary_log_status_mca_rs);

  _FIID_OBJ_GET (mca_cmd_rs,
                 "timestamp",
                 &val);

  t = val;
  localtime_r (&t, &tm);
  strftime (str, sizeof (str), "%m/%d/%Y - %H:%M:%S", &tm);
  pstdout_printf (state_data->pstate,
                  "Last Entry Added to MCA Log: %s\n",
                  str);
  
  _FIID_OBJ_GET (mca_cmd_rs,
                 "mca_log_entry_count",
                 &val);
  
  pstdout_printf (state_data->pstate,
                  "Number of entries in MCA log: %u\n",
                  (uint32_t)val);

  rv = 0;
 cleanup:
  _FIID_OBJ_DESTROY(cmd_rs);
  _FIID_OBJ_DESTROY(mca_cmd_rs);
  return rv;
}

static int
get_ssif_interface_capabilities (bmc_device_state_data_t *state_data)
{
  fiid_obj_t cmd_rs = NULL;
  uint64_t val;
  int rv = -1;

  assert(state_data);

  _FIID_OBJ_CREATE (cmd_rs, tmpl_cmd_get_system_interface_capabilities_ssif_rs);

  if (ipmi_cmd_get_system_interface_capabilities_ssif (state_data->ipmi_ctx, 
                                                       cmd_rs) < 0)
    {
      pstdout_fprintf(state_data->pstate,
                      stderr,
                      "ipmi_cmd_get_system_interface_capabilities_ssif: %s\n",
                      ipmi_ctx_strerror(ipmi_ctx_errnum(state_data->ipmi_ctx)));
      goto cleanup;
    }

  _FIID_OBJ_GET(cmd_rs, "ssif_version", &val);

  /* achu: for some stupid reason 000b == "version 1" */
  if (val == IPMI_SSIF_SYSTEM_INTERFACE_VERSION_1)
    pstdout_printf(state_data->pstate,
                   "SSIF Version:                     %X (version 1)\n", (unsigned int) val);
  else
    pstdout_printf(state_data->pstate,
                   "SSIF Version:                     %X\n", (unsigned int) val);
  
  _FIID_OBJ_GET(cmd_rs, "pec_support", &val);
  
  pstdout_printf(state_data->pstate,
                 "SSIF PEC Support:                 ");
  switch (val)
    {
    case IPMI_SSIF_SYSTEM_INTERFACE_IMPLEMENTS_PEC:
      pstdout_printf (state_data->pstate, "Yes\n");
      break;
    case IPMI_SSIF_SYSTEM_INTERFACE_DOES_NOT_SUPPORT_PEC:
      pstdout_printf (state_data->pstate, "No\n");
      break;
    default:
      pstdout_printf (state_data->pstate, "Unknown\n");
      break;
    }

  _FIID_OBJ_GET(cmd_rs, "transaction_support", &val);

  pstdout_printf(state_data->pstate,
                 "SSIF Transaction Support:         ");
  switch (val)
    {
    case IPMI_SSIF_SYSTEM_INTERFACE_TRANSACTION_SUPPORT_SINGLE_PART_READS_WRITES_SUPPORTED:
      pstdout_printf (state_data->pstate, "Only single-part reads/writes supported.\n");
      break;
    case IPMI_SSIF_SYSTEM_INTERFACE_TRANSACTION_SUPPORT_MULTI_PART_READS_WRITES_SUPPORTED_START_AND_END_ONLY:
      pstdout_printf (state_data->pstate, "multi-part reads/writes upported.  Start and End transactions only.\n");
      break;
    case IPMI_SSIF_SYSTEM_INTERFACE_TRANSACTION_SUPPORT_MULTI_PART_READS_WRITES_SUPPORTED_START_MIDDLE_END:
      pstdout_printf (state_data->pstate, "multi-part reads/writes upported.  Start, Middle, and End transactions supported.\n");
      break;
    default:
      pstdout_printf (state_data->pstate, "Unknown\n");
      break;
    }

  _FIID_OBJ_GET(cmd_rs, "input_message_size", &val);

  pstdout_printf (state_data->pstate,
                  "SSIF Maximum Input Message Size:  %u bytes\n",
                  val);

  _FIID_OBJ_GET(cmd_rs, "output_message_size", &val);

  pstdout_printf (state_data->pstate,
                  "SSIF Maximum Output Message Size: %u bytes\n",
                  val);

  rv = 0;
 cleanup:
  _FIID_OBJ_DESTROY(cmd_rs);
  return (rv);
}

static int
get_kcs_interface_capabilities (bmc_device_state_data_t *state_data)
{
  fiid_obj_t cmd_rs = NULL;
  uint64_t val;
  int rv = -1;

  assert(state_data);

  _FIID_OBJ_CREATE (cmd_rs, tmpl_cmd_get_system_interface_capabilities_kcs_rs);

  if (ipmi_cmd_get_system_interface_capabilities_kcs (state_data->ipmi_ctx, 
                                                       cmd_rs) < 0)
    {
      pstdout_fprintf(state_data->pstate,
                      stderr,
                      "ipmi_cmd_get_system_interface_capabilities_kcs: %s\n",
                      ipmi_ctx_strerror(ipmi_ctx_errnum(state_data->ipmi_ctx)));
      goto cleanup;
    }

  _FIID_OBJ_GET(cmd_rs, "system_interface_version", &val);

  /* achu: for some stupid reason 000b == "version 1" */
  if (val == IPMI_KCS_SYSTEM_INTERFACE_VERSION_1)
    pstdout_printf(state_data->pstate,
                   "KCS Version:                    %X (version 1)\n", (unsigned int) val);
  else
    pstdout_printf(state_data->pstate,
                   "KCS Version:                    %X\n", (unsigned int) val);
  
  _FIID_OBJ_GET(cmd_rs, "input_maximum_message_size", &val);

  pstdout_printf (state_data->pstate,
                  "KCS Maximum Input Message Size: %u bytes\n",
                  val);

  rv = 0;
 cleanup:
  _FIID_OBJ_DESTROY(cmd_rs);
  return (rv);
}

static int
get_bt_interface_capabilities (bmc_device_state_data_t *state_data)
{
  fiid_obj_t cmd_rs = NULL;
  uint64_t val;
  int rv = -1;

  assert(state_data);

  _FIID_OBJ_CREATE (cmd_rs, tmpl_cmd_get_bt_interface_capabilities_rs);

  if (ipmi_cmd_get_bt_interface_capabilities (state_data->ipmi_ctx, 
                                              cmd_rs) < 0)
    {
      pstdout_fprintf(state_data->pstate,
                      stderr,
                      "ipmi_cmd_get_bt_interface_capabilities: %s\n",
                      ipmi_ctx_strerror(ipmi_ctx_errnum(state_data->ipmi_ctx)));
      goto cleanup;
    }

  _FIID_OBJ_GET(cmd_rs, "number_of_outstanding_requests_supported", &val);

  pstdout_printf (state_data->pstate,
                  "BT Number of Outstanding Requests Supported: %u\n",
                  val);

  _FIID_OBJ_GET(cmd_rs, "input_buffer_size", &val);

  pstdout_printf (state_data->pstate,
                  "BT Input Buffer Size:                        %u bytes\n",
                  val);

  _FIID_OBJ_GET(cmd_rs, "output_buffer_size", &val);

  pstdout_printf (state_data->pstate,
                  "BT Output Buffer Size:                       %u bytes\n",
                  val);

  _FIID_OBJ_GET(cmd_rs, "bmc_request_to_response_time", &val);

  pstdout_printf (state_data->pstate,
                  "BT Request to Response Time:                 %u seconds\n",
                  val);

  _FIID_OBJ_GET(cmd_rs, "recommended_retries", &val);

  pstdout_printf (state_data->pstate,
                  "BT Recommended Retries:                      %u\n",
                  val);

  rv = 0;
 cleanup:
  _FIID_OBJ_DESTROY(cmd_rs);
  return (rv);
}

int
run_cmd_args (bmc_device_state_data_t *state_data)
{
  struct bmc_device_arguments *args;
  int rv = -1;

  assert(state_data);
  
  args = state_data->prog_data->args;

  if (args->cold_reset)
    return cold_reset (state_data);

  if (args->warm_reset)
    return warm_reset (state_data);

  if (args->get_self_test_results)
    return get_self_test_results (state_data);

  if (args->get_acpi_power_state)
    return get_acpi_power_state (state_data);

  if (args->set_acpi_power_state)
    return set_acpi_power_state (state_data);

  if (args->get_lan_statistics)
    return get_lan_statistics (state_data);

  if (args->clear_lan_statistics)
    return clear_lan_statistics (state_data);

  if (args->get_sdr_repository_time)
    return get_sdr_repository_time (state_data);

  if (args->set_sdr_repository_time)
    return set_sdr_repository_time (state_data);

  if (args->get_sel_time)
    return get_sel_time (state_data);

  if (args->set_sel_time)
    return set_sel_time (state_data);

  if (args->platform_event)
    return (platform_event (state_data));

  if (args->get_mca_auxiliary_log_status)
    return get_mca_auxiliary_log_status (state_data);

  if (args->get_ssif_interface_capabilities)
    return get_ssif_interface_capabilities (state_data);

  if (args->get_kcs_interface_capabilities)
    return get_kcs_interface_capabilities (state_data);

  if (args->get_bt_interface_capabilities)
    return get_bt_interface_capabilities (state_data);

  rv = 0;
  return (rv);
}

static int
_bmc_device(pstdout_state_t pstate,
          const char *hostname,
          void *arg)
{
  bmc_device_state_data_t state_data;
  bmc_device_prog_data_t *prog_data;
  char errmsg[IPMI_OPEN_ERRMSGLEN];
  int exit_code = -1;

  prog_data = (bmc_device_prog_data_t *)arg;
  memset(&state_data, '\0', sizeof(bmc_device_state_data_t));

  state_data.prog_data = prog_data;
  state_data.pstate = pstate;
  
  if (!(state_data.ipmi_ctx = ipmi_open(prog_data->progname,
                                        hostname,
                                        &(prog_data->args->common),
                                        errmsg,
                                        IPMI_OPEN_ERRMSGLEN)))
    {
      pstdout_fprintf(pstate,
                      stderr,
                      "%s\n", 
                      errmsg);
      exit_code = EXIT_FAILURE;
      goto cleanup;
    }

  if (run_cmd_args (&state_data) < 0)
    {
      exit_code = EXIT_FAILURE;
      goto cleanup;
    }

  exit_code = 0;
 cleanup:
  if (state_data.ipmi_ctx)
    {
      ipmi_ctx_close (state_data.ipmi_ctx);
      ipmi_ctx_destroy (state_data.ipmi_ctx);
    }
  return exit_code;
}

int 
main (int argc, char **argv)
{
  bmc_device_prog_data_t prog_data;
  struct bmc_device_arguments cmd_args;
  int exit_code;
  int rv;
  
  ipmi_disable_coredump();
  
  memset(&prog_data, '\0', sizeof(bmc_device_prog_data_t));
  prog_data.progname = argv[0];
  bmc_device_argp_parse (argc, argv, &cmd_args);
  prog_data.args = &cmd_args;

  if (pstdout_setup(&(prog_data.args->common.hostname),
                    prog_data.args->hostrange.buffer_output,
                    prog_data.args->hostrange.consolidate_output,
                    prog_data.args->hostrange.fanout,
                    prog_data.args->hostrange.eliminate,
                    prog_data.args->hostrange.always_prefix) < 0)
    {
      exit_code = EXIT_FAILURE;
      goto cleanup;
    }

  if ((rv = pstdout_launch(prog_data.args->common.hostname,
                           _bmc_device,
                           &prog_data)) < 0)
    {
      fprintf(stderr, 
              "pstdout_launch: %s\n",
              pstdout_strerror(pstdout_errnum));
      exit_code = EXIT_FAILURE;
      goto cleanup;
    }

  exit_code = rv;
 cleanup:
  return (exit_code);
}
