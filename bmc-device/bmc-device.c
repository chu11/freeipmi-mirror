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
#include "tool-hostrange-common.h"
#include "tool-sdr-cache-common.h"
#include "tool-util-common.h"

typedef int (*Bmc_device_system_info_first_set)(ipmi_ctx_t ctx,
						uint8_t set_selector,
						uint8_t encoding,
						uint8_t string_length,
						const void *string_block,
						unsigned int string_block_length,
						fiid_obj_t obj_cmd_rs);

typedef int (*Bmc_device_system_info)(ipmi_ctx_t ctx,
				      uint8_t set_selector,
				      const void *string_block,
				      unsigned int string_block_length,
				      fiid_obj_t obj_cmd_rs);

#define BMC_DEVICE_MIN_REARM_SENSOR_ARGS 1
#define BMC_DEVICE_MAX_REARM_SENSOR_ARGS 3

#define BMC_DEVICE_MAX_PLATFORM_EVENT_ARGS 9

#define BMC_DEVICE_SET_SENSOR_READING_AND_EVENT_STATUS_ARGS 11

#define BMC_DEVICE_TIME_BUFLEN 512

static int
cold_reset (bmc_device_state_data_t *state_data)
{
  fiid_obj_t obj_cmd_rs = NULL;
  int rv = -1;

  assert (state_data);

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_cold_reset_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if (ipmi_cmd_cold_reset (state_data->ipmi_ctx, obj_cmd_rs) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_cmd_cold_reset: %s\n",
                       ipmi_ctx_errormsg (state_data->ipmi_ctx));
      goto cleanup;
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static int
warm_reset (bmc_device_state_data_t *state_data)
{
  fiid_obj_t obj_cmd_rs = NULL;
  int rv = -1;

  assert (state_data);

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_warm_reset_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if (ipmi_cmd_warm_reset (state_data->ipmi_ctx, obj_cmd_rs) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_cmd_warm_reset: %s\n",
                       ipmi_ctx_errormsg (state_data->ipmi_ctx));
      goto cleanup;
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static int
get_self_test_results (bmc_device_state_data_t *state_data)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint8_t self_test_result;
  uint64_t val;
  int rv = -1;

  assert (state_data);

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_get_self_test_results_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if (ipmi_cmd_get_self_test_results (state_data->ipmi_ctx, obj_cmd_rs) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_cmd_get_self_test_results: %s\n",
                       ipmi_ctx_errormsg (state_data->ipmi_ctx));
      goto cleanup;
    }

  if (FIID_OBJ_GET (obj_cmd_rs,
                    "self_test_result",
                    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'self_test_result': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  self_test_result = val;
  
  if (self_test_result == IPMI_SELF_TEST_RESULT_NO_ERROR)
    pstdout_printf (state_data->pstate,
                    "Self Test Result : No Error\n");
  else if (self_test_result == IPMI_SELF_TEST_RESULT_SELF_TEST_FUNCTION_NOT_IMPLEMENTED_IN_THIS_CONTROLLER)
    pstdout_printf (state_data->pstate,
                    "Self Test Result : Self Test function not implemented in this controller.\n");
  else if (self_test_result == IPMI_SELF_TEST_RESULT_CORRUPTED_OR_INACCESSIBLE_DATA_OR_DEVICES)
    pstdout_printf (state_data->pstate,
                    "Self Test Result                                  : Corrupted or inaccessible data or devices\n");
  else if (self_test_result == IPMI_SELF_TEST_RESULT_FATAL_HARDWARE_ERROR)
    pstdout_printf (state_data->pstate,
                    "Self Test Result : Fatal hardware error (system should consider BMC inoperative).  Controller hardware may need to be repaired or replaced.\n");
  else
    pstdout_printf (state_data->pstate,
                    "Self Test Result : Device-specific error: %Xh\n",
                    self_test_result);

  if (self_test_result == IPMI_SELF_TEST_RESULT_CORRUPTED_OR_INACCESSIBLE_DATA_OR_DEVICES)
    {
      if (FIID_OBJ_GET (obj_cmd_rs,
                        "controller_operation_firmware_corrupted",
                        &val) < 0)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "fiid_obj_get: 'controller_operation_firmware_corrupted': %s\n",
                           fiid_obj_errormsg (obj_cmd_rs));
          goto cleanup;
        }

      pstdout_printf (state_data->pstate,
                      "Controller operation firmware corrupted           : %s\n",
                      val ? "failed" : "unknown");

      if (FIID_OBJ_GET (obj_cmd_rs,
                        "controller_update_boot_block_firmware_corrupted",
                        &val) < 0)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "fiid_obj_get: 'controller_update_boot_block_firmware_corrupted': %s\n",
                           fiid_obj_errormsg (obj_cmd_rs));
          goto cleanup;
        }

      pstdout_printf (state_data->pstate,
                      "Controller update 'boot block' firmware corrupted : %s\n",
                      val ? "failed" : "unknown");

      if (FIID_OBJ_GET (obj_cmd_rs,
                        "internal_use_area_of_bmc_fru_corrupted",
                        &val) < 0)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "fiid_obj_get: 'internal_use_area_of_bmc_fru_corrupted': %s\n",
                           fiid_obj_errormsg (obj_cmd_rs));
          goto cleanup;
        }

      pstdout_printf (state_data->pstate,
                      "Internal Use Area of BMC FRU corrupted            : %s\n",
                      val ? "failed" : "unknown");

      if (FIID_OBJ_GET (obj_cmd_rs,
                        "sdr_repository_empty",
                        &val) < 0)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "fiid_obj_get: 'sdr_repository_empty': %s\n",
                           fiid_obj_errormsg (obj_cmd_rs));
          goto cleanup;
        }

      pstdout_printf (state_data->pstate,
                      "SDR Repository empty                              : %s\n",
                      val ? "failed" : "unknown");

      if (FIID_OBJ_GET (obj_cmd_rs,
                        "ipmb_signal_lines_do_not_respond",
                        &val) < 0)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "fiid_obj_get: 'ipmb_signal_lines_do_not_respond': %s\n",
                           fiid_obj_errormsg (obj_cmd_rs));
          goto cleanup;
        }

      pstdout_printf (state_data->pstate,
                      "IPMB signal lines do not respond                  : %s\n",
                      val ? "failed" : "unknown");

      if (FIID_OBJ_GET (obj_cmd_rs,
                        "cannot_access_bmc_fru_device",
                        &val) < 0)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "fiid_obj_get: 'cannot_access_bmc_fru_device': %s\n",
                           fiid_obj_errormsg (obj_cmd_rs));
          goto cleanup;
        }

      pstdout_printf (state_data->pstate,
                      "Cannot access BMC FRU device                      : %s\n",
                      val ? "failed" : "unknown");

      if (FIID_OBJ_GET (obj_cmd_rs,
                        "cannot_access_sdr_repository",
                        &val) < 0)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "fiid_obj_get: 'cannot_access_sdr_repository': %s\n",
                           fiid_obj_errormsg (obj_cmd_rs));
          goto cleanup;
        }

      pstdout_printf (state_data->pstate,
                      "Cannot access SDR Repository                      : %s\n",
                      val ? "failed" : "unknown");

      if (FIID_OBJ_GET (obj_cmd_rs,
                        "cannot_access_sel_device",
                        &val) < 0)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "fiid_obj_get: 'cannot_access_sel_device': %s\n",
                           fiid_obj_errormsg (obj_cmd_rs));
          goto cleanup;
        }

      pstdout_printf (state_data->pstate,
                      "Cannot access SEL device                          : %s\n",
                      val ? "failed" : "unknown");

    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static int
get_acpi_power_state (bmc_device_state_data_t *state_data)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint8_t system_power_state_enumeration;
  uint8_t device_power_state_enumeration;
  uint64_t val;
  char *statestr = NULL;
  char statestrbuf[1024];
  char *verbosestr = NULL;
  char verbosestrbuf[1024];
  int rv = -1;

  assert (state_data);

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_get_acpi_power_state_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if (ipmi_cmd_get_acpi_power_state (state_data->ipmi_ctx, obj_cmd_rs) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_cmd_get_acpi_power_state: %s\n",
                       ipmi_ctx_errormsg (state_data->ipmi_ctx));
      goto cleanup;
    }

  if (FIID_OBJ_GET (obj_cmd_rs,
                    "system_power_state_enumeration",
                    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'system_power_state_enumeration': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  system_power_state_enumeration = val;

  switch (system_power_state_enumeration)
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
      snprintf (statestrbuf, 1024, "UNSPECIFIED");
      statestr = statestrbuf;
      snprintf (verbosestrbuf, 1024, "%Xh", system_power_state_enumeration);
      verbosestr = verbosestrbuf;
      break;
    }

  if (state_data->prog_data->args->verbose && verbosestr)
    pstdout_printf (state_data->pstate,
                    "ACPI System Power State : %s - %s\n",
                    statestr,
                    verbosestr);
  else
    pstdout_printf (state_data->pstate,
                    "ACPI System Power State : %s\n",
                    statestr);

  statestr = NULL;
  verbosestr = NULL;

  if (FIID_OBJ_GET (obj_cmd_rs,
                    "device_power_state_enumeration",
                    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'device_power_state_enumeration': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  device_power_state_enumeration = val;

  switch (device_power_state_enumeration)
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
      snprintf (statestrbuf, 1024, "UNSPECIFIED");
      statestr = statestrbuf;
      snprintf (verbosestrbuf, 1024, "%Xh", device_power_state_enumeration);
      verbosestr = verbosestrbuf;
      break;
    }

  if (state_data->prog_data->args->verbose && verbosestr)
    pstdout_printf (state_data->pstate,
                    "ACPI Device Power State : %s - %s\n",
                    statestr,
                    verbosestr);
  else
    pstdout_printf (state_data->pstate,
                    "ACPI Device Power State : %s\n",
                    statestr);

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static int
set_acpi_power_state (bmc_device_state_data_t *state_data)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint8_t system_power_state;
  uint8_t device_power_state;
  int rv = -1;

  assert (state_data);

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_set_acpi_power_state_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  system_power_state = state_data->prog_data->args->set_acpi_power_state_args.system_power_state;
  device_power_state = state_data->prog_data->args->set_acpi_power_state_args.device_power_state;

  if (ipmi_cmd_set_acpi_power_state (state_data->ipmi_ctx,
                                     system_power_state,
                                     (system_power_state == IPMI_ACPI_SYSTEM_POWER_STATE_NO_CHANGE) ? IPMI_ACPI_SET_SYSTEM_POWER_STATE_DONT_SET_SYSTEM_POWER_STATE : IPMI_ACPI_SET_SYSTEM_POWER_STATE_SET_SYSTEM_POWER_STATE,
                                     device_power_state,
                                     (device_power_state == IPMI_ACPI_DEVICE_POWER_STATE_NO_CHANGE) ? IPMI_ACPI_SET_DEVICE_POWER_STATE_DONT_SET_DEVICE_POWER_STATE : IPMI_ACPI_SET_DEVICE_POWER_STATE_SET_DEVICE_POWER_STATE,
                                     obj_cmd_rs) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_cmd_set_acpi_power_state: %s\n",
                       ipmi_ctx_errormsg (state_data->ipmi_ctx));
      goto cleanup;
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static int
get_lan_statistics (bmc_device_state_data_t *state_data)
{
  uint16_t ip_packets_received, received_ip_header_errors, received_ip_address_errors;
  uint16_t fragmented_ip_packets_received, ip_packets_transmitted, udp_packets_received;
  uint16_t valid_rmcp_packets_received, udp_proxy_packets_received, udp_proxy_packets_dropped;
  fiid_obj_t obj_cmd_rs = NULL;
  uint8_t lan_channel_number;
  uint64_t val;
  int rv = -1;

  assert (state_data);

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_get_ip_udp_rmcp_statistics_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

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

  if (ipmi_cmd_get_ip_udp_rmcp_statistics (state_data->ipmi_ctx,
                                           lan_channel_number,
                                           IPMI_DONT_CLEAR_ALL_STATISTICS,
                                           obj_cmd_rs) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_cmd_get_ip_udp_rmcp_statistics: %s\n",
                       ipmi_ctx_errormsg (state_data->ipmi_ctx));
      goto cleanup;
    }

  if (FIID_OBJ_GET (obj_cmd_rs,
                    "ip_packets_received",
                    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'ip_packets_received': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  ip_packets_received = val;

  pstdout_printf (state_data->pstate,
                  "IP Packets Received            : %u\n",
                  ip_packets_received);

  if (FIID_OBJ_GET (obj_cmd_rs,
                    "received_ip_header_errors",
                    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'received_ip_header_errors': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  received_ip_header_errors = val;

  pstdout_printf (state_data->pstate,
                  "Received IP Header Errors      : %u\n",
                  received_ip_header_errors);

  if (FIID_OBJ_GET (obj_cmd_rs,
                    "received_ip_address_errors",
                    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'received_ip_address_errors': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  received_ip_address_errors = val;

  pstdout_printf (state_data->pstate,
                  "Received IP Address Errors     : %u\n",
                  received_ip_address_errors);

  if (FIID_OBJ_GET (obj_cmd_rs,
                    "fragmented_ip_packets_received",
                    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'fragmented_ip_packets_received': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  fragmented_ip_packets_received = val;

  pstdout_printf (state_data->pstate,
                  "Fragmented IP Packets Received : %u\n",
                  fragmented_ip_packets_received);

  if (FIID_OBJ_GET (obj_cmd_rs,
                    "ip_packets_transmitted",
                    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'ip_packets_transmitted': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  ip_packets_transmitted = val;

  pstdout_printf (state_data->pstate,
                  "IP Packets Transmitted         : %u\n",
                  ip_packets_transmitted);

  if (FIID_OBJ_GET (obj_cmd_rs,
                    "udp_packets_received",
                    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'udp_packets_received': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  udp_packets_received = val;

  pstdout_printf (state_data->pstate,
                  "UDP Packets Received           : %u\n",
                  udp_packets_received);

  if (FIID_OBJ_GET (obj_cmd_rs,
                    "valid_rmcp_packets_received",
                    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'valid_rmcp_packets_received': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  valid_rmcp_packets_received = val;

  pstdout_printf (state_data->pstate,
                  "Valid RMCP Packets Received    : %u\n",
                  valid_rmcp_packets_received);

  if (FIID_OBJ_GET (obj_cmd_rs,
                    "udp_proxy_packets_received",
                    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'udp_proxy_packets_received': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  udp_proxy_packets_received = val;
  
  pstdout_printf (state_data->pstate,
                  "UDP Proxy Packets Received     : %u\n",
                  udp_proxy_packets_received);

  if (FIID_OBJ_GET (obj_cmd_rs,
                    "udp_proxy_packets_dropped",
                    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'udp_proxy_packets_dropped': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  udp_proxy_packets_dropped = val;

  pstdout_printf (state_data->pstate,
                  "UDP Proxy Packets Dropped      : %u\n",
                  udp_proxy_packets_dropped);

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static int
clear_lan_statistics (bmc_device_state_data_t *state_data)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint8_t lan_channel_number;
  int rv = -1;

  assert (state_data);

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_get_ip_udp_rmcp_statistics_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

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

  if (ipmi_cmd_get_ip_udp_rmcp_statistics (state_data->ipmi_ctx,
                                           lan_channel_number,
                                           IPMI_CLEAR_ALL_STATISTICS,
                                           obj_cmd_rs) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_cmd_get_ip_udp_rmcp_statistics: %s\n",
                       ipmi_ctx_errormsg (state_data->ipmi_ctx));
      goto cleanup;
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static int
parse_check_hex (bmc_device_state_data_t *state_data,
		 char *from,
		 char *str,
		 unsigned int max_len_bytes)
{
  unsigned int i;

  assert (state_data);
  assert (from);
  assert (str);
  assert (max_len_bytes);

  for (i = 0; from[i] != '\0'; i++)
    {
      if (i >= (max_len_bytes * 2))
	{
	  pstdout_fprintf (state_data->pstate,
			   stderr,
			   "invalid hex length for %s\n",
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

  return (0);
}

static int
parse_uint16 (bmc_device_state_data_t *state_data,
	      char *from,
	      uint16_t *to,
	      char *str)
{
  char *endptr;

  assert (state_data);
  assert (from);
  assert (to);
  assert (str);

  if (strlen (from) >= 2)
    {
      if (!strncmp (from, "0x", 2))
	{
	  if (parse_check_hex (state_data, from + 2, str, 2) < 0)
	    return (-1);
	}
    }

  errno = 0;
  (*to) = strtol (from, &endptr, 0);
  if (errno
      || endptr[0] != '\0')
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "invalid argument for %s\n",
		       str);
      return (-1);
    }

  return (0);
}

static int
parse_hex_uint16 (bmc_device_state_data_t *state_data,
		  char *from,
		  uint16_t *to,
		  char *str)
{
  char *endptr;

  assert (state_data);
  assert (from);
  assert (to);
  assert (str);

  if (strlen (from) >= 2)
    {
      if (!strncmp (from, "0x", 2))
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
  
  if (parse_check_hex (state_data, from, str, 2) < 0)
    return (-1);
  
  errno = 0;
  (*to) = strtol (from, &endptr, 16);
  if (errno
      || endptr[0] != '\0')
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "invalid hex byte argument for %s\n",
		       str);
      return (-1);
    }

  return (0);
}

static int
parse_int16 (bmc_device_state_data_t *state_data,
	     char *from,
	     int16_t *to,
	     char *str)
{
  char *endptr;

  assert (state_data);
  assert (from);
  assert (to);
  assert (str);

  if (strlen (from) >= 2)
    {
      if (!strncmp (from, "0x", 2))
	{
	  if (parse_check_hex (state_data, from + 2, str, 2) < 0)
	    return (-1);
	}
    }

  errno = 0;
  (*to) = strtol (from, &endptr, 0);
  if (errno
      || endptr[0] != '\0')
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "invalid argument for %s\n",
		       str);
      return (-1);
    }

  return (0);
}

static int
rearm_sensor (bmc_device_state_data_t *state_data)
{
  struct bmc_device_arguments *args;
  fiid_obj_t obj_cmd_rs = NULL;
  char *rearm_sensor_arg_cpy = NULL;
  char *str_args[BMC_DEVICE_MAX_REARM_SENSOR_ARGS];
  unsigned int num_str_args = 0;
  char *str_ptr;
  char *lasts;
  uint16_t record_id;
  uint16_t assertion_bitmask;
  uint16_t deassertion_bitmask;
  uint16_t *assertion_bitmask_ptr;
  uint16_t *deassertion_bitmask_ptr;
  uint8_t re_arm_all_event_status_from_this_sensor;
  uint8_t record_type;
  uint8_t sensor_number;
  uint8_t sensor_owner_id_type = 0;
  uint8_t sensor_owner_id = 0;
  uint8_t sensor_owner_lun = 0;
  uint8_t channel_number = 0;
  uint8_t slave_address = 0;
  int rv = -1;

  assert (state_data);

  args = state_data->prog_data->args;

  if (!(rearm_sensor_arg_cpy = strdup(args->rearm_sensor_arg)))
    {
      pstdout_perror (state_data->pstate, "strdup");
      goto cleanup;
    }

  str_ptr = strtok_r (rearm_sensor_arg_cpy, " \t\0", &lasts);
  while (str_ptr && num_str_args < BMC_DEVICE_MAX_REARM_SENSOR_ARGS)
    {
      str_args[num_str_args] = str_ptr;
      num_str_args++;

      str_ptr = strtok_r (NULL, " \t\0", &lasts);
    }

  if (num_str_args != BMC_DEVICE_MIN_REARM_SENSOR_ARGS
      && num_str_args != BMC_DEVICE_MAX_REARM_SENSOR_ARGS)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "Invalid number of arguments specified\n");
      goto cleanup;
    }

  if (parse_uint16 (state_data,
		    str_args[0],
		    &record_id,
		    "record_id") < 0)
    goto cleanup;

  if (num_str_args == BMC_DEVICE_MAX_REARM_SENSOR_ARGS)
    {
      if (parse_hex_uint16 (state_data,
			    str_args[1],
			    &assertion_bitmask,
			    "assertion_bitmask") < 0)
	goto cleanup;
      
      if (parse_hex_uint16 (state_data,
			    str_args[2],
			    &deassertion_bitmask,
			    "deassertion_bitmask") < 0)
	goto cleanup;

      re_arm_all_event_status_from_this_sensor = IPMI_SENSOR_RE_ARM_ALL_EVENT_STATUS_DISABLED;
      assertion_bitmask_ptr = &assertion_bitmask;
      deassertion_bitmask_ptr = &deassertion_bitmask;
    }
  else
    {
      re_arm_all_event_status_from_this_sensor = IPMI_SENSOR_RE_ARM_ALL_EVENT_STATUS_ENABLED;
      assertion_bitmask_ptr = NULL;
      deassertion_bitmask_ptr = NULL;
    }

  if (sdr_cache_create_and_load (state_data->sdr_ctx,
                                 state_data->pstate,
                                 state_data->ipmi_ctx,
                                 state_data->hostname,
				 &state_data->prog_data->args->common_args) < 0)
    goto cleanup;
  
  if (ipmi_sdr_cache_search_record_id (state_data->sdr_ctx,
                                       record_id) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_sdr_cache_search_record_id: %s\n",
                       ipmi_sdr_ctx_errormsg (state_data->sdr_ctx));
      goto cleanup;
    }
  
  if (ipmi_sdr_parse_record_id_and_type (state_data->sdr_ctx,
					 NULL,
					 0,
                                         NULL,
                                         &record_type) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_sdr_parse_record_id_and_type: %s\n",
                       ipmi_sdr_ctx_errormsg (state_data->sdr_ctx));
      goto cleanup;
    }

  if (record_type != IPMI_SDR_FORMAT_FULL_SENSOR_RECORD
      && record_type != IPMI_SDR_FORMAT_COMPACT_SENSOR_RECORD
      && record_type != IPMI_SDR_FORMAT_EVENT_ONLY_RECORD)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "Record ID points to invalid record type: %Xh\n",
                       record_type);
      goto cleanup;
    }
  
  if (ipmi_sdr_parse_sensor_number (state_data->sdr_ctx,
				    NULL,
				    0,
                                    &sensor_number) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_sdr_parse_sensor_number: %s\n",
                       ipmi_sdr_ctx_errormsg (state_data->sdr_ctx));
      goto cleanup;
    }

  if (ipmi_sdr_parse_sensor_owner_id (state_data->sdr_ctx,
				      NULL,
				      0,
                                      &sensor_owner_id_type,
                                      &sensor_owner_id) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_sdr_parse_sensor_owner_id: %s\n",
                       ipmi_sdr_ctx_errormsg (state_data->sdr_ctx));
      goto cleanup;
    }

  if (ipmi_sdr_parse_sensor_owner_lun (state_data->sdr_ctx,
				       NULL,
				       0,
                                       &sensor_owner_lun,
                                       &channel_number) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_sdr_parse_sensor_owner_lun: %s\n",
                       ipmi_sdr_ctx_errormsg (state_data->sdr_ctx));
      goto cleanup;
    }

  slave_address = (sensor_owner_id << 1) | sensor_owner_id_type;

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_re_arm_sensor_events_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if (slave_address == IPMI_SLAVE_ADDRESS_BMC)
    {
      if (ipmi_cmd_re_arm_sensor_events (state_data->ipmi_ctx,
					 sensor_number,
					 re_arm_all_event_status_from_this_sensor,
					 assertion_bitmask_ptr,
					 deassertion_bitmask_ptr,
					 obj_cmd_rs) < 0)
	{
	  pstdout_fprintf (state_data->pstate,
			   stderr,
			   "ipmi_cmd_re_arm_sensor_events: %s\n",
			   ipmi_ctx_errormsg (state_data->ipmi_ctx));
	  goto cleanup;
	}
    }
  else
    {
      if (ipmi_cmd_re_arm_sensor_events_ipmb (state_data->ipmi_ctx,
					      channel_number,
					      slave_address,
					      sensor_owner_lun,
					      sensor_number,
					      re_arm_all_event_status_from_this_sensor,
					      assertion_bitmask_ptr,
					      deassertion_bitmask_ptr,
					      obj_cmd_rs) < 0)
	{
	  pstdout_fprintf (state_data->pstate,
			   stderr,
			   "ipmi_cmd_re_arm_sensor_events: %s\n",
			   ipmi_ctx_errormsg (state_data->ipmi_ctx));
	  goto cleanup;
	}
    }
  
  rv = 0;
 cleanup:
  free (rearm_sensor_arg_cpy);
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static int
get_sdr_repository_time (bmc_device_state_data_t *state_data)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  char timestr[BMC_DEVICE_TIME_BUFLEN + 1];
  int rv = -1;

  assert (state_data);

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_get_sdr_repository_time_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if (ipmi_cmd_get_sdr_repository_time (state_data->ipmi_ctx, obj_cmd_rs) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_cmd_get_sdr_repository_time: %s\n",
                       ipmi_ctx_errormsg (state_data->ipmi_ctx));
      goto cleanup;
    }

  if (FIID_OBJ_GET (obj_cmd_rs, "time", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'time': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }

  memset (timestr, '\0', BMC_DEVICE_TIME_BUFLEN + 1);

  if (ipmi_timestamp_string ((uint32_t)val,
			     state_data->prog_data->args->common_args.utc_offset,
			     get_timestamp_flags (&(state_data->prog_data->args->common_args),
						  IPMI_TIMESTAMP_FLAG_DEFAULT), 
			     "%m/%d/%Y - %H:%M:%S",
			     timestr,
			     BMC_DEVICE_TIME_BUFLEN) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_timestamp_string: %s\n",
		       strerror (errno));
      goto cleanup;
    }
			     
  pstdout_printf (state_data->pstate,
                  "SDR Repository Time : %s\n",
                  timestr);
  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static int
set_sdr_repository_time (bmc_device_state_data_t *state_data)
{
  struct bmc_device_arguments *args;
  fiid_obj_t obj_cmd_rs = NULL;
  int rv = -1;
  time_t t;
  struct tm tm;

  assert (state_data);

  args = state_data->prog_data->args;

  /* Posix says individual calls need not clear/set all portions of
   * 'struct tm', thus passing 'struct tm' between functions could
   * have issues.  So we need to memset.
   */
  memset (&tm, '\0', sizeof(struct tm));

  if (!strcasecmp (args->set_sdr_repository_time_arg, "now"))
    t = time (NULL);
  else
    {
      if (!strptime (args->set_sdr_repository_time_arg, "%m/%d/%Y - %H:%M:%S", &tm))
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "Invalid time specification '%s'.\n",
                           args->set_sdr_repository_time_arg);
          goto cleanup;
        }

      /* strptime() does not set tm_isdst.  Set so mktime() will not
       * adjust for daylight savings time.
       */
      tm.tm_isdst = -1;

      if ((t = mktime (&tm)) == (time_t)-1)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "Time specification '%s' cannot be represented.\n",
                           args->set_sdr_repository_time_arg);
          goto cleanup;
        }
    }

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_set_sdr_repository_time_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if (ipmi_cmd_set_sdr_repository_time (state_data->ipmi_ctx, t, obj_cmd_rs) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_cmd_set_sdr_repository_time: %s\n",
                       ipmi_ctx_errormsg (state_data->ipmi_ctx));
      goto cleanup;
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static int
get_sel_time (bmc_device_state_data_t *state_data)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  char timestr[BMC_DEVICE_TIME_BUFLEN + 1];
  int rv = -1;

  assert (state_data);

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_get_sel_time_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if (ipmi_cmd_get_sel_time (state_data->ipmi_ctx, obj_cmd_rs) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_cmd_get_sel_time: %s\n",
                       ipmi_ctx_errormsg (state_data->ipmi_ctx));
      goto cleanup;
    }

  if (FIID_OBJ_GET (obj_cmd_rs, "time", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'time': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }

  memset (timestr, '\0', BMC_DEVICE_TIME_BUFLEN + 1);

  if (ipmi_timestamp_string ((uint32_t)val,
			     state_data->prog_data->args->common_args.utc_offset,
			     get_timestamp_flags (&(state_data->prog_data->args->common_args),
						  IPMI_TIMESTAMP_FLAG_DEFAULT), 
			     "%m/%d/%Y - %H:%M:%S",
			     timestr,
			     BMC_DEVICE_TIME_BUFLEN) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_timestamp_string: %s\n",
		       strerror (errno));
      goto cleanup;
    }

  pstdout_printf (state_data->pstate,
                  "SEL Time : %s\n",
                  timestr);
  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static int
set_sel_time (bmc_device_state_data_t *state_data)
{
  struct bmc_device_arguments *args;
  fiid_obj_t obj_cmd_rs = NULL;
  int rv = -1;
  time_t t;
  struct tm tm;

  assert (state_data);

  args = state_data->prog_data->args;

  /* Posix says individual calls need not clear/set all portions of
   * 'struct tm', thus passing 'struct tm' between functions could
   * have issues.  So we need to memset.
   */
  memset (&tm, '\0', sizeof(struct tm));

  if (!strcasecmp (args->set_sel_time_arg, "now"))
    t = time (NULL);
  else
    {
      if (!strptime (args->set_sel_time_arg, "%m/%d/%Y - %H:%M:%S", &tm))
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "Invalid time specification '%s'.\n",
                           args->set_sel_time_arg);
          goto cleanup;
        }

      /* strptime() does not set tm_isdst.  Set so mktime() will not
       * adjust for daylight savings time.
       */
      tm.tm_isdst = -1;

      if ((t = mktime (&tm)) == (time_t)-1)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "Time specification '%s' cannot be represented.\n",
                           args->set_sel_time_arg);
          goto cleanup;
        }
    }

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_set_sel_time_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if (ipmi_cmd_set_sel_time (state_data->ipmi_ctx, t, obj_cmd_rs) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_cmd_set_sel_time: %s\n",
                       ipmi_ctx_errormsg (state_data->ipmi_ctx));
      goto cleanup;
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static int
get_sel_time_utc_offset (bmc_device_state_data_t *state_data)
{
  fiid_obj_t obj_cmd_rs = NULL;
  int16_t offset;
  uint64_t val;
  int rv = -1;

  assert (state_data);

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_get_sel_time_utc_offset_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if (ipmi_cmd_get_sel_time_utc_offset (state_data->ipmi_ctx, obj_cmd_rs) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_cmd_get_sel_time_utc_offset: %s\n",
                       ipmi_ctx_errormsg (state_data->ipmi_ctx));
      goto cleanup;
    }

  if (FIID_OBJ_GET (obj_cmd_rs, "offset", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'offset': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  offset = (int16_t)val;

  if (offset == IPMI_SEL_TIME_UTC_OFFSET_UNSPECIFIED)
    pstdout_printf (state_data->pstate,
		    "SEL UTC Offset : Unspecified\n",
		    offset);
  else
    pstdout_printf (state_data->pstate,
		    "SEL UTC Offset : %d minutes\n",
		    offset);
  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static int
set_sel_time_utc_offset (bmc_device_state_data_t *state_data)
{
  struct bmc_device_arguments *args;
  fiid_obj_t obj_cmd_rs = NULL;
  int16_t offset;
  int rv = -1;

  assert (state_data);

  args = state_data->prog_data->args;

  if (!strcasecmp (args->set_sel_time_utc_offset_arg, "none"))
    offset = IPMI_SEL_TIME_UTC_OFFSET_UNSPECIFIED;
  else
    {
      if (parse_int16 (state_data,
		       args->set_sel_time_utc_offset_arg,
		       &offset,
		       "offset") < 0)
	goto cleanup;
    }

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_set_sel_time_utc_offset_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if (ipmi_cmd_set_sel_time_utc_offset (state_data->ipmi_ctx, offset, obj_cmd_rs) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_cmd_set_sel_time_utc_offset: %s\n",
                       ipmi_ctx_errormsg (state_data->ipmi_ctx));
      goto cleanup;
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static int
parse_hex_byte (bmc_device_state_data_t *state_data,
                char *from,
                uint8_t *to,
                char *str)
{
  char *endptr;

  assert (state_data);
  assert (from);
  assert (to);
  assert (str);

  if (strlen (from) >= 2)
    {
      if (!strncmp (from, "0x", 2))
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
  
  if (parse_check_hex (state_data, from, str, 1) < 0)
    return (-1);

  errno = 0;
  (*to) = strtol (from, &endptr, 16);
  if (errno
      || endptr[0] != '\0')
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "invalid hex byte argument for %s\n",
		       str);
      return (-1);
    }

  return (0);
}

static int
platform_event (bmc_device_state_data_t *state_data)
{
  struct bmc_device_arguments *args;
  fiid_obj_t obj_cmd_get_channel_info_rs = NULL;
  fiid_obj_t obj_cmd_rs = NULL;
  char *platform_event_arg_cpy = NULL;
  char *str_args[BMC_DEVICE_MAX_PLATFORM_EVENT_ARGS];
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
  while (str_ptr && num_str_args < BMC_DEVICE_MAX_PLATFORM_EVENT_ARGS)
    {
      str_args[num_str_args] = str_ptr;
      num_str_args++;

      str_ptr = strtok_r (NULL, " \t\0", &lasts);
    }

  if (num_str_args != BMC_DEVICE_MAX_PLATFORM_EVENT_ARGS
      && num_str_args != (BMC_DEVICE_MAX_PLATFORM_EVENT_ARGS - 1))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "Invalid number of arguments specified\n");
      goto cleanup;
    }

  if (!(obj_cmd_get_channel_info_rs = fiid_obj_create (tmpl_cmd_get_channel_info_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if (ipmi_cmd_get_channel_info (state_data->ipmi_ctx,
                                 IPMI_CHANNEL_NUMBER_CURRENT_CHANNEL,
                                 obj_cmd_get_channel_info_rs) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_cmd_get_channel_info: %s",
                       ipmi_ctx_errormsg (state_data->ipmi_ctx));
      goto cleanup;
    }

  if (FIID_OBJ_GET (obj_cmd_get_channel_info_rs,
                    "channel_medium_type",
                    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'channel_medium_type': %s\n",
                       fiid_obj_errormsg (obj_cmd_get_channel_info_rs));
      goto cleanup;
    }
  channel_medium_type = val;

  if (channel_medium_type == IPMI_CHANNEL_MEDIUM_TYPE_SYSTEM_INTERFACE
      && num_str_args != BMC_DEVICE_MAX_PLATFORM_EVENT_ARGS)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "Generator ID required for given system interface\n");
      goto cleanup;
    }
  
  /* see if generator_id specified */
  if (num_str_args == BMC_DEVICE_MAX_PLATFORM_EVENT_ARGS)
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

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_platform_event_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

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
                       ipmi_ctx_errormsg (state_data->ipmi_ctx));
      goto cleanup;
    }

  rv = 0;
 cleanup:
  free (platform_event_arg_cpy);
  fiid_obj_destroy (obj_cmd_get_channel_info_rs);
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static int
set_sensor_reading_and_event_status (bmc_device_state_data_t *state_data)
{
  struct bmc_device_arguments *args;
  fiid_obj_t obj_cmd_rs = NULL;
  char *set_sensor_reading_and_event_status_arg_cpy = NULL;
  char *str_args[BMC_DEVICE_SET_SENSOR_READING_AND_EVENT_STATUS_ARGS];
  unsigned int num_str_args = 0;
  char *str_ptr;
  char *lasts;
  unsigned int str_args_index = 0;
  uint8_t sensor_number;
  uint8_t sensor_reading;
  uint8_t sensor_reading_operation;
  uint16_t assertion_bitmask;
  uint8_t assertion_bitmask_operation;
  uint16_t deassertion_bitmask;
  uint8_t deassertion_bitmask_operation;
  uint8_t event_data1;
  uint8_t event_data2;
  uint8_t event_data3;
  uint8_t event_data_operation;
  int rv = -1;

  assert (state_data);

  args = state_data->prog_data->args;

  if (!(set_sensor_reading_and_event_status_arg_cpy = strdup(args->set_sensor_reading_and_event_status_arg)))
    {
      pstdout_perror (state_data->pstate, "strdup");
      goto cleanup;
    }

  str_ptr = strtok_r (set_sensor_reading_and_event_status_arg_cpy, " \t\0", &lasts);
  while (str_ptr && num_str_args < BMC_DEVICE_SET_SENSOR_READING_AND_EVENT_STATUS_ARGS)
    {
      str_args[num_str_args] = str_ptr;
      num_str_args++;

      str_ptr = strtok_r (NULL, " \t\0", &lasts);
    }

  if (num_str_args != BMC_DEVICE_SET_SENSOR_READING_AND_EVENT_STATUS_ARGS)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "Invalid number of arguments specified\n");
      goto cleanup;
    }

  if (parse_hex_byte (state_data,
		      str_args[str_args_index],
		      &sensor_number,
		      "sensor number") < 0)
    goto cleanup;
  str_args_index++;
  
  if (parse_hex_byte (state_data,
		      str_args[str_args_index],
		      &sensor_reading,
		      "sensor reading") < 0)
    goto cleanup;
  str_args_index++;

  if (!strcasecmp (str_args[str_args_index], "write"))
    sensor_reading_operation = IPMI_SENSOR_READING_OPERATION_WRITE_GIVEN_VALUE_TO_SENSOR_READING_BYTE;
  else if (!strcasecmp (str_args[str_args_index], "nochange"))
    sensor_reading_operation = IPMI_SENSOR_READING_OPERATION_DONT_CHANGE_SENSOR_READING_BYTE;
  else
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "Invalid sensor reading operation specified\n"); 
      goto cleanup;
    }
  str_args_index++;

  if (parse_uint16 (state_data,
		    str_args[str_args_index],
		    &assertion_bitmask,
		    "assertion bitmask") < 0)
    goto cleanup;
  str_args_index++;

  if (!strcasecmp (str_args[str_args_index], "clear0bits"))
    assertion_bitmask_operation = IPMI_ASSERTION_DEASSERTION_EVENT_STATUS_BITS_OPERATION_CLEAR_EVENT_STATUS_BITS;
  else if (!strcasecmp (str_args[str_args_index], "set1bits"))
    assertion_bitmask_operation = IPMI_ASSERTION_DEASSERTION_EVENT_STATUS_BITS_OPERATION_SET_EVENT_STATUS_BITS;
  else if (!strcasecmp (str_args[str_args_index], "write"))
    assertion_bitmask_operation = IPMI_ASSERTION_DEASSERTION_EVENT_STATUS_BITS_OPERATION_WRITE_EVENT_STATUS_BITS;
  else if (!strcasecmp (str_args[str_args_index], "nochange"))
    assertion_bitmask_operation = IPMI_ASSERTION_DEASSERTION_EVENT_STATUS_BITS_OPERATION_DONT_CHANGE_EVENT_STATUS_BITS;
  else
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "Invalid assertion bitmask operation specified\n"); 
      goto cleanup;
    }
  str_args_index++;

  if (parse_uint16 (state_data,
		    str_args[str_args_index],
		    &deassertion_bitmask,
		    "deassertion bitmask") < 0)
    goto cleanup;
  str_args_index++;

  if (!strcasecmp (str_args[str_args_index], "clear0bits"))
    deassertion_bitmask_operation = IPMI_ASSERTION_DEASSERTION_EVENT_STATUS_BITS_OPERATION_CLEAR_EVENT_STATUS_BITS;
  else if (!strcasecmp (str_args[str_args_index], "set1bits"))
    deassertion_bitmask_operation = IPMI_ASSERTION_DEASSERTION_EVENT_STATUS_BITS_OPERATION_SET_EVENT_STATUS_BITS;
  else if (!strcasecmp (str_args[str_args_index], "write"))
    deassertion_bitmask_operation = IPMI_ASSERTION_DEASSERTION_EVENT_STATUS_BITS_OPERATION_WRITE_EVENT_STATUS_BITS;
  else if (!strcasecmp (str_args[str_args_index], "nochange"))
    deassertion_bitmask_operation = IPMI_ASSERTION_DEASSERTION_EVENT_STATUS_BITS_OPERATION_DONT_CHANGE_EVENT_STATUS_BITS;
  else
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "Invalid deassertion bitmask operation specified\n"); 
      goto cleanup;
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
    
  if (!strcasecmp (str_args[str_args_index], "write"))
    event_data_operation = IPMI_EVENT_DATA_BYTES_OPERATION_WRITE_EVENT_DATA_BYTES_INCLUDING_EVENT_OFFSET;
  else if (!strcasecmp (str_args[str_args_index], "nooffsetwrite"))
    event_data_operation = IPMI_EVENT_DATA_BYTES_OPERATION_WRITE_EVENT_DATA_BYTES_EXCLUDING_EVENT_OFFSET;
  else if (!strcasecmp (str_args[str_args_index], "nochange"))
    event_data_operation = IPMI_EVENT_DATA_BYTES_OPERATION_DONT_WRITE_EVENT_DATA_BYTES;
  else
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "Invalid event data operation specified\n"); 
      goto cleanup;
    }
  str_args_index++;

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_set_sensor_reading_and_event_status_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if (ipmi_cmd_set_sensor_reading_and_event_status (state_data->ipmi_ctx,
						    sensor_number,
						    sensor_reading_operation,
						    deassertion_bitmask_operation,
						    assertion_bitmask_operation,
						    event_data_operation,
						    sensor_reading,
						    assertion_bitmask,
						    deassertion_bitmask,
						    event_data1,
						    event_data2,
						    event_data3,
						    obj_cmd_rs) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_cmd_set_sensor_reading_and_event_status: %s\n",
                       ipmi_ctx_errormsg (state_data->ipmi_ctx));
      goto cleanup;
    }

  rv = 0;
 cleanup:
  free (set_sensor_reading_and_event_status_arg_cpy);
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static int
get_mca_auxiliary_log_status (bmc_device_state_data_t *state_data)
{
  fiid_obj_t obj_cmd_rs = NULL;
  fiid_obj_t mca_obj_cmd_rs = NULL;
  uint32_t mca_log_entry_count;
  uint64_t val;
  char timestr[BMC_DEVICE_TIME_BUFLEN + 1];
  int rv = -1;
  time_t t;
  struct tm tm;

  assert (state_data);

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_get_auxiliary_log_status_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if (ipmi_cmd_get_auxiliary_log_status (state_data->ipmi_ctx,
                                         IPMI_AUXILIARY_LOG_TYPE_MCA,
                                         obj_cmd_rs) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_cmd_get_auxiliary_log_status: %s\n",
                       ipmi_ctx_errormsg (state_data->ipmi_ctx));
      goto cleanup;
    }

  if (!(mca_obj_cmd_rs = fiid_obj_copy (obj_cmd_rs, tmpl_cmd_get_auxiliary_log_status_mca_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_copy: %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }

  if (FIID_OBJ_GET (mca_obj_cmd_rs,
                    "timestamp",
                    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'timestamp': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }

  memset (timestr, '\0', BMC_DEVICE_TIME_BUFLEN + 1);

  /* Posix says individual calls need not clear/set all portions of
   * 'struct tm', thus passing 'struct tm' between functions could
   * have issues.  So we need to memset.
   */
  memset (&tm, '\0', sizeof(struct tm));

  t = val;
  localtime_r (&t, &tm);
  strftime (timestr, sizeof (timestr), "%m/%d/%Y - %H:%M:%S", &tm);
  pstdout_printf (state_data->pstate,
                  "Last Entry Added to MCA Log  : %s\n",
                  timestr);

  if (FIID_OBJ_GET (mca_obj_cmd_rs,
                    "mca_log_entry_count",
                    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'mca_log_entry_count': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  mca_log_entry_count = val;
  
  pstdout_printf (state_data->pstate,
                  "Number of entries in MCA log : %u\n",
                  mca_log_entry_count);

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  fiid_obj_destroy (mca_obj_cmd_rs);
  return (rv);
}

static int
get_ssif_interface_capabilities (bmc_device_state_data_t *state_data)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint8_t ssif_version;
  uint8_t pec_support;
  uint8_t transaction_support;
  uint8_t input_message_size;
  uint8_t output_message_size;
  uint64_t val;
  char *str;
  int rv = -1;

  assert (state_data);

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_get_system_interface_capabilities_ssif_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if (ipmi_cmd_get_system_interface_capabilities_ssif (state_data->ipmi_ctx,
                                                       obj_cmd_rs) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_cmd_get_system_interface_capabilities_ssif: %s\n",
                       ipmi_ctx_errormsg (state_data->ipmi_ctx));
      goto cleanup;
    }

  if (FIID_OBJ_GET (obj_cmd_rs, "ssif_version", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'ssif_version': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  ssif_version = val;

  /* achu: for some stupid reason 000b == "version 1" */
  if (ssif_version == IPMI_SSIF_SYSTEM_INTERFACE_VERSION_1)
    pstdout_printf (state_data->pstate,
                    "SSIF Version                     : version 1 (%Xh)\n", ssif_version);
  else
    pstdout_printf (state_data->pstate,
                    "SSIF Version                     : %Xh\n", ssif_version);

  if (FIID_OBJ_GET (obj_cmd_rs, "pec_support", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'pec_support': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  pec_support = val;

  if (pec_support == IPMI_SSIF_SYSTEM_INTERFACE_IMPLEMENTS_PEC)
    str = "Yes";
  else if (pec_support == IPMI_SSIF_SYSTEM_INTERFACE_DOES_NOT_SUPPORT_PEC)
    str = "No";
  else
    str = "unknown";

  pstdout_printf (state_data->pstate,
                  "SSIF PEC Support                 : %s\n",
                  str);
  
  if (FIID_OBJ_GET (obj_cmd_rs, "transaction_support", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'transaction_support': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  transaction_support = val;

  if (transaction_support == IPMI_SSIF_SYSTEM_INTERFACE_TRANSACTION_SUPPORT_SINGLE_PART_READS_WRITES_SUPPORTED)
    str = "Only single-part reads/writes supported.";
  else if (transaction_support == IPMI_SSIF_SYSTEM_INTERFACE_TRANSACTION_SUPPORT_MULTI_PART_READS_WRITES_SUPPORTED_START_AND_END_ONLY)
    str = "multi-part reads/writes upported.  Start and End transactions only.";
  else if (transaction_support == IPMI_SSIF_SYSTEM_INTERFACE_TRANSACTION_SUPPORT_MULTI_PART_READS_WRITES_SUPPORTED_START_MIDDLE_END)
    str = "multi-part reads/writes upported.  Start, Middle, and End transactions supported.";
  else
    str = "unknown";

  pstdout_printf (state_data->pstate,
                  "SSIF Transaction Support         : %s\n",
                  str);

  if (FIID_OBJ_GET (obj_cmd_rs, "input_message_size", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'input_message_size': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  input_message_size = val;

  pstdout_printf (state_data->pstate,
                  "SSIF Maximum Input Message Size  : %u bytes\n",
                  input_message_size);

  if (FIID_OBJ_GET (obj_cmd_rs, "output_message_size", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'output_message_size': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  output_message_size = val;

  pstdout_printf (state_data->pstate,
                  "SSIF Maximum Output Message Size : %u bytes\n",
                  output_message_size);

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static int
get_kcs_interface_capabilities (bmc_device_state_data_t *state_data)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint8_t system_interface_version;
  uint8_t input_maximum_message_size;
  uint64_t val;
  int rv = -1;

  assert (state_data);

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_get_system_interface_capabilities_kcs_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if (ipmi_cmd_get_system_interface_capabilities_kcs (state_data->ipmi_ctx,
                                                      obj_cmd_rs) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_cmd_get_system_interface_capabilities_kcs: %s\n",
                       ipmi_ctx_errormsg (state_data->ipmi_ctx));
      goto cleanup;
    }

  if (FIID_OBJ_GET (obj_cmd_rs, "system_interface_version", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'system_interface_version': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  system_interface_version = val;

  /* achu: for some stupid reason 000b == "version 1" */
  if (system_interface_version == IPMI_KCS_SYSTEM_INTERFACE_VERSION_1)
    pstdout_printf (state_data->pstate,
                    "KCS Version                    : version 1 (%Xh)\n", system_interface_version);
  else
    pstdout_printf (state_data->pstate,
                    "KCS Version                    : %Xh\n", system_interface_version);

  if (FIID_OBJ_GET (obj_cmd_rs, "input_maximum_message_size", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'input_maximum_message_size': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  input_maximum_message_size = val;

  pstdout_printf (state_data->pstate,
                  "KCS Maximum Input Message Size : %u bytes\n",
                  input_maximum_message_size);

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static int
get_bt_interface_capabilities (bmc_device_state_data_t *state_data)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint8_t number_of_outstanding_requests_supported;
  uint8_t input_buffer_size;
  uint8_t output_buffer_size;
  uint8_t bmc_request_to_response_time;
  uint8_t recommended_retries;
  uint64_t val;
  int rv = -1;

  assert (state_data);

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_get_bt_interface_capabilities_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if (ipmi_cmd_get_bt_interface_capabilities (state_data->ipmi_ctx,
                                              obj_cmd_rs) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_cmd_get_bt_interface_capabilities: %s\n",
                       ipmi_ctx_errormsg (state_data->ipmi_ctx));
      goto cleanup;
    }

  if (FIID_OBJ_GET (obj_cmd_rs, "number_of_outstanding_requests_supported", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'number_of_outstanding_requests_supported': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  number_of_outstanding_requests_supported = val;

  pstdout_printf (state_data->pstate,
                  "BT Number of Outstanding Requests Supported : %u\n",
                  number_of_outstanding_requests_supported);

  if (FIID_OBJ_GET (obj_cmd_rs, "input_buffer_size", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'input_buffer_size': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  input_buffer_size = val;

  pstdout_printf (state_data->pstate,
                  "BT Input Buffer Size                        : %u bytes\n",
                  input_buffer_size);

  if (FIID_OBJ_GET (obj_cmd_rs, "output_buffer_size", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'output_buffer_size': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  output_buffer_size = val;

  pstdout_printf (state_data->pstate,
                  "BT Output Buffer Size                       : %u bytes\n",
                  output_buffer_size);

  if (FIID_OBJ_GET (obj_cmd_rs, "bmc_request_to_response_time", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'bmc_request_to_response_time': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  bmc_request_to_response_time = val;

  pstdout_printf (state_data->pstate,
                  "BT Request to Response Time                 : %u seconds\n",
                  bmc_request_to_response_time);

  if (FIID_OBJ_GET (obj_cmd_rs, "recommended_retries", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'recommended_retries': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  recommended_retries = val;

  pstdout_printf (state_data->pstate,
                  "BT Recommended Retries                      : %u\n",
                  recommended_retries);

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static int
get_bmc_global_enables (bmc_device_state_data_t *state_data)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  int rv = -1;

  assert (state_data);

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_get_bmc_global_enables_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if (ipmi_cmd_get_bmc_global_enables (state_data->ipmi_ctx,
                                       obj_cmd_rs) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_cmd_get_bmc_global_enables: %s\n",
                       ipmi_ctx_errormsg (state_data->ipmi_ctx));
      goto cleanup;
    }

  if (FIID_OBJ_GET (obj_cmd_rs, "receive_message_queue_interrupt", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'receive_message_queue_interrupt': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  
  pstdout_printf (state_data->pstate,
                  "Receive Message Queue Interrupt : %s\n",
                  (val == IPMI_BMC_GLOBAL_ENABLES_ENABLED) ? "enabled" : "disabled");

  if (FIID_OBJ_GET (obj_cmd_rs, "event_message_buffer_full_interrupt", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'event_message_buffer_full_interrupt': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  
  pstdout_printf (state_data->pstate,
                  "Event Message Buffer Full       : %s\n",
                  (val == IPMI_BMC_GLOBAL_ENABLES_ENABLED) ? "enabled" : "disabled");

  if (FIID_OBJ_GET (obj_cmd_rs, "event_message_buffer", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'event_message_buffer': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  
  pstdout_printf (state_data->pstate,
                  "Event Message Buffer            : %s\n",
                  (val == IPMI_BMC_GLOBAL_ENABLES_ENABLED) ? "enabled" : "disabled");

  if (FIID_OBJ_GET (obj_cmd_rs, "system_event_logging", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'system_event_logging': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  
  pstdout_printf (state_data->pstate,
                  "System Event Logging            : %s\n",
                  (val == IPMI_BMC_GLOBAL_ENABLES_ENABLED) ? "enabled" : "disabled");

  if (FIID_OBJ_GET (obj_cmd_rs, "oem_0", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'oem_0': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  
  pstdout_printf (state_data->pstate,
                  "OEM 0                           : %s\n",
                  (val == IPMI_BMC_GLOBAL_ENABLES_ENABLED) ? "enabled" : "disabled");

  if (FIID_OBJ_GET (obj_cmd_rs, "oem_1", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'oem_1': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  
  pstdout_printf (state_data->pstate,
                  "OEM 1                           : %s\n",
                  (val == IPMI_BMC_GLOBAL_ENABLES_ENABLED) ? "enabled" : "disabled");

  if (FIID_OBJ_GET (obj_cmd_rs, "oem_2", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'oem_2': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  
  pstdout_printf (state_data->pstate,
                  "OEM 2                           : %s\n",
                  (val == IPMI_BMC_GLOBAL_ENABLES_ENABLED) ? "enabled" : "disabled");

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static int
set_system_info_common (bmc_device_state_data_t *state_data,
			Bmc_device_system_info_first_set func_cmd_first_set,
			const char *func_cmd_first_set_str,
			Bmc_device_system_info func_cmd,
			const char *func_cmd_str,
			const char *string)
{
  fiid_obj_t obj_cmd_first_set_rs = NULL;
  fiid_obj_t obj_cmd_rs = NULL;
  uint8_t string_length = 0;
  uint8_t string_block_length = 0;
  uint8_t set_selector = 0;
  unsigned int string_count = 0;
  int rv = -1;

  assert (state_data);
  assert (func_cmd_first_set);
  assert (func_cmd_first_set_str);
  assert (func_cmd);
  assert (func_cmd_str);
  assert (state_data);
  assert (string);
  assert (strlen (string) <= IPMI_SYSTEM_INFO_STRING_LEN_MAX);

  if (!(obj_cmd_first_set_rs = fiid_obj_create (tmpl_cmd_set_system_info_parameters_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_set_system_info_parameters_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  string_length = strlen (string);
  if (string_length > IPMI_SYSTEM_INFO_FIRST_SET_STRING_LEN_MAX)
    string_block_length = IPMI_SYSTEM_INFO_FIRST_SET_STRING_LEN_MAX;
  else
    string_block_length = string_length;

  if (func_cmd_first_set (state_data->ipmi_ctx,
			  set_selector,
			  IPMI_SYSTEM_INFO_ENCODING_ASCII_LATIN1,
			  string_length,
			  string + string_count,
			  string_block_length,
			  obj_cmd_first_set_rs) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "%s: %s\n",
                       func_cmd_first_set_str,
                       ipmi_ctx_errormsg (state_data->ipmi_ctx));
      goto cleanup;
    }
  
  string_count += string_block_length;

  /* string_length is 8 bits, so we should not call >= 17 times,
   *
   * ceiling ( (255 - 14) / 16 ) + 1 = 17
   *
   */

  set_selector++;
  while (string_count < string_length && set_selector < 17)
    {
      if (fiid_obj_clear (obj_cmd_rs) < 0)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "fiid_obj_clear: %s\n", 
                           fiid_obj_errormsg (obj_cmd_rs));
          goto cleanup;
        }
      
      if ((string_length - string_count) > IPMI_SYSTEM_INFO_SET_STRING_LEN_MAX)
	string_block_length = IPMI_SYSTEM_INFO_SET_STRING_LEN_MAX;
      else
	string_block_length = string_length - string_count;
      
      if (func_cmd (state_data->ipmi_ctx,
                    set_selector,
		    string + string_count,
		    string_block_length,
                    obj_cmd_rs) < 0)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "%s: %s\n",
                           func_cmd_str,
                           ipmi_ctx_errormsg (state_data->ipmi_ctx));
          goto cleanup;
        }

      string_count += string_block_length;
      set_selector++;
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_first_set_rs);
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);

}

static int
set_system_firmware_version (bmc_device_state_data_t *state_data)
{
  assert (state_data);

  return (set_system_info_common (state_data,
				  ipmi_cmd_set_system_info_parameters_system_firmware_version_first_set,
				  "ipmi_cmd_set_system_info_parameters_system_firmware_version_first_set",
				  ipmi_cmd_set_system_info_parameters_system_firmware_version,
				  "ipmi_cmd_set_system_info_parameters_system_firmware_version",
				  state_data->prog_data->args->set_system_firmware_version_arg));
}

static int
set_system_name (bmc_device_state_data_t *state_data)
{
  assert (state_data);

  return (set_system_info_common (state_data,
				  ipmi_cmd_set_system_info_parameters_system_name_first_set,
				  "ipmi_cmd_set_system_info_parameters_system_name_first_set",
				  ipmi_cmd_set_system_info_parameters_system_name,
				  "ipmi_cmd_set_system_info_parameters_system_name",
				  state_data->prog_data->args->set_system_name_arg));
}

static int
set_primary_operating_system_name (bmc_device_state_data_t *state_data)
{
  assert (state_data);

  return (set_system_info_common (state_data,
				  ipmi_cmd_set_system_info_parameters_primary_operating_system_name_first_set,
				  "ipmi_cmd_set_system_info_parameters_primary_operating_system_name_first_set",
				  ipmi_cmd_set_system_info_parameters_primary_operating_system_name,
				  "ipmi_cmd_set_system_info_parameters_primary_operating_system_name",
				  state_data->prog_data->args->set_primary_operating_system_name_arg));
}

static int
set_operating_system_name (bmc_device_state_data_t *state_data)
{
  assert (state_data);

  return (set_system_info_common (state_data,
				  ipmi_cmd_set_system_info_parameters_operating_system_name_first_set,
				  "ipmi_cmd_set_system_info_parameters_operating_system_name_first_set",
				  ipmi_cmd_set_system_info_parameters_operating_system_name,
				  "ipmi_cmd_set_system_info_parameters_operating_system_name",
				  state_data->prog_data->args->set_operating_system_name_arg));
}

static int
set_present_os_version_number (bmc_device_state_data_t *state_data)
{
  assert (state_data);

  return (set_system_info_common (state_data,
				  ipmi_cmd_set_system_info_parameters_present_os_version_number_first_set,
				  "ipmi_cmd_set_system_info_parameters_present_os_version_number_first_set",
				  ipmi_cmd_set_system_info_parameters_present_os_version_number,
				  "ipmi_cmd_set_system_info_parameters_present_os_version_number",
				  state_data->prog_data->args->set_present_os_version_number_arg));
}

static int
set_bmc_url (bmc_device_state_data_t *state_data)
{
  assert (state_data);

  return (set_system_info_common (state_data,
				  ipmi_cmd_set_system_info_parameters_bmc_url_first_set,
				  "ipmi_cmd_set_system_info_parameters_bmc_url_first_set",
				  ipmi_cmd_set_system_info_parameters_bmc_url,
				  "ipmi_cmd_set_system_info_parameters_bmc_url",
				  state_data->prog_data->args->set_bmc_url_arg));
}

static int
set_base_os_hypervisor_url (bmc_device_state_data_t *state_data)
{
  assert (state_data);

  return (set_system_info_common (state_data,
				  ipmi_cmd_set_system_info_parameters_base_os_hypervisor_url_first_set,
				  "ipmi_cmd_set_system_info_parameters_base_os_hypervisor_url_first_set",
				  ipmi_cmd_set_system_info_parameters_base_os_hypervisor_url,
				  "ipmi_cmd_set_system_info_parameters_base_os_hypervisor_url",
				  state_data->prog_data->args->set_base_os_hypervisor_url_arg));
}


static int
run_cmd_args (bmc_device_state_data_t *state_data)
{
  struct bmc_device_arguments *args;
  int rv = -1;

  assert (state_data);

  args = state_data->prog_data->args;

  assert (!args->common_args.flush_cache);

  if (args->cold_reset)
    return (cold_reset (state_data));

  if (args->warm_reset)
    return (warm_reset (state_data));

  if (args->get_self_test_results)
    return (get_self_test_results (state_data));

  if (args->get_acpi_power_state)
    return (get_acpi_power_state (state_data));

  if (args->set_acpi_power_state)
    return (set_acpi_power_state (state_data));

  if (args->get_lan_statistics)
    return (get_lan_statistics (state_data));

  if (args->clear_lan_statistics)
    return (clear_lan_statistics (state_data));

  if (args->rearm_sensor)
    return (rearm_sensor (state_data));

  if (args->get_sdr_repository_time)
    return (get_sdr_repository_time (state_data));

  if (args->set_sdr_repository_time)
    return (set_sdr_repository_time (state_data));

  if (args->get_sel_time)
    return (get_sel_time (state_data));

  if (args->set_sel_time)
    return (set_sel_time (state_data));

  if (args->get_sel_time_utc_offset)
    return (get_sel_time_utc_offset (state_data));

  if (args->set_sel_time_utc_offset)
    return (set_sel_time_utc_offset (state_data));

  if (args->platform_event)
    return (platform_event (state_data));

  if (args->set_sensor_reading_and_event_status)
    return (set_sensor_reading_and_event_status (state_data));

  if (args->get_mca_auxiliary_log_status)
    return (get_mca_auxiliary_log_status (state_data));

  if (args->get_ssif_interface_capabilities)
    return (get_ssif_interface_capabilities (state_data));

  if (args->get_kcs_interface_capabilities)
    return (get_kcs_interface_capabilities (state_data));

  if (args->get_bt_interface_capabilities)
    return (get_bt_interface_capabilities (state_data));

  if (args->get_bmc_global_enables)
    return (get_bmc_global_enables (state_data));

  if (args->set_system_firmware_version)
    return (set_system_firmware_version (state_data));

  if (args->set_system_name)
    return (set_system_name (state_data));

  if (args->set_primary_operating_system_name)
    return (set_primary_operating_system_name (state_data));
  
  if (args->set_operating_system_name)
    return (set_operating_system_name (state_data));

  if (args->set_present_os_version_number)
    return (set_present_os_version_number (state_data));

  if (args->set_bmc_url)
    return (set_bmc_url (state_data));

  if (args->set_base_os_hypervisor_url)
    return (set_base_os_hypervisor_url (state_data));

  rv = 0;
  return (rv);
}

static int
_bmc_device (pstdout_state_t pstate,
             const char *hostname,
             void *arg)
{
  bmc_device_state_data_t state_data;
  bmc_device_prog_data_t *prog_data;
  int exit_code = EXIT_FAILURE;

  assert (pstate);
  assert (arg);

  prog_data = (bmc_device_prog_data_t *)arg;

  if (prog_data->args->common_args.flush_cache)
    {
      if (sdr_cache_flush_cache (pstate,
				 hostname,
				 &prog_data->args->common_args) < 0)
	return (EXIT_FAILURE);
      return (EXIT_SUCCESS);
    }
  
  memset (&state_data, '\0', sizeof (bmc_device_state_data_t));
  state_data.prog_data = prog_data;
  state_data.pstate = pstate;
  state_data.hostname = (char *)hostname;

  if (!(state_data.ipmi_ctx = ipmi_open (prog_data->progname,
					 hostname,
					 &(prog_data->args->common_args),
					 state_data.pstate)))
    goto cleanup;

  if (!(state_data.sdr_ctx = ipmi_sdr_ctx_create ()))
    {
      pstdout_perror (pstate, "ipmi_sdr_ctx_create()");
      goto cleanup;
    }
  
  if (run_cmd_args (&state_data) < 0)
    goto cleanup;

  exit_code = EXIT_SUCCESS;
 cleanup:
  ipmi_sdr_ctx_destroy (state_data.sdr_ctx);
  ipmi_ctx_close (state_data.ipmi_ctx);
  ipmi_ctx_destroy (state_data.ipmi_ctx);
  return (exit_code);
}

int
main (int argc, char **argv)
{
  bmc_device_prog_data_t prog_data;
  struct bmc_device_arguments cmd_args;
  int hosts_count;
  int rv;

  ipmi_disable_coredump ();

  memset (&prog_data, '\0', sizeof (bmc_device_prog_data_t));
  prog_data.progname = argv[0];
  bmc_device_argp_parse (argc, argv, &cmd_args);
  prog_data.args = &cmd_args;

  if ((hosts_count = pstdout_setup (&(prog_data.args->common_args.hostname),
				    &(prog_data.args->common_args))) < 0)
    return (EXIT_FAILURE);

  if (!hosts_count)
    return (EXIT_SUCCESS);

  /* We don't want caching info to output when are doing ranged output */
  if (hosts_count > 1)
    prog_data.args->common_args.quiet_cache = 1;

  if ((rv = pstdout_launch (prog_data.args->common_args.hostname,
                            _bmc_device,
                            &prog_data)) < 0)
    {
      fprintf (stderr,
               "pstdout_launch: %s\n",
               pstdout_strerror (pstdout_errnum));
      return (EXIT_FAILURE);
    }

  return (rv);
}
