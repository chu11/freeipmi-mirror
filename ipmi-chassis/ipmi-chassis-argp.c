/*
 * Copyright (C) 2007-2014 FreeIPMI Core Team
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
#if HAVE_ARGP_H
#include <argp.h>
#else /* !HAVE_ARGP_H */
#include "freeipmi-argp.h"
#endif /* !HAVE_ARGP_H */
#include <assert.h>
#include <errno.h>

#include <freeipmi/freeipmi.h>

#include "ipmi-chassis.h"
#include "ipmi-chassis-argp.h"

#include "freeipmi-portability.h"
#include "tool-cmdline-common.h"
#include "tool-config-file-common.h"

const char *argp_program_version =
  "ipmi-chassis - " PACKAGE_VERSION "\n"
  "Copyright (C) 2007-2014 FreeIPMI Core Team\n"
  "This program is free software; you may redistribute it under the terms of\n"
  "the GNU General Public License.  This program has absolutely no warranty.";

const char *argp_program_bug_address =
  "<" PACKAGE_BUGREPORT ">";

static char cmdline_doc[] =
  "ipmi-chassis - IPMI chassis management utility";

static char cmdline_args_doc[] = "";

static struct argp_option cmdline_options[] =
  {
    ARGP_COMMON_OPTIONS_DRIVER,
    ARGP_COMMON_OPTIONS_INBAND,
    ARGP_COMMON_OPTIONS_OUTOFBAND_HOSTRANGED,
    ARGP_COMMON_OPTIONS_AUTHENTICATION_TYPE,
    ARGP_COMMON_OPTIONS_CIPHER_SUITE_ID,
    ARGP_COMMON_OPTIONS_PRIVILEGE_LEVEL,
    ARGP_COMMON_OPTIONS_CONFIG_FILE,
    ARGP_COMMON_OPTIONS_WORKAROUND_FLAGS,
    ARGP_COMMON_HOSTRANGED_OPTIONS,
    ARGP_COMMON_OPTIONS_DEBUG,
    /* for backwards compatability */
    { "get-capabilities", GET_CHASSIS_CAPABILITIES_KEY, NULL, OPTION_HIDDEN,
      "Get chassis capabilities.", 40},
    /* for backwards compatability */
    { "get-status", GET_CHASSIS_STATUS_KEY, NULL, OPTION_HIDDEN,
      "Get chassis status.", 41},
    { "get-chassis-capabilities", GET_CHASSIS_CAPABILITIES_KEY, NULL, 0,
      "Get chassis capabilities.", 40},
    { "get-chassis-status", GET_CHASSIS_STATUS_KEY, NULL, 0,
      "Get chassis status.", 41},
    { "chassis-control", CHASSIS_CONTROL_KEY, "CONTROL", 0,
      "Control the chassis.", 42},
    { "chassis-identify", CHASSIS_IDENTIFY_KEY, "IDENTIFY", 0,
      "Set chassis Identification.", 43},
    /* All chassis "set" operations are legacy, see ipmi-config for chassis configuration */ 
    { "set-power-restore-policy", SET_POWER_RESTORE_POLICY_KEY, "POLICY", OPTION_HIDDEN,
      "Set power restore policy.", 44},
    { "set-power-cycle-interval", SET_POWER_CYCLE_INTERVAL_KEY, "SECONDS", OPTION_HIDDEN,
      "Set Power cycle interval in seconds.", 45},
    { "get-system-restart-cause", GET_SYSTEM_RESTART_CAUSE_KEY, NULL, 0,
      "Get system restart cause.", 46},
    { "set-boot-flags", SET_BOOT_FLAGS_KEY, NULL, OPTION_HIDDEN,
      "Set system boot flags.", 47},
    { "boot-type", SET_BOOT_FLAGS_BOOT_TYPE_KEY, "BOOT_TYPE", OPTION_HIDDEN,
      "Set BIOS boot type to BOOT_TYPE.", 48},
    { "lock-out-reset-button", SET_BOOT_FLAGS_LOCK_OUT_RESET_BUTTON_KEY, "LOCK_OUT_RESET_BUTTON", OPTION_HIDDEN,
      "Modify lock out reset button support.", 49},
    { "blank-screen", SET_BOOT_FLAGS_SCREEN_BLANK_KEY, "BLANK_SCREEN", OPTION_HIDDEN,
      "Modify blank screen support.", 50},
    { "boot-device", SET_BOOT_FLAGS_BOOT_DEVICE_KEY, "BOOT_DEVICE", OPTION_HIDDEN,
      "Set device to boot from to BOOT_DEVICE.", 51},
    { "lock-keyboard", SET_BOOT_FLAGS_LOCK_KEYBOARD_KEY, "LOCK_KEYBOARD", OPTION_HIDDEN,
      "Modify lock keyboard support.", 52},
    { "clear-cmos", SET_BOOT_FLAGS_CMOS_CLEAR_KEY, "CMOS_CLEAR", OPTION_HIDDEN,
      "Modify clear CMOS support.", 53},
    { "console-redirection", SET_BOOT_FLAGS_CONSOLE_REDIRECTION_KEY, "CONSOLE_REDIRECTION", OPTION_HIDDEN,
      "Set console redirection type.", 54},
    { "user-password-bypass", SET_BOOT_FLAGS_USER_PASSWORD_BYPASS_KEY, "USER_PASSWORD_BYPASS", OPTION_HIDDEN,
      "Modify user password bypass support.", 54},
    { "force-progress-event-traps", SET_BOOT_FLAGS_FORCE_PROGRESS_EVENT_TRAPS_KEY, "FORCE_PROGRESS_EVENT_TRAPS", OPTION_HIDDEN,
      "Modify force progress event traps support.", 55},
    { "firmware-bios-verbosity", SET_BOOT_FLAGS_FIRMWARE_BIOS_VERBOSITY_KEY, "FIRMWARE_BIOS_VERBOSITY", OPTION_HIDDEN,
      "Set firmware verbosity.", 56},
    { "get-boot-flags", GET_BOOT_FLAGS_KEY, NULL, OPTION_HIDDEN,
      "Get system boot-flags.", 57},
    { "get-power-on-hours-counter", GET_POWER_ON_HOURS_COUNTER_KEY, NULL, 0,
      "Get power on hours (POH) counter.", 58},
    { NULL, 0, NULL, 0, NULL, 0}
  };

static error_t cmdline_parse (int key, char *arg, struct argp_state *state);

static struct argp cmdline_argp = { cmdline_options,
                                    cmdline_parse,
                                    cmdline_args_doc,
                                    cmdline_doc };

static struct argp cmdline_config_file_argp = { cmdline_options,
                                                cmdline_config_file_parse,
                                                cmdline_args_doc,
                                                cmdline_doc };

static error_t boot_flag_parse (int key, char *arg, struct argp_state *state);

/* All chassis "set" operations are legacy, see ipmi-config for chassis configuration */ 
static error_t
boot_flag_parse (int key, char *arg, struct argp_state *state)
{
  struct ipmi_chassis_arguments *cmd_args;
  uint8_t value = 0;

  assert (state);
  
  cmd_args = state->input;

  switch (key)
    {
    case SET_BOOT_FLAGS_BOOT_TYPE_KEY:
      if (!strcasecmp (arg, "pc-compatible"))
        value = IPMI_SYSTEM_BOOT_OPTION_BOOT_FLAG_BOOT_TYPE_PC_COMPATIBLE;
      else if (!strcasecmp (arg, "efi"))
        value = IPMI_SYSTEM_BOOT_OPTION_BOOT_FLAG_BOOT_TYPE_EFI;
      else
        {
          fprintf (stderr, "invalid value for boot-type\n");
          exit (EXIT_FAILURE);
        }

      cmd_args->set_system_boot_options_args.bios_boot_type++;
      cmd_args->set_system_boot_options_args.bios_boot_type_arg = value;
      break;

    case SET_BOOT_FLAGS_LOCK_OUT_RESET_BUTTON_KEY:
      if (!strcasecmp (arg, "yes"))
        value = IPMI_SYSTEM_BOOT_OPTION_ENABLE;
      else if (!strcasecmp (arg, "no"))
        value = IPMI_SYSTEM_BOOT_OPTION_DISABLE;
      else
        {
          fprintf (stderr, "invalid value for lock-out-reset-button\n");
          exit (EXIT_FAILURE);
        }

      cmd_args->set_system_boot_options_args.lock_out_reset_button++;
      cmd_args->set_system_boot_options_args.lock_out_reset_button_arg = value;
      break;

    case SET_BOOT_FLAGS_SCREEN_BLANK_KEY:
      if (!strcasecmp (arg, "yes"))
        value = IPMI_SYSTEM_BOOT_OPTION_ENABLE;
      else if (!strcasecmp (arg, "no"))
        value = IPMI_SYSTEM_BOOT_OPTION_DISABLE;
      else
        {
          fprintf (stderr, "invalid value for blank-screen\n");
          exit (EXIT_FAILURE);
        }

      cmd_args->set_system_boot_options_args.screen_blank++;
      cmd_args->set_system_boot_options_args.screen_blank_arg = value;
      break;

    case SET_BOOT_FLAGS_BOOT_DEVICE_KEY:
      /* achu: many legacy inputs are preserved */
      if (!strcasecmp (arg, "no-override")
          || !strcasecmp (arg, "none")) /* legacy */
        value = IPMI_SYSTEM_BOOT_OPTION_BOOT_FLAG_BOOT_DEVICE_NO_OVERRIDE;
      else if (!strcasecmp (arg, "pxe"))
        value = IPMI_SYSTEM_BOOT_OPTION_BOOT_FLAG_BOOT_DEVICE_FORCE_PXE;
      else if (!strcasecmp (arg, "hard-drive")
               || !strcasecmp (arg, "disk")) /* legacy */
        value = IPMI_SYSTEM_BOOT_OPTION_BOOT_FLAG_BOOT_DEVICE_FORCE_HARD_DRIVE;
      else if (!strcasecmp (arg, "hard-drive-safe")
               || !strcasecmp (arg, "disk-safe")) /* legacy */
        value = IPMI_SYSTEM_BOOT_OPTION_BOOT_FLAG_BOOT_DEVICE_FORCE_HARD_DRIVE_SAFE_MODE;
      else if (!strcasecmp (arg, "diag"))
        value = IPMI_SYSTEM_BOOT_OPTION_BOOT_FLAG_BOOT_DEVICE_FORCE_DIAGNOSTIC_PARTITION;
      else if (!strcasecmp (arg, "cd-dvd"))
        value = IPMI_SYSTEM_BOOT_OPTION_BOOT_FLAG_BOOT_DEVICE_FORCE_CD_DVD;
      else if (!strcasecmp (arg, "bios"))
        value = IPMI_SYSTEM_BOOT_OPTION_BOOT_FLAG_BOOT_DEVICE_FORCE_BIOS_SETUP;
      else if (!strcasecmp (arg, "floppy"))
        value = IPMI_SYSTEM_BOOT_OPTION_BOOT_FLAG_BOOT_DEVICE_FORCE_FLOPPY_REMOVEABLE_MEDIA;
      else
        {
          fprintf (stderr, "invalid value for boot-device\n");
          exit (EXIT_FAILURE);
        }

      cmd_args->set_system_boot_options_args.boot_device++;
      cmd_args->set_system_boot_options_args.boot_device_arg = value;
      break;

    case SET_BOOT_FLAGS_LOCK_KEYBOARD_KEY:
      if (!strcasecmp (arg, "yes"))
        value = IPMI_SYSTEM_BOOT_OPTION_ENABLE;
      else if (!strcasecmp (arg, "no"))
        value = IPMI_SYSTEM_BOOT_OPTION_DISABLE;
      else
        {
          fprintf (stderr, "invalid value for lock-keyboard\n");
          exit (EXIT_FAILURE);
        }

      cmd_args->set_system_boot_options_args.lock_keyboard++;
      cmd_args->set_system_boot_options_args.lock_keyboard_arg = value;
      break;

    case SET_BOOT_FLAGS_CMOS_CLEAR_KEY:
      if (!strcasecmp (arg, "yes"))
        value = IPMI_SYSTEM_BOOT_OPTION_ENABLE;
      else if (!strcasecmp (arg, "no"))
        value = IPMI_SYSTEM_BOOT_OPTION_DISABLE;
      else
        {
          fprintf (stderr, "invalid value for clear-cmos\n");
          exit (EXIT_FAILURE);
        }

      cmd_args->set_system_boot_options_args.cmos_clear++;
      cmd_args->set_system_boot_options_args.cmos_clear_arg = value;
      break;

    case SET_BOOT_FLAGS_CONSOLE_REDIRECTION_KEY:
      if (!strcasecmp (arg, "default"))
        value = IPMI_SYSTEM_BOOT_OPTION_BOOT_FLAG_CONSOLE_REDIRECTION_DEFAULT;
      else if (!strcasecmp (arg, "suppress"))
        value = IPMI_SYSTEM_BOOT_OPTION_BOOT_FLAG_CONSOLE_REDIRECTION_SUPPRESS;
      else if (!strcasecmp (arg, "enable"))
        value = IPMI_SYSTEM_BOOT_OPTION_BOOT_FLAG_CONSOLE_REDIRECTION_ENABLE;
      else
        {
          fprintf (stderr, "invalid value for console-redirection\n");
          exit (EXIT_FAILURE);
        }

      cmd_args->set_system_boot_options_args.console_redirection++;
      cmd_args->set_system_boot_options_args.console_redirection_arg = value;
      break;

    case SET_BOOT_FLAGS_USER_PASSWORD_BYPASS_KEY:
      if (!strcasecmp (arg, "yes"))
        value = IPMI_SYSTEM_BOOT_OPTION_ENABLE;
      else if (!strcasecmp (arg, "no"))
        value = IPMI_SYSTEM_BOOT_OPTION_DISABLE;
      else
        {
          fprintf (stderr, "invalid value for user-password-bypass\n");
          exit (EXIT_FAILURE);
        }

      cmd_args->set_system_boot_options_args.user_password_bypass++;
      cmd_args->set_system_boot_options_args.user_password_bypass_arg = value;
      break;

    case SET_BOOT_FLAGS_FORCE_PROGRESS_EVENT_TRAPS_KEY:
      if (!strcasecmp (arg, "yes"))
        value = IPMI_SYSTEM_BOOT_OPTION_ENABLE;
      else if (!strcasecmp (arg, "no"))
        value = IPMI_SYSTEM_BOOT_OPTION_DISABLE;
      else
        {
          fprintf (stderr, "invalid value for force-progress-event-traps\n");
          exit (EXIT_FAILURE);
        }

      cmd_args->set_system_boot_options_args.force_progress_event_traps++;
      cmd_args->set_system_boot_options_args.force_progress_event_traps_arg = value;
      break;

    case SET_BOOT_FLAGS_FIRMWARE_BIOS_VERBOSITY_KEY:
      if (!strcasecmp (arg, "quiet"))
        value = IPMI_SYSTEM_BOOT_OPTION_BOOT_FLAG_FIRMWARE_BIOS_VERBOSITY_QUIET;
      else if (!strcasecmp (arg, "default"))
        value = IPMI_SYSTEM_BOOT_OPTION_BOOT_FLAG_FIRMWARE_BIOS_VERBOSITY_DEFAULT;
      else if (!strcasecmp (arg, "verbose"))
        value = IPMI_SYSTEM_BOOT_OPTION_BOOT_FLAG_FIRMWARE_BIOS_VERBOSITY_VERBOSE;
      else
        {
          fprintf (stderr, "invalid value for firmware verbosity\n");
          exit (EXIT_FAILURE);
        }

      cmd_args->set_system_boot_options_args.firmware_bios_verbosity++;
      cmd_args->set_system_boot_options_args.firmware_bios_verbosity_arg = value;
      break;

    default:
      return (ARGP_ERR_UNKNOWN);
    }

  return (0);
}

static error_t
cmdline_parse (int key, char *arg, struct argp_state *state)
{
  error_t ret;
  char *endptr = NULL;
  struct ipmi_chassis_arguments *cmd_args;
  int tmp;

  assert (state);
  
  cmd_args = state->input;

  switch (key)
    {
    case GET_CHASSIS_CAPABILITIES_KEY:
      cmd_args->get_chassis_capabilities++;
      break;

    case GET_CHASSIS_STATUS_KEY:
      cmd_args->get_chassis_status++;
      break;

    case CHASSIS_CONTROL_KEY:
      if (!strcasecmp (arg, "power-down"))
        cmd_args->chassis_control_arg = IPMI_CHASSIS_CONTROL_POWER_DOWN;
      else if (!strcasecmp (arg, "power-up"))
        cmd_args->chassis_control_arg = IPMI_CHASSIS_CONTROL_POWER_UP;
      else if (!strcasecmp (arg, "power-cycle"))
        cmd_args->chassis_control_arg = IPMI_CHASSIS_CONTROL_POWER_CYCLE;
      else if (!strcasecmp (arg, "hard-reset"))
        cmd_args->chassis_control_arg = IPMI_CHASSIS_CONTROL_HARD_RESET;
      else if (!strcasecmp (arg, "diagnostic-interrupt"))
        cmd_args->chassis_control_arg = IPMI_CHASSIS_CONTROL_PULSE_DIAGNOSTIC_INTERRUPT;
      else if (!strcasecmp (arg, "soft-shutdown"))
        cmd_args->chassis_control_arg = IPMI_CHASSIS_CONTROL_INITIATE_SOFT_SHUTDOWN;
      else
        {
          fprintf (stderr, "invalid value for chassis control\n");
          exit (EXIT_FAILURE);
        }
      cmd_args->chassis_control++;
      break;

    case CHASSIS_IDENTIFY_KEY:
      if (!strcasecmp (arg, "turn-off"))
        {
          cmd_args->chassis_identify_args.identify_interval = 1;
          cmd_args->chassis_identify_args.identify_interval_arg = 0;

          cmd_args->chassis_identify_args.force_identify = 0;
        }
      else if (!strcasecmp (arg, "force"))
        {
          cmd_args->chassis_identify_args.force_identify = 1;
          cmd_args->chassis_identify_args.force_identify_arg = IPMI_CHASSIS_FORCE_IDENTIFY_ON;

          /* Need to have identify_interval set if force_identify is set */
          cmd_args->chassis_identify_args.identify_interval = 1;
          cmd_args->chassis_identify_args.identify_interval_arg = 0xFF;
        }
      else
        {
	  errno = 0;
          tmp = strtol (arg, &endptr, 10);
	  if (errno
	      || endptr[0] != '\0')
            {
              fprintf (stderr, "invalid value for chassis-identify\n");
              exit (EXIT_FAILURE);
            }
          if (tmp < IPMI_CHASSIS_IDENTIFY_INTERVAL_MIN
              || tmp > IPMI_CHASSIS_IDENTIFY_INTERVAL_MAX)
            {
              fprintf (stderr, "chassis-identify interval out of range\n");
              exit (EXIT_FAILURE);
            }
          cmd_args->chassis_identify_args.identify_interval = 1;
          cmd_args->chassis_identify_args.identify_interval_arg = tmp;
          cmd_args->chassis_identify_args.force_identify = 0;
        }
      cmd_args->chassis_identify++;
      break;

    case SET_POWER_RESTORE_POLICY_KEY:
      if (!strcasecmp (arg, "always-on"))
        cmd_args->set_power_restore_policy_arg = IPMI_POWER_RESTORE_POLICY_ALWAYS_POWER_UP_AFTER_AC_IS_LOST;
      else if (!strcasecmp (arg, "always-off"))
        cmd_args->set_power_restore_policy_arg = IPMI_POWER_RESTORE_POLICY_ALWAYS_STAY_POWERED_OFF;
      else if (!strcasecmp (arg, "restore"))
        cmd_args->set_power_restore_policy_arg = IPMI_POWER_RESTORE_POLICY_RESTORE_POWER_TO_STATE_WHEN_AC_WAS_LOST;
      else if (!strcasecmp (arg, "list-supported-policies"))
        cmd_args->set_power_restore_policy_arg = IPMI_POWER_RESTORE_POLICY_NO_CHANGE;
      else
        {
          fprintf (stderr, "invalid value for power restore policy\n");
          exit (EXIT_FAILURE);
        }
      cmd_args->set_power_restore_policy++;
      break;

    case SET_POWER_CYCLE_INTERVAL_KEY:
      errno = 0;
      tmp = strtol (arg, &endptr, 10);
      if (errno
	  || endptr[0] != '\0')
        {
          fprintf (stderr, "invalid value for power cycle interval\n");
          exit (EXIT_FAILURE);
        }
      if (tmp < IPMI_CHASSIS_POWER_CYCLE_INTERVAL_MIN
          || tmp > IPMI_CHASSIS_POWER_CYCLE_INTERVAL_MAX)
        {
          fprintf (stderr, "power-cycle interval out of range\n");
          exit (EXIT_FAILURE);
        }
      cmd_args->set_power_cycle_interval_arg = tmp;
      cmd_args->set_power_cycle_interval++;
      break;

    case GET_SYSTEM_RESTART_CAUSE_KEY:
      cmd_args->get_system_restart_cause++;
      break;

    case SET_BOOT_FLAGS_KEY:
      cmd_args->set_system_boot_options++;
      break;

    case GET_BOOT_FLAGS_KEY:
      cmd_args->get_system_boot_options++;
      break;

    case GET_POWER_ON_HOURS_COUNTER_KEY:
      cmd_args->get_power_on_hours_counter++;
      break;

    case ARGP_KEY_ARG:
      /* Too many arguments. */
      argp_usage (state);
      break;

    case ARGP_KEY_END:
      break;

    default:
      /* All chassis "set" operations are legacy, see ipmi-config for chassis configuration */ 
      ret = boot_flag_parse (key, arg, state);
      if (ret == ARGP_ERR_UNKNOWN)
        ret = common_parse_opt (key, arg, &(cmd_args->common_args));
      return (ret);
    }

  return (0);
}

static void
_ipmi_chassis_config_file_parse (struct ipmi_chassis_arguments *cmd_args)
{
  assert (cmd_args);

  if (config_file_parse (cmd_args->common_args.config_file,
                         0,
                         &(cmd_args->common_args),
                         CONFIG_FILE_INBAND | CONFIG_FILE_OUTOFBAND | CONFIG_FILE_HOSTRANGE,
                         CONFIG_FILE_TOOL_IPMI_CHASSIS,
                         NULL) < 0)
    {
      fprintf (stderr, "config_file_parse: %s\n", strerror (errno));
      exit (EXIT_FAILURE);
    }
}

static void
_ipmi_chassis_args_validate (struct ipmi_chassis_arguments *cmd_args)
{
  assert (cmd_args);

  if (!cmd_args->get_chassis_capabilities
      && !cmd_args->get_chassis_status
      && !cmd_args->chassis_control
      && !cmd_args->chassis_identify
      && !cmd_args->set_power_restore_policy
      && !cmd_args->set_power_cycle_interval
      && !cmd_args->get_system_restart_cause
      && !cmd_args->set_system_boot_options
      && !cmd_args->get_system_boot_options
      && !cmd_args->get_power_on_hours_counter)
    {
      fprintf (stderr,
               "No command specified.\n");
      exit (EXIT_FAILURE);
    }

  if ((cmd_args->get_chassis_capabilities
       + cmd_args->get_chassis_status
       + cmd_args->chassis_control
       + cmd_args->chassis_identify
       + cmd_args->set_power_restore_policy
       + cmd_args->set_power_cycle_interval
       + cmd_args->get_system_restart_cause
       + cmd_args->set_system_boot_options
       + cmd_args->get_system_boot_options
       + cmd_args->get_power_on_hours_counter) > 1)
    {
      fprintf (stderr,
               "Multiple commands specified.\n");
      exit (EXIT_FAILURE);
    }

  if (cmd_args->set_system_boot_options
      && (!cmd_args->set_system_boot_options_args.bios_boot_type
          && !cmd_args->set_system_boot_options_args.lock_out_reset_button
          && !cmd_args->set_system_boot_options_args.screen_blank
          && !cmd_args->set_system_boot_options_args.boot_device
          && !cmd_args->set_system_boot_options_args.lock_keyboard
          && !cmd_args->set_system_boot_options_args.cmos_clear
          && !cmd_args->set_system_boot_options_args.console_redirection
          && !cmd_args->set_system_boot_options_args.user_password_bypass
          && !cmd_args->set_system_boot_options_args.force_progress_event_traps
          && !cmd_args->set_system_boot_options_args.firmware_bios_verbosity))
    {
      fprintf (stderr,
               "No boot flags configuration changes specified\n");
      exit (EXIT_FAILURE);
    }
}

void
ipmi_chassis_argp_parse (int argc,
                         char **argv,
                         struct ipmi_chassis_arguments *cmd_args)
{
  assert (argc >= 0);
  assert (argv);
  assert (cmd_args);

  init_common_cmd_args_admin (&(cmd_args->common_args));

  cmd_args->get_chassis_capabilities = 0;
  cmd_args->get_chassis_status = 0;
  cmd_args->chassis_control = 0;
  cmd_args->chassis_control_arg = 0;
  cmd_args->chassis_identify = 0;
  cmd_args->chassis_identify_args.identify_interval = 0;
  cmd_args->chassis_identify_args.force_identify = 0;
  cmd_args->set_power_restore_policy = 0;
  cmd_args->set_power_restore_policy_arg = 0;
  cmd_args->set_power_cycle_interval = 0;
  cmd_args->set_power_cycle_interval_arg = 0;
  cmd_args->get_system_restart_cause = 0;
  cmd_args->get_power_on_hours_counter = 0;
  cmd_args->set_system_boot_options = 0;
  cmd_args->set_system_boot_options_args.bios_boot_type = 0;
  cmd_args->set_system_boot_options_args.lock_out_reset_button = 0;
  cmd_args->set_system_boot_options_args.screen_blank = 0;
  cmd_args->set_system_boot_options_args.boot_device = 0;
  cmd_args->set_system_boot_options_args.lock_keyboard = 0;
  cmd_args->set_system_boot_options_args.cmos_clear = 0;
  cmd_args->set_system_boot_options_args.console_redirection = 0;
  cmd_args->set_system_boot_options_args.user_password_bypass = 0;
  cmd_args->set_system_boot_options_args.force_progress_event_traps = 0;
  cmd_args->set_system_boot_options_args.firmware_bios_verbosity = 0;
  cmd_args->get_system_boot_options = 0;

  argp_parse (&cmdline_config_file_argp,
              argc,
              argv,
              ARGP_IN_ORDER,
              NULL,
              &(cmd_args->common_args));

  _ipmi_chassis_config_file_parse (cmd_args);

  argp_parse (&cmdline_argp,
              argc,
              argv,
              ARGP_IN_ORDER,
              NULL,
              cmd_args);

  verify_common_cmd_args (&(cmd_args->common_args));
  _ipmi_chassis_args_validate (cmd_args);
}

