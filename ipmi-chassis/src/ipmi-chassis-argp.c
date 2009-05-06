/* 
   Copyright (C) 2007-2008 FreeIPMI Core Team
   
   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.
   
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.
   
   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software Foundation,
   Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA.  
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

#include <freeipmi/freeipmi.h>

#include "ipmi-chassis.h"
#include "ipmi-chassis-argp.h"

#include "freeipmi-portability.h"
#include "tool-cmdline-common.h"
#include "tool-config-file-common.h"

const char *argp_program_version = 
  "ipmi-chassis - " PACKAGE_VERSION "\n"
  "Copyright (C) 2007-2008 FreeIPMI Core Team\n"
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
    ARGP_COMMON_OPTIONS_OUTOFBAND,
    ARGP_COMMON_OPTIONS_AUTHENTICATION_TYPE,
    ARGP_COMMON_OPTIONS_CIPHER_SUITE_ID,
    ARGP_COMMON_OPTIONS_PRIVILEGE_LEVEL_ADMIN,
    ARGP_COMMON_OPTIONS_CONFIG_FILE,
    ARGP_COMMON_OPTIONS_WORKAROUND_FLAGS,
    ARGP_COMMON_HOSTRANGED_OPTIONS,
    ARGP_COMMON_OPTIONS_DEBUG,
    {"get-capabilities", GET_CAPABILITIES_KEY, NULL, 0, 
     "Get chassis capabilities.", 30},
    {"get-status", GET_STATUS_KEY, NULL, 0, 
     "Get chassis status.", 31},
    {"chassis-control", CHASSIS_CONTROL_KEY, "CONTROL", 0, 
     "Control the chassis.", 32},
    {"chassis-identify", CHASSIS_IDENTIFY_KEY, "IDENTIFY", 0,
     "Set chassis Identification.", 33},
    {"set-power-restore-policy", SET_POWER_RESTORE_POLICY_KEY, "POLICY", OPTION_HIDDEN, 
     "Set power restore policy.", 34},
    {"set-power-cycle-interval", SET_POWER_CYCLE_INTERVAL_KEY, "SECONDS", OPTION_HIDDEN, 
     "Set Power cycle interval in seconds.", 35},
    {"get-system-restart-cause", GET_SYSTEM_RESTART_CAUSE_KEY, NULL, 0, 
     "Get system restart cause.", 36},
    {"get-power-on-hours-counter", GET_POWER_ON_HOURS_COUNTER_KEY, NULL, 0,
     "Get power on hours (POH) counter.", 37},
    {"get-boot-flags", GET_BOOT_FLAGS_KEY, NULL, OPTION_HIDDEN, 
     "Get system boot-flags.", 38},
    {"set-boot-flags", SET_BOOT_FLAGS_KEY, NULL, OPTION_HIDDEN, 
     "Set system boot flags.", 39},
    {"boot-type", SET_BOOT_FLAGS_BOOT_TYPE_KEY, "BOOT_TYPE", OPTION_ARG_OPTIONAL | OPTION_HIDDEN, 
     "Set BIOS boot type to BOOT_TYPE.", 40},
    {"lock-out-reset-button", SET_BOOT_FLAGS_LOCK_OUT_RESET_BUTTON_KEY, "LOCK_OUT_RESET_BUTTON", OPTION_ARG_OPTIONAL | OPTION_HIDDEN, 
     "Modify lock out reset button support.", 41},
    {"blank-screen", SET_BOOT_FLAGS_SCREEN_BLANK_KEY, "BLANK_SCREEN", OPTION_ARG_OPTIONAL | OPTION_HIDDEN, 
     "Modify blank screen support.", 42},
    {"boot-device", SET_BOOT_FLAGS_BOOT_DEVICE_KEY, "BOOT_DEVICE", OPTION_ARG_OPTIONAL | OPTION_HIDDEN, 
     "Set device to boot from to BOOT_DEVICE.", 43},
    {"lock-keyboard", SET_BOOT_FLAGS_LOCK_KEYBOARD_KEY, "LOCK_KEYBOARD", OPTION_ARG_OPTIONAL | OPTION_HIDDEN, 
     "Modify lock keyboard support.", 44},
    {"clear-cmos", SET_BOOT_FLAGS_CMOS_CLEAR_KEY, "CMOS_CLEAR", OPTION_ARG_OPTIONAL | OPTION_HIDDEN, 
     "Modify clear CMOS support.", 45},
    {"console-redirection", SET_BOOT_FLAGS_CONSOLE_REDIRECTION_KEY, "CONSOLE_REDIRECTION", OPTION_ARG_OPTIONAL | OPTION_HIDDEN, 
     "Set console redirection type.", 46},
    {"user-password-bypass", SET_BOOT_FLAGS_USER_PASSWORD_BYPASS_KEY, "USER_PASSWORD_BYPASS", OPTION_ARG_OPTIONAL | OPTION_HIDDEN, 
     "Modify user password bypass support.", 47},
    {"force-progress-event-traps", SET_BOOT_FLAGS_FORCE_PROGRESS_EVENT_TRAPS_KEY, "FORCE_PROGRESS_EVENT_TRAPS", OPTION_ARG_OPTIONAL | OPTION_HIDDEN, 
     "Modify force progress event traps support.", 48},
    {"firmware-bios-verbosity", SET_BOOT_FLAGS_FIRMWARE_BIOS_VERBOSITY_KEY, "FIRMWARE_BIOS_VERBOSITY", OPTION_ARG_OPTIONAL | OPTION_HIDDEN, 
     "Set firmware verbosity.", 49},
    { 0 }
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

static error_t
boot_flag_parse (int key, char *arg, struct argp_state *state)
{
  struct ipmi_chassis_arguments *cmd_args = state->input;
  uint8_t value = 0;

  switch (key)
    {
    case SET_BOOT_FLAGS_BOOT_TYPE_KEY:
      if (!strcasecmp(arg, "pc-compatible"))
        value = IPMI_CHASSIS_BOOT_OPTIONS_BOOT_FLAG_BOOT_TYPE_PC_COMPATIBLE;
      else if (!strcasecmp(arg, "efi"))
        value = IPMI_CHASSIS_BOOT_OPTIONS_BOOT_FLAG_BOOT_TYPE_EFI;
      else
        {
          fprintf (stderr, "invalid value for boot-type\n");
          exit(1);
        }

      cmd_args->boot_option_args.bios_boot_type = value;
      break;

    case SET_BOOT_FLAGS_LOCK_OUT_RESET_BUTTON_KEY:
      if (!strcasecmp(arg, "yes"))
        value = IPMI_CHASSIS_BOOT_OPTIONS_ENABLE;
      else if (!strcasecmp(arg, "no"))
        value = IPMI_CHASSIS_BOOT_OPTIONS_DISABLE;
      else
        {
          fprintf (stderr, "invalid value for lock-out-reset-button\n");
          exit(1);
        }

      cmd_args->boot_option_args.lock_out_reset_button = value;
      break;

    case SET_BOOT_FLAGS_SCREEN_BLANK_KEY:
      if (!strcasecmp(arg, "yes"))
        value = IPMI_CHASSIS_BOOT_OPTIONS_ENABLE;
      else if (!strcasecmp(arg, "no"))
        value = IPMI_CHASSIS_BOOT_OPTIONS_DISABLE;
      else
        {
          fprintf (stderr, "invalid value for blank-screen\n");
          exit(1);
        }

      cmd_args->boot_option_args.screen_blank = value;
      break;

    case SET_BOOT_FLAGS_BOOT_DEVICE_KEY:
      /* achu: many legacy inputs are preserved */
      if (!strcasecmp(arg, "no-override")
          || !strcasecmp(arg, "none")) /* legacy */
        value = IPMI_CHASSIS_BOOT_OPTIONS_BOOT_FLAG_BOOT_DEVICE_NO_OVERRIDE;
      else if (!strcasecmp(arg, "pxe"))
        value = IPMI_CHASSIS_BOOT_OPTIONS_BOOT_FLAG_BOOT_DEVICE_FORCE_PXE;
      else if (!strcasecmp(arg, "hard-drive")
               || !strcasecmp(arg, "disk")) /* legacy */
        value = IPMI_CHASSIS_BOOT_OPTIONS_BOOT_FLAG_BOOT_DEVICE_FORCE_HARD_DRIVE;
      else if (!strcasecmp(arg, "hard-drive-safe")
               || !strcasecmp(arg, "disk-safe")) /* legacy */
        value = IPMI_CHASSIS_BOOT_OPTIONS_BOOT_FLAG_BOOT_DEVICE_FORCE_HARD_DRIVE_SAFE_MODE;
      else if (!strcasecmp(arg, "diag"))
        value = IPMI_CHASSIS_BOOT_OPTIONS_BOOT_FLAG_BOOT_DEVICE_FORCE_DIAGNOSTIC_PARTITION;
      else if (!strcasecmp(arg, "cd-dvd"))
        value = IPMI_CHASSIS_BOOT_OPTIONS_BOOT_FLAG_BOOT_DEVICE_FORCE_CD_DVD;
      else if (!strcasecmp(arg, "bios"))
        value = IPMI_CHASSIS_BOOT_OPTIONS_BOOT_FLAG_BOOT_DEVICE_FORCE_BIOS_SETUP;
      else if (!strcasecmp(arg, "floppy"))
        value = IPMI_CHASSIS_BOOT_OPTIONS_BOOT_FLAG_BOOT_DEVICE_FORCE_FLOPPY_REMOVEABLE_MEDIA;
      else
        {
          fprintf (stderr, "invalid value for boot-device\n");
          exit(1);
        }

      cmd_args->boot_option_args.boot_device = value;
      break;

    case SET_BOOT_FLAGS_LOCK_KEYBOARD_KEY:
      if (!strcasecmp(arg, "yes"))
        value = IPMI_CHASSIS_BOOT_OPTIONS_ENABLE;
      else if (!strcasecmp(arg, "no"))
        value = IPMI_CHASSIS_BOOT_OPTIONS_DISABLE;
      else
        {
          fprintf (stderr, "invalid value for lock-keyboard\n");
          exit(1);
        }

      cmd_args->boot_option_args.lock_keyboard = value;
      break;

    case SET_BOOT_FLAGS_CMOS_CLEAR_KEY:
      if (!strcasecmp (arg, "yes"))
        value = IPMI_CHASSIS_BOOT_OPTIONS_ENABLE;
      else if (!strcasecmp (arg, "no"))
        value = IPMI_CHASSIS_BOOT_OPTIONS_DISABLE;
      else
        {
          fprintf (stderr, "invalid value for clear-cmos\n");
          exit(1);
        }

      cmd_args->boot_option_args.cmos_clear = value;
      break;

    case SET_BOOT_FLAGS_CONSOLE_REDIRECTION_KEY:
      if (!strcasecmp (arg, "default"))
        value = IPMI_CHASSIS_BOOT_OPTIONS_BOOT_FLAG_CONSOLE_REDIRECTION_DEFAULT;
      else if (!strcasecmp (arg, "suppress"))
        value = IPMI_CHASSIS_BOOT_OPTIONS_BOOT_FLAG_CONSOLE_REDIRECTION_SUPPRESS;
      else if (!strcasecmp (arg, "enable"))
        value = IPMI_CHASSIS_BOOT_OPTIONS_BOOT_FLAG_CONSOLE_REDIRECTION_ENABLE;
      else
        {
          fprintf (stderr, "invalid value for console-redirection\n");
          exit(1);
        }

      cmd_args->boot_option_args.console_redirection = value;
      break;

    case SET_BOOT_FLAGS_USER_PASSWORD_BYPASS_KEY:
      if (!strcasecmp(arg, "yes"))
        value = IPMI_CHASSIS_BOOT_OPTIONS_ENABLE;
      else if (!strcasecmp(arg, "no"))
        value = IPMI_CHASSIS_BOOT_OPTIONS_DISABLE;
      else
        {
          fprintf (stderr, "invalid value for user-password-bypass\n");
          exit(1);
        }

      cmd_args->boot_option_args.user_password_bypass = value;
      break;

    case SET_BOOT_FLAGS_FORCE_PROGRESS_EVENT_TRAPS_KEY:
      if (!strcasecmp(arg, "yes"))
        value = IPMI_CHASSIS_BOOT_OPTIONS_ENABLE;
      else if (!strcasecmp(arg, "no"))
        value = IPMI_CHASSIS_BOOT_OPTIONS_DISABLE;
      else
        {
          fprintf (stderr, "invalid value for force-progress-event-traps\n");
          exit(1);
        }

      cmd_args->boot_option_args.force_progress_event_traps = value;
      break;

    case SET_BOOT_FLAGS_FIRMWARE_BIOS_VERBOSITY_KEY:
      if (!strcasecmp(arg, "quiet"))
        value = IPMI_CHASSIS_BOOT_OPTIONS_BOOT_FLAG_FIRMWARE_BIOS_VERBOSITY_QUIET;
      else if (!strcasecmp(arg, "default"))
        value = IPMI_CHASSIS_BOOT_OPTIONS_BOOT_FLAG_FIRMWARE_BIOS_VERBOSITY_DEFAULT;
      else if (!strcasecmp (arg, "verbose"))
        value = IPMI_CHASSIS_BOOT_OPTIONS_BOOT_FLAG_FIRMWARE_BIOS_VERBOSITY_VERBOSE;
      else
        {
          fprintf (stderr, "invalid value for firmware verbosity\n");
          exit(1);
        }

      cmd_args->boot_option_args.firmware_bios_verbosity = value;
      break;

    default:
      return ARGP_ERR_UNKNOWN;
    }

  if ((cmd_args->cmd != CHASSIS_CMD_SET_SYSTEM_BOOT_OPTIONS))
    {
      fprintf (stderr, "please specify set-boot-flags option\n");
      exit(1);
    }
  return 0;
}

static error_t
cmdline_parse (int key, char *arg, struct argp_state *state)
{
  error_t ret;
  char *ptr = NULL;
  struct ipmi_chassis_arguments *cmd_args = state->input;

  switch (key)
    {
    case GET_CAPABILITIES_KEY:
      if (cmd_args->cmd != -1)
        {
          fprintf (stderr, "Error: Multiple commands specified\n");
          exit(EXIT_FAILURE);
        }
      cmd_args->cmd = CHASSIS_CMD_GET_CHASSIS_CAPABILITIES;
      break;

    case GET_STATUS_KEY:
      if (cmd_args->cmd != -1)
        {
          fprintf (stderr, "Error: Multiple commands specified\n");
          exit(EXIT_FAILURE);
        }
      cmd_args->cmd = CHASSIS_CMD_GET_CHASSIS_STATUS;
      break;

    case CHASSIS_CONTROL_KEY:
      if (cmd_args->cmd != -1)
        {
          fprintf (stderr, "Error: Multiple commands specified\n");
          exit(EXIT_FAILURE);
        }
      cmd_args->cmd = CHASSIS_CMD_CHASSIS_CONTROL;
      if (!strcasecmp(arg, "power-down"))
        cmd_args->chassis_control = IPMI_CHASSIS_CONTROL_POWER_DOWN;
      else if (!strcasecmp(arg, "power-up"))
        cmd_args->chassis_control = IPMI_CHASSIS_CONTROL_POWER_UP;
      else if (!strcasecmp(arg, "power-cycle"))
        cmd_args->chassis_control = IPMI_CHASSIS_CONTROL_POWER_CYCLE;
      else if (!strcasecmp(arg, "hard-reset"))
        cmd_args->chassis_control = IPMI_CHASSIS_CONTROL_HARD_RESET;
      else if (!strcasecmp(arg, "diagnostic-interrupt"))
        cmd_args->chassis_control = IPMI_CHASSIS_CONTROL_PULSE_DIAGNOSTIC_INTERRUPT;
      else if (!strcasecmp(arg, "soft-shutdown"))
        cmd_args->chassis_control = IPMI_CHASSIS_CONTROL_INITIATE_SOFT_SHUTDOWN;
      else
        {
          fprintf (stderr, "invalid value for chassis control\n");
          exit(1);
        }
      break;

    case CHASSIS_IDENTIFY_KEY:
      if (cmd_args->cmd != -1)
        {
          fprintf (stderr, "Error: Multiple commands specified\n");
          exit(EXIT_FAILURE);
        }

      cmd_args->cmd = CHASSIS_CMD_CHASSIS_IDENTIFY;
      if (!strcasecmp(arg, "turn-off"))
        {
          cmd_args->identify_args.identify_interval = 0;
          cmd_args->identify_args.identify_interval_set = 1;

          cmd_args->identify_args.force_identify_set = 0;
        }
      else if (!strcasecmp(arg, "force"))
        {
          cmd_args->identify_args.force_identify = IPMI_CHASSIS_FORCE_IDENTIFY_ON;
          cmd_args->identify_args.force_identify_set = 1;

          /* Need to have identify_interval set if force_identify is set */
          cmd_args->identify_args.identify_interval = 0xFF;
          cmd_args->identify_args.identify_interval_set = 1;
        }
      else
        {
          cmd_args->identify_args.identify_interval = strtol (arg, &ptr, 10);
          if (*ptr != '\0')
            {
              fprintf (stderr, "invalid value for chassis-identify\n");
              exit(1);
            }
          cmd_args->identify_args.identify_interval_set = 1;
          cmd_args->identify_args.force_identify_set = 0;
        }

      break;

    case GET_SYSTEM_RESTART_CAUSE_KEY:
      if (cmd_args->cmd != -1)
        {
          fprintf (stderr, "Error: Multiple commands specified\n");
          exit(EXIT_FAILURE);
        }
      cmd_args->cmd = CHASSIS_CMD_GET_SYSTEM_RESTART_CAUSE;
      break;

    case GET_POWER_ON_HOURS_COUNTER_KEY:
      if (cmd_args->cmd != -1)
        {
          fprintf (stderr, "Error: Multiple commands specified\n");
          exit(EXIT_FAILURE);
        }
      cmd_args->cmd = CHASSIS_CMD_GET_POWER_ON_HOURS_COUNTER;
      break;

    case SET_POWER_CYCLE_INTERVAL_KEY:
      if (cmd_args->cmd != -1)
        {
          fprintf (stderr, "Error: Multiple commands specified\n");
          exit(EXIT_FAILURE);
        }

      cmd_args->cmd = CHASSIS_CMD_SET_POWER_CYCLE_INTERVAL;
      cmd_args->power_cycle_interval = strtol (arg, &ptr, 10);
      if (*ptr != '\0')
        {
          fprintf (stderr, "invalid value for power cycle interval\n");
          exit(1);
        }
      break;

    case GET_BOOT_FLAGS_KEY:
      if (cmd_args->cmd != -1)
        {
          fprintf (stderr, "Error: Multiple commands specified\n");
          exit(EXIT_FAILURE);
        }
      cmd_args->cmd = CHASSIS_CMD_GET_SYSTEM_BOOT_OPTIONS;
      break;

    case SET_BOOT_FLAGS_KEY:
      if (cmd_args->cmd != -1)
        {
          fprintf (stderr, "Error: Multiple commands specified\n");
          exit(EXIT_FAILURE);
        }

      cmd_args->cmd = CHASSIS_CMD_SET_SYSTEM_BOOT_OPTIONS;
      break;

    case SET_POWER_RESTORE_POLICY_KEY:
      if (cmd_args->cmd != -1)
        {
          fprintf (stderr, "Error: Multiple commands specified\n");
          exit(EXIT_FAILURE);
        }
      cmd_args->cmd = CHASSIS_CMD_SET_POWER_RESTORE_POLICY;

      if (!strcasecmp (arg, "always-on"))
        cmd_args->power_restore_policy = IPMI_POWER_RESTORE_POLICY_ALWAYS_POWER_UP_AFTER_AC_IS_LOST;
      else if (!strcasecmp (arg, "always-off"))
        cmd_args->power_restore_policy = IPMI_POWER_RESTORE_POLICY_ALWAYS_STAY_POWERED_OFF;
      else if (!strcasecmp (arg, "restore"))
        cmd_args->power_restore_policy = IPMI_POWER_RESTORE_POLICY_RESTORE_POWER_TO_STATE_WHEN_AC_WAS_LOST;
      else if (!strcasecmp (arg, "list-supported-policies"))
        cmd_args->power_restore_policy = IPMI_POWER_RESTORE_POLICY_NO_CHANGE;
      else
        {
          fprintf (stderr, "invalid value for power restore policy\n");
          exit(1);
        }
      break;
    case ARGP_KEY_ARG:
      /* Too many arguments. */
      argp_usage (state);
      break;

    case ARGP_KEY_END:
      break;

    default:
      ret = boot_flag_parse (key, arg, state);
      if (ret == ARGP_ERR_UNKNOWN)
        ret = common_parse_opt (key, arg, state, &(cmd_args->common));
      if (ret == ARGP_ERR_UNKNOWN)
        ret = hostrange_parse_opt (key, arg, state, &(cmd_args->hostrange));
      return ret;
    }

  return 0;
}

static void
_ipmi_chassis_config_file_parse(struct ipmi_chassis_arguments *cmd_args)
{
  if (config_file_parse (cmd_args->common.config_file,
                         0,
                         &(cmd_args->common),
                         NULL,
                         &(cmd_args->hostrange),
                         CONFIG_FILE_INBAND | CONFIG_FILE_OUTOFBAND | CONFIG_FILE_HOSTRANGE,
                         CONFIG_FILE_TOOL_IPMI_CHASSIS,
                         NULL) < 0)
    {
      fprintf(stderr, "config_file_parse: %s\n", strerror(errno));
      exit(1);
    }
}

void 
_ipmi_chassis_args_validate (struct ipmi_chassis_arguments *args)
{
  if (args->cmd < 0)
    {   
      fprintf (stderr,
               "Error: No command specified\n");
      exit(1);
    }
}

void 
ipmi_chassis_argp_parse (int argc, 
                         char **argv,
			 struct ipmi_chassis_arguments *cmd_args)
{
  init_common_cmd_args_admin (&(cmd_args->common));
  init_hostrange_cmd_args (&(cmd_args->hostrange));
  cmd_args->cmd = -1;

  cmd_args->identify_args.identify_interval = 0;
  cmd_args->identify_args.identify_interval_set = 0;
  cmd_args->identify_args.force_identify = 0;
  cmd_args->identify_args.force_identify_set = 0;
 
  cmd_args->boot_option_args.bios_boot_type = -1;
  cmd_args->boot_option_args.lock_out_reset_button = -1;
  cmd_args->boot_option_args.screen_blank = -1;
  cmd_args->boot_option_args.boot_device = -1;
  cmd_args->boot_option_args.lock_keyboard = -1;
  cmd_args->boot_option_args.cmos_clear = -1;
  cmd_args->boot_option_args.console_redirection = -1;
  cmd_args->boot_option_args.user_password_bypass = -1;
  cmd_args->boot_option_args.force_progress_event_traps = -1;
  cmd_args->boot_option_args.firmware_bios_verbosity = -1;

  argp_parse (&cmdline_config_file_argp, argc, argv, ARGP_IN_ORDER, NULL, &(cmd_args->common));

  _ipmi_chassis_config_file_parse(cmd_args);

  argp_parse (&cmdline_argp, argc, argv, ARGP_IN_ORDER, NULL, cmd_args);
  verify_common_cmd_args (&(cmd_args->common));
  verify_hostrange_cmd_args (&(cmd_args->hostrange));
  _ipmi_chassis_args_validate (cmd_args);
}

