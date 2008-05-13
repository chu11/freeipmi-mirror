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
#endif

#include <stdlib.h>
#include <argp.h>

#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */

#include <freeipmi/freeipmi.h>

#include "ipmi-chassis.h"
#include "ipmi-chassis-argp.h"
#include "tool-cmdline-common.h"

#include "freeipmi-portability.h"

static error_t parse_opt (int key, char *arg, struct argp_state *state);

const char *argp_program_version = 
  "IPMI Chassis [ipmi-chassis-" PACKAGE_VERSION "]\n"
  "Copyright (C) 2007-2008 FreeIPMI Core Team\n"
  "This program is free software; you may redistribute it under the terms of\n"
  "the GNU General Public License.  This program has absolutely no warranty.";

const char *argp_program_bug_address = "<freeipmi-devel@gnu.org>";

static char doc[] = "IPMI Chassis - Management a chassis via IPMI";

static char args_doc[] = "";

static struct argp_option options[] = 
  {
    ARGP_COMMON_OPTIONS_DRIVER,
    ARGP_COMMON_OPTIONS_INBAND,
    ARGP_COMMON_OPTIONS_OUTOFBAND,
    ARGP_COMMON_OPTIONS_AUTHENTICATION_TYPE,
    ARGP_COMMON_OPTIONS_CIPHER_SUITE_ID,
    ARGP_COMMON_OPTIONS_PRIVILEGE_LEVEL_ADMIN,
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
    {"set-power-restore-policy", SET_POWER_RESTORE_POLICY_KEY, "POLICY", 0, 
     "Set power restore policy.", 34},
    {"set-power-cycle-interval", SET_POWER_CYCLE_INTERVAL_KEY, "SECONDS", 0, 
     "Set Power cycle interval in seconds.", 35},
    {"get-system-restart-cause", GET_SYSTEM_RESTART_CAUSE_KEY, NULL, 0, 
     "Get system restart cause.", 36},
    {"get-power-on-hours-counter", GET_POWER_ON_HOURS_COUNTER_KEY, NULL, 0,
     "Get power on hours counter.", 37},
    {"get-boot-flags", GET_BOOT_FLAGS_KEY, NULL, 0, 
     "Get system boot-flags.", 38},
    {"set-boot-flags", SET_BOOT_FLAGS_KEY, NULL, 0, 
     "Set system boot flags.", 39},
    {"boot-type", SET_BOOT_FLAGS_BOOT_TYPE_KEY, "BOOT_TYPE", OPTION_ARG_OPTIONAL, 
     "Set BIOS boot type to BOOT_TYPE.", 40},
    {"lock-out-reset-button", SET_BOOT_FLAGS_LOCK_OUT_RESET_BUTTON_KEY, "LOCK_OUT_RESET_BUTTON", OPTION_ARG_OPTIONAL, 
     "Modify lock out reset button support.", 41},
    {"blank-screen", SET_BOOT_FLAGS_SCREEN_BLANK_KEY, "BLANK_SCREEN", OPTION_ARG_OPTIONAL, 
     "Modify blank screen support.", 42},
    {"boot-device", SET_BOOT_FLAGS_BOOT_DEVICE_SELECTOR_KEY, "BOOT_DEVICE", OPTION_ARG_OPTIONAL, 
     "Set device to boot from to BOOT_DEVICE.", 43},
    {"lock-keyboard", SET_BOOT_FLAGS_LOCK_KEYBOARD_KEY, "LOCK_KEYBOARD", OPTION_ARG_OPTIONAL, 
     "Modify lock keyboard support.", 44},
    {"clear-cmos", SET_BOOT_FLAGS_CLEAR_CMOS_KEY, "CMOS_CLEAR", OPTION_ARG_OPTIONAL, 
     "Modify clear CMOS support.", 45},
    {"console-redirection", SET_BOOT_FLAGS_CONSOLE_REDIRECTION_KEY, "CONSOLE_REDIRECTION", OPTION_ARG_OPTIONAL, 
     "Set console redirection type.", 46},
    {"user-password-bypass", SET_BOOT_FLAGS_USER_PASSWORD_BYPASS_KEY, "USER_PASSWORD_BYPASS", OPTION_ARG_OPTIONAL, 
     "Modify user password bypass support.", 47},
    {"force-progress-event-traps", SET_BOOT_FLAGS_FORCE_PROGRESS_EVENT_TRAPS_KEY, "FORCE_PROGRESS_EVENT_TRAPS", OPTION_ARG_OPTIONAL, 
     "Modify force progress event traps support.", 48},
    {"firmware-bios-verbosity", SET_BOOT_FLAGS_FIRMWARE_BIOS_VERBOSITY_KEY, "FIRMWARE_BIOS_VERBOSITY", OPTION_ARG_OPTIONAL, 
     "Set firmware verbosity.", 49},
    { 0 }
  };

static char *boot_argv[];
static error_t boot_flag_parse_opt (int, char *, struct argp_state *);
static error_t

parse_opt (int key, char *arg, struct argp_state *state);

static struct argp argp = { options, parse_opt, args_doc, doc };

static error_t
boot_flag_parse_opt (int key, char *arg, struct argp_state *state)
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
          fprintf (stderr, "Invalid value for boot-type\n");
          argp_usage (state);
        }

      cmd_args->args.boot_option_args.bios_boot_type = value;
      break;

    case SET_BOOT_FLAGS_LOCK_OUT_RESET_BUTTON_KEY:
      if (!strcasecmp(arg, "yes"))
        value = IPMI_CHASSIS_BOOT_OPTIONS_ENABLE;
      else if (!strcasecmp(arg, "no"))
        value = IPMI_CHASSIS_BOOT_OPTIONS_DISABLE;
      else
        {
          fprintf (stderr, "Invalid value for lock-out-reset-button\n");
          argp_usage (state);
        }

      cmd_args->args.boot_option_args.lock_out_reset_button = value;
      break;

    case SET_BOOT_FLAGS_SCREEN_BLANK_KEY:
      if (!strcasecmp(arg, "yes"))
        value = IPMI_CHASSIS_BOOT_OPTIONS_ENABLE;
      else if (!strcasecmp(arg, "no"))
        value = IPMI_CHASSIS_BOOT_OPTIONS_DISABLE;
      else
        {
          fprintf (stderr, "Invalid value for blank-screen\n");
          argp_usage (state);
        }

      cmd_args->args.boot_option_args.screen_blank = value;
      break;

    case SET_BOOT_FLAGS_BOOT_DEVICE_SELECTOR_KEY:
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
          fprintf (stderr, "Invalid value for boot-device\n");
          argp_usage (state);
        }

      cmd_args->args.boot_option_args.boot_device_selector = value;
      break;

    case SET_BOOT_FLAGS_LOCK_KEYBOARD_KEY:
      if (!strcasecmp(arg, "yes"))
        value = IPMI_CHASSIS_BOOT_OPTIONS_ENABLE;
      else if (!strcasecmp(arg, "no"))
        value = IPMI_CHASSIS_BOOT_OPTIONS_DISABLE;
      else
        {
          fprintf (stderr, "Invalid value for lock-keyboard\n");
          argp_usage (state);
        }

      cmd_args->args.boot_option_args.lock_keyboard = value;
      break;

    case SET_BOOT_FLAGS_CLEAR_CMOS_KEY:
      if (!strcasecmp (arg, "yes"))
        value = IPMI_CHASSIS_BOOT_OPTIONS_ENABLE;
      else if (!strcasecmp (arg, "no"))
        value = IPMI_CHASSIS_BOOT_OPTIONS_DISABLE;
      else
        {
          fprintf (stderr, "Invalid value for clear-cmos\n");
          argp_usage (state);
        }

      cmd_args->args.boot_option_args.clear_cmos = value;
      break;

    case SET_BOOT_FLAGS_CONSOLE_REDIRECTION_KEY:
      if (!strcasecmp (arg, "default"))
        value = IPMI_CHASSIS_BOOT_OPTIONS_BOOT_FLAG_CONSOLE_REDIRECTION_DEFAULT;
      else if (!strcasecmp (arg, "suppress"))
        value = IPMI_CHASSIS_BOOT_OPTIONS_BOOT_FLAG_CONSOLE_REDIRECTION_SUPRESS;
      else if (!strcasecmp (arg, "enable"))
        value = IPMI_CHASSIS_BOOT_OPTIONS_BOOT_FLAG_CONSOLE_REDIRECTION_ENABLE;
      else
        {
          fprintf (stderr, "Invalid value for console-redirection\n");
          argp_usage (state);
        }

      cmd_args->args.boot_option_args.console_redirection = value;
      break;

    case SET_BOOT_FLAGS_USER_PASSWORD_BYPASS_KEY:
      if (!strcasecmp(arg, "yes"))
        value = IPMI_CHASSIS_BOOT_OPTIONS_ENABLE;
      else if (!strcasecmp(arg, "no"))
        value = IPMI_CHASSIS_BOOT_OPTIONS_DISABLE;
      else
        {
          fprintf (stderr, "Invalid value for user-password-bypass\n");
          argp_usage (state);
        }

      cmd_args->args.boot_option_args.user_password_bypass = value;
      break;

    case SET_BOOT_FLAGS_FORCE_PROGRESS_EVENT_TRAPS_KEY:
      if (!strcasecmp(arg, "yes"))
        value = IPMI_CHASSIS_BOOT_OPTIONS_ENABLE;
      else if (!strcasecmp(arg, "no"))
        value = IPMI_CHASSIS_BOOT_OPTIONS_DISABLE;
      else
        {
          fprintf (stderr, "Invalid value for force-progress-event-traps\n");
          argp_usage (state);
        }

      cmd_args->args.boot_option_args.force_progress_event_traps = value;
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
          fprintf (stderr, "Invalid value for firmware verbosity\n");
          argp_usage (state);
        }

      cmd_args->args.boot_option_args.firmware_bios_verbosity = value;
      break;

    default:
      return ARGP_ERR_UNKNOWN;
    }

  if ((cmd_args->cmd != CHASSIS_CMD_SET_SYSTEM_BOOT_OPTIONS))
    {
      fprintf (stderr, "please specify set-boot-flags option\n");
      argp_usage (state);
    }
  return 0;
}

static error_t
parse_opt (int key, char *arg, struct argp_state *state)
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
        cmd_args->args.chassis_control = IPMI_CHASSIS_CONTROL_POWER_DOWN;
      else if (!strcasecmp(arg, "power-up"))
        cmd_args->args.chassis_control = IPMI_CHASSIS_CONTROL_POWER_UP;
      else if (!strcasecmp(arg, "power-cycle"))
        cmd_args->args.chassis_control = IPMI_CHASSIS_CONTROL_POWER_CYCLE;
      else if (!strcasecmp(arg, "hard-reset"))
        cmd_args->args.chassis_control = IPMI_CHASSIS_CONTROL_HARD_RESET;
      else if (!strcasecmp(arg, "diagnostic-interrupt"))
        cmd_args->args.chassis_control = IPMI_CHASSIS_CONTROL_PULSE_DIAGNOSTIC_INTERRUPT;
      else if (!strcasecmp(arg, "soft-shutdown"))
        cmd_args->args.chassis_control = IPMI_CHASSIS_CONTROL_INITIATE_SOFT_SHUTDOWN;
      else
        {
          fprintf (stderr, "Invalid value for chassis control\n");
          argp_usage (state);
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
          cmd_args->args.identify_args.identify_interval = 0;
          cmd_args->args.identify_args.identify_interval_set = 1;

          cmd_args->args.identify_args.force_identify_set = 0;
        }
      else if (!strcasecmp(arg, "force"))
        {
          cmd_args->args.identify_args.force_identify = IPMI_CHASSIS_FORCE_IDENTIFY_ON;
          cmd_args->args.identify_args.force_identify_set = 1;

          /* Need to have identify_interval set if force_identify is set */
          cmd_args->args.identify_args.identify_interval = 0xFF;
          cmd_args->args.identify_args.identify_interval_set = 1;
        }
      else
        {
          cmd_args->args.identify_args.identify_interval = strtol (arg, &ptr, 10);
          if (*ptr != '\0')
            {
              fprintf (stderr, "Invalid value for chassis-identify\n");
              argp_usage (state);
            }
          cmd_args->args.identify_args.force_identify_set = 1;

          cmd_args->args.identify_args.force_identify_set = 0;
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
      cmd_args->args.power_cycle_interval = strtol (arg, &ptr, 10);
      if (*ptr != '\0')
        {
          fprintf (stderr, "Invalid value for power cycle interval\n");
          argp_usage (state);
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
        cmd_args->args.power_restore_policy = IPMI_POWER_RESTORE_POLICY_ALWAYS_POWER_UP_AFTER_AC_IS_LOST;
      else if (!strcasecmp (arg, "always-off"))
        cmd_args->args.power_restore_policy = IPMI_POWER_RESTORE_POLICY_ALWAYS_STAY_POWERED_OFF;
      else if (!strcasecmp (arg, "restore"))
        cmd_args->args.power_restore_policy = IPMI_POWER_RESTORE_POLICY_RESTORE_POWER_TO_STATE_WHEN_AC_WAS_LOST;
      else if (!strcasecmp (arg, "list-supported-policies"))
        cmd_args->args.power_restore_policy = IPMI_POWER_RESTORE_POLICY_NO_CHANGE;
      else
        {
          fprintf (stderr, "Invalid value for power restore policy\n");
          argp_usage (state);
        }
      break;
    case ARGP_KEY_ARG:
      /* Too many arguments. */
      argp_usage (state);
      break;

    case ARGP_KEY_END:
      break;

    default:
      ret = boot_flag_parse_opt (key, arg, state);

      if (ret == ARGP_ERR_UNKNOWN)
        ret = common_parse_opt (key, arg, state, &(cmd_args->common));
      if (ret == ARGP_ERR_UNKNOWN)
        ret = hostrange_parse_opt (key, arg, state, &(cmd_args->hostrange));
      return ret;
    }

  return 0;
}


void 
ipmi_chassis_argp_parse (int argc, 
                         char **argv,
			 struct ipmi_chassis_arguments *cmd_args)
{
  init_common_cmd_args_admin (&(cmd_args->common));
  init_hostrange_cmd_args (&(cmd_args->hostrange));
  cmd_args->cmd = -1;

  cmd_args->args.identify_args.identify_interval = 0;
  cmd_args->args.identify_args.identify_interval_set = 0;
  cmd_args->args.identify_args.force_identify = 0;
  cmd_args->args.identify_args.force_identify_set = 0;
 
  cmd_args->args.boot_option_args.bios_boot_type = -1;
  cmd_args->args.boot_option_args.lock_out_reset_button = -1;
  cmd_args->args.boot_option_args.screen_blank = -1;
  cmd_args->args.boot_option_args.boot_device_selector = -1;
  cmd_args->args.boot_option_args.lock_keyboard = -1;
  cmd_args->args.boot_option_args.clear_cmos = -1;
  cmd_args->args.boot_option_args.console_redirection = -1;
  cmd_args->args.boot_option_args.user_password_bypass = -1;
  cmd_args->args.boot_option_args.force_progress_event_traps = -1;
  cmd_args->args.boot_option_args.firmware_bios_verbosity = -1;

  argp_parse (&argp, argc, argv, ARGP_IN_ORDER, NULL, cmd_args);
  verify_common_cmd_args (&(cmd_args->common));
  verify_hostrange_cmd_args (&(cmd_args->hostrange));
}

int 
ipmi_chassis_args_validate (struct ipmi_chassis_arguments *args)
{
  if (args->cmd < 0)
    {   
      fprintf (stderr,
               "Error: No command specified\n");
      return -1;
    }

  return 0;
}
