/*
ipmi-chassis.c: IPMI Chassis control.
Copyright (C) 2007 FreeIPMI Core Team

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
#endif /* STDC_HEADERS */
#include <sys/socket.h>
#include <netinet/in.h>
#include <err.h>
#include <argp.h>
#include <assert.h>

#include <freeipmi/freeipmi.h>
#include <freeipmi/udm/udm.h>

#include "argp-common.h"
#include "ipmi-chassis.h"
#include "ipmi-chassis-argp.h"
#include "ipmi-common.h"


#define _FIID_OBJ_GET(bytes, field, val)               \
do {                                                   \
    uint64_t _val = 0, *_val_ptr;                      \
    _val_ptr = val;                                    \
    if (fiid_obj_get (bytes, field, &_val) < 0)        \
      {                                                \
        fprintf (stderr,                               \
                 "fiid_obj_get: %s: %s\n",             \
                 field,                                \
                 strerror(errno));                     \
        return (-1);                                   \
      }                                                \
    *_val_ptr = _val;                                  \
} while (0)

static int32_t
get_chassis_capabilities (ipmi_chassis_state_data_t *state_data)
{
  fiid_obj_t cmd_rs = NULL;
  uint64_t val = 0;

  if (!(cmd_rs = fiid_obj_create (tmpl_cmd_get_chassis_capabilities_rs)))
    {
      perror ("fiid_obj_create");
      return (-1);
    }

  if (ipmi_cmd_get_chassis_capabilities (state_data->dev, cmd_rs) != 0)
    {
      perror ("ipmi_cmd_get_chassis_capabilities");
      return (-1);
    }

  _FIID_OBJ_GET (cmd_rs, "comp_code", &val);
  fprintf (stdout, "Completion Code         : %Xh\n",
	   (unsigned int) val);


  _FIID_OBJ_GET (cmd_rs,
		 "capabilities_flags.intrusion_sensor",
		 &val);
  fprintf (stdout, "Intrusion Sensor        : %s\n",
	   (val ? "Provided" : "Not Provided"));


  _FIID_OBJ_GET (cmd_rs,
		 "capabilities_flags.front_panel_lockout",
		 &val);
  fprintf (stdout, "Front Panel Lockout     : %s\n",
	   (val ? "Provided" : "Not Provided"));


  _FIID_OBJ_GET (cmd_rs,
		 "capabilities_flags.diagnostic_interrupt",
		 &val);
  fprintf (stdout, "Diagnostic Interrupt    : %s\n",
	   (val ? "Provided" : "Not Provided"));


  _FIID_OBJ_GET (cmd_rs,
		 "capabilities_flags.power_interlock",
		 &val);
  fprintf (stdout, "Power Interlock         : %s\n",
	   (val ? "Provided" : "Not Provided"));


  _FIID_OBJ_GET (cmd_rs,
		 "fru_info_device_address",
		 &val);
  fprintf (stdout, "FRU Info Device Address : %Xh %s\n",
	   (unsigned char) val,
	   (val ? "" : "(Unspecified)"));


  _FIID_OBJ_GET (cmd_rs,
		 "sdr_device_address",
		 &val);
  fprintf (stdout, "SDR Device Address      : %Xh\n",
	   (unsigned char) val);


  _FIID_OBJ_GET (cmd_rs,
		 "sel_device_address",
		 &val);
  fprintf (stdout, "SEL Device Address      : %Xh\n",
	   (unsigned char) val);


  _FIID_OBJ_GET (cmd_rs,
		 "system_management_device_address",
		 &val);
  fprintf (stdout, "Sys Mgmt Device Address : %Xh\n",
	   (unsigned char) val);

  if (fiid_obj_get (cmd_rs, "bridge_device_address", &val) >= 0) {
    fprintf (stdout, "Bridge Device Address   : %Xh\n",
	     (unsigned char) val);
  } else {
    fprintf (stdout, "Bridge Device Address   : 20h (assuming default)\n");
  }

  return 0;
}

int
run_cmd_args (ipmi_chassis_state_data_t *state_data)
{
  int rv = -1;

  assert(state_data);
  
  switch (state_data->prog_data->args->cmd) {
  case IPMI_CMD_GET_CHASSIS_CAPABILITIES:
    get_chassis_capabilities (state_data);
    break;
  case IPMI_CMD_GET_CHASSIS_STATUS:
    break;
  default:
    fprintf (stderr, "Error: No commands given\n");
    goto cleanup;
  }

  rv = 0;
 cleanup:
  return (rv);
}

static int
_ipmi_chassis (void *arg)
{
  ipmi_chassis_state_data_t state_data;
  ipmi_chassis_prog_data_t *prog_data;
  ipmi_device_t dev = NULL;
  int exit_code = -1;
  
  prog_data = (ipmi_chassis_prog_data_t *)arg;

  if (prog_data->args->common.host != NULL)
    {
      if (!(dev = ipmi_open_outofband (IPMI_DEVICE_LAN,
                                       prog_data->args->common.host,
                                       prog_data->args->common.username,
                                       prog_data->args->common.password,
                                       prog_data->args->common.authentication_type,
                                       prog_data->args->common.privilege_level,
                                       prog_data->args->common.session_timeout,
                                       prog_data->args->common.retry_timeout,
                                       prog_data->debug_flags)))
        {
          perror ("ipmi_open_outofband()");
          exit_code = EXIT_FAILURE;
          goto cleanup;
        }
    }
  else
    {
      if (!ipmi_is_root())
        {
          fprintf(stderr, "%s: Permission Denied\n", prog_data->progname);
          exit_code = EXIT_FAILURE;
          goto cleanup;
        }

      if (prog_data->args->common.driver_type == IPMI_DEVICE_UNKNOWN)
        {
          if (!(dev = ipmi_open_inband (IPMI_DEVICE_OPENIPMI,
                                        prog_data->args->common.disable_auto_probe,
                                        prog_data->args->common.driver_address,
                                        prog_data->args->common.register_spacing,
                                        prog_data->args->common.driver_device,
                                        prog_data->debug_flags)))
            {
              if (!(dev = ipmi_open_inband (IPMI_DEVICE_KCS,
                                            prog_data->args->common.disable_auto_probe,
                                            prog_data->args->common.driver_address,
                                            prog_data->args->common.register_spacing,
                                            prog_data->args->common.driver_device,
                                            prog_data->debug_flags)))
                {
                  if (!(dev = ipmi_open_inband (IPMI_DEVICE_SSIF,
                                                prog_data->args->common.disable_auto_probe,
                                                prog_data->args->common.driver_address,
                                                prog_data->args->common.register_spacing,
                                                prog_data->args->common.driver_device,
                                                prog_data->debug_flags)))
                    {
                      perror ("ipmi_open_inband()");
                      exit_code = EXIT_FAILURE;
                      goto cleanup;
                    }
                }
            }
        }
      else
        {
          if (!(dev = ipmi_open_inband (prog_data->args->common.driver_type,
                                        prog_data->args->common.disable_auto_probe,
                                        prog_data->args->common.driver_address,
                                        prog_data->args->common.register_spacing,
                                        prog_data->args->common.driver_device,
                                        prog_data->debug_flags)))
            {
              perror ("ipmi_open_inband()");
              exit_code = EXIT_FAILURE;
              goto cleanup;
            }
        }
    }

  memset(&state_data, '\0', sizeof(ipmi_chassis_state_data_t));
  state_data.dev = dev;
  state_data.prog_data = prog_data;

  if (run_cmd_args (&state_data) < 0)
    {
      exit_code = EXIT_FAILURE;
      goto cleanup;
    }
 
  exit_code = 0;
 cleanup:
  if (dev)
    ipmi_close_device (dev);
  return exit_code;
}

int 
main (int argc, char **argv)
{
  ipmi_chassis_prog_data_t prog_data;
  struct ipmi_chassis_arguments cmd_args;
  int exit_code;
#ifdef NDEBUG
  int i;
#endif /* NDEBUG */
  
  ipmi_disable_coredump();
  
  prog_data.progname = argv[0];
  ipmi_chassis_argp_parse (argc, argv, &cmd_args);
  prog_data.args = &cmd_args;

#ifndef NDEBUG
  if (prog_data.args->common.debug)
    prog_data.debug_flags = IPMI_FLAGS_DEBUG_DUMP;
  else
    prog_data.debug_flags = IPMI_FLAGS_DEFAULT;
#else  /* NDEBUG */
  prog_data.debug_flags = IPMI_FLAGS_DEFAULT;
#endif /* NDEBUG */
  
  exit_code = _ipmi_chassis (&prog_data);
  
  return (exit_code);
}
