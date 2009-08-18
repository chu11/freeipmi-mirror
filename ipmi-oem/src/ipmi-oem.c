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
#endif /* STDC_HEADERS */
#include <assert.h>

#include <freeipmi/freeipmi.h>

#include "ipmi-oem.h"
#include "ipmi-oem-argp.h"
#include "ipmi-oem-common.h"
#include "ipmi-oem-dell.h"
#include "ipmi-oem-inventec.h"
#include "ipmi-oem-supermicro.h"

#include "freeipmi-portability.h"
#include "pstdout.h"
#include "tool-common.h"
#include "tool-cmdline-common.h"
#include "tool-sdr-cache-common.h"
#include "tool-hostrange-common.h"

typedef int (*oem_callback)(ipmi_oem_state_data_t *);

struct ipmi_oem_command
{
  char *oem_command;
  char *command_options;
  int required_oem_options;
  oem_callback func;
};

struct ipmi_oem_id
{
  char *oem_id;
  struct ipmi_oem_command *oem_commands;
};

struct ipmi_oem_command oem_dell[] =
  {
    {
      "get-system-info",
      "<asset-tag|service-tag|product-name|mac-addresses>",
      1,
      ipmi_oem_dell_get_system_info
    },
    {
      "get-nic-selection",
      NULL,
      0,
      ipmi_oem_dell_get_nic_selection
    },
    {
      "set-nic-selection",
      "<dedicated|shared|shared_failover_nic2|shared_failover_all>",
      1,
      ipmi_oem_dell_set_nic_selection
    },
    {
      "get-ssh-config",
      NULL,
      0,
      ipmi_oem_dell_get_ssh_config
    },
    {
      "set-ssh-config",
      "KEY=VALUE ...",
      1,
      ipmi_oem_dell_set_ssh_config
    },
    {
      "get-telnet-config",
      NULL,
      0,
      ipmi_oem_dell_get_telnet_config
    },
    {
      "set-telnet-config",
      "KEY=VALUE ...",
      1,
      ipmi_oem_dell_set_telnet_config
    },
    {
      "get-web-server-config",
      NULL,
      0,
      ipmi_oem_dell_get_web_server_config
    },
    {
      "set-web-server-config",
      "KEY=VALUE ...",
      1,
      ipmi_oem_dell_set_web_server_config
    },
    {
      "reset-to-defaults",
      NULL,
      0,
      ipmi_oem_dell_reset_to_defaults
    },
    {
      "get-power-info",
      NULL,
      0,
      ipmi_oem_dell_get_power_info
    },
    {
      "reset-power-info",
      "<cumulative|peak>",
      1,
      ipmi_oem_dell_reset_power_info
    },
    {
      "get-power-supply-info",
      NULL,
      0,
      ipmi_oem_dell_get_power_supply_info
    },
    {
      "get-instantaneous-power-consumption-info",
      NULL,
      0,
      ipmi_oem_dell_get_instantaneous_power_consumption_info
    },
    {
      "get-power-headroom-info",
      NULL,
      0,
      ipmi_oem_dell_get_power_headroom_info
    },
    {
      "get-average-power-history",
      NULL,
      0,
      ipmi_oem_dell_get_average_power_history
    },
    {
      "get-peak-power-history",
      NULL,
      0,
      ipmi_oem_dell_get_peak_power_history
    },
    {
      "get-power-capacity",
      NULL,
      0,
      ipmi_oem_dell_get_power_capacity
    },
    {
      "set-power-capacity",
      "<power-capacity>",
      1,
      ipmi_oem_dell_set_power_capacity
    },
    {
      "get-power-capacity-status",
      NULL,
      0,
      ipmi_oem_dell_get_power_capacity_status
    },
    {
      "set-power-capacity-status",
      "<enable|disable>",
      1,
      ipmi_oem_dell_set_power_capacity_status
    },
    {
      "get-fcb-version",
      NULL,
      0,
      ipmi_oem_dell_get_fcb_version
    },
#if 0
    /* cannot verify */
    {
      "get-dhcp-retry",
      NULL,
      0,
      ipmi_oem_dell_get_dhcp_retry
    },
    {
      "set-dhcp-retry",
      "<retry-count> <retry-interval> <retry-timeout>",
      3,
      ipmi_oem_dell_set_dhcp_retry
    },
#endif
    {
      "get-sol-inactivity-timeout",
      NULL,
      0,
      ipmi_oem_dell_get_sol_inactivity_timeout
    },
    {
      "set-sol-inactivity-timeout",
      "<inactivity-timeout>",
      1,
      ipmi_oem_dell_set_sol_inactivity_timeout
    },
    {
      NULL,
      NULL,
      0,
      NULL
    },
  };

struct ipmi_oem_command oem_inventec[] =
  {
    {
      "get-nic-status",
      NULL,
      0,
      ipmi_oem_inventec_get_nic_status
    },
    {
      "set-nic-status",
      "<dedicated|shared>",
      1,
      ipmi_oem_inventec_set_nic_status
    },
    {
      "get-mac-address",
      NULL,
      0,
      ipmi_oem_inventec_get_mac_address
    },
    {
      "set-mac-address",
      "<dedicated|shared> <MACADDRESS>",
      2,
      ipmi_oem_inventec_set_mac_address
    },
    {
      "get-bmc-services",
      NULL,
      0,
      ipmi_oem_inventec_get_bmc_services
    },
    {
      "set-bmc-services",
      "<enable|disable> <all|kvm|http|ssh>",
      2,
      ipmi_oem_inventec_set_bmc_services
    },
    {
      NULL,
      NULL,
      0,
      NULL
    },
  };

struct ipmi_oem_command oem_supermicro[] =
  {
    {
      "reset-intrusion",
      NULL,
      0,
      ipmi_oem_supermicro_reset_intrusion
    },
    {
      NULL,
      NULL,
      0,
      NULL
    },
  };

struct ipmi_oem_id oem_cb[] =
  {
    {
      "Dell",
      oem_dell
    },
    {
      "Inventec",
      oem_inventec
    },
    {
      "Supermicro",
      oem_supermicro
    },
    {
      NULL,
      NULL
    },
  };

static int
_list (void)
{
  struct ipmi_oem_id *oem_id = oem_cb;

  while (oem_id && oem_id->oem_id)
    {
      struct ipmi_oem_command *oem_cmd = oem_id->oem_commands;

      printf ("OEM ID: %s\n", oem_id->oem_id);

      while (oem_cmd && oem_cmd->oem_command)
        {
          if (oem_cmd->command_options)
            printf ("    Command: %s %s\n",
                    oem_cmd->oem_command,
                    oem_cmd->command_options);
          else
            printf ("    Command: %s\n",
                    oem_cmd->oem_command);
          oem_cmd++;
        }

      printf ("\n");
      oem_id++;
    }

  return (0);
}

static int
_flush_cache (ipmi_oem_state_data_t *state_data)
{
  assert (state_data);
  
  if (sdr_cache_flush_cache (state_data->sdr_cache_ctx,
                             state_data->pstate,
                             state_data->prog_data->args->sdr.quiet_cache,
                             state_data->hostname,
                             state_data->prog_data->args->sdr.sdr_cache_directory) < 0)
    return (-1);
  
  return (0);
}

static int
_run_oem_cmd (ipmi_oem_state_data_t *state_data)
{
  struct ipmi_oem_arguments *args;
  struct ipmi_oem_id *oem_id = oem_cb;
  int id_found = 0;
  int rv = -1;

  assert (state_data);

  args = state_data->prog_data->args;

  while (oem_id && oem_id->oem_id)
    {
      if (!strcasecmp (oem_id->oem_id, args->oem_id))
        {
          struct ipmi_oem_command *oem_cmd = oem_id->oem_commands;
          int cmd_found = 0;

          id_found++;

          if (!strcasecmp (args->oem_command, "list"))
            {
              while (oem_cmd && oem_cmd->oem_command)
                {
                  if (oem_cmd->command_options)
                    printf ("%s Command: %s %s\n",
                            oem_id->oem_id,
                            oem_cmd->oem_command,
                            oem_cmd->command_options);
                  else
                    printf ("%s Command: %s\n",
                            oem_id->oem_id,
                            oem_cmd->oem_command);
                  oem_cmd++;
                }

              break;
            }

          while (oem_cmd && oem_cmd->oem_command)
            {
              if (!strcasecmp (oem_cmd->oem_command,
                               args->oem_command))
                {
                  cmd_found++;

                  if (state_data->prog_data->args->oem_options_count < oem_cmd->required_oem_options)
                    {
                      pstdout_fprintf (state_data->pstate,
                                       stderr,
                                       "%s:%s invalid number of OEM option arguments\n",
                                       state_data->prog_data->args->oem_id,
                                       state_data->prog_data->args->oem_command);
                      goto cleanup;
                    }
                  

                  if (((*oem_cmd->func)(state_data)) < 0)
                    goto cleanup;

                  break;
                }

              oem_cmd++;
            }

          if (!cmd_found)
            {
              pstdout_fprintf (state_data->pstate,
                               stderr,
                               "OEM Command '%s' unknown\n",
                               args->oem_command);
              goto cleanup;
            }

          break;
        }

      oem_id++;
    }

  if (!id_found)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "OEM Id '%s' unknown\n",
                       args->oem_id);
      goto cleanup;
    }

  rv = 0;
 cleanup:
  return (rv);
}

static int
run_cmd_args (ipmi_oem_state_data_t *state_data)
{
  struct ipmi_oem_arguments *args;
  int rv = -1;

  assert (state_data);

  args = state_data->prog_data->args;

  /* shouldn't be possible at this point, make sure we've already
   * exitted
   */
  assert (!args->list);

  if (args->sdr.flush_cache)
    return (_flush_cache (state_data));

  if (!args->oem_id)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "OEM Id not specified\n");
      goto cleanup;
    }

  /* Special case, just output list, don't do anything else */
  if (!strcasecmp (args->oem_id, "list"))
    {
      if (_list () < 0)
        goto cleanup;
      goto out;
    }

  if (!args->oem_command)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "OEM Command not specified\n");
      goto cleanup;
    }

  if (_run_oem_cmd (state_data) < 0)
    goto cleanup;

 out:
  rv = 0;
 cleanup:
  return (rv);
}

static int
_ipmi_oem (pstdout_state_t pstate,
           const char *hostname,
           void *arg)
{
  ipmi_oem_state_data_t state_data;
  ipmi_oem_prog_data_t *prog_data;
  char errmsg[IPMI_OPEN_ERRMSGLEN];
  int exit_code = -1;

  prog_data = (ipmi_oem_prog_data_t *)arg;
  memset (&state_data, '\0', sizeof (ipmi_oem_state_data_t));

  state_data.prog_data = prog_data;
  state_data.pstate = pstate;
  state_data.hostname = (char *)hostname;

  /* Special case, just flush, don't do an IPMI connection */
  if (!prog_data->args->sdr.flush_cache)
    {
      if (!(state_data.ipmi_ctx = ipmi_open (prog_data->progname,
					     hostname,
					     &(prog_data->args->common),
					     errmsg,
					     IPMI_OPEN_ERRMSGLEN)))
	{
	  pstdout_fprintf (pstate,
			   stderr,
			   "%s\n",
			   errmsg);
	  exit_code = EXIT_FAILURE;
	  goto cleanup;
	}
    }

  if (!(state_data.sdr_cache_ctx = ipmi_sdr_cache_ctx_create ()))
    {
      pstdout_perror (pstate, "ipmi_sdr_cache_ctx_create()");
      exit_code = EXIT_FAILURE;
      goto cleanup;
    }

  if (state_data.prog_data->args->common.debug)
    {
      /* Don't error out, if this fails we can still continue */
      if (ipmi_sdr_cache_ctx_set_flags (state_data.sdr_cache_ctx,
                                        IPMI_SDR_CACHE_FLAGS_DEBUG_DUMP) < 0)
        pstdout_fprintf (pstate,
                         stderr,
                         "ipmi_sdr_cache_ctx_set_flags: %s\n",
                         ipmi_sdr_cache_ctx_strerror (ipmi_sdr_cache_ctx_errnum (state_data.sdr_cache_ctx)));
      
      if (hostname)
        {
          if (ipmi_sdr_cache_ctx_set_debug_prefix (state_data.sdr_cache_ctx,
                                                   hostname) < 0)
            pstdout_fprintf (pstate,
                             stderr,
                             "ipmi_sdr_cache_ctx_set_debug_prefix: %s\n",
                             ipmi_sdr_cache_ctx_strerror (ipmi_sdr_cache_ctx_errnum (state_data.sdr_cache_ctx)));
        }
    }
  
  if (!(state_data.sdr_parse_ctx = ipmi_sdr_parse_ctx_create ()))
    {
      pstdout_perror (pstate, "ipmi_sdr_parse_ctx_create()");
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
  if (state_data.sdr_cache_ctx)
    ipmi_sdr_cache_ctx_destroy (state_data.sdr_cache_ctx);
  if (state_data.sdr_parse_ctx)
    ipmi_sdr_parse_ctx_destroy (state_data.sdr_parse_ctx);
  if (state_data.ipmi_ctx)
    {
      ipmi_ctx_close (state_data.ipmi_ctx);
      ipmi_ctx_destroy (state_data.ipmi_ctx);
    }
  return (exit_code);
}

int
main (int argc, char **argv)
{
  ipmi_oem_prog_data_t prog_data;
  struct ipmi_oem_arguments cmd_args;
  int exit_code;
  int rv;

  ipmi_disable_coredump ();

  memset (&prog_data, '\0', sizeof (ipmi_oem_prog_data_t));
  prog_data.progname = argv[0];
  ipmi_oem_argp_parse (argc, argv, &cmd_args);
  prog_data.args = &cmd_args;

  /* Special case, just output list, don't do anything else */
  if (cmd_args.list)
    {
      if (_list () < 0)
        {
          exit_code = EXIT_FAILURE;
          goto cleanup;
        }
      exit_code = EXIT_SUCCESS;
      goto cleanup;
    }

  if (pstdout_setup (&(prog_data.args->common.hostname),
                     prog_data.args->hostrange.buffer_output,
                     prog_data.args->hostrange.consolidate_output,
                     prog_data.args->hostrange.fanout,
                     prog_data.args->hostrange.eliminate,
                     prog_data.args->hostrange.always_prefix) < 0)
    {
      exit_code = EXIT_FAILURE;
      goto cleanup;
    }

  if ((rv = pstdout_launch (prog_data.args->common.hostname,
                            _ipmi_oem,
                            &prog_data)) < 0)
    {
      fprintf (stderr,
               "pstdout_launch: %s\n",
               pstdout_strerror (pstdout_errnum));
      exit_code = EXIT_FAILURE;
      goto cleanup;
    }

  exit_code = rv;
 cleanup:
  return (exit_code);
}
