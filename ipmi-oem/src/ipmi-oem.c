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
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <limits.h>
#include <assert.h>

#include <freeipmi/freeipmi.h>

#include "ipmi-oem.h"
#include "ipmi-oem-argp.h"

#include "freeipmi-portability.h"
#include "pstdout.h"
#include "tool-common.h"
#include "tool-cmdline-common.h"
#include "tool-hostrange-common.h"

#define IPMI_OEM_MAX_BYTES   256
#define IPMI_OEM_ERR_BUFLEN 1024

typedef int (*oem_callback)(ipmi_oem_state_data_t *);

struct ipmi_oem_command
{
  char *oem_command;
  char *description;
  oem_callback func;
};

struct ipmi_oem_id
{
  char *oem_id;
  struct ipmi_oem_command *oem_commands;
};

static int _supermicro_reset_intrusion (ipmi_oem_state_data_t *);

struct ipmi_oem_command oem_supermicro[] =
  {
    {"reset-intrusion",
     "reset motherboard intrusion flag.",
     _supermicro_reset_intrusion},
    {NULL, NULL, NULL},
  };

struct ipmi_oem_id oem_cb[] =
  {
    {"supermicro", oem_supermicro},
    {NULL, NULL},
  };

static int
_list (ipmi_oem_state_data_t *state_data)
{
  struct ipmi_oem_id *oem_id = oem_cb;

  assert(state_data);

  while (oem_id && oem_id->oem_id)
    {
      struct ipmi_oem_command *oem_cmd = oem_cb->oem_commands;

      printf("OEM ID: %s\n", oem_id->oem_id);

      while (oem_cmd && oem_cmd->oem_command)
        {
          printf("    Command: %s - %s\n", 
                 oem_cmd->oem_command,
                 oem_cmd->description);
          oem_cmd++;
        }

      printf("\n");
      oem_id++;
    }

  return 0;
}

static int
_supermicro_reset_intrusion (ipmi_oem_state_data_t *state_data)
{
  uint8_t bytes_rq[IPMI_OEM_MAX_BYTES];
  uint8_t bytes_rs[IPMI_OEM_MAX_BYTES];
  int32_t rs_len;
  int rv = -1;
  
  assert(state_data);
  
  /* Supermicro OEM 
   *
   * 0x30 - OEM network function
   * 0x03 - OEM cmd
   */

  bytes_rq[0] = 0x03;

  if ((rs_len = ipmi_cmd_raw (state_data->ipmi_ctx,
                              0, /* lun */
                              0x30, /* network function */
                              bytes_rq, /* data */
                              1, /* num bytes */
                              bytes_rs,
                              IPMI_OEM_MAX_BYTES)) < 0)
    {
      pstdout_fprintf(state_data->pstate,
                      stderr,
                      "ipmi_cmd_raw: %s\n",
                      ipmi_ctx_strerror(ipmi_ctx_errnum(state_data->ipmi_ctx)));
      goto cleanup;
    }

  if (rs_len < 2)
    {
      pstdout_fprintf(state_data->pstate,
                      stderr,
                      "reset-intrusion invalid response length: %d\n",
                      rs_len);
      goto cleanup;
    }

  if (bytes_rs[1] != IPMI_COMP_CODE_COMMAND_SUCCESS)
    {
      char errbuf[IPMI_OEM_ERR_BUFLEN];

      memset(errbuf, '\0', IPMI_OEM_ERR_BUFLEN);
      if (ipmi_completion_code_strerror_r(0x3,
                                          0x30,
                                          bytes_rs[1],
                                          errbuf,
                                          IPMI_OEM_ERR_BUFLEN) < 0)
        {
          pstdout_perror(state_data->pstate, "ipmi_completion_code_strerror_r");
          snprintf(errbuf, "completion-code = 0x%X", bytes_rs[1]);
        }
      
      pstdout_fprintf(state_data->pstate,
                      stderr,
                      "reset-intrusion failed: %s\n",
                      errbuf);
      goto cleanup;
    }
  
  rv = 0;
 cleanup:
  return (rv);
}

static int
_run_oem_cmd (ipmi_oem_state_data_t *state_data)
{
  struct ipmi_oem_arguments *args;
  struct ipmi_oem_id *oem_id = oem_cb;
  int id_found = 0;
  int rv = -1;

  assert(state_data);

  args = state_data->prog_data->args;

  while (oem_id && oem_id->oem_id)
    {
      if (!strcasecmp(oem_id->oem_id, args->oem_id))
        {
          struct ipmi_oem_command *oem_cmd = oem_cb->oem_commands;
          int cmd_found = 0;
          
          id_found++;

          while (oem_cmd && oem_cmd->oem_command)
            {
              if (!strcasecmp(oem_cmd->oem_command, 
                              args->oem_command))
                {
                  cmd_found++;

                  if (((*oem_cmd->func)(state_data)) < 0)
                    goto cleanup;
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

  assert(state_data);

  args = state_data->prog_data->args;

  if (args->list)
    return _list (state_data);

  if (!args->oem_id)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "OEM Id not specified\n");
      goto cleanup;
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
  memset(&state_data, '\0', sizeof(ipmi_oem_state_data_t));

  state_data.prog_data = prog_data;
  state_data.pstate = pstate;

  /* Special case, just output list, don't do an IPMI connection */
  if (!prog_data->args->list)
    {
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
  ipmi_oem_prog_data_t prog_data;
  struct ipmi_oem_arguments cmd_args;
  int exit_code;
  int rv;

  ipmi_disable_coredump();

  memset(&prog_data, '\0', sizeof(ipmi_oem_prog_data_t));
  prog_data.progname = argv[0];
  ipmi_oem_argp_parse (argc, argv, &cmd_args);
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
                           _ipmi_oem,
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
