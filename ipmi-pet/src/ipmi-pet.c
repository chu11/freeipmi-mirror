/*
 * Copyright (C) 2011 FreeIPMI Core Team
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
#if HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */
#include <assert.h>
#include <errno.h>

#include <freeipmi/freeipmi.h>

#include "ipmi-pet.h"
#include "ipmi-pet-argp.h"

#include "freeipmi-portability.h"
#include "tool-common.h"
#include "tool-cmdline-common.h"
#include "tool-hostrange-common.h"
#include "tool-oem-common.h"
#include "tool-sdr-cache-common.h"
#include "tool-sensor-common.h"

static int
_flush_cache (ipmi_pet_state_data_t *state_data)
{
  assert (state_data);
  
  if (sdr_cache_flush_cache (state_data->sdr_cache_ctx,
                             NULL,
                             state_data->prog_data->args->sdr.quiet_cache,
                             state_data->hostname,
                             state_data->prog_data->args->sdr.sdr_cache_directory) < 0)
    return (-1);
  
  return (0);
}

static int
run_cmd_args (ipmi_pet_state_data_t *state_data)
{
  struct ipmi_pet_arguments *args;
  FILE *infile = NULL;
  int rv = -1;

  assert (state_data);
  
  args = state_data->prog_data->args;
  
  if (args->sdr.flush_cache)
    return (_flush_cache (state_data));
  
  if (args->cmd_length)
    {
#if 0
      if (ipmi_pet_cmdline (state_data) < 0)
        goto cleanup;
#endif
      return (0);
    }

  if (args->cmd_file)
    {
      if (!(infile = fopen (args->cmd_file, "r")))
        {
          perror ("fopen()");
          goto cleanup;
        }
    }
  else
    infile = stdin;

#if 0
  if (ipmi_pet_stream (state_data, infile) < 0)
    goto cleanup;
#endif

  rv = 0;
 cleanup:
  if (infile && infile != stdin)
    fclose (infile);
  return (rv);
}

static int
_ipmi_pet (ipmi_pet_prog_data_t *prog_data)
{
  ipmi_pet_state_data_t state_data;
  char errmsg[IPMI_OPEN_ERRMSGLEN];
  int exit_code = -1;

  memset (&state_data, '\0', sizeof (ipmi_pet_state_data_t));
  state_data.prog_data = prog_data;
  state_data.hostname = prog_data->args->common.hostname;

  /* Special case, just flush, don't do an IPMI connection */
  if (!prog_data->args->sdr.flush_cache)
    {
      if (!(state_data.ipmi_ctx = ipmi_open (prog_data->progname,
                                             prog_data->args->common.hostname,
                                             &(prog_data->args->common),
                                             errmsg,
                                             IPMI_OPEN_ERRMSGLEN)))
        {
          fprintf (stderr,
		   "%s\n",
		   errmsg);
          exit_code = EXIT_FAILURE;
          goto cleanup;
        }
    }

  if (!(state_data.sdr_cache_ctx = ipmi_sdr_cache_ctx_create ()))
    {
      perror ("ipmi_sdr_cache_ctx_create()");
      exit_code = EXIT_FAILURE;
      goto cleanup;
    }

  if (state_data.prog_data->args->common.debug)
    {
      /* Don't error out, if this fails we can still continue */
      if (ipmi_sdr_cache_ctx_set_flags (state_data.sdr_cache_ctx,
                                        IPMI_SDR_CACHE_FLAGS_DEBUG_DUMP) < 0)
        fprintf (stderr,
		 "ipmi_sdr_cache_ctx_set_flags: %s\n",
		 ipmi_sdr_cache_ctx_errormsg (state_data.sdr_cache_ctx));

      if (prog_data->args->common.hostname)
        {
          if (ipmi_sdr_cache_ctx_set_debug_prefix (state_data.sdr_cache_ctx,
                                                   prog_data->args->common.hostname) < 0)
            fprintf (stderr,
		     "ipmi_sdr_cache_ctx_set_debug_prefix: %s\n",
		     ipmi_sdr_cache_ctx_errormsg (state_data.sdr_cache_ctx));
        }
    }

  if (!(state_data.sdr_parse_ctx = ipmi_sdr_parse_ctx_create ()))
    {
      perror ("ipmi_sdr_parse_ctx_create()");
      exit_code = EXIT_FAILURE;
      goto cleanup;
    }

  if (prog_data->args->output_event_state)
    {
      unsigned int flags = 0;

      if (!(state_data.interpret_ctx = ipmi_interpret_ctx_create ()))
        {
          perror ("ipmi_interpret_ctx_create()");
          exit_code = EXIT_FAILURE;
          goto cleanup;
        }

      if (prog_data->args->event_state_config_file)
        {
          if (ipmi_interpret_load_sel_config (state_data.interpret_ctx,
                                              prog_data->args->event_state_config_file) < 0)
            {
              if (ipmi_interpret_ctx_errnum (state_data.interpret_ctx) == IPMI_INTERPRET_ERR_SEL_CONFIG_FILE_DOES_NOT_EXIST)
                fprintf (stderr,
			 "event state config file '%s' does not exist\n",
			 prog_data->args->event_state_config_file);
              else if (ipmi_interpret_ctx_errnum (state_data.interpret_ctx) == IPMI_INTERPRET_ERR_SEL_CONFIG_FILE_PARSE)
                fprintf (stderr,
			 "event state config file '%s' parse error\n",
			 prog_data->args->event_state_config_file);
              else
                fprintf (stderr,
			 "ipmi_interpret_load_sel_config: %s\n",
			 ipmi_interpret_ctx_errormsg (state_data.interpret_ctx));
              exit_code = EXIT_FAILURE;
              goto cleanup;
            }
        }
      else
        {
          if (ipmi_interpret_load_sel_config (state_data.interpret_ctx, NULL) < 0)
            {
              if (ipmi_interpret_ctx_errnum (state_data.interpret_ctx) == IPMI_INTERPRET_ERR_SEL_CONFIG_FILE_PARSE)
                fprintf (stderr,
			 "event state config file parse error\n");
              else
                fprintf (stderr,
			 "ipmi_interpret_load_sel_config: %s\n",
			 ipmi_interpret_ctx_errormsg (state_data.interpret_ctx));
              exit_code = EXIT_FAILURE;
              goto cleanup;
            }
        }

      if (prog_data->args->interpret_oem_data)
        flags |= IPMI_INTERPRET_FLAGS_INTERPRET_OEM_DATA;

      if (flags)
        {
          if (ipmi_interpret_ctx_set_flags (state_data.interpret_ctx, flags) < 0)
            {
              fprintf (stderr,
		       "ipmi_interpret_ctx_set_flags: %s\n",
		       ipmi_interpret_ctx_errormsg (state_data.interpret_ctx));
              exit_code = EXIT_FAILURE;
              goto cleanup;
            }
        }
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
  ipmi_pet_prog_data_t prog_data;
  struct ipmi_pet_arguments cmd_args;

  ipmi_disable_coredump ();

  memset (&prog_data, '\0', sizeof (ipmi_pet_prog_data_t));
  prog_data.progname = argv[0];
  ipmi_pet_argp_parse (argc, argv, &cmd_args);
  prog_data.args = &cmd_args;

  return (_ipmi_pet (&prog_data));
}

