/*
 * Copyright (C) 2008-2012 FreeIPMI Core Team
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
#include <assert.h>

#include "ipmi-sensors-config.h"
#include "ipmi-sensors-config-argp.h"
#include "ipmi-sensors-config-sections.h"

#include "freeipmi-portability.h"
#include "pstdout.h"
#include "tool-common.h"
#include "tool-cmdline-common.h"
#include "tool-hostrange-common.h"
#include "tool-sdr-cache-common.h"

static int
_ipmi_sensors_config (pstdout_state_t pstate,
                      const char *hostname,
                      void *arg)
{
  ipmi_sensors_config_state_data_t state_data;
  ipmi_sensors_config_prog_data_t *prog_data;
  char errmsg[IPMI_OPEN_ERRMSGLEN];
  int exit_code = -1;
  config_err_t ret = 0;
  int file_opened = 0;
  FILE *fp = NULL;              /* init NULL to remove warnings */

  prog_data = (ipmi_sensors_config_prog_data_t *) arg;

  memset (&state_data, '\0', sizeof (ipmi_sensors_config_state_data_t));
  state_data.prog_data = prog_data;
  state_data.pstate = pstate;

  /* Special case, just flush, don't do an IPMI connection */
  if (!prog_data->args->sdr.flush_cache)
    {
      if (!(state_data.ipmi_ctx = ipmi_open (prog_data->progname,
                                             hostname,
                                             &(prog_data->args->config_args.common),
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

  if (state_data.prog_data->args->config_args.common.debug)
    {
      /* Don't error out, if this fails we can still continue */
      if (ipmi_sdr_cache_ctx_set_flags (state_data.sdr_cache_ctx,
                                        IPMI_SDR_CACHE_FLAGS_DEBUG_DUMP) < 0)
        pstdout_fprintf (pstate,
                         stderr,
                         "ipmi_sdr_cache_ctx_set_flags: %s\n",
                         ipmi_sdr_cache_ctx_errormsg (state_data.sdr_cache_ctx));

      if (hostname)
        {
          if (ipmi_sdr_cache_ctx_set_debug_prefix (state_data.sdr_cache_ctx,
                                                   hostname) < 0)
            pstdout_fprintf (pstate,
                             stderr,
                             "ipmi_sdr_cache_ctx_set_debug_prefix: %s\n",
                             ipmi_sdr_cache_ctx_errormsg (state_data.sdr_cache_ctx));
        }
    }

  if (!(state_data.sdr_parse_ctx = ipmi_sdr_parse_ctx_create ()))
    {
      pstdout_perror (pstate, "ipmi_sdr_parse_ctx_create()");
      exit_code = EXIT_FAILURE;
      goto cleanup;
    }

  if (prog_data->args->sdr.flush_cache)
    {
      if (sdr_cache_flush_cache (state_data.sdr_cache_ctx,
                                 NULL,
                                 state_data.prog_data->args->sdr.quiet_cache,
                                 hostname,
                                 state_data.prog_data->args->sdr.sdr_cache_directory,
				 state_data.prog_data->args->sdr.sdr_cache_file) < 0)
        {
          exit_code = EXIT_FAILURE;
          goto cleanup;
        }
      exit_code = 0;
      goto cleanup;
    }

  if (sdr_cache_create_and_load (state_data.sdr_cache_ctx,
                                 NULL,
                                 state_data.ipmi_ctx,
                                 prog_data->args->sdr.quiet_cache,
                                 prog_data->args->sdr.sdr_cache_recreate,
                                 hostname,
                                 prog_data->args->sdr.sdr_cache_directory,
                                 prog_data->args->sdr.sdr_cache_file) < 0)
    {
      exit_code = EXIT_FAILURE;
      goto cleanup;
    }

  if (!(state_data.sections = ipmi_sensors_config_sections_create (&state_data)))
    {
      exit_code = EXIT_FAILURE;
      goto cleanup;
    }

  if (prog_data->args->config_args.action == CONFIG_ACTION_CHECKOUT)
    {
      if (prog_data->args->config_args.filename)
        {
          if (prog_data->hosts_count > 1)
            {
              pstdout_fprintf (pstate,
                               stderr,
                               "Cannot output multiple host checkout into a single file\n");
              exit_code = EXIT_FAILURE;
              goto cleanup;
            }

          if (!(fp = fopen (prog_data->args->config_args.filename, "w")))
            {
              pstdout_perror (pstate, "fopen");
              exit_code = EXIT_FAILURE;
              goto cleanup;
            }
          file_opened++;
        }
      else
        fp = stdout;
    }
  else if (prog_data->args->config_args.action == CONFIG_ACTION_COMMIT
           || prog_data->args->config_args.action == CONFIG_ACTION_DIFF)
    {
      if (prog_data->args->config_args.filename
          && strcmp (prog_data->args->config_args.filename, "-"))
        {
          if (!(fp = fopen (prog_data->args->config_args.filename, "r")))
            {
              pstdout_perror (pstate, "fopen");
              exit_code = EXIT_FAILURE;
              goto cleanup;
            }
          file_opened++;
        }
      else
        fp = stdin;
    }

  /* parse if there is an input file or no pairs at all */
  if ((prog_data->args->config_args.action == CONFIG_ACTION_COMMIT
       && prog_data->args->config_args.filename)
      || (prog_data->args->config_args.action == CONFIG_ACTION_COMMIT
          && !prog_data->args->config_args.filename
          && !prog_data->args->config_args.keypairs)
      || (prog_data->args->config_args.action == CONFIG_ACTION_DIFF
          && prog_data->args->config_args.filename)
      || (prog_data->args->config_args.action == CONFIG_ACTION_DIFF
          && !prog_data->args->config_args.filename
          && !prog_data->args->config_args.keypairs))
    {
      if (config_parse (pstate,
                        state_data.sections,
                        &(prog_data->args->config_args),
                        fp) < 0)
        {
          /* errors printed in function call */
          exit_code = EXIT_FAILURE;
          goto cleanup;
        }
    }

  /* note: argp validation catches if user specified keypair and
     filename for a diff
  */
  if ((prog_data->args->config_args.action == CONFIG_ACTION_CHECKOUT
       || prog_data->args->config_args.action == CONFIG_ACTION_COMMIT
       || prog_data->args->config_args.action == CONFIG_ACTION_DIFF)
      && prog_data->args->config_args.keypairs)
    {
      if (config_sections_insert_keyvalues (pstate,
                                            state_data.sections,
                                            prog_data->args->config_args.keypairs) < 0)
        {
          /* errors printed in function call */
          exit_code = EXIT_FAILURE;
          goto cleanup;
        }
    }

  if (prog_data->args->config_args.action == CONFIG_ACTION_COMMIT
      || prog_data->args->config_args.action == CONFIG_ACTION_DIFF)
    {
      int num;

      if ((num = config_sections_validate_keyvalue_inputs (pstate,
                                                           state_data.sections,
                                                           &state_data)) < 0)
        {
          /* errors printed in function call */
          exit_code = EXIT_FAILURE;
          goto cleanup;
        }

      /* some errors found */
      if (num)
        {
          exit_code = EXIT_FAILURE;
          goto cleanup;
        }
    }

  if (prog_data->args->config_args.action == CONFIG_ACTION_CHECKOUT
      && prog_data->args->config_args.section_strs)
    {
      struct config_section_str *sstr;

      sstr = prog_data->args->config_args.section_strs;
      while (sstr)
        {
          if (!config_find_section (state_data.sections,
                                    sstr->section_name))
            {
              pstdout_fprintf (pstate,
                               stderr,
                               "Unknown section `%s'\n",
                               sstr->section_name);
              exit_code = EXIT_FAILURE;
              goto cleanup;
            }
          sstr = sstr->next;
        }
    }

  switch (prog_data->args->config_args.action)
    {
    case CONFIG_ACTION_CHECKOUT:
      if (prog_data->args->config_args.section_strs)
	{
	  struct config_section_str *sstr;

	  /* note: argp validation catches if user specified --section
	   * and --keypair, so all_keys_if_none_specified should be '1'.
	   */

	  sstr = prog_data->args->config_args.section_strs;
	  while (sstr)
	    {
	      struct config_section *s;
	      config_err_t this_ret;

	      if (!(s = config_find_section (state_data.sections,
					     sstr->section_name)))
		{
		  pstdout_fprintf (pstate,
				   stderr,
				   "## FATAL: Cannot checkout section '%s'\n",
				   sstr->section_name);
		  continue;
		}

	      this_ret = config_checkout_section (pstate,
						  s,
						  &(prog_data->args->config_args),
						  1,
						  fp,
						  75,
						  &state_data);
	      if (this_ret != CONFIG_ERR_SUCCESS)
		ret = this_ret;
	      if (ret == CONFIG_ERR_FATAL_ERROR)
		break;

	      sstr = sstr->next;
	    }
	}
      else
	{
	  int all_keys_if_none_specified = 0;

	  if (!prog_data->args->config_args.keypairs)
	    all_keys_if_none_specified++;

	  ret = config_checkout (pstate,
				 state_data.sections,
				 &(prog_data->args->config_args),
				 all_keys_if_none_specified,
				 fp,
				 75,
				 &state_data);
	}
      break;
    case CONFIG_ACTION_COMMIT:
      ret = config_commit (pstate,
			   state_data.sections,
			   &(prog_data->args->config_args),
			   &state_data);
      break;
    case CONFIG_ACTION_DIFF:
      ret = config_diff (pstate,
			 state_data.sections,
			 &(prog_data->args->config_args),
			 &state_data);
      break;
    case CONFIG_ACTION_LIST_SECTIONS:
      ret = config_output_sections_list (pstate, state_data.sections);
      break;
    }

  if (ret == CONFIG_ERR_FATAL_ERROR || ret == CONFIG_ERR_NON_FATAL_ERROR)
    {
      exit_code = EXIT_FAILURE;
      goto cleanup;
    }

  exit_code = 0;
 cleanup:
  ipmi_sdr_cache_ctx_destroy (state_data.sdr_cache_ctx);
  ipmi_ctx_close (state_data.ipmi_ctx);
  ipmi_ctx_destroy (state_data.ipmi_ctx);
  config_sections_destroy (state_data.sections);
  if (file_opened)
    fclose (fp);
  return (exit_code);
}

int
main (int argc, char **argv)
{
  ipmi_sensors_config_prog_data_t prog_data;
  struct ipmi_sensors_config_arguments cmd_args;
  int exit_code;
  int hosts_count;
  int rv;

  ipmi_disable_coredump ();

  memset (&prog_data, '\0', sizeof (ipmi_sensors_config_prog_data_t));
  prog_data.progname = argv[0];
  ipmi_sensors_config_argp_parse (argc, argv, &cmd_args);

  prog_data.args = &cmd_args;

  if ((hosts_count = pstdout_setup (&(prog_data.args->config_args.common.hostname),
                                    prog_data.args->config_args.hostrange.buffer_output,
                                    prog_data.args->config_args.hostrange.consolidate_output,
                                    prog_data.args->config_args.hostrange.fanout,
                                    prog_data.args->config_args.hostrange.eliminate,
                                    prog_data.args->config_args.hostrange.always_prefix)) < 0)
    {
      exit_code = EXIT_FAILURE;
      goto cleanup;
    }

  if (!hosts_count)
    {
      exit_code = EXIT_SUCCESS;
      goto cleanup;
    }

  /* We don't want caching info to output when are doing ranged output */
  if (hosts_count > 1)
    prog_data.args->sdr.quiet_cache = 1;

  prog_data.hosts_count = hosts_count;

  if ((rv = pstdout_launch (prog_data.args->config_args.common.hostname,
                            _ipmi_sensors_config,
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

