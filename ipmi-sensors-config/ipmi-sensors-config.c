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
#include "tool-util-common.h"

static int
_ipmi_sensors_config (pstdout_state_t pstate,
                      const char *hostname,
                      void *arg)
{
  ipmi_sensors_config_state_data_t state_data;
  ipmi_sensors_config_prog_data_t *prog_data;
  int exit_code = EXIT_FAILURE;
  config_err_t ret = 0;
  int file_opened = 0;
  FILE *fp = NULL;              /* init NULL to remove warnings */

  assert (pstate);
  assert (arg);

  prog_data = (ipmi_sensors_config_prog_data_t *) arg;

  if (prog_data->args->config_args.common_args.flush_cache)
    {
      if (sdr_cache_flush_cache (pstate,
                                 hostname,
				 &prog_data->args->config_args.common_args) < 0)
	return (EXIT_FAILURE);
      return (EXIT_SUCCESS);
    }

  memset (&state_data, '\0', sizeof (ipmi_sensors_config_state_data_t));
  state_data.prog_data = prog_data;
  state_data.pstate = pstate;

  if (!(state_data.ipmi_ctx = ipmi_open (prog_data->progname,
					 hostname,
					 &(prog_data->args->config_args.common_args),
					 state_data.pstate)))
    goto cleanup;

  if (!(state_data.sdr_ctx = ipmi_sdr_ctx_create ()))
    {
      pstdout_perror (pstate, "ipmi_sdr_ctx_create()");
      goto cleanup;
    }

  if (sdr_cache_create_and_load (state_data.sdr_ctx,
                                 NULL,
                                 state_data.ipmi_ctx,
                                 hostname,
				 &state_data.prog_data->args->config_args.common_args) < 0)
    goto cleanup;

  if (!(state_data.sections = ipmi_sensors_config_sections_create (&state_data)))
    goto cleanup;

  if (prog_data->args->config_args.action == CONFIG_ACTION_CHECKOUT)
    {
      if (prog_data->args->config_args.filename)
        {
          if (prog_data->hosts_count > 1)
            {
              pstdout_fprintf (pstate,
                               stderr,
                               "Cannot output multiple host checkout into a single file\n");
              goto cleanup;
            }

          if (!(fp = fopen (prog_data->args->config_args.filename, "w")))
            {
              pstdout_perror (pstate, "fopen");
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
	goto cleanup;

      /* some errors found */
      if (num)
	goto cleanup;
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
    goto cleanup;

  exit_code = EXIT_SUCCESS;
 cleanup:
  ipmi_sdr_ctx_destroy (state_data.sdr_ctx);
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
  int hosts_count;
  int rv;

  ipmi_disable_coredump ();

  memset (&prog_data, '\0', sizeof (ipmi_sensors_config_prog_data_t));
  prog_data.progname = argv[0];
  ipmi_sensors_config_argp_parse (argc, argv, &cmd_args);

  prog_data.args = &cmd_args;

  if ((hosts_count = pstdout_setup (&(prog_data.args->config_args.common_args.hostname),
				    &(prog_data.args->config_args.common_args))) < 0)
    return (EXIT_FAILURE);

  if (!hosts_count)
    return (EXIT_SUCCESS);

  /* We don't want caching info to output when are doing ranged output */
  if (hosts_count > 1)
    prog_data.args->config_args.common_args.quiet_cache = 1;

  prog_data.hosts_count = hosts_count;

  if ((rv = pstdout_launch (prog_data.args->config_args.common_args.hostname,
                            _ipmi_sensors_config,
                            &prog_data)) < 0)
    {
      fprintf (stderr,
               "pstdout_launch: %s\n",
               pstdout_strerror (pstdout_errnum));
      return (EXIT_FAILURE);
    }

  return (rv);
}

