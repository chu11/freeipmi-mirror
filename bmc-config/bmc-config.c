/*
 * Copyright (C) 2003-2012 FreeIPMI Core Team
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
#include <errno.h>

#include "bmc-config.h"
#include "bmc-config-argp.h"
#include "bmc-config-sections.h"

#include "freeipmi-portability.h"
#include "pstdout.h"
#include "tool-common.h"
#include "tool-cmdline-common.h"
#include "tool-hostrange-common.h"
#include "tool-util-common.h"

static int
_bmc_config (pstdout_state_t pstate,
             const char *hostname,
             void *arg)
{
  bmc_config_state_data_t state_data;
  bmc_config_prog_data_t *prog_data;
  int exit_code = EXIT_FAILURE;
  config_err_t ret = 0;
  int file_opened = 0;
  FILE *fp = NULL;              /* init NULL to remove warnings */

  assert (pstate);
  assert (arg);

  prog_data = (bmc_config_prog_data_t *)arg;

  memset (&state_data, '\0', sizeof (bmc_config_state_data_t));
  state_data.prog_data = prog_data;
  state_data.pstate = pstate;

  if (!(state_data.ipmi_ctx = ipmi_open (prog_data->progname,
                                         hostname,
                                         &(prog_data->args->config_args.common),
					 state_data.pstate)))
    goto cleanup;

  if (!(state_data.sections = bmc_config_sections_create (&state_data)))
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
	goto cleanup;
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
	goto cleanup;
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

  /* Special case(s): 
   *
   * On some motherboards, the "Enable_User" must come after the
   * "Password" configure.  So we store information for this fact.
   * See workaround details in user section code.
   */
  if (prog_data->args->config_args.action == CONFIG_ACTION_COMMIT)
    {
      struct config_section *section;
      unsigned int user_count = 0;

      /* First, see how many user sections there are */
      section = state_data.sections;
      while (section)
        {
          if (stristr (section->section_name, "User"))
            user_count++;
          section = section->next;
        }

      if (user_count)
        {
          unsigned int enable_user_found = 0;
          unsigned int datasize;

          section = state_data.sections;
          while (section)
            {
              struct config_keyvalue *kv;

              if (stristr (section->section_name, "User"))
                {
                  uint8_t userid;

                  userid = atoi (section->section_name + strlen ("User"));

                  if (userid < user_count)
                    {
                      if ((kv = config_find_keyvalue (section,
                                                      "Enable_User")))
                        enable_user_found = 1;
                    }
                }

              section = section->next;
            }

          if (enable_user_found)
            {
              datasize = sizeof (bmc_config_enable_user_after_password_t) * user_count;
              
              if (!(state_data.enable_user_after_password = (bmc_config_enable_user_after_password_t *)malloc (datasize)))
                {
                  pstdout_perror (pstate, "malloc");
                  goto cleanup;
                }
              state_data.enable_user_after_password_len = user_count;
              memset (state_data.enable_user_after_password, '\0', datasize);
            }
        }
    }

  /* Special case: IP addresses and MAC addresses cannot be configured
   * in parallel.  Reject input if user attempts to configure the same
   * IP or MAC on multiple hosts.
   */
  if (prog_data->args->config_args.action == CONFIG_ACTION_COMMIT
      && prog_data->hosts_count > 1)
    {
      struct config_section *section;

      if ((section = config_find_section (state_data.sections,
                                          "Lan_Conf")))
        {
          if (config_find_keyvalue (section,
                                    "IP_Address"))
            {
              pstdout_fprintf (pstate,
                               stderr,
                               "Cannot configure Lan_Conf:IP_Address on multiple hosts\n");
              goto cleanup;
            }

          if (config_find_keyvalue (section,
                                    "MAC_Address"))
            {
              pstdout_fprintf (pstate,
                               stderr,
                               "Cannot configure Lan_Conf:MAC_Address on multiple hosts\n");
              goto cleanup;
            }
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
						  0,
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
				 0,
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
  ipmi_ctx_close (state_data.ipmi_ctx);
  ipmi_ctx_destroy (state_data.ipmi_ctx);
  config_sections_destroy (state_data.sections);
  if (file_opened)
    fclose (fp);
  return (exit_code);
}

int
main (int argc, char *argv[])
{
  bmc_config_prog_data_t prog_data;
  struct bmc_config_arguments cmd_args;
  int hosts_count;
  int rv;

  ipmi_disable_coredump ();

  memset (&prog_data, '\0', sizeof (bmc_config_prog_data_t));
  prog_data.progname = argv[0];
  bmc_config_argp_parse (argc, argv, &cmd_args);

  prog_data.args = &cmd_args;

  if ((hosts_count = pstdout_setup (&(prog_data.args->config_args.common.hostname),
				    &(prog_data.args->config_args.hostrange))) < 0)
    return (EXIT_FAILURE);

  if (!hosts_count)
    return (EXIT_SUCCESS);

  prog_data.hosts_count = hosts_count;

  if ((rv = pstdout_launch (prog_data.args->config_args.common.hostname,
                            _bmc_config,
                            &prog_data)) < 0)
    {
      fprintf (stderr,
               "pstdout_launch: %s\n",
               pstdout_strerror (pstdout_errnum));
      return (EXIT_FAILURE);
    }

  return (rv);
}
