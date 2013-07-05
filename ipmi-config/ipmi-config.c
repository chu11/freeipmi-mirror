/*
 * Copyright (C) 2003-2013 FreeIPMI Core Team
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

#include "ipmi-config.h"
#include "ipmi-config-argp.h"
#include "ipmi-config-tool-checkout.h"
#include "ipmi-config-tool-commit.h"
#include "ipmi-config-tool-diff.h"
#include "ipmi-config-tool-parse.h"
#include "ipmi-config-tool-section.h"
#include "ipmi-config-sections.h"
#include "ipmi-config-utils.h"

#include "freeipmi-portability.h"
#include "pstdout.h"
#include "tool-common.h"
#include "tool-cmdline-common.h"
#include "tool-hostrange-common.h"
#include "tool-util-common.h"

static int
_ipmi_config (pstdout_state_t pstate,
              const char *hostname,
              void *arg)
{
  ipmi_config_state_data_t state_data;
  ipmi_config_prog_data_t *prog_data;
  int exit_code = EXIT_FAILURE;
  ipmi_config_err_t ret = 0;
  int file_opened = 0;
  FILE *fp = NULL;              /* init NULL to remove warnings */

  assert (pstate);
  assert (arg);

  prog_data = (ipmi_config_prog_data_t *)arg;

  memset (&state_data, '\0', sizeof (ipmi_config_state_data_t));
  state_data.prog_data = prog_data;
  state_data.pstate = pstate;

  if (!(state_data.ipmi_ctx = ipmi_open (prog_data->progname,
                                         hostname,
                                         &(prog_data->args->common_args),
                                         state_data.pstate)))
    goto cleanup;

  if (!(state_data.sections = ipmi_config_sections_create (&state_data)))
    goto cleanup;

  if (prog_data->args->action == IPMI_CONFIG_ACTION_CHECKOUT)
    {
      if (prog_data->args->filename)
        {
          if (prog_data->hosts_count > 1)
            {
              pstdout_fprintf (pstate,
                               stderr,
                               "Cannot output multiple host checkout into a single file\n");
              goto cleanup;
            }

          if (!(fp = fopen (prog_data->args->filename, "w")))
            {
              pstdout_perror (pstate, "fopen");
              goto cleanup;
            }
          file_opened++;
        }
      else
        fp = stdout;
    }
  else if (prog_data->args->action == IPMI_CONFIG_ACTION_COMMIT
           || prog_data->args->action == IPMI_CONFIG_ACTION_DIFF)
    {
      if (prog_data->args->filename
          && strcmp (prog_data->args->filename, "-"))
        {
          if (!(fp = fopen (prog_data->args->filename, "r")))
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
  if ((prog_data->args->action == IPMI_CONFIG_ACTION_COMMIT
       && prog_data->args->filename)
      || (prog_data->args->action == IPMI_CONFIG_ACTION_COMMIT
          && !prog_data->args->filename
          && !prog_data->args->keypairs)
      || (prog_data->args->action == IPMI_CONFIG_ACTION_DIFF
          && prog_data->args->filename)
      || (prog_data->args->action == IPMI_CONFIG_ACTION_DIFF
          && !prog_data->args->filename
          && !prog_data->args->keypairs))
    {
      if (ipmi_config_parse (&state_data, fp) < 0)
        goto cleanup;
    }
  
  /* note: argp validation catches if user specified keypair and
     filename for a diff
  */
  if ((prog_data->args->action == IPMI_CONFIG_ACTION_CHECKOUT
       || prog_data->args->action == IPMI_CONFIG_ACTION_COMMIT
       || prog_data->args->action == IPMI_CONFIG_ACTION_DIFF)
      && prog_data->args->keypairs)
    {
      if (ipmi_config_sections_insert_keyvalues (&state_data,
                                                 prog_data->args->keypairs) < 0)
        goto cleanup;
    }

  if (prog_data->args->action == IPMI_CONFIG_ACTION_COMMIT
      || prog_data->args->action == IPMI_CONFIG_ACTION_DIFF)
    {
      int num;

      if ((num = ipmi_config_sections_validate_keyvalue_inputs (&state_data)) < 0)
        goto cleanup;

      /* some errors found */
      if (num)
        goto cleanup;
    }

  if (prog_data->args->action == IPMI_CONFIG_ACTION_CHECKOUT
      && prog_data->args->section_strs)
    {
      struct ipmi_config_section_str *sstr;

      sstr = prog_data->args->section_strs;
      while (sstr)
        {
          if (!ipmi_config_find_section (&state_data, sstr->section_name))
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
  if (prog_data->args->action == IPMI_CONFIG_ACTION_COMMIT)
    {
      struct ipmi_config_section *section;
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
              struct ipmi_config_keyvalue *kv;

              if (stristr (section->section_name, "User"))
                {
                  uint8_t userid;

                  userid = atoi (section->section_name + strlen ("User"));

                  if (userid < user_count)
                    {
                      if ((kv = ipmi_config_find_keyvalue (section,
                                                           "Enable_User")))
                        enable_user_found = 1;
                    }
                }

              section = section->next;
            }

          if (enable_user_found)
            {
              datasize = sizeof (ipmi_config_enable_user_after_password_t) * user_count;
              
              if (!(state_data.enable_user_after_password = (ipmi_config_enable_user_after_password_t *)malloc (datasize)))
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
  if (prog_data->args->action == IPMI_CONFIG_ACTION_COMMIT
      && prog_data->hosts_count > 1)
    {
      struct ipmi_config_section *section;

      if ((section = ipmi_config_find_section (&state_data, "Lan_Conf")))
        {
          if (ipmi_config_find_keyvalue (section,
                                         "IP_Address"))
            {
              pstdout_fprintf (pstate,
                               stderr,
                               "Cannot configure Lan_Conf:IP_Address on multiple hosts\n");
              goto cleanup;
            }

          if (ipmi_config_find_keyvalue (section,
                                         "MAC_Address"))
            {
              pstdout_fprintf (pstate,
                               stderr,
                               "Cannot configure Lan_Conf:MAC_Address on multiple hosts\n");
              goto cleanup;
            }
        }
    }

  switch (prog_data->args->action)
    {
    case IPMI_CONFIG_ACTION_CHECKOUT:
      if (prog_data->args->section_strs)
        {
          struct ipmi_config_section_str *sstr;
          
          /* note: argp validation catches if user specified --section
           * and --keypair, so all_keys_if_none_specified should be '1'.
           */

          sstr = prog_data->args->section_strs;
          while (sstr)
            {
              struct ipmi_config_section *s;
              ipmi_config_err_t this_ret;

              if (!(s = ipmi_config_find_section (&state_data, sstr->section_name)))
                {
                  pstdout_fprintf (pstate,
                                   stderr,
                                   "## FATAL: Cannot checkout section '%s'\n",
                                   sstr->section_name);
                  continue;
                }

              this_ret = ipmi_config_checkout_section (&state_data,
                                                       s,
                                                       1,
                                                       fp,
                                                       0);
              if (this_ret != IPMI_CONFIG_ERR_SUCCESS)
                ret = this_ret;
              if (ret == IPMI_CONFIG_ERR_FATAL_ERROR)
                break;

              sstr = sstr->next;
            }
        }
      else
        {
          int all_keys_if_none_specified = 0;

          if (!prog_data->args->keypairs)
            all_keys_if_none_specified++;

          ret = ipmi_config_checkout (&state_data,
                                      all_keys_if_none_specified,
                                      fp,
                                      0);
        }
      break;
    case IPMI_CONFIG_ACTION_COMMIT:
      ret = ipmi_config_commit (&state_data);
      break;
    case IPMI_CONFIG_ACTION_DIFF:
      ret = ipmi_config_diff (&state_data);
      break;
    case IPMI_CONFIG_ACTION_LIST_SECTIONS:
      ret = ipmi_config_output_sections_list (&state_data);
      break;
    }

  if (ret == IPMI_CONFIG_ERR_FATAL_ERROR)
    {
      exit_code = IPMI_CONFIG_FATAL_EXIT_VALUE;
      goto cleanup;
    }

  if (ret == IPMI_CONFIG_ERR_NON_FATAL_ERROR)
    {
      exit_code = IPMI_CONFIG_NON_FATAL_EXIT_VALUE;
      goto cleanup;
    }

  exit_code = EXIT_SUCCESS;
 cleanup:
  ipmi_ctx_close (state_data.ipmi_ctx);
  ipmi_ctx_destroy (state_data.ipmi_ctx);
  ipmi_config_sections_destroy (state_data.sections);
  if (file_opened)
    fclose (fp);
  return (exit_code);
}

int
main (int argc, char *argv[])
{
  ipmi_config_prog_data_t prog_data;
  struct ipmi_config_arguments cmd_args;
  int hosts_count;
  int rv;

  ipmi_disable_coredump ();

  memset (&prog_data, '\0', sizeof (ipmi_config_prog_data_t));
  prog_data.progname = argv[0];
  ipmi_config_argp_parse (argc, argv, &cmd_args);

  prog_data.args = &cmd_args;

  if ((hosts_count = pstdout_setup (&(prog_data.args->common_args.hostname),
                                    &(prog_data.args->common_args))) < 0)
    return (EXIT_FAILURE);

  if (!hosts_count)
    return (EXIT_SUCCESS);

  prog_data.hosts_count = hosts_count;

  if ((rv = pstdout_launch (prog_data.args->common_args.hostname,
                            _ipmi_config,
                            &prog_data)) < 0)
    {
      fprintf (stderr,
               "pstdout_launch: %s\n",
               pstdout_strerror (pstdout_errnum));
      return (EXIT_FAILURE);
    }

  return (rv);
}
