/* 
   Copyright (C) 2003-2008 FreeIPMI Core Team

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

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif
#include <assert.h>
#include <errno.h>

#include "bmc-config.h"
#include "bmc-config-argp.h"
#include "bmc-config-sections.h"

#include "freeipmi-portability.h"
#include "hostrange.h"
#include "pstdout.h"
#include "tool-common.h"
#include "tool-cmdline-common.h"

static void
_bmc_config_state_data_init(bmc_config_state_data_t *state_data)
{
  assert (state_data);

  memset(state_data, '\0', sizeof(bmc_config_state_data_t));
  state_data->prog_data = NULL;
  state_data->ipmi_ctx = NULL;

  state_data->cipher_suite_entry_count = 0;
  state_data->cipher_suite_id_supported_set = 0;
  state_data->cipher_suite_priv_set = 0;

  state_data->lan_channel_number_initialized = 0;
  state_data->serial_channel_number_initialized = 0;
  state_data->sol_channel_number_initialized = 0;
  state_data->number_of_users_initialized = 0;
}

static int
_bmc_config (pstdout_state_t pstate,
             const char *hostname,
             void *arg)
{
  bmc_config_state_data_t state_data;
  bmc_config_prog_data_t *prog_data;
  char errmsg[IPMI_OPEN_ERRMSGLEN];
  struct config_section *sections = NULL;
  int exit_code = -1;
  config_err_t ret = 0;
  int file_opened = 0;
  FILE *fp = NULL;              /* init NULL to remove warnings */

  prog_data = (bmc_config_prog_data_t *)arg;

  _bmc_config_state_data_init(&state_data);
  state_data.prog_data = prog_data;
  state_data.pstate = pstate;

  if (!(state_data.ipmi_ctx = ipmi_open(prog_data->progname,
                                        hostname,
                                        &(prog_data->args->config_args.common),
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

  if (!(sections = bmc_config_sections_create (&state_data)))
    {
      exit_code = EXIT_FAILURE;
      goto cleanup;
    }

  if (prog_data->args->config_args.action == CONFIG_ACTION_CHECKOUT)
    {
      if (prog_data->args->config_args.filename)
        {
          if (!(fp = fopen (prog_data->args->config_args.filename, "w")))
            {
              pstdout_perror(pstate,
                             "fopen");
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
              pstdout_perror(pstate,
                             "fopen");
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
      if (config_parse(pstate, 
                       sections,
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
      if (config_sections_insert_keyvalues(pstate, 
                                           sections,
                                           prog_data->args->config_args.keypairs) < 0)
        {
          /* errors printed in function call */
          exit_code = EXIT_FAILURE;
          goto cleanup;
        }
    }

  if (prog_data->args->config_args.action == CONFIG_ACTION_CHECKOUT
      || prog_data->args->config_args.action == CONFIG_ACTION_COMMIT
      || prog_data->args->config_args.action == CONFIG_ACTION_DIFF)
    {
      int num;
      int value_input_required = 0;

      if (prog_data->args->config_args.action != CONFIG_ACTION_CHECKOUT)
        value_input_required = 1;

      if ((num = config_sections_validate_keyvalue_inputs(pstate, 
                                                          sections,
                                                          value_input_required,
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
          if (!config_find_section(pstate, 
                                   sections,
                                   sstr->section_name))
            {
              pstdout_fprintf(pstate,
                              stderr,
                              "Unknown section `%s'\n",
                              sstr->section_name);
              goto cleanup;
            }
          sstr = sstr->next;
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
      
      if ((section = config_find_section(pstate,
                                         sections,
                                         "Lan_Conf")))
        {
          if (config_find_keyvalue(pstate,
                                   section,
                                   "IP_Address"))
            {
              pstdout_fprintf(pstate,
                              stderr,
                              "Cannot configure Lan_Conf:IP_Address on multiple hosts\n");
              goto cleanup;
            }

          if (config_find_keyvalue(pstate,
                                   section,
                                   "MAC_Address"))
            {
              pstdout_fprintf(pstate,
                              stderr,
                              "Cannot configure Lan_Conf:MAC_Address on multiple hosts\n");
              goto cleanup;
            }
        }
    }

  switch (prog_data->args->config_args.action) {
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
            
            if (!(s = config_find_section(pstate, 
                                          sections, 
                                          sstr->section_name)))
              {
                pstdout_fprintf(pstate,
                                stderr, 
                                "## FATAL: Cannot checkout section '%s'\n",
                                sstr->section_name);
                continue;
              }

            this_ret = config_checkout_section(pstate, 
                                               s,
                                               &(prog_data->args->config_args),
                                               1,
                                               fp,
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
                               sections,
                               &(prog_data->args->config_args),
                               all_keys_if_none_specified,
                               fp,
                               &state_data);
      }
    break;
  case CONFIG_ACTION_COMMIT:
    ret = config_commit (pstate, 
                         sections,
                         &(prog_data->args->config_args),
                         &state_data);
    break;
  case CONFIG_ACTION_DIFF:
    ret = config_diff (pstate, 
                       sections,
                       &(prog_data->args->config_args),
                       &state_data);
    break;
  case CONFIG_ACTION_LIST_SECTIONS:
    ret = config_output_sections_list (pstate, sections);
    break;
  }
  
  if (ret == CONFIG_ERR_FATAL_ERROR || ret == CONFIG_ERR_NON_FATAL_ERROR)
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
  if (file_opened)
    fclose(fp);
  if (sections)
    config_sections_destroy(pstate, sections);
  return exit_code;
}

int
main (int argc, char *argv[])
{
  bmc_config_prog_data_t prog_data;
  struct bmc_config_arguments cmd_args;
  int exit_code;
  int hosts_count;
  int rv;

  ipmi_disable_coredump();

  memset(&prog_data, '\0', sizeof(bmc_config_prog_data_t));
  prog_data.progname = argv[0];
  bmc_config_argp_parse (argc, argv, &cmd_args);

  prog_data.args = &cmd_args;

  if ((hosts_count = pstdout_setup(&(prog_data.args->config_args.common.hostname),
                                   prog_data.args->config_args.hostrange.buffer_output,
                                   prog_data.args->config_args.hostrange.consolidate_output,
                                   prog_data.args->config_args.hostrange.fanout,
                                   prog_data.args->config_args.hostrange.eliminate,
                                   prog_data.args->config_args.hostrange.always_prefix)) < 0)
    {
      exit_code = EXIT_FAILURE;
      goto cleanup;
    }

  prog_data.hosts_count = hosts_count;

  if ((rv = pstdout_launch(prog_data.args->config_args.common.hostname,
                           _bmc_config,
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
  return exit_code;
}
