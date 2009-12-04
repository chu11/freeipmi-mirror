/*
  Copyright (C) 2007-2008 FreeIPMI Core Team

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

#include "pef-config.h"
#include "pef-config-argp.h"
#include "pef-config-info.h"
#include "pef-config-sections.h"

#include "freeipmi-portability.h"
#include "pstdout.h"
#include "tool-common.h"
#include "tool-cmdline-common.h"
#include "tool-hostrange-common.h"

static void
_pef_config_state_data_init(pef_config_state_data_t *state_data)
{
  assert (state_data);

  memset(state_data, '\0', sizeof(pef_config_state_data_t));
  state_data->prog_data = NULL;
  state_data->ipmi_ctx = NULL;
  
  state_data->alert_policy_sections_len = 0;
  state_data->alert_policy_sections = NULL;

  state_data->lan_channel_number_initialized = 0;
  state_data->number_of_lan_alert_destinations_initialized = 0;
  state_data->number_of_alert_strings_initialized = 0;
  state_data->number_of_alert_policy_entries_initialized = 0;
  state_data->number_of_event_filters_initialized = 0;
}

static int 
_pef_config (pstdout_state_t pstate,
             const char *hostname,
             void *arg)
{
  pef_config_state_data_t state_data;
  pef_config_prog_data_t *prog_data;
  char errmsg[IPMI_OPEN_ERRMSGLEN];
  struct config_section *sections = NULL;
  int exit_code = -1;
  config_err_t ret = 0;
  int file_opened = 0;
  FILE *fp = NULL;              /* init NULL to remove warnings */

  prog_data = (pef_config_prog_data_t *) arg;
  
  _pef_config_state_data_init (&state_data);
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

  if (!(sections = pef_config_sections_create (&state_data)))
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
              pstdout_fprintf(pstate,
                              stderr,
                              "Cannot output multiple host checkout into a single file\n");
              exit_code = EXIT_FAILURE;
              goto cleanup;
            }

          if (!(fp = fopen (prog_data->args->config_args.filename, "w")))
            {
              pstdout_perror(pstate, 
                             "fopen");
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
              pstdout_perror(pstate,
                             "fopen");
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
      if (config_parse(pstate,
                       sections,
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
              exit_code = EXIT_FAILURE;
              goto cleanup;
            }
          sstr = sstr->next;
        }
    }

  /* Special case(s):
   *
   * A) On some motherboards, the "Alert_Policy" fields must be input
   * all at once.  See workaround details in alert policy section
   * code.
   */
  if (prog_data->args->config_args.action == CONFIG_ACTION_COMMIT)
    {
      struct config_section *section;
      unsigned int alert_policy_count = 0;

      /* First, see how many alert policy sections there are */
      section = sections;
      while (section)
        {
          if (stristr (section->section_name, "Alert_Policy"))
            alert_policy_count++;
          section = section->next;
        }

      if (alert_policy_count)
        {
          unsigned int index = 0;

          if (!(state_data.alert_policy_sections = (struct config_section **) malloc (alert_policy_count * sizeof (struct config_section *))))
            {
              pstdout_perror (pstate, "malloc");
              goto cleanup;
            }
          
          state_data.alert_policy_sections_len = alert_policy_count;
          
          section = sections;
          while (section)
            {
              if (stristr (section->section_name, "Alert_Policy"))
                {
                  state_data.alert_policy_sections[index] = section;
                  index++;
                }
              section = section->next;
            }
        }
    }
  
  if (prog_data->args->info)
    ret = pef_info (&state_data);
  else
    {
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
                                   sections,
                                   &(prog_data->args->config_args),
                                   all_keys_if_none_specified,
                                   fp,
                                   0,
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
main (int argc, char **argv)
{
  pef_config_prog_data_t prog_data;
  struct pef_config_arguments cmd_args;
  int exit_code;
  int hosts_count;
  int rv;

  ipmi_disable_coredump();

  memset(&prog_data, '\0', sizeof(pef_config_prog_data_t));
  prog_data.progname = argv[0];
  pef_config_argp_parse (argc, argv, &cmd_args);

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
                           _pef_config,
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

