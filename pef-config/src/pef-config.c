/*
pef-config.c: Platform Event Filtering utility.
Copyright (C) 2005-2007 FreeIPMI Core Team

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
#ifdef HAVE_ERROR_H
#include <error.h>
#endif
#include <argp.h>
#include <assert.h>

#include "cmdline-parse-common.h"
#include "freeipmi-portability.h"
#include "tool-common.h"
#include "pef-config.h"
#include "pef-config-argp.h"
#include "pef-config-info.h"
#include "pef-config-sections.h"

void
_pef_config_state_data_init(pef_config_state_data_t *state_data)
{
  assert (state_data);

  memset(state_data, '\0', sizeof(pef_config_state_data_t));
  state_data->prog_data = NULL;
  state_data->ipmi_ctx = NULL;

  state_data->lan_channel_number_initialized = 0;
  state_data->number_of_lan_alert_destinations_initialized = 0;
  state_data->number_of_alert_strings_initialized = 0;
  state_data->number_of_alert_policy_entries_initialized = 0;
  state_data->number_of_event_filters_initialized = 0;
}

static int 
_pef_config (void *arg)
{
  pef_config_state_data_t state_data;
  pef_config_prog_data_t *prog_data;
  ipmi_ctx_t ipmi_ctx = NULL;
  char errmsg[IPMI_OPEN_ERRMSGLEN];
  struct config_section *sections = NULL;
  int exit_code = -1;
  config_err_t ret = 0;
  int file_opened = 0;
  FILE *fp;

  prog_data = (pef_config_prog_data_t *) arg;
  
  if (!(ipmi_ctx = ipmi_open(prog_data->progname,
                             prog_data->args->common.hostname,
                             &(prog_data->args->common),
                             errmsg,
                             IPMI_OPEN_ERRMSGLEN)))
    {
      fprintf(stderr, "%s\n", errmsg);
      exit_code = EXIT_FAILURE;
      goto cleanup;
    }

  _pef_config_state_data_init (&state_data);
  state_data.ipmi_ctx = ipmi_ctx;
  state_data.prog_data = prog_data;

  if (!(sections = pef_config_sections_create (&state_data)))
    {
      exit_code = EXIT_FAILURE;
      goto cleanup;
    }
  state_data.sections = sections;

  if (prog_data->args->action == CONFIG_ACTION_CHECKOUT)
    {
      if (prog_data->args->filename)
        {
          if (!(fp = fopen (prog_data->args->filename, "w")))
            {
              perror("fopen");
              goto cleanup;
            }
          file_opened++;
        }
      else
        fp = stdout;
    }
  else if (prog_data->args->action == CONFIG_ACTION_COMMIT
           || prog_data->args->action == CONFIG_ACTION_DIFF)
    {
      if (prog_data->args->filename && strcmp (prog_data->args->filename, "-"))
        {
          if (!(fp = fopen (prog_data->args->filename, "r")))
            {
              perror("fopen");
              goto cleanup;
            }
          file_opened++;
        }
      else
        fp = stdin;
    }

  /* parse if there is an input file or no pairs at all */
  if ((prog_data->args->action == CONFIG_ACTION_COMMIT
       && prog_data->args->filename)
      || (prog_data->args->action == CONFIG_ACTION_COMMIT
          && !prog_data->args->filename
          && !prog_data->args->keypairs)
      || (prog_data->args->action == CONFIG_ACTION_DIFF
          && prog_data->args->filename)
      || (prog_data->args->action == CONFIG_ACTION_DIFF
          && !prog_data->args->filename
          && !prog_data->args->keypairs))
    {
      if (config_parse(sections,
                       prog_data->args,
                       fp) < 0)
        {
          /* errors printed in function call */
          goto cleanup;
        }
    }

  /* note: argp validation catches if user specified keypair and
     filename for a diff
  */
  if ((prog_data->args->action == CONFIG_ACTION_CHECKOUT
       || prog_data->args->action == CONFIG_ACTION_COMMIT
       || prog_data->args->action == CONFIG_ACTION_DIFF)
      && prog_data->args->keypairs)
    {
      if (config_sections_insert_keyvalues(sections,
                                           prog_data->args->keypairs) < 0)
        {
          /* errors printed in function call */
          exit_code = EXIT_FAILURE;
          goto cleanup;
        }
    }

  if (prog_data->args->action == CONFIG_ACTION_CHECKOUT
      || prog_data->args->action == CONFIG_ACTION_COMMIT
      || prog_data->args->action == CONFIG_ACTION_DIFF)
    {
      int num;
      int value_input_required = 0;

      if (prog_data->args->action != CONFIG_ACTION_CHECKOUT)
        value_input_required = 1;

      if ((num = config_sections_validate_keyvalue_inputs(sections,
                                                          value_input_required)) < 0)
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
  

  if (prog_data->args->action == CONFIG_ACTION_CHECKOUT
      && prog_data->args->section_strs)
    {
      struct config_section_str *sstr;

      sstr = prog_data->args->section_strs;
      while (sstr)
        {
          if (!config_find_section(sections,
                                   sstr->section_name))
            {
              fprintf(stderr,
                      "Unknown section `%s'\n",
                      sstr->section_name);
              goto cleanup;
            }
          sstr = sstr->next;
        }
    }


  switch (prog_data->args->action) {
  case CONFIG_ACTION_INFO:
    ret = pef_info (&state_data);
    break;
  case CONFIG_ACTION_CHECKOUT:
    if (prog_data->args->section_strs)
      {
        struct config_section_str *sstr;

        /* note: argp validation catches if user specified --section
         * and --keypair, so all_keys_if_none_specified should be '1'.
         */

        sstr = prog_data->args->section_strs;
        while (sstr)
          {
	    struct config_section *s;
            config_err_t this_ret;

	    if (!(s = config_find_section(sections, sstr->section_name)))
              {
                fprintf(stderr, "## FATAL: Cannot checkout section '%s'\n",
                        sstr->section_name);
                continue;
              }

            this_ret = config_checkout_section(s,
                                               prog_data->args,
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

        if (!prog_data->args->keypairs)
          all_keys_if_none_specified++;

        ret = config_checkout (sections,
                               prog_data->args,
                               all_keys_if_none_specified,
                               fp,
                               &state_data);
      }
    break;
  case CONFIG_ACTION_COMMIT:
    ret = config_commit (sections,
                         prog_data->args,
                         fp,
                         &state_data);
    break;
  case CONFIG_ACTION_DIFF:
    ret = config_diff (sections,
                       prog_data->args,
                       &state_data);
    break;
  case CONFIG_ACTION_LIST_SECTIONS:
    ret = config_output_sections_list (sections);
    break;
  }

  if (ret == CONFIG_ERR_FATAL_ERROR || ret == CONFIG_ERR_NON_FATAL_ERROR)
    {
      exit_code = EXIT_FAILURE;
      goto cleanup;
    }
  
  exit_code = 0;
 cleanup:
  if (ipmi_ctx)
    {
      ipmi_ctx_close (ipmi_ctx);
      ipmi_ctx_destroy (ipmi_ctx);
    }
  if (file_opened)
    fclose(fp);
  if (sections)
    config_sections_destroy(sections);
  return exit_code;
}

int 
main (int argc, char **argv)
{
  pef_config_prog_data_t prog_data;
  struct config_arguments cmd_args;
  int exit_code;
  
  ipmi_disable_coredump();

  prog_data.progname = argv[0];
  pef_config_argp_parse (argc, argv, &cmd_args);

  if (pef_config_args_validate (&cmd_args) < 0)
    return (EXIT_FAILURE);

  prog_data.args = &cmd_args;

  exit_code = _pef_config (&prog_data);
  
  return exit_code;
}

