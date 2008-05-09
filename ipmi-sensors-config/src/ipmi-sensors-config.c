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

#include "freeipmi-portability.h"
#include "tool-common.h"
#include "tool-cmdline-common.h"
#include "tool-sdr-cache-common.h"
#include "ipmi-sensors-config.h"
#include "ipmi-sensors-config-argp.h"
#include "ipmi-sensors-config-sections.h"

static void
_ipmi_sensors_config_state_data_init(ipmi_sensors_config_state_data_t *state_data)
{
  assert (state_data);

  memset(state_data, '\0', sizeof(ipmi_sensors_config_state_data_t));
  state_data->prog_data = NULL;
  state_data->ipmi_ctx = NULL;
}

static int 
_ipmi_sensors_config (void *arg)
{
  ipmi_sensors_config_state_data_t state_data;
  ipmi_sensors_config_prog_data_t *prog_data;
  char errmsg[IPMI_OPEN_ERRMSGLEN];
  struct config_section *sections = NULL;
  int exit_code = -1;
  config_err_t ret = 0;
  int file_opened = 0;
  FILE *fp = NULL;              /* init NULL to remove warnings */

  prog_data = (ipmi_sensors_config_prog_data_t *) arg;
  
  _ipmi_sensors_config_state_data_init (&state_data);
  state_data.prog_data = prog_data;

  /* Special case, just flush, don't do an IPMI connection */
  if (!prog_data->args->sdr.flush_cache_wanted)
    {
      if (!(state_data.ipmi_ctx = ipmi_open(prog_data->progname,
                                            prog_data->args->config_args.common.hostname,
                                            &(prog_data->args->config_args.common),
                                            errmsg,
                                            IPMI_OPEN_ERRMSGLEN)))
        {
          fprintf(stderr, "%s\n", errmsg);
          exit_code = EXIT_FAILURE;
          goto cleanup;
        }
    }

  if (!(state_data.ipmi_sdr_cache_ctx = ipmi_sdr_cache_ctx_create()))
    {
      perror ("ipmi_sdr_cache_ctx_create()");
      exit_code = EXIT_FAILURE;
      goto cleanup;
    }

  if (state_data.prog_data->args->config_args.common.flags & IPMI_FLAGS_DEBUG_DUMP)
    {
      /* Don't error out, if this fails we can still continue */
      if (ipmi_sdr_cache_ctx_set_flags(state_data.ipmi_sdr_cache_ctx,
                                       IPMI_SDR_CACHE_FLAGS_DEBUG_DUMP) < 0)
        fprintf (stderr,
                 "ipmi_sdr_cache_ctx_set_flags: %s\n",
                 ipmi_sdr_cache_ctx_strerror(ipmi_sdr_cache_ctx_errnum(state_data.ipmi_sdr_cache_ctx)));

      if (prog_data->args->config_args.common.hostname)
        {
          if (ipmi_sdr_cache_ctx_set_debug_prefix(state_data.ipmi_sdr_cache_ctx,
                                                  prog_data->args->config_args.common.hostname) < 0)
            fprintf (stderr,
                     "ipmi_sdr_cache_ctx_set_debug_prefix: %s\n",
                     ipmi_sdr_cache_ctx_strerror(ipmi_sdr_cache_ctx_errnum(state_data.ipmi_sdr_cache_ctx)));
        }
    }  

  if (prog_data->args->sdr.flush_cache_wanted)
    {
      if (sdr_cache_flush_cache(state_data.ipmi_sdr_cache_ctx,
                                NULL,
                                state_data.prog_data->args->config_args.common.hostname,
                                state_data.prog_data->args->sdr.sdr_cache_dir_wanted ? state_data.prog_data->args->sdr.sdr_cache_dir : NULL) < 0)
        {
          exit_code = EXIT_FAILURE;
          goto cleanup;
        }
    }

  if (sdr_cache_create_and_load (state_data.ipmi_sdr_cache_ctx,
                                 NULL,
                                 state_data.ipmi_ctx,
                                 prog_data->args->sdr.quiet_cache_wanted,
                                 prog_data->args->config_args.common.hostname,
                                 prog_data->args->sdr.sdr_cache_dir_wanted ? prog_data->args->sdr.sdr_cache_dir : NULL) < 0)
    {
      exit_code = EXIT_FAILURE;
      goto cleanup;
    }

  if (!(sections = ipmi_sensors_config_sections_create (&state_data)))
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
              perror("fopen");
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
      if (prog_data->args->config_args.filename && strcmp (prog_data->args->config_args.filename, "-"))
        {
          if (!(fp = fopen (prog_data->args->config_args.filename, "r")))
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
      if (config_parse(sections,
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
      if (config_sections_insert_keyvalues(sections,
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

      if ((num = config_sections_validate_keyvalue_inputs(sections,
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

	    if (!(s = config_find_section(sections, sstr->section_name)))
              {
                fprintf(stderr, "## FATAL: Cannot checkout section '%s'\n",
                        sstr->section_name);
                continue;
              }

            this_ret = config_checkout_section(s,
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

        ret = config_checkout (sections,
                               &(prog_data->args->config_args),
                               all_keys_if_none_specified,
                               fp,
                               &state_data);
      }
    break;
  case CONFIG_ACTION_COMMIT:
    ret = config_commit (sections,
                         &(prog_data->args->config_args),
                         fp,
                         &state_data);
    break;
  case CONFIG_ACTION_DIFF:
    ret = config_diff (sections,
                       &(prog_data->args->config_args),
                       &state_data);
    break;
  case CONFIG_ACTION_LIST_SECTIONS:
    ret = config_output_sections_list (sections);
    break;
  case CONFIG_ACTION_INFO:
    /* shutup gcc warning */
    ;
  }

  if (ret == CONFIG_ERR_FATAL_ERROR || ret == CONFIG_ERR_NON_FATAL_ERROR)
    {
      exit_code = EXIT_FAILURE;
      goto cleanup;
    }
  
  exit_code = 0;
 cleanup:
  if (state_data.ipmi_sdr_cache_ctx)
    ipmi_sdr_cache_ctx_destroy(state_data.ipmi_sdr_cache_ctx);
  if (state_data.ipmi_ctx)
    {
      ipmi_ctx_close (state_data.ipmi_ctx);
      ipmi_ctx_destroy (state_data.ipmi_ctx);
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
  ipmi_sensors_config_prog_data_t prog_data;
  struct ipmi_sensors_config_arguments cmd_args;
  int exit_code;
  
  ipmi_disable_coredump();

  memset(&prog_data, '\0', sizeof(ipmi_sensors_config_prog_data_t));
  prog_data.progname = argv[0];
  ipmi_sensors_config_argp_parse (argc, argv, &cmd_args);

  if (ipmi_sensors_config_args_validate (&cmd_args) < 0)
    return (EXIT_FAILURE);

  prog_data.args = &cmd_args;

  exit_code = _ipmi_sensors_config (&prog_data);
  
  return exit_code;
}

