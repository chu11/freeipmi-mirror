/*
 * Copyright (C) 2003-2014 FreeIPMI Core Team
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
#include "ipmi-config-checkout.h"
#include "ipmi-config-commit.h"
#include "ipmi-config-diff.h"
#include "ipmi-config-parse.h"
#include "ipmi-config-section.h"
#include "ipmi-config-utils.h"

#include "ipmi-config-category-core-sections.h"
#include "ipmi-config-category-chassis-sections.h"
#include "ipmi-config-category-dcmi-sections.h"
#include "ipmi-config-category-sensors-sections.h"
#include "ipmi-config-category-pef-sections.h"
#include "ipmi-config-legacy-pef-info.h"

#include "freeipmi-portability.h"
#include "pstdout.h"
#include "tool-common.h"
#include "tool-cmdline-common.h"
#include "tool-hostrange-common.h"
#include "tool-sdr-cache-common.h"
#include "tool-util-common.h"

static int
_ipmi_config (pstdout_state_t pstate,
              const char *hostname,
              void *arg)
{
  ipmi_config_state_data_t state_data;
  ipmi_config_prog_data_t *prog_data;
  struct ipmi_config_section *tmp_sections;
  int exit_code = EXIT_FAILURE;
  ipmi_config_err_t ret = 0;
  int file_opened = 0;
  FILE *fp = NULL;              /* init NULL to remove warnings */

  assert (pstate);
  assert (arg);

  prog_data = (ipmi_config_prog_data_t *)arg;

  assert (!(prog_data->args->category_mask & ~IPMI_CONFIG_CATEGORY_MASK_ALL));

  memset (&state_data, '\0', sizeof (ipmi_config_state_data_t));
  state_data.prog_data = prog_data;
  state_data.pstate = pstate;

  if (!(state_data.ipmi_ctx = ipmi_open (prog_data->progname,
                                         hostname,
                                         &(prog_data->args->common_args),
                                         state_data.pstate)))
    goto cleanup;

  state_data.sections = NULL;
  state_data.sdr_ctx = NULL;

  if (prog_data->args->category_mask & IPMI_CONFIG_CATEGORY_MASK_CORE)
    {
      if (!(tmp_sections = ipmi_config_core_sections_create (&state_data)))
	goto cleanup;

      if (ipmi_config_set_category (tmp_sections, IPMI_CONFIG_CATEGORY_MASK_CORE) < 0)
	goto cleanup;

      if (ipmi_config_section_append (&state_data.sections, tmp_sections) < 0)
	goto cleanup;
    }

  if (prog_data->args->category_mask & IPMI_CONFIG_CATEGORY_MASK_CHASSIS)
    {
      if (!(tmp_sections = ipmi_config_chassis_sections_create (&state_data)))
	goto cleanup;

      if (ipmi_config_set_category (tmp_sections, IPMI_CONFIG_CATEGORY_MASK_CHASSIS) < 0)
	goto cleanup;

      if (ipmi_config_section_append (&state_data.sections, tmp_sections) < 0)
	goto cleanup;
    }

  if (prog_data->args->category_mask & IPMI_CONFIG_CATEGORY_MASK_SENSORS)
    {
      if (!(state_data.sdr_ctx = ipmi_sdr_ctx_create ()))
	{
	  pstdout_perror (pstate, "ipmi_sdr_ctx_create()");
	  goto cleanup;
	}

      if (sdr_cache_create_and_load (state_data.sdr_ctx,
				     NULL,
				     state_data.ipmi_ctx,
				     hostname,
				     &(prog_data->args->common_args)) < 0)
	goto cleanup;

      if (!(tmp_sections = ipmi_config_sensors_sections_create (&state_data)))
	goto cleanup;

      if (ipmi_config_set_category (tmp_sections, IPMI_CONFIG_CATEGORY_MASK_SENSORS) < 0)
	goto cleanup;

      /* Many fields long in length, use average of 75 for this section */
      if (ipmi_config_set_line_length (tmp_sections, 75) < 0)
	goto cleanup;

      if (ipmi_config_section_append (&state_data.sections, tmp_sections) < 0)
	goto cleanup;
    }

  if (prog_data->args->category_mask & IPMI_CONFIG_CATEGORY_MASK_PEF)
    {
      if (!(tmp_sections = ipmi_config_pef_sections_create (&state_data)))
	goto cleanup;

      if (ipmi_config_set_category (tmp_sections, IPMI_CONFIG_CATEGORY_MASK_PEF) < 0)
	goto cleanup;

      if (ipmi_config_section_append (&state_data.sections, tmp_sections) < 0)
	goto cleanup;
    }

  if (prog_data->args->category_mask & IPMI_CONFIG_CATEGORY_MASK_DCMI)
    {
      if (!(tmp_sections = ipmi_config_dcmi_sections_create (&state_data)))
	goto cleanup;

      if (ipmi_config_set_category (tmp_sections, IPMI_CONFIG_CATEGORY_MASK_DCMI) < 0)
	goto cleanup;

      /* Many fields long in length, use average of 60 for this section */
      if (ipmi_config_set_line_length (tmp_sections, 60) < 0)
	goto cleanup;

      if (ipmi_config_section_append (&state_data.sections, tmp_sections) < 0)
	goto cleanup;
    }

  assert (state_data.sections);
  
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
                               "Unknown section `%s', perhaps another category must be specified (see --category)\n",
                               sstr->section_name);
              goto cleanup;
            }
          sstr = sstr->next;
        }
    }

  /*
   * Special case(s)
   */
  if (prog_data->args->category_mask & IPMI_CONFIG_CATEGORY_MASK_CORE)
    {
      /* Special case: On some motherboards, the "Enable_User" must
       * come after the "Password" configure.  So we store information
       * for this fact.  See workaround details in user section code.
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
    }
  else if (prog_data->args->category_mask & IPMI_CONFIG_CATEGORY_MASK_CHASSIS)
    {
      /* Special case: There may not be a way to checkout the front panel
       * buttons, so we have to store before hand it if we intend to
       * commit it.
       */
      if (prog_data->args->action == IPMI_CONFIG_ACTION_COMMIT)
	{
	  struct ipmi_config_section *section;

	  section = state_data.sections;
	  while (section)
	    {
	      struct ipmi_config_keyvalue *kv;

	      if (!strcasecmp (section->section_name, "Chassis_Front_Panel_Buttons"))
		{
		  if ((kv = ipmi_config_find_keyvalue (section,
						       "Enable_Standby_Button_For_Entering_Standby")))
		    {
		      state_data.front_panel_enable_standby_button_for_entering_standby_initialized++;
		      state_data.front_panel_enable_standby_button_for_entering_standby = same (kv->value_input, "yes") ? IPMI_CHASSIS_BUTTON_ENABLE : IPMI_CHASSIS_BUTTON_DISABLE;
		    }
		  
		  if ((kv = ipmi_config_find_keyvalue (section,
						       "Enable_Diagnostic_Interrupt_Button")))
		    {
		      state_data.front_panel_enable_diagnostic_interrupt_button_initialized++;
		      state_data.front_panel_enable_diagnostic_interrupt_button = same (kv->value_input, "yes") ? IPMI_CHASSIS_BUTTON_ENABLE : IPMI_CHASSIS_BUTTON_DISABLE;
		    }
		  
		  if ((kv = ipmi_config_find_keyvalue (section,
						       "Enable_Reset_Button")))
		    {
		      state_data.front_panel_enable_reset_button_initialized++;
		      state_data.front_panel_enable_reset_button = same (kv->value_input, "yes") ? IPMI_CHASSIS_BUTTON_ENABLE : IPMI_CHASSIS_BUTTON_DISABLE;
		    }
		  
		  if ((kv = ipmi_config_find_keyvalue (section,
						       "Enable_Power_Off_Button_For_Power_Off_Only")))
		    {
		      state_data.front_panel_enable_power_off_button_for_power_off_only_initialized++;
		      state_data.front_panel_enable_power_off_button_for_power_off_only = same (kv->value_input, "yes") ? IPMI_CHASSIS_BUTTON_ENABLE : IPMI_CHASSIS_BUTTON_DISABLE;
		    }
		}
	      section = section->next;
	    }
	}
    }

  if (prog_data->args->info)
    {
      ret = ipmi_config_pef_info (&state_data);
      goto out;
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
                                                       fp);
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
                                      fp);
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

 out:
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
  if (state_data.sdr_ctx)
    ipmi_sdr_ctx_destroy (state_data.sdr_ctx);
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
