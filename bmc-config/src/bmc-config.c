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

static void
_bmc_config_state_data_init(bmc_config_state_data_t *state_data)
{
  assert (state_data);

  memset(state_data, '\0', sizeof(bmc_config_state_data_t));
  state_data->prog_data = NULL;
  state_data->ipmi_ctx = NULL;

  state_data->lan_user_session_limit_len = 0;
  state_data->lan_user_session_limit = NULL;
  state_data->serial_user_session_limit_len = 0;
  state_data->serial_user_session_limit = NULL;

  state_data->enable_user_after_password_len = 0;
  state_data->enable_user_after_password = NULL;

  state_data->lan_conf_auth_callback_level_oem_proprietary_set = 0;
  state_data->lan_conf_auth_user_level_oem_proprietary_set = 0;
  state_data->lan_conf_auth_operator_level_oem_proprietary_set = 0;
  state_data->lan_conf_auth_admin_level_oem_proprietary_set = 0;
  state_data->lan_conf_auth_oem_level_none_set = 0;
  state_data->lan_conf_auth_oem_level_md2_set = 0;
  state_data->lan_conf_auth_oem_level_md5_set = 0;
  state_data->lan_conf_auth_oem_level_straight_password_set = 0;
  state_data->lan_conf_auth_oem_level_oem_proprietary_set = 0;

  state_data->authentication_type_initialized = 0;

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
   * A) There is no way to checkout the user session limit, so we have
   * to store before hand it if we intend to commit it (along with the
   * other calls to set user access that commit things)
   *
   * B) On some motherboards, the "Enable_User" must come after the
   * "Password" configure.  So we store information for this fact.
   * See workaround details in user section code.
   *
   * C) On some motherboards, the default motherboard settings set
   * MD2/MD5 authentication at the OEM privilege, but you cannot
   * configure them to that.  See workaround details in the lan conf
   * authentication section.
   */
  if (prog_data->args->config_args.action == CONFIG_ACTION_COMMIT)
    {
      struct config_section *section;
      unsigned int user_count = 0;

      /* First, see how many user sections there are */
      section = sections;
      while (section)
        {
          if (stristr(section->section_name, "User"))
            user_count++;
          section = section->next;
        }

      if (user_count)
        {
          unsigned int lan_session_limit_found = 0;
          unsigned int serial_session_limit_found = 0;
          unsigned int enable_user_found = 0;
          unsigned int lan_conf_auth_found = 0;
          unsigned int datasize;

          /* Two, is the user configuring anything these special cases
           * care about?
           */
          section = sections;
          while (section)
            {
              struct config_keyvalue *kv;

              if (stristr (section->section_name, "User"))
                {
                  uint8_t userid;

                  userid = atoi (section->section_name + strlen ("User"));

                  if (userid < user_count)
                    {
                      if ((kv = config_find_keyvalue (pstate,
                                                      section,
                                                      "Lan_Session_Limit")))
                        lan_session_limit_found = 1;

                      if ((kv = config_find_keyvalue (pstate,
                                                      section,
                                                      "Serial_Session_Limit")))
                        serial_session_limit_found = 1;

                      if ((kv = config_find_keyvalue (pstate,
                                                      section,
                                                      "Enable_User")))
                        enable_user_found = 1;
                    }
                }

              if (stristr (section->section_name, "Lan_Conf_Auth"))
                lan_conf_auth_found = 1;

              section = section->next;
            }

          if (lan_session_limit_found)
            {
              datasize = sizeof (uint8_t) * user_count;
              
              if (!(state_data.lan_user_session_limit = (uint8_t *)malloc (datasize)))
                {
                  pstdout_perror (pstate,
                                  "malloc");
                  goto cleanup;
                }
              state_data.lan_user_session_limit_len = user_count;
              memset (state_data.lan_user_session_limit, '\0', datasize);
            }

          if (serial_session_limit_found)
            {
              datasize = sizeof (uint8_t) * user_count;
              
              if (!(state_data.serial_user_session_limit = (uint8_t *)malloc (datasize)))
                {
                  pstdout_perror (pstate,
                                  "malloc");
                  goto cleanup;
                }
              state_data.serial_user_session_limit_len = user_count;
              memset (state_data.serial_user_session_limit, '\0', datasize);
            }

          if (enable_user_found)
            {
              datasize = sizeof (bmc_config_enable_user_after_password_t) * user_count;
              
              if (!(state_data.enable_user_after_password = (bmc_config_enable_user_after_password_t *)malloc (datasize)))
                {
                  pstdout_perror (pstate,
                                  "malloc");
                  goto cleanup;
                }
              state_data.enable_user_after_password_len = user_count;
              memset (state_data.enable_user_after_password, '\0', datasize);
            }

          /* Third, store the info we care about */
          if (lan_session_limit_found 
              || serial_session_limit_found
              || lan_conf_auth_found)
            {
              section = sections;
              while (section)
                {
                  struct config_keyvalue *kv;
                  
                  if (stristr (section->section_name, "User"))
                    {
                      uint8_t userid;
                      
                      userid = atoi (section->section_name + strlen ("User"));
                      
                      if (userid < user_count)
                        {
                          if (lan_session_limit_found)
                            {
                              if ((kv = config_find_keyvalue (pstate,
                                                              section,
                                                              "Lan_Session_Limit")))
                                state_data.lan_user_session_limit[userid-1] = atoi (kv->value_input);
                            }
                             
                          if (serial_session_limit_found)
                            {
                              if ((kv = config_find_keyvalue (pstate,
                                                              section,
                                                              "Serial_Session_Limit")))
                                state_data.serial_user_session_limit[userid-1] = atoi (kv->value_input);
                            }
                        }
                    }

                  if (stristr (section->section_name, "Lan_Conf_Auth"))
                    {
                      if ((kv = config_find_keyvalue (pstate,
                                                      section,
                                                      "Callback_Enable_Auth_Type_OEM_Proprietary")))
                        {
                          state_data.lan_conf_auth_callback_level_oem_proprietary_set = 1;
                          state_data.lan_conf_auth_callback_level_oem_proprietary = same (kv->value_input, "yes");
                        }
                      
                      if ((kv = config_find_keyvalue (pstate,
                                                      section,
                                                      "User_Enable_Auth_Type_OEM_Proprietary")))
                        {
                          state_data.lan_conf_auth_user_level_oem_proprietary_set = 1;
                          state_data.lan_conf_auth_user_level_oem_proprietary = same (kv->value_input, "yes");
                        }
                      
                      if ((kv = config_find_keyvalue (pstate,
                                                      section,
                                                      "Operator_Enable_Auth_Type_OEM_Proprietary")))
                        {
                          state_data.lan_conf_auth_operator_level_oem_proprietary_set = 1;
                          state_data.lan_conf_auth_operator_level_oem_proprietary = same (kv->value_input, "yes");
                        }
                      
                      if ((kv = config_find_keyvalue (pstate,
                                                      section,
                                                      "Admin_Enable_Auth_Type_OEM_Proprietary")))
                        {
                          state_data.lan_conf_auth_admin_level_oem_proprietary_set = 1;
                          state_data.lan_conf_auth_admin_level_oem_proprietary = same (kv->value_input, "yes");
                        }
                      
                      if ((kv = config_find_keyvalue (pstate,
                                                      section,
                                                      "OEM_Enable_Auth_Type_None")))
                        {
                          state_data.lan_conf_auth_oem_level_none_set = 1;
                          state_data.lan_conf_auth_oem_level_none = same (kv->value_input, "yes");
                        }
                      
                      if ((kv = config_find_keyvalue (pstate,
                                                      section,
                                                      "OEM_Enable_Auth_Type_MD2")))
                        {
                          state_data.lan_conf_auth_oem_level_md2_set = 1;
                          state_data.lan_conf_auth_oem_level_md2 = same (kv->value_input, "yes");
                        }
                      
                      if ((kv = config_find_keyvalue (pstate,
                                                      section,
                                                      "OEM_Enable_Auth_Type_MD5")))
                        {
                          state_data.lan_conf_auth_oem_level_md5_set = 1;
                          state_data.lan_conf_auth_oem_level_md5 = same (kv->value_input, "yes");
                        }
                      
                      if ((kv = config_find_keyvalue (pstate,
                                                      section,
                                                      "OEM_Enable_Auth_Type_Straight_Password")))
                        {
                          state_data.lan_conf_auth_oem_level_straight_password_set = 1;
                          state_data.lan_conf_auth_oem_level_straight_password = same (kv->value_input, "yes");
                        }
                      
                      if ((kv = config_find_keyvalue (pstate,
                                                      section,
                                                      "OEM_Enable_Auth_Type_OEM_Proprietary")))
                        {
                          state_data.lan_conf_auth_oem_level_oem_proprietary_set = 1;
                          state_data.lan_conf_auth_oem_level_oem_proprietary = same (kv->value_input, "yes");
                        }
                    }
                  
                  section = section->next;
                }
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
              exit_code = EXIT_FAILURE;
              goto cleanup;
            }

          if (config_find_keyvalue(pstate,
                                   section,
                                   "MAC_Address"))
            {
              pstdout_fprintf(pstate,
                              stderr,
                              "Cannot configure Lan_Conf:MAC_Address on multiple hosts\n");
              exit_code = EXIT_FAILURE;
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
  if (state_data.lan_user_session_limit)
    free(state_data.lan_user_session_limit);
  if (state_data.serial_user_session_limit)
    free(state_data.serial_user_session_limit);
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
