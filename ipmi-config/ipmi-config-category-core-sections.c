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
#include <errno.h>
#include <assert.h>

#include "ipmi-config.h"
#include "ipmi-config-category-core-sections.h"
#include "ipmi-config-section.h"
#include "ipmi-config-utils.h"

#include "ipmi-config-category-core-lan-channel-section.h"
#include "ipmi-config-category-core-lan-conf-section.h"
#include "ipmi-config-category-core-lan-conf-auth-section.h"
#include "ipmi-config-category-core-lan-conf-security-keys-section.h"
#include "ipmi-config-category-core-lan-conf-misc-section.h"
#include "ipmi-config-category-core-lan-conf-user-security-section.h"
#include "ipmi-config-category-core-misc-section.h"
#include "ipmi-config-category-core-pef-conf-section.h"
#include "ipmi-config-category-core-rmcpplus-conf-privilege-section.h"
#include "ipmi-config-category-core-serial-channel-section.h"
#include "ipmi-config-category-core-serial-conf-section.h"
#include "ipmi-config-category-core-sol-conf-section.h"
#include "ipmi-config-category-core-user-sections.h"

#include "freeipmi-portability.h"
#include "pstdout.h"

static ipmi_config_err_t
_get_number_of_users (ipmi_config_state_data_t *state_data, uint8_t *number_of_users)
{
  fiid_obj_t obj_cmd_rs = NULL;
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;
  ipmi_config_err_t ret;
  uint64_t val;
  uint8_t lan_channel_number;

  assert (state_data);
  assert (number_of_users);

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_get_user_access_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  /* for the time being, we assume equal users per channel, so NULL for section_name */
  if ((ret = get_lan_channel_number (state_data, NULL, &lan_channel_number)) != IPMI_CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_get_user_access (state_data->ipmi_ctx,
                                lan_channel_number,
                                1, /* user_id number */
                                obj_cmd_rs) < 0)
    {
      if (!IPMI_CTX_ERRNUM_IS_FATAL_ERROR (state_data->ipmi_ctx))
        rv = IPMI_CONFIG_ERR_NON_FATAL_ERROR;

      if (rv == IPMI_CONFIG_ERR_FATAL_ERROR
	  || state_data->prog_data->args->common_args.debug)
        pstdout_fprintf (state_data->pstate,
                         stderr,
                         "ipmi_cmd_get_user_access: %s\n",
                         ipmi_ctx_errormsg (state_data->ipmi_ctx));

      goto cleanup;
    }

  if (FIID_OBJ_GET (obj_cmd_rs,
                    "max_channel_user_ids",
                    &val) < 0)
    {
      rv = IPMI_CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  (*number_of_users) = val;

  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

struct ipmi_config_section *
ipmi_config_core_sections_create (ipmi_config_state_data_t *state_data)
{
  struct ipmi_config_section *sections = NULL;
  struct ipmi_config_section *section = NULL;
  uint8_t number_of_users;
  unsigned int userindex;
  int channelindex;

  assert (state_data);

  if (load_lan_channel_numbers (state_data) == IPMI_CONFIG_ERR_FATAL_ERROR)
    return (NULL);
  
  if (load_serial_channel_numbers (state_data) == IPMI_CONFIG_ERR_FATAL_ERROR)
    return (NULL);

  if (load_sol_channel_numbers (state_data) == IPMI_CONFIG_ERR_FATAL_ERROR)
    return (NULL);

  if (_get_number_of_users (state_data, &number_of_users) != IPMI_CONFIG_ERR_SUCCESS)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "Unable to get Number of Users\n");
      return (NULL);
    }
 
  if (state_data->prog_data->args->verbose_count
      && state_data->lan_channel_numbers_count > 1)
    {
      state_data->lan_base_config_flags = IPMI_CONFIG_DO_NOT_CHECKOUT;
      state_data->lan_channel_config_flags = 0;
    }
  else
    {
      state_data->lan_base_config_flags = 0;
      state_data->lan_channel_config_flags = IPMI_CONFIG_DO_NOT_CHECKOUT;
    }

  if (state_data->prog_data->args->verbose_count
      && state_data->serial_channel_numbers_count > 1)
    {
      state_data->serial_base_config_flags = IPMI_CONFIG_DO_NOT_CHECKOUT;
      state_data->serial_channel_config_flags = 0;
    }
  else
    {
      state_data->serial_base_config_flags = 0;
      state_data->serial_channel_config_flags = IPMI_CONFIG_DO_NOT_CHECKOUT;
    }

  if (state_data->prog_data->args->verbose_count
      && state_data->sol_channel_numbers_unique_count > 1)
    {
      state_data->sol_base_config_flags = IPMI_CONFIG_DO_NOT_CHECKOUT;
      state_data->sol_channel_config_flags = 0;
    }
  else
    {
      state_data->sol_base_config_flags = 0;
      state_data->sol_channel_config_flags = IPMI_CONFIG_DO_NOT_CHECKOUT;
    }

  /* User Section(s) */

  for (userindex = 0; userindex < number_of_users; userindex++)
    {
      if (!(section = ipmi_config_core_user_section_get (state_data, userindex + 1)))
        goto cleanup;
      if (ipmi_config_section_append (&sections, section) < 0)
        goto cleanup;
    }

  /* Lan_Channel Section(s) */

  if (!(section = ipmi_config_core_lan_channel_section_get (state_data,
							    state_data->lan_base_config_flags,
							    -1)))
    goto cleanup;
  if (ipmi_config_section_append (&sections, section) < 0)
    goto cleanup;

  if (state_data->lan_channel_numbers_count > 1)
    {
      for (channelindex = 0; channelindex < state_data->lan_channel_numbers_count; channelindex++)
        {
          if (!(section = ipmi_config_core_lan_channel_section_get (state_data,
								    state_data->lan_channel_config_flags,
								    channelindex)))
            goto cleanup;
          if (ipmi_config_section_append (&sections, section) < 0)
            goto cleanup;
        }
    }

  /* Lan_Conf Section(s) */

  if (!(section = ipmi_config_core_lan_conf_section_get (state_data,
							 state_data->lan_base_config_flags,
							 -1)))
    goto cleanup;
  if (ipmi_config_section_append (&sections, section) < 0)
    goto cleanup;

  if (state_data->lan_channel_numbers_count > 1)
    {
      for (channelindex = 0; channelindex < state_data->lan_channel_numbers_count; channelindex++)
        {
          if (!(section = ipmi_config_core_lan_conf_section_get (state_data,
								 state_data->lan_channel_config_flags,
								 channelindex)))
            goto cleanup;
          if (ipmi_config_section_append (&sections, section) < 0)
            goto cleanup;
        }
    }

  /* Lan_Conf_Auth Section(s) */

  if (!(section = ipmi_config_core_lan_conf_auth_section_get (state_data,
							      state_data->lan_base_config_flags,
							      -1)))
    goto cleanup;
  if (ipmi_config_section_append (&sections, section) < 0)
    goto cleanup;

  if (state_data->lan_channel_numbers_count > 1)
    {
      for (channelindex = 0; channelindex < state_data->lan_channel_numbers_count; channelindex++)
        {
          if (!(section = ipmi_config_core_lan_conf_auth_section_get (state_data,
								      state_data->lan_channel_config_flags,
								      channelindex)))
            goto cleanup;
          if (ipmi_config_section_append (&sections, section) < 0)
            goto cleanup;
        }
    }

  /* Lan_Conf_Security_Keys Section(s) */

  if (!(section = ipmi_config_core_lan_conf_security_keys_section_get (state_data,
								       state_data->lan_base_config_flags,
								       -1)))
    goto cleanup;
  if (ipmi_config_section_append (&sections, section) < 0)
    goto cleanup;

  if (state_data->lan_channel_numbers_count > 1)
    {
      for (channelindex = 0; channelindex < state_data->lan_channel_numbers_count; channelindex++)
        {
          if (!(section = ipmi_config_core_lan_conf_security_keys_section_get (state_data,
									       state_data->lan_channel_config_flags,
									       channelindex)))
            goto cleanup;
          if (ipmi_config_section_append (&sections, section) < 0)
            goto cleanup;
        }
    }

  /* Lan_Conf_User_Security Section(s) */

  if (!(section = ipmi_config_core_lan_conf_user_security_section_get (state_data,
								       state_data->lan_base_config_flags,
								       -1)))
    goto cleanup;
  if (ipmi_config_section_append (&sections, section) < 0)
    goto cleanup;

  if (state_data->lan_channel_numbers_count > 1)
    {
      for (channelindex = 0; channelindex < state_data->lan_channel_numbers_count; channelindex++)
        {
          if (!(section = ipmi_config_core_lan_conf_user_security_section_get (state_data,
									       state_data->lan_channel_config_flags,
									       channelindex)))
            goto cleanup;
          if (ipmi_config_section_append (&sections, section) < 0)
            goto cleanup;
        }
    }

  /* Lan_Conf_Misc Section(s) */

  if (!(section = ipmi_config_core_lan_conf_misc_section_get (state_data,
							      state_data->lan_base_config_flags,
							      -1)))
    goto cleanup;
  if (ipmi_config_section_append (&sections, section) < 0)
    goto cleanup;

  if (state_data->lan_channel_numbers_count > 1)
    {
      for (channelindex = 0; channelindex < state_data->lan_channel_numbers_count; channelindex++)
        {
          if (!(section = ipmi_config_core_lan_conf_misc_section_get (state_data,
								      state_data->lan_channel_config_flags,
								      channelindex)))
            goto cleanup;
          if (ipmi_config_section_append (&sections, section) < 0)
            goto cleanup;
        }
    }

  /* Rmcpplus_Conf_Privilege Section(s) */
 
  if (!(section = ipmi_config_core_rmcpplus_conf_privilege_section_get (state_data,
									state_data->lan_base_config_flags,
									-1)))
    goto cleanup;
  if (ipmi_config_section_append (&sections, section) < 0)
    goto cleanup;

  if (state_data->lan_channel_numbers_count > 1)
    {
      for (channelindex = 0; channelindex < state_data->lan_channel_numbers_count; channelindex++)
        {
          if (!(section = ipmi_config_core_rmcpplus_conf_privilege_section_get (state_data,
										state_data->lan_channel_config_flags,
										channelindex)))
            goto cleanup;
          if (ipmi_config_section_append (&sections, section) < 0)
            goto cleanup;
        }
    }

  /* Serial_Channel Section(s) */

  if (!(section = ipmi_config_core_serial_channel_section_get (state_data,
							       state_data->serial_base_config_flags,
							       -1)))
    goto cleanup;
  if (ipmi_config_section_append (&sections, section) < 0)
    goto cleanup;

  if (state_data->serial_channel_numbers_count > 1)
    {
      for (channelindex = 0; channelindex < state_data->serial_channel_numbers_count; channelindex++)
        {
          if (!(section = ipmi_config_core_serial_channel_section_get (state_data,
								       state_data->serial_channel_config_flags,
								       channelindex)))
            goto cleanup;
          if (ipmi_config_section_append (&sections, section) < 0)
            goto cleanup;
        }
    }

  /* Serial_Conf Section(s) */

  if (!(section = ipmi_config_core_serial_conf_section_get (state_data,
							    state_data->serial_base_config_flags,
							    -1)))
    goto cleanup;
  if (ipmi_config_section_append (&sections, section) < 0)
    goto cleanup;

  if (state_data->serial_channel_numbers_count > 1)
    {
      for (channelindex = 0; channelindex < state_data->serial_channel_numbers_count; channelindex++)
        {
          if (!(section = ipmi_config_core_serial_conf_section_get (state_data,
								    state_data->serial_channel_config_flags,
								    channelindex)))
            goto cleanup;
          if (ipmi_config_section_append (&sections, section) < 0)
            goto cleanup;
        }
    }

  /* PEF Conf Section */

  if (!(section = ipmi_config_core_pef_conf_section_get (state_data)))
    goto cleanup;
  if (ipmi_config_section_append (&sections, section) < 0)
    goto cleanup;

  /* SOL_Conf Section(s) */

  if (!(section = ipmi_config_core_sol_conf_section_get (state_data,
							 state_data->sol_base_config_flags,
							 -1)))
    goto cleanup;
  if (ipmi_config_section_append (&sections, section) < 0)
    goto cleanup;

  if (state_data->sol_channel_numbers_unique_count > 1)
    {
      for (channelindex = 0; channelindex < state_data->sol_channel_numbers_unique_count; channelindex++)
        {
          if (!(section = ipmi_config_core_sol_conf_section_get (state_data,
								 state_data->sol_channel_config_flags,
								 channelindex)))
            goto cleanup;
          if (ipmi_config_section_append (&sections, section) < 0)
            goto cleanup;
        }
    }
  
  /* Misc Section */

  if (!(section = ipmi_config_core_misc_section_get (state_data)))
    goto cleanup;
  if (ipmi_config_section_append (&sections, section) < 0)
    goto cleanup;

  return (sections);

 cleanup:
  ipmi_config_sections_destroy (sections);
  return (NULL);
}
