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

#include "ipmi-config.h"
#include "ipmi-config-map.h"
#include "ipmi-config-section.h"
#include "ipmi-config-utils.h"
#include "ipmi-config-validate.h"

#include "freeipmi-portability.h"
#include "pstdout.h"

/* convenience struct */
struct bad_password_threshold
{
  uint8_t user_disabled_event_message;
  uint8_t bad_password_threshold_number;
  uint16_t attempt_count_reset_interval;
  uint16_t user_lockout_interval;
};

static ipmi_config_err_t
_get_bad_password_threshold (ipmi_config_state_data_t *state_data,
                             const char *section_name,
                             struct bad_password_threshold *bpt)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;
  ipmi_config_err_t ret;
  uint8_t channel_number;

  assert (state_data);
  assert (section_name);
  assert (bpt);

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_get_lan_configuration_parameters_bad_password_threshold_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }
  
  if ((ret = get_lan_channel_number (state_data, section_name, &channel_number)) != IPMI_CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }
  
  if (ipmi_cmd_get_lan_configuration_parameters_bad_password_threshold (state_data->ipmi_ctx,
                                                                        channel_number,
                                                                        IPMI_GET_LAN_PARAMETER,
                                                                        IPMI_LAN_CONFIGURATION_PARAMETERS_NO_SET_SELECTOR,
                                                                        IPMI_LAN_CONFIGURATION_PARAMETERS_NO_BLOCK_SELECTOR,
                                                                        obj_cmd_rs) < 0)
    {
      if (ipmi_config_param_errnum_is_non_fatal (state_data,
                                                 obj_cmd_rs,
                                                 &ret))
        rv = ret;
      
      if (rv == IPMI_CONFIG_ERR_FATAL_ERROR
	  || state_data->prog_data->args->common_args.debug)
        pstdout_fprintf (state_data->pstate,
                         stderr,
                         "ipmi_cmd_get_lan_configuration_parameters_bad_password_threshold: %s\n",
                         ipmi_ctx_errormsg (state_data->ipmi_ctx));

      goto cleanup;
    }
  
  if (FIID_OBJ_GET (obj_cmd_rs, "user_disabled_event_message", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'user_disabled_event_message': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  bpt->user_disabled_event_message = val;

  if (FIID_OBJ_GET (obj_cmd_rs, "bad_password_threshold_number", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'bad_password_threshold_number': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  bpt->bad_password_threshold_number = val;

  if (FIID_OBJ_GET (obj_cmd_rs, "attempt_count_reset_interval", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'attempt_count_reset_interval': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  bpt->attempt_count_reset_interval = val;

  if (FIID_OBJ_GET (obj_cmd_rs, "user_lockout_interval", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'user_lockout_interval': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  bpt->user_lockout_interval = val;

  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static ipmi_config_err_t
_set_bad_password_threshold (ipmi_config_state_data_t *state_data,
                             const char *section_name,
                             struct bad_password_threshold *bpt)
{
  fiid_obj_t obj_cmd_rs = NULL;
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;
  ipmi_config_err_t ret;
  uint8_t channel_number;

  assert (state_data);
  assert (section_name);
  assert (bpt);

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_set_lan_configuration_parameters_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if ((ret = get_lan_channel_number (state_data, section_name, &channel_number)) != IPMI_CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_set_lan_configuration_parameters_bad_password_threshold (state_data->ipmi_ctx,
                                                                        channel_number,
                                                                        bpt->user_disabled_event_message,
                                                                        bpt->bad_password_threshold_number,
                                                                        bpt->attempt_count_reset_interval,
                                                                        bpt->user_lockout_interval,
                                                                        obj_cmd_rs) < 0)
    {
      if (ipmi_config_param_errnum_is_non_fatal (state_data,
                                                 obj_cmd_rs,
                                                 &ret))
        rv = ret;

      if (rv == IPMI_CONFIG_ERR_FATAL_ERROR
	  || state_data->prog_data->args->common_args.debug)
        pstdout_fprintf (state_data->pstate,
                         stderr,
                         "ipmi_cmd_set_lan_configuration_parameters_bad_password_threshold: %s\n",
                         ipmi_ctx_errormsg (state_data->ipmi_ctx));

      goto cleanup;
    }

  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);

}

static ipmi_config_err_t
bad_password_threshold_checkout (ipmi_config_state_data_t *state_data,
				 const char *section_name,
                                 struct ipmi_config_keyvalue *kv)
{
  struct bad_password_threshold bpt;
  ipmi_config_err_t ret;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if ((ret = _get_bad_password_threshold (state_data, section_name, &bpt)) != IPMI_CONFIG_ERR_SUCCESS)
    return (ret);
  
  if (ipmi_config_section_update_keyvalue_output_unsigned_int (state_data,
                                                               kv,
                                                               bpt.bad_password_threshold_number) < 0)
    return (IPMI_CONFIG_ERR_FATAL_ERROR);
  
  return (IPMI_CONFIG_ERR_SUCCESS);
}

static ipmi_config_err_t
bad_password_threshold_commit (ipmi_config_state_data_t *state_data,
			       const char *section_name,
                               const struct ipmi_config_keyvalue *kv)
{
  struct bad_password_threshold bpt;
  ipmi_config_err_t ret;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if ((ret = _get_bad_password_threshold (state_data, section_name, &bpt)) != IPMI_CONFIG_ERR_SUCCESS)
    return (ret);
  
  bpt.bad_password_threshold_number = atoi (kv->value_input);
  return (_set_bad_password_threshold (state_data, section_name, &bpt));
}

static ipmi_config_err_t
attempt_count_reset_interval_checkout (ipmi_config_state_data_t *state_data,
				       const char *section_name,
                                       struct ipmi_config_keyvalue *kv)
{
  struct bad_password_threshold bpt;
  ipmi_config_err_t ret;
  
  assert (state_data);
  assert (section_name);
  assert (kv);

  if ((ret = _get_bad_password_threshold (state_data, section_name, &bpt)) != IPMI_CONFIG_ERR_SUCCESS)
    return (ret);
  
  if (ipmi_config_section_update_keyvalue_output_unsigned_int (state_data,
                                                               kv,
                                                               bpt.attempt_count_reset_interval) < 0)
    return (IPMI_CONFIG_ERR_FATAL_ERROR);
  
  return (IPMI_CONFIG_ERR_SUCCESS);
}

static ipmi_config_err_t
attempt_count_reset_interval_commit (ipmi_config_state_data_t *state_data,
				     const char *section_name,
                                     const struct ipmi_config_keyvalue *kv)
{
  struct bad_password_threshold bpt;
  ipmi_config_err_t ret;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if ((ret = _get_bad_password_threshold (state_data, section_name, &bpt)) != IPMI_CONFIG_ERR_SUCCESS)
    return (ret);
  
  bpt.attempt_count_reset_interval = atoi (kv->value_input);
  return (_set_bad_password_threshold (state_data, section_name, &bpt));
}

static ipmi_config_err_t
user_lockout_interval_checkout (ipmi_config_state_data_t *state_data,
				const char *section_name,
                                struct ipmi_config_keyvalue *kv)
{
  struct bad_password_threshold bpt;
  ipmi_config_err_t ret;
  
  assert (state_data);
  assert (section_name);
  assert (kv);

  if ((ret = _get_bad_password_threshold (state_data, section_name, &bpt)) != IPMI_CONFIG_ERR_SUCCESS)
    return (ret);
  
  if (ipmi_config_section_update_keyvalue_output_unsigned_int (state_data,
                                                               kv,
                                                               bpt.user_lockout_interval) < 0)
    return (IPMI_CONFIG_ERR_FATAL_ERROR);
  
  return (IPMI_CONFIG_ERR_SUCCESS);
}

static ipmi_config_err_t
user_lockout_interval_commit (ipmi_config_state_data_t *state_data,
			      const char *section_name,
                              const struct ipmi_config_keyvalue *kv)
{
  struct bad_password_threshold bpt;
  ipmi_config_err_t ret;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if ((ret = _get_bad_password_threshold (state_data, section_name, &bpt)) != IPMI_CONFIG_ERR_SUCCESS)
    return (ret);
  
  bpt.user_lockout_interval = atoi (kv->value_input);
  return (_set_bad_password_threshold (state_data, section_name, &bpt));
}

static ipmi_config_err_t
enable_event_message_when_user_disabled_checkout (ipmi_config_state_data_t *state_data,
						  const char *section_name,
                                                  struct ipmi_config_keyvalue *kv)
{
  struct bad_password_threshold bpt;
  ipmi_config_err_t ret;
  
  assert (state_data);
  assert (section_name);
  assert (kv);

  if ((ret = _get_bad_password_threshold (state_data, section_name, &bpt)) != IPMI_CONFIG_ERR_SUCCESS)
    return (ret);
  
  if (ipmi_config_section_update_keyvalue_output (state_data,
                                                  kv,
                                                  bpt.user_disabled_event_message ? "Yes" : "No") < 0)
    return (IPMI_CONFIG_ERR_FATAL_ERROR);
  
  return (IPMI_CONFIG_ERR_SUCCESS);
}

static ipmi_config_err_t
enable_event_message_when_user_disabled_commit (ipmi_config_state_data_t *state_data,
						const char *section_name,
                                                const struct ipmi_config_keyvalue *kv)
{
  struct bad_password_threshold bpt;
  ipmi_config_err_t ret;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if ((ret = _get_bad_password_threshold (state_data, section_name, &bpt)) != IPMI_CONFIG_ERR_SUCCESS)
    return (ret);
  
  bpt.user_disabled_event_message = same (kv->value_input, "yes");
  return (_set_bad_password_threshold (state_data, section_name, &bpt));
}

struct ipmi_config_section *
ipmi_config_core_lan_conf_user_security_section_get (ipmi_config_state_data_t *state_data,
						     unsigned int config_flags,
						     int channel_index)
{
  struct ipmi_config_section *section = NULL;
  char *section_comment =
    "The following user security configuration options are optionally "
    "implemented by the vendor.  They may not be available your system and "
    "may not be visible below."
    "\n"
    "The following configuration supports the ability for the BMC to "
    "disable a user if a number of bad passwords are entered sequentially. "
    "\"Bad_Password_Threshold\" determines the number of bad passwords that "
    "must be entered sequentially.  \"Attempt_Count_Reset_Interval\" determines "
    "the range of time the bad passwords must occur in.  \"User_Lockout_Interval\" "
    "determines the time a user will be locked off if the bad password "
    "threshold is reached.  If set to \"Yes\", \"Enable_Event_Message_When_User_Disabled\" "
    "will inform the BMC to log an event message when a user is disabled.";
  char *section_name_base_str = "Lan_Conf_User_Security";

  assert (state_data);

  if (!(section = ipmi_config_section_multi_channel_create (state_data,
                                                            section_name_base_str,
                                                            section_comment,
                                                            NULL,
                                                            NULL,
                                                            config_flags,
                                                            channel_index,
                                                            state_data->lan_channel_numbers,
                                                            state_data->lan_channel_numbers_count)))
    goto cleanup;

  if (ipmi_config_section_add_key (state_data,
                                   section,
                                   "Bad_Password_Threshold",
                                   "Possible values: 0-255, 0 indicates no limit",
                                   0,
                                   bad_password_threshold_checkout,
                                   bad_password_threshold_commit,
                                   number_range_one_byte_validate) < 0)
    goto cleanup;

  if (ipmi_config_section_add_key (state_data,
                                   section,
                                   "Attempt_Count_Reset_Interval",
                                   "Possible values: 0-65535, in 10 second increments (e.g. 2 = 20 sec)\n"
                                   "                 0 indicates no interval (i.e. don't reset counter)",
                                   0,
                                   attempt_count_reset_interval_checkout,
                                   attempt_count_reset_interval_commit,
                                   number_range_two_bytes_validate) < 0)
    goto cleanup;

  if (ipmi_config_section_add_key (state_data,
                                   section,
                                   "User_Lockout_Interval",
                                   "Possible values: 0-65535, in 10 second increments (e.g. 2 = 20 sec)\n"
                                   "                 0 indicates no interval (i.e. don't re-enable user)",
                                   0,
                                   user_lockout_interval_checkout,
                                   user_lockout_interval_commit,
                                   number_range_two_bytes_validate) < 0)
    goto cleanup;

  if (ipmi_config_section_add_key (state_data,
                                   section,
                                   "Enable_Event_Message_When_User_Disabled",
                                   "Possible values: Yes/No",
                                   0,
                                   enable_event_message_when_user_disabled_checkout,
                                   enable_event_message_when_user_disabled_commit,
                                   yes_no_validate) < 0)
    goto cleanup;

  return (section);

 cleanup:
  if (section)
    ipmi_config_section_destroy (section);
  return (NULL);
}
