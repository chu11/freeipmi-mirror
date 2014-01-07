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

/* convenience structs */

struct sol_authentication
{
  uint8_t sol_privilege_level;
  uint8_t force_sol_payload_authentication;
  uint8_t force_sol_payload_encryption;
};

struct interval_and_threshold {
  uint8_t character_accumulate_interval;
  uint8_t character_send_threshold;
};

struct sol_retry {
  uint8_t retry_count;
  uint8_t retry_interval;
};

static ipmi_config_err_t
enable_sol_checkout (ipmi_config_state_data_t *state_data,
		     const char *section_name,
                     struct ipmi_config_keyvalue *kv)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;
  ipmi_config_err_t ret;
  uint8_t channel_number;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_get_sol_configuration_parameters_sol_enable_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if ((ret = get_sol_channel_number (state_data, section_name, &channel_number)) != IPMI_CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_get_sol_configuration_parameters_sol_enable (state_data->ipmi_ctx,
                                                            channel_number,
                                                            IPMI_GET_SOL_PARAMETER,
                                                            IPMI_SOL_CONFIGURATION_PARAMETERS_NO_SET_SELECTOR,
                                                            IPMI_SOL_CONFIGURATION_PARAMETERS_NO_BLOCK_SELECTOR,
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
                         "ipmi_cmd_get_sol_configuration_parameters_sol_enable: %s\n",
                         ipmi_ctx_errormsg (state_data->ipmi_ctx));

      goto cleanup;
    }

  if (FIID_OBJ_GET (obj_cmd_rs, "sol_enable", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'sol_enable': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }

  if (ipmi_config_section_update_keyvalue_output (state_data,
                                                  kv,
                                                  val ? "Yes" : "No") < 0)
    return (IPMI_CONFIG_ERR_FATAL_ERROR);

  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static ipmi_config_err_t
enable_sol_commit (ipmi_config_state_data_t *state_data,
		   const char *section_name,
                   const struct ipmi_config_keyvalue *kv)
{
  fiid_obj_t obj_cmd_rs = NULL;
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;
  ipmi_config_err_t ret;
  uint8_t channel_number;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_set_sol_configuration_parameters_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if ((ret = get_sol_channel_number (state_data, section_name, &channel_number)) != IPMI_CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_set_sol_configuration_parameters_sol_enable (state_data->ipmi_ctx,
                                                            channel_number,
                                                            same (kv->value_input, "yes"),
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
                         "ipmi_cmd_set_sol_configuration_parameters_sol_enable: %s\n",
                         ipmi_ctx_errormsg (state_data->ipmi_ctx));

      goto cleanup;
    }

  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static ipmi_config_err_t
_get_sol_sol_authentication (ipmi_config_state_data_t *state_data,
                             const char *section_name,
                             struct sol_authentication *sa)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;
  ipmi_config_err_t ret;
  uint8_t channel_number;

  assert (state_data);
  assert (section_name);
  assert (sa);

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_get_sol_configuration_parameters_sol_authentication_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if ((ret = get_sol_channel_number (state_data, section_name, &channel_number)) != IPMI_CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_get_sol_configuration_parameters_sol_authentication (state_data->ipmi_ctx,
                                                                    channel_number,
                                                                    IPMI_GET_SOL_PARAMETER,
                                                                    IPMI_SOL_CONFIGURATION_PARAMETERS_NO_SET_SELECTOR,
                                                                    IPMI_SOL_CONFIGURATION_PARAMETERS_NO_BLOCK_SELECTOR,
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
                         "ipmi_cmd_get_sol_configuration_parameters_sol_authentication: %s\n",
                         ipmi_ctx_errormsg (state_data->ipmi_ctx));

      goto cleanup;
    }

  if (FIID_OBJ_GET (obj_cmd_rs, "sol_privilege_level", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'sol_privilege_level': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  sa->sol_privilege_level = val;

  if (FIID_OBJ_GET (obj_cmd_rs, "force_sol_payload_authentication", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'force_sol_payload_authentication': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  sa->force_sol_payload_authentication = val;

  if (FIID_OBJ_GET (obj_cmd_rs, "force_sol_payload_encryption", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'force_sol_payload_encryption': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  sa->force_sol_payload_encryption = val;

  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static ipmi_config_err_t
_set_sol_sol_authentication (ipmi_config_state_data_t *state_data,
                             const char *section_name,
                             struct sol_authentication *sa)
{
  fiid_obj_t obj_cmd_rs = NULL;
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;
  ipmi_config_err_t ret;
  uint8_t channel_number;

  assert (state_data);
  assert (section_name);
  assert (sa);

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_set_sol_configuration_parameters_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if ((ret = get_sol_channel_number (state_data, section_name, &channel_number)) != IPMI_CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_set_sol_configuration_parameters_sol_authentication (state_data->ipmi_ctx,
                                                                    channel_number,
                                                                    sa->sol_privilege_level,
                                                                    sa->force_sol_payload_authentication,
                                                                    sa->force_sol_payload_encryption,
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
                         "ipmi_cmd_set_sol_configuration_parameters_sol_authentication: %s\n",
                         ipmi_ctx_errormsg (state_data->ipmi_ctx));

      goto cleanup;
    }

  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static ipmi_config_err_t
sol_privilege_level_checkout (ipmi_config_state_data_t *state_data,
			      const char *section_name,
                              struct ipmi_config_keyvalue *kv)
{
  struct sol_authentication sa;
  ipmi_config_err_t ret;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if ((ret = _get_sol_sol_authentication (state_data, section_name, &sa)) != IPMI_CONFIG_ERR_SUCCESS)
    return (ret);

  if (ipmi_config_section_update_keyvalue_output (state_data,
                                                  kv,
                                                  privilege_level_string (sa.sol_privilege_level)) < 0)
    return (IPMI_CONFIG_ERR_FATAL_ERROR);

  return (IPMI_CONFIG_ERR_SUCCESS);
}

static ipmi_config_err_t
sol_privilege_level_commit (ipmi_config_state_data_t *state_data,
			    const char *section_name,
                            const struct ipmi_config_keyvalue *kv)
{
  struct sol_authentication sa;
  ipmi_config_err_t ret;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if ((ret = _get_sol_sol_authentication (state_data, section_name, &sa)) != IPMI_CONFIG_ERR_SUCCESS)
    return (ret);

  sa.sol_privilege_level = privilege_level_number (kv->value_input);

  return (_set_sol_sol_authentication (state_data, section_name, &sa));
}

static ipmi_config_err_t
force_sol_payload_authentication_checkout (ipmi_config_state_data_t *state_data,
					   const char *section_name,
                                           struct ipmi_config_keyvalue *kv)
{
  struct sol_authentication sa;
  ipmi_config_err_t ret;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if ((ret = _get_sol_sol_authentication (state_data, section_name, &sa)) != IPMI_CONFIG_ERR_SUCCESS)
    return (ret);

  if (ipmi_config_section_update_keyvalue_output (state_data,
                                                  kv,
                                                  sa.force_sol_payload_authentication ? "Yes" : "No") < 0)
    return (IPMI_CONFIG_ERR_FATAL_ERROR);

  return (IPMI_CONFIG_ERR_SUCCESS);
}

static ipmi_config_err_t
force_sol_payload_authentication_commit (ipmi_config_state_data_t *state_data,
					 const char *section_name,
                                         const struct ipmi_config_keyvalue *kv)
{
  struct sol_authentication sa;
  ipmi_config_err_t ret;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if ((ret = _get_sol_sol_authentication (state_data, section_name, &sa)) != IPMI_CONFIG_ERR_SUCCESS)
    return (ret);

  sa.force_sol_payload_authentication = same (kv->value_input, "yes") ? 1 : 0;

  return (_set_sol_sol_authentication (state_data, section_name, &sa));
}

static ipmi_config_err_t
force_sol_payload_encryption_checkout (ipmi_config_state_data_t *state_data,
				       const char *section_name,
                                       struct ipmi_config_keyvalue *kv)
{
  struct sol_authentication sa;
  ipmi_config_err_t ret;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if ((ret = _get_sol_sol_authentication (state_data, section_name, &sa)) != IPMI_CONFIG_ERR_SUCCESS)
    return (ret);

  if (ipmi_config_section_update_keyvalue_output (state_data,
                                                  kv,
                                                  sa.force_sol_payload_encryption ? "Yes" : "No") < 0)
    return (IPMI_CONFIG_ERR_FATAL_ERROR);

  return (IPMI_CONFIG_ERR_SUCCESS);
}

static ipmi_config_err_t
force_sol_payload_encryption_commit (ipmi_config_state_data_t *state_data,
				     const char *section_name,
                                     const struct ipmi_config_keyvalue *kv)
{
  struct sol_authentication sa;
  ipmi_config_err_t ret;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if ((ret = _get_sol_sol_authentication (state_data, section_name, &sa)) != IPMI_CONFIG_ERR_SUCCESS)
    return (ret);

  sa.force_sol_payload_encryption = same (kv->value_input, "yes") ? 1 : 0;

  return (_set_sol_sol_authentication (state_data, section_name, &sa));
}

static ipmi_config_err_t
_get_sol_character_accumulate_interval_and_send_threshold (ipmi_config_state_data_t *state_data,
                                                           const char *section_name,
                                                           struct interval_and_threshold *it)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;
  ipmi_config_err_t ret;
  uint8_t channel_number;

  assert (state_data);
  assert (section_name);
  assert (it);

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_get_sol_configuration_parameters_character_accumulate_interval_and_send_threshold_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if ((ret = get_sol_channel_number (state_data, section_name, &channel_number)) != IPMI_CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_get_sol_configuration_parameters_character_accumulate_interval_and_send_threshold (state_data->ipmi_ctx,
                                                                                                  channel_number,
                                                                                                  IPMI_GET_SOL_PARAMETER,
                                                                                                  IPMI_SOL_CONFIGURATION_PARAMETERS_NO_SET_SELECTOR,
                                                                                                  IPMI_SOL_CONFIGURATION_PARAMETERS_NO_BLOCK_SELECTOR,
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
                         "ipmi_cmd_get_sol_configuration_parameters_character_accumulate_interval_and_send_threshold: %s\n",
                         ipmi_ctx_errormsg (state_data->ipmi_ctx));

      goto cleanup;
    }

  if (FIID_OBJ_GET (obj_cmd_rs, "character_accumulate_interval", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'character_accumulate_interval': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  it->character_accumulate_interval = val;

  if (FIID_OBJ_GET (obj_cmd_rs, "character_send_threshold", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'character_send_threshold': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  it->character_send_threshold = val;

  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static ipmi_config_err_t
_set_sol_character_accumulate_interval_and_send_threshold (ipmi_config_state_data_t *state_data,
                                                           const char *section_name,
                                                           struct interval_and_threshold *it)
{
  fiid_obj_t obj_cmd_rs = NULL;
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;
  ipmi_config_err_t ret;
  uint8_t channel_number;

  assert (state_data);
  assert (section_name);
  assert (it);

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_set_sol_configuration_parameters_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if ((ret = get_sol_channel_number (state_data, section_name, &channel_number)) != IPMI_CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_set_sol_configuration_parameters_character_accumulate_interval_and_send_threshold (state_data->ipmi_ctx,
                                                                                                  channel_number,
                                                                                                  it->character_accumulate_interval,
                                                                                                  it->character_send_threshold,
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
                         "ipmi_cmd_set_sol_configuration_parameters_character_accumulate_interval_and_send_threshold: %s\n",
                         ipmi_ctx_errormsg (state_data->ipmi_ctx));

      goto cleanup;
    }

  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static ipmi_config_err_t
character_accumulate_interval_checkout (ipmi_config_state_data_t *state_data,
					const char *section_name,
                                        struct ipmi_config_keyvalue *kv)
{
  struct interval_and_threshold it;
  ipmi_config_err_t ret;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if ((ret = _get_sol_character_accumulate_interval_and_send_threshold (state_data, section_name, &it)) != IPMI_CONFIG_ERR_SUCCESS)
    return (ret);

  if (ipmi_config_section_update_keyvalue_output_unsigned_int (state_data,
                                                               kv,
                                                               it.character_accumulate_interval) < 0)
    return (IPMI_CONFIG_ERR_FATAL_ERROR);

  return (IPMI_CONFIG_ERR_SUCCESS);
}

static ipmi_config_err_t
character_accumulate_interval_commit (ipmi_config_state_data_t *state_data,
				      const char *section_name,
                                      const struct ipmi_config_keyvalue *kv)
{
  struct interval_and_threshold it;
  ipmi_config_err_t ret;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if ((ret = _get_sol_character_accumulate_interval_and_send_threshold (state_data, section_name, &it)) != IPMI_CONFIG_ERR_SUCCESS)
    return (ret);

  it.character_accumulate_interval = atoi (kv->value_input);

  return (_set_sol_character_accumulate_interval_and_send_threshold (state_data, section_name, &it));
}

static ipmi_config_err_t
character_send_threshold_checkout (ipmi_config_state_data_t *state_data,
				   const char *section_name,
                                   struct ipmi_config_keyvalue *kv)
{
  struct interval_and_threshold it;
  ipmi_config_err_t ret;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if ((ret = _get_sol_character_accumulate_interval_and_send_threshold (state_data, section_name, &it)) != IPMI_CONFIG_ERR_SUCCESS)
    return (ret);

  if (ipmi_config_section_update_keyvalue_output_unsigned_int (state_data,
                                                               kv,
                                                               it.character_send_threshold) < 0)
    return (IPMI_CONFIG_ERR_FATAL_ERROR);

  return (IPMI_CONFIG_ERR_SUCCESS);
}

static ipmi_config_err_t
character_send_threshold_commit (ipmi_config_state_data_t *state_data,
				 const char *section_name,
                                 const struct ipmi_config_keyvalue *kv)
{
  struct interval_and_threshold it;
  ipmi_config_err_t ret;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if ((ret = _get_sol_character_accumulate_interval_and_send_threshold (state_data, section_name, &it)) != IPMI_CONFIG_ERR_SUCCESS)
    return (ret);

  it.character_send_threshold = atoi (kv->value_input);

  return (_set_sol_character_accumulate_interval_and_send_threshold (state_data, section_name, &it));
}

static ipmi_config_err_t
_get_sol_sol_retry (ipmi_config_state_data_t *state_data,
                    const char *section_name,
                    struct sol_retry *sr)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;
  ipmi_config_err_t ret;
  uint8_t channel_number;

  assert (state_data);
  assert (section_name);
  assert (sr);

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_get_sol_configuration_parameters_sol_retry_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if ((ret = get_sol_channel_number (state_data, section_name, &channel_number)) != IPMI_CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_get_sol_configuration_parameters_sol_retry (state_data->ipmi_ctx,
                                                           channel_number,
                                                           IPMI_GET_SOL_PARAMETER,
                                                           IPMI_SOL_CONFIGURATION_PARAMETERS_NO_SET_SELECTOR,
                                                           IPMI_SOL_CONFIGURATION_PARAMETERS_NO_BLOCK_SELECTOR,
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
                         "ipmi_cmd_get_sol_configuration_parameters_sol_retry: %s\n",
                         ipmi_ctx_errormsg (state_data->ipmi_ctx));

      goto cleanup;
    }

  if (FIID_OBJ_GET (obj_cmd_rs, "retry_count", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'retry_count': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  sr->retry_count = val;

  if (FIID_OBJ_GET (obj_cmd_rs, "retry_interval", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'retry_interval': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  sr->retry_interval = val;

  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static ipmi_config_err_t
_set_sol_sol_retry (ipmi_config_state_data_t *state_data,
                    const char *section_name,
                    struct sol_retry *sr)
{
  fiid_obj_t obj_cmd_rs = NULL;
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;
  ipmi_config_err_t ret;
  uint8_t channel_number;

  assert (state_data);
  assert (section_name);
  assert (sr);

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_set_sol_configuration_parameters_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if ((ret = get_sol_channel_number (state_data, section_name, &channel_number)) != IPMI_CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_set_sol_configuration_parameters_sol_retry (state_data->ipmi_ctx,
                                                           channel_number,
                                                           sr->retry_count,
                                                           sr->retry_interval,
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
                         "ipmi_cmd_set_sol_configuration_parameters_sol_retry: %s\n",
                         ipmi_ctx_errormsg (state_data->ipmi_ctx));

      goto cleanup;
    }

  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static ipmi_config_err_t
sol_retry_count_checkout (ipmi_config_state_data_t *state_data,
			  const char *section_name,
                          struct ipmi_config_keyvalue *kv)
{
  struct sol_retry sr;
  ipmi_config_err_t ret;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if ((ret = _get_sol_sol_retry (state_data, section_name, &sr)) != IPMI_CONFIG_ERR_SUCCESS)
    return (ret);

  if (ipmi_config_section_update_keyvalue_output_unsigned_int (state_data,
                                                               kv,
                                                               sr.retry_count) < 0)
    return (IPMI_CONFIG_ERR_FATAL_ERROR);

  return (IPMI_CONFIG_ERR_SUCCESS);
}


static ipmi_config_err_t
sol_retry_count_commit (ipmi_config_state_data_t *state_data,
			const char *section_name,
                        const struct ipmi_config_keyvalue *kv)
{
  struct sol_retry sr;
  ipmi_config_err_t ret;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if ((ret = _get_sol_sol_retry (state_data, section_name, &sr)) != IPMI_CONFIG_ERR_SUCCESS)
    return (ret);

  sr.retry_count = atoi (kv->value_input);

  return (_set_sol_sol_retry (state_data, section_name, &sr));
}

static ipmi_config_err_t
sol_retry_interval_checkout (ipmi_config_state_data_t *state_data,
			     const char *section_name,
                             struct ipmi_config_keyvalue *kv)
{
  struct sol_retry sr;
  ipmi_config_err_t ret;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if ((ret = _get_sol_sol_retry (state_data, section_name, &sr)) != IPMI_CONFIG_ERR_SUCCESS)
    return (ret);

  if (ipmi_config_section_update_keyvalue_output_unsigned_int (state_data,
                                                               kv,
                                                               sr.retry_interval) < 0)
    return (IPMI_CONFIG_ERR_FATAL_ERROR);

  return (IPMI_CONFIG_ERR_SUCCESS);
}

static ipmi_config_err_t
sol_retry_interval_commit (ipmi_config_state_data_t *state_data,
			   const char *section_name,
                           const struct ipmi_config_keyvalue *kv)
{
  struct sol_retry sr;
  ipmi_config_err_t ret;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if ((ret = _get_sol_sol_retry (state_data, section_name, &sr)) != IPMI_CONFIG_ERR_SUCCESS)
    return (ret);

  sr.retry_interval = atoi (kv->value_input);

  return (_set_sol_sol_retry (state_data, section_name, &sr));
}

static ipmi_config_err_t
non_volatile_bit_rate_checkout (ipmi_config_state_data_t *state_data,
				const char *section_name,
                                struct ipmi_config_keyvalue *kv)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint8_t bit_rate;
  uint64_t val;
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;
  ipmi_config_err_t ret;
  uint8_t channel_number;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_get_sol_configuration_parameters_sol_non_volatile_bit_rate_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if ((ret = get_sol_channel_number (state_data, section_name, &channel_number)) != IPMI_CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_get_sol_configuration_parameters_sol_non_volatile_bit_rate (state_data->ipmi_ctx,
                                                                           channel_number,
                                                                           IPMI_GET_SOL_PARAMETER,
                                                                           IPMI_SOL_CONFIGURATION_PARAMETERS_NO_SET_SELECTOR,
                                                                           IPMI_SOL_CONFIGURATION_PARAMETERS_NO_BLOCK_SELECTOR,
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
                         "ipmi_cmd_get_sol_configuration_parameters_sol_non_volatile_bit_rate: %s\n",
                         ipmi_ctx_errormsg (state_data->ipmi_ctx));

      goto cleanup;
    }

  if (FIID_OBJ_GET (obj_cmd_rs, "bit_rate", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'bit_rate': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  bit_rate = val;

  if (ipmi_config_section_update_keyvalue_output (state_data,
                                                  kv,
                                                  sol_bit_rate_string (bit_rate)) < 0)
    return (IPMI_CONFIG_ERR_FATAL_ERROR);

  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static ipmi_config_err_t
non_volatile_bit_rate_commit (ipmi_config_state_data_t *state_data,
			      const char *section_name,
                              const struct ipmi_config_keyvalue *kv)
{
  fiid_obj_t obj_cmd_rs = NULL;
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;
  ipmi_config_err_t ret;
  uint8_t channel_number;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_set_sol_configuration_parameters_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if ((ret = get_sol_channel_number (state_data, section_name, &channel_number)) != IPMI_CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_set_sol_configuration_parameters_sol_non_volatile_bit_rate (state_data->ipmi_ctx,
                                                                           channel_number,
                                                                           sol_bit_rate_number (kv->value_input),
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
                         "ipmi_cmd_set_sol_configuration_parameters_sol_non_volatile_bit_rate: %s\n",
                         ipmi_ctx_errormsg (state_data->ipmi_ctx));

      goto cleanup;
    }

  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static ipmi_config_err_t
volatile_bit_rate_checkout (ipmi_config_state_data_t *state_data,
			    const char *section_name,
                            struct ipmi_config_keyvalue *kv)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint8_t bit_rate;
  uint64_t val;
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;
  ipmi_config_err_t ret;
  uint8_t channel_number;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_get_sol_configuration_parameters_sol_volatile_bit_rate_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if ((ret = get_sol_channel_number (state_data, section_name, &channel_number)) != IPMI_CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_get_sol_configuration_parameters_sol_volatile_bit_rate (state_data->ipmi_ctx,
                                                                       channel_number,
                                                                       IPMI_GET_SOL_PARAMETER,
                                                                       IPMI_SOL_CONFIGURATION_PARAMETERS_NO_SET_SELECTOR,
                                                                       IPMI_SOL_CONFIGURATION_PARAMETERS_NO_BLOCK_SELECTOR,
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
                         "ipmi_cmd_get_sol_configuration_parameters_sol_volatile_bit_rate: %s\n",
                         ipmi_ctx_errormsg (state_data->ipmi_ctx));

      goto cleanup;
    }

  if (FIID_OBJ_GET (obj_cmd_rs, "bit_rate", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'bit_rate': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  bit_rate = val;

  if (ipmi_config_section_update_keyvalue_output (state_data,
                                                  kv,
                                                  sol_bit_rate_string (bit_rate)) < 0)
    return (IPMI_CONFIG_ERR_FATAL_ERROR);

  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static ipmi_config_err_t
volatile_bit_rate_commit (ipmi_config_state_data_t *state_data,
			  const char *section_name,
                          const struct ipmi_config_keyvalue *kv)
{
  fiid_obj_t obj_cmd_rs = NULL;
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;
  ipmi_config_err_t ret;
  uint8_t channel_number;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_set_sol_configuration_parameters_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if ((ret = get_sol_channel_number (state_data, section_name, &channel_number)) != IPMI_CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_set_sol_configuration_parameters_sol_volatile_bit_rate (state_data->ipmi_ctx,
                                                                       channel_number,
                                                                       sol_bit_rate_number (kv->value_input),
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
                         "ipmi_cmd_set_sol_configuration_parameters_sol_volatile_bit_rate: %s\n",
                         ipmi_ctx_errormsg (state_data->ipmi_ctx));

      goto cleanup;
    }

  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static ipmi_config_err_t
sol_payload_port_checkout (ipmi_config_state_data_t *state_data,
			   const char *section_name,
                           struct ipmi_config_keyvalue *kv)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint16_t port_number;
  uint64_t val;
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;
  ipmi_config_err_t ret;
  uint8_t channel_number;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_get_sol_configuration_parameters_sol_payload_port_number_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if ((ret = get_sol_channel_number (state_data, section_name, &channel_number)) != IPMI_CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_get_sol_configuration_parameters_sol_payload_port_number (state_data->ipmi_ctx,
                                                                         channel_number,
                                                                         IPMI_GET_SOL_PARAMETER,
                                                                         IPMI_SOL_CONFIGURATION_PARAMETERS_NO_SET_SELECTOR,
                                                                         IPMI_SOL_CONFIGURATION_PARAMETERS_NO_BLOCK_SELECTOR,
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
                         "ipmi_cmd_get_sol_configuration_parameters_sol_payload_port_number: %s\n",
                         ipmi_ctx_errormsg (state_data->ipmi_ctx));

      goto cleanup;
    }

  if (FIID_OBJ_GET (obj_cmd_rs, "port_number", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'port_number': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  port_number = val;

  if (ipmi_config_section_update_keyvalue_output_unsigned_int (state_data,
                                                               kv,
                                                               port_number) < 0)
    return (IPMI_CONFIG_ERR_FATAL_ERROR);

  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static ipmi_config_err_t
sol_payload_port_commit (ipmi_config_state_data_t *state_data,
			 const char *section_name,
                         const struct ipmi_config_keyvalue *kv)
{
  fiid_obj_t obj_cmd_rs = NULL;
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;
  ipmi_config_err_t ret;
  uint8_t channel_number;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_set_sol_configuration_parameters_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if ((ret = get_sol_channel_number (state_data, section_name, &channel_number)) != IPMI_CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_set_sol_configuration_parameters_sol_payload_port_number (state_data->ipmi_ctx,
                                                                         channel_number,
                                                                         atoi (kv->value_input),
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
                         "ipmi_cmd_set_sol_configuration_parameters_sol_payload_port_number: %s\n",
                         ipmi_ctx_errormsg (state_data->ipmi_ctx));

      goto cleanup;
    }

  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

struct ipmi_config_section *
ipmi_config_core_sol_conf_section_get (ipmi_config_state_data_t *state_data,
				       unsigned int config_flags,
				       int channel_index)
{
  struct ipmi_config_section * section = NULL;
  char *section_comment =
    "If your system supports IPMI 2.0 and Serial-over-LAN (SOL), the "
    "following configuration options will allow SOL configuration."
    "\n"
    "For most users that want to enable SOL, minimally \"Enable_SOL\" "
    "should be set to \"Yes\" and \"SOL_Privilege_Level\" should be set to "
    "the highest privilege level any username configured can authenticate "
    "with (typically \"Administrator\").  For security purposes, "
    "\"Force_SOL_Payload_Authentication\" and "
    "\"Force_SOL_Payload_Encryption\" should be set to \"Yes\", however "
    "forced authentication and/or encryption depends on the cipher suite "
    "IDs supported. The \"Non_Volatile_Bit_Rate\" "
    "and \"Volatile_Bit_Rate\" should both be set to the appropriate baud "
    "rate for your system.  This is typically the same baud rate configured "
    "in the BIOS and/or operating system.";
  char *section_name_base_str = "SOL_Conf";

  assert (state_data);

  if (!(section = ipmi_config_section_multi_channel_create (state_data,
                                                            section_name_base_str,
                                                            section_comment,
                                                            NULL,
                                                            NULL,
                                                            config_flags,
                                                            channel_index,
                                                            state_data->sol_channel_numbers_unique,
                                                            state_data->sol_channel_numbers_unique_count)))
    goto cleanup;

  if (ipmi_config_section_add_key (state_data,
                                   section,
                                   "Enable_SOL",
                                   "Possible values: Yes/No",
                                   0,
                                   enable_sol_checkout,
                                   enable_sol_commit,
                                   yes_no_validate) < 0)
    goto cleanup;

  if (ipmi_config_section_add_key (state_data,
                                   section,
                                   "SOL_Privilege_Level",
                                   "Possible values: Callback/User/Operator/Administrator/OEM_Proprietary",
                                   0,
                                   sol_privilege_level_checkout,
                                   sol_privilege_level_commit,
                                   privilege_level_number_validate) < 0)
    goto cleanup;

  if (ipmi_config_section_add_key (state_data,
                                   section,
                                   "Force_SOL_Payload_Authentication",
                                   "Possible values: Yes/No",
                                   0,
                                   force_sol_payload_authentication_checkout,
                                   force_sol_payload_authentication_commit,
                                   yes_no_validate) < 0)
    goto cleanup;

  if (ipmi_config_section_add_key (state_data,
                                   section,
                                   "Force_SOL_Payload_Encryption",
                                   "Possible values: Yes/No",
                                   0,
                                   force_sol_payload_encryption_checkout,
                                   force_sol_payload_encryption_commit,
                                   yes_no_validate) < 0)
    goto cleanup;

  if (ipmi_config_section_add_key (state_data,
                                   section,
                                   "Character_Accumulate_Interval",
                                   "Give a non-zero valid integer. Each unit is 5ms",
                                   0,
                                   character_accumulate_interval_checkout,
                                   character_accumulate_interval_commit,
                                   number_range_one_byte_non_zero_validate) < 0)
    goto cleanup;

  if (ipmi_config_section_add_key (state_data,
                                   section,
                                   "Character_Send_Threshold",
                                   "Give a valid number",
                                   0,
                                   character_send_threshold_checkout,
                                   character_send_threshold_commit,
                                   number_range_one_byte_validate) < 0)
    goto cleanup;

  if (ipmi_config_section_add_key (state_data,
                                   section,
                                   "SOL_Retry_Count",
                                   "Give a valid integer",
                                   0,
                                   sol_retry_count_checkout,
                                   sol_retry_count_commit,
                                   number_range_three_bits_validate) < 0)
    goto cleanup;

  if (ipmi_config_section_add_key (state_data,
                                   section,
                                   "SOL_Retry_Interval",
                                   "Give a valid integer. Interval unit is 10ms",
                                   0,
                                   sol_retry_interval_checkout,
                                   sol_retry_interval_commit,
                                   number_range_one_byte_validate) < 0)
    goto cleanup;

  if (ipmi_config_section_add_key (state_data,
                                   section,
                                   "Non_Volatile_Bit_Rate",
                                   "Possible values: Serial/9600/19200/38400/57600/115200",
                                   0,
                                   non_volatile_bit_rate_checkout,
                                   non_volatile_bit_rate_commit,
                                   sol_bit_rate_number_validate) < 0)
    goto cleanup;

  if (ipmi_config_section_add_key (state_data,
                                   section,
                                   "Volatile_Bit_Rate",
                                   "Possible values: Serial/9600/19200/38400/57600/115200",
                                   0,
                                   volatile_bit_rate_checkout,
                                   volatile_bit_rate_commit,
                                   sol_bit_rate_number_validate) < 0)
    goto cleanup;

  if (ipmi_config_section_add_key (state_data,
                                   section,
                                   "SOL_Payload_Port_Number",
                                   "Give a valid port number",
                                   IPMI_CONFIG_CHECKOUT_KEY_COMMENTED_OUT,
                                   sol_payload_port_checkout,
                                   sol_payload_port_commit,
                                   number_range_two_bytes_validate) < 0)
    goto cleanup;

  return (section);

 cleanup:
  if (section)
    ipmi_config_section_destroy (section);
  return (NULL);
}
