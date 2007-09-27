#if HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <assert.h>

#include "bmc-config.h"
#include "bmc-config-wrapper.h"
#include "bmc-config-map.h"
#include "bmc-config-validate.h"
#include "bmc-config-utils.h"

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

static config_err_t
enable_sol_checkout (const char *section_name,
		     struct config_keyvalue *kv,
                     void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;
  
  if (!(obj_cmd_rs = Fiid_obj_create(tmpl_cmd_get_sol_configuration_parameters_sol_enable_rs)))
    goto cleanup;

  if ((ret = get_sol_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_get_sol_configuration_parameters_sol_enable (state_data->dev, 
							    channel_number, 
							    IPMI_GET_SOL_PARAMETER, 
							    SET_SELECTOR, 
							    BLOCK_SELECTOR, 
							    obj_cmd_rs) < 0)
    {
      if (state_data->prog_data->args->common.flags & IPMI_FLAGS_DEBUG_DUMP)
        fprintf(stderr,
                "ipmi_cmd_get_sol_configuration_parameters_sol_enable: %s\n",
                ipmi_device_strerror(ipmi_device_errnum(state_data->dev)));
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }
  
  if (Fiid_obj_get (obj_cmd_rs, "sol_enable", &val) < 0)
    goto cleanup;
  
  if (config_section_update_keyvalue_output(kv, val ? "Yes" : "No") < 0)
    return CONFIG_ERR_FATAL_ERROR;

  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  Fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

static config_err_t
enable_sol_commit (const char *section_name,
		   const struct config_keyvalue *kv,
                   void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  fiid_obj_t obj_cmd_rs = NULL;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;
  
  if (!(obj_cmd_rs = Fiid_obj_create(tmpl_cmd_set_sol_configuration_parameters_rs)))
    goto cleanup;
  
  if ((ret = get_sol_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }
  
  if (ipmi_cmd_set_sol_configuration_parameters_sol_enable (state_data->dev, 
							    channel_number,
                                                            same (kv->value_input, "yes"),
							    obj_cmd_rs) < 0)
    {
      if (state_data->prog_data->args->common.flags & IPMI_FLAGS_DEBUG_DUMP)
        fprintf(stderr,
                "ipmi_cmd_set_sol_configuration_parameters_sol_enable: %s\n",
                ipmi_device_strerror(ipmi_device_errnum(state_data->dev)));
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }
  
  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  Fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

static config_err_t 
_get_sol_sol_authentication (bmc_config_state_data_t *state_data, 
                             struct sol_authentication *sa)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;
  
  assert(state_data);
  assert(sa);
  
  if (!(obj_cmd_rs = Fiid_obj_create(tmpl_cmd_get_sol_configuration_parameters_sol_authentication_rs)))
    goto cleanup;

  if ((ret = get_sol_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }
  
  if (ipmi_cmd_get_sol_configuration_parameters_sol_authentication (state_data->dev, 
								    channel_number, 
								    IPMI_GET_SOL_PARAMETER, 
								    SET_SELECTOR, 
								    BLOCK_SELECTOR, 
								    obj_cmd_rs) < 0)
    {
      if (state_data->prog_data->args->common.flags & IPMI_FLAGS_DEBUG_DUMP)
        fprintf(stderr,
                "ipmi_cmd_get_sol_configuration_parameters_sol_authentication: %s\n",
                ipmi_device_strerror(ipmi_device_errnum(state_data->dev)));
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }
  
  if (Fiid_obj_get (obj_cmd_rs, "sol_privilege_level", &val) < 0)
    goto cleanup;
  sa->sol_privilege_level = val;
  
  if (Fiid_obj_get (obj_cmd_rs, "force_sol_payload_authentication", &val) < 0)
    goto cleanup;
  sa->force_sol_payload_authentication = val;

  if (Fiid_obj_get (obj_cmd_rs, "force_sol_payload_encryption", &val) < 0)
    goto cleanup;
  sa->force_sol_payload_encryption = val;
  
  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  Fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

static config_err_t
_set_sol_sol_authentication(bmc_config_state_data_t *state_data,
                            struct sol_authentication *sa)
{
  fiid_obj_t obj_cmd_rs = NULL;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;

  assert(state_data);
  assert(sa);
  
  if (!(obj_cmd_rs = Fiid_obj_create(tmpl_cmd_set_sol_configuration_parameters_rs)))
    goto cleanup;

  if ((ret = get_sol_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_set_sol_configuration_parameters_sol_authentication (state_data->dev, 
								    channel_number,
								    sa->sol_privilege_level,
								    sa->force_sol_payload_authentication,
								    sa->force_sol_payload_encryption,
								    obj_cmd_rs) < 0)
    {
      if (state_data->prog_data->args->common.flags & IPMI_FLAGS_DEBUG_DUMP)
        fprintf(stderr,
                "ipmi_cmd_set_sol_configuration_parameters_sol_authentication: %s\n",
                ipmi_device_strerror(ipmi_device_errnum(state_data->dev)));
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }
  
  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  Fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

static config_err_t
sol_privilege_level_checkout (const char *section_name,
			      struct config_keyvalue *kv,
                              void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  struct sol_authentication sa;
  config_err_t ret;

  if ((ret = _get_sol_sol_authentication (state_data,
                                          &sa)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (config_section_update_keyvalue_output(kv, privilege_level_string (sa.sol_privilege_level)) < 0)
    return CONFIG_ERR_FATAL_ERROR;

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
sol_privilege_level_commit (const char *section_name,
			    const struct config_keyvalue *kv,
                            void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  struct sol_authentication sa;
  config_err_t ret;

  if ((ret = _get_sol_sol_authentication (state_data, 
                                          &sa)) != CONFIG_ERR_SUCCESS)
    return ret;

  sa.sol_privilege_level = privilege_level_number (kv->value_input);

  return _set_sol_sol_authentication (state_data, &sa);
}

static config_err_t
force_sol_payload_authentication_checkout (const char *section_name,
					   struct config_keyvalue *kv,
                                           void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  struct sol_authentication sa;
  config_err_t ret;

  if ((ret = _get_sol_sol_authentication (state_data, 
                                          &sa)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (config_section_update_keyvalue_output(kv, sa.force_sol_payload_authentication ? "Yes" : "No") < 0)
    return CONFIG_ERR_FATAL_ERROR;
  
  return CONFIG_ERR_SUCCESS;
}

static config_err_t
force_sol_payload_authentication_commit (const char *section_name,
					 const struct config_keyvalue *kv,
                                         void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  struct sol_authentication sa;
  config_err_t ret;

  if ((ret = _get_sol_sol_authentication (state_data, 
                                          &sa)) != CONFIG_ERR_SUCCESS)
    return ret;

  sa.force_sol_payload_authentication = same (kv->value_input, "yes") ? 1 : 0;
  
  return _set_sol_sol_authentication (state_data, &sa);
}

static config_err_t
force_sol_payload_encryption_checkout (const char *section_name,
                                       struct config_keyvalue *kv,
                                       void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  struct sol_authentication sa;
  config_err_t ret;

  if ((ret = _get_sol_sol_authentication (state_data, 
                                          &sa)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (config_section_update_keyvalue_output(kv, sa.force_sol_payload_encryption ? "Yes" : "No") < 0)
    return CONFIG_ERR_FATAL_ERROR;
  
  return CONFIG_ERR_SUCCESS;
}

static config_err_t
force_sol_payload_encryption_commit (const char *section_name,
				     const struct config_keyvalue *kv,
                                     void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  struct sol_authentication sa;
  config_err_t ret;

  if ((ret = _get_sol_sol_authentication (state_data, 
                                          &sa)) != CONFIG_ERR_SUCCESS)
    return ret;

  sa.force_sol_payload_encryption = same (kv->value_input, "yes") ? 1 : 0;

  return _set_sol_sol_authentication (state_data, &sa);
}

static config_err_t 
_get_sol_character_accumulate_interval_and_send_threshold (bmc_config_state_data_t *state_data, 
                                                           struct interval_and_threshold *it)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;

  assert(state_data);
  assert(it);
 
  if (!(obj_cmd_rs = Fiid_obj_create(tmpl_cmd_get_sol_configuration_parameters_character_accumulate_interval_and_send_threshold_rs)))
    goto cleanup;

  if ((ret = get_sol_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_get_sol_configuration_parameters_character_accumulate_interval_and_send_threshold (state_data->dev, 
												  channel_number, 
												  IPMI_GET_SOL_PARAMETER, 
												  SET_SELECTOR, 
												  BLOCK_SELECTOR, 
												  obj_cmd_rs) < 0)
    {
      if (state_data->prog_data->args->common.flags & IPMI_FLAGS_DEBUG_DUMP)
        fprintf(stderr,
                "ipmi_cmd_get_sol_configuration_parameters_character_accumulate_interval_and_send_threshold: %s\n",
                ipmi_device_strerror(ipmi_device_errnum(state_data->dev)));
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }
  
  if (Fiid_obj_get (obj_cmd_rs, "character_accumulate_interval", &val) < 0)
    goto cleanup;
  it->character_accumulate_interval = val;

  if (Fiid_obj_get (obj_cmd_rs, "character_send_threshold", &val) < 0)
    goto cleanup;
  it->character_send_threshold = val;
  
  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  Fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

static config_err_t
_set_sol_character_accumulate_interval_and_send_threshold(bmc_config_state_data_t *state_data,
                                                          struct interval_and_threshold *it)
{
  fiid_obj_t obj_cmd_rs = NULL;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;
  
  assert(state_data);
  assert(it);

  if (!(obj_cmd_rs = Fiid_obj_create(tmpl_cmd_set_sol_configuration_parameters_rs)))
    goto cleanup;

  if ((ret = get_sol_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_set_sol_configuration_parameters_character_accumulate_interval_and_send_threshold (state_data->dev, 
												  channel_number,
												  it->character_accumulate_interval,
												  it->character_send_threshold,
												  obj_cmd_rs) < 0)
    {
      if (state_data->prog_data->args->common.flags & IPMI_FLAGS_DEBUG_DUMP)
        fprintf(stderr,
                "ipmi_cmd_set_sol_configuration_parameters_character_accumulate_interval_and_send_threshold: %s\n",
                ipmi_device_strerror(ipmi_device_errnum(state_data->dev)));
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }
  
  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  Fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

static config_err_t
character_accumulate_interval_checkout (const char *section_name,
					struct config_keyvalue *kv,
                                        void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  struct interval_and_threshold it;
  config_err_t ret;

  if ((ret = _get_sol_character_accumulate_interval_and_send_threshold (state_data,
                                                                        &it)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (config_section_update_keyvalue_output_int(kv, it.character_accumulate_interval) < 0)
    return CONFIG_ERR_FATAL_ERROR;

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
character_accumulate_interval_commit (const char *section_name,
				      const struct config_keyvalue *kv,
                                      void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  struct interval_and_threshold it;
  config_err_t ret;

  if ((ret = _get_sol_character_accumulate_interval_and_send_threshold (state_data,
                                                                        &it)) != CONFIG_ERR_SUCCESS)
    return ret;

  it.character_accumulate_interval = atoi (kv->value_input);

  return _set_sol_character_accumulate_interval_and_send_threshold (state_data, &it);
}

static config_err_t
character_send_threshold_checkout (const char *section_name,
				   struct config_keyvalue *kv,
                                   void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  struct interval_and_threshold it;
  config_err_t ret;

  if ((ret = _get_sol_character_accumulate_interval_and_send_threshold (state_data,
                                                                        &it)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (config_section_update_keyvalue_output_int(kv, it.character_send_threshold) < 0)
    return CONFIG_ERR_FATAL_ERROR;

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
character_send_threshold_commit (const char *section_name,
				 const struct config_keyvalue *kv,
                                 void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  struct interval_and_threshold it;
  config_err_t ret;

  if ((ret = _get_sol_character_accumulate_interval_and_send_threshold (state_data,
                                                                        &it)) != CONFIG_ERR_SUCCESS)
    return ret;

  it.character_send_threshold = atoi (kv->value_input);

  return _set_sol_character_accumulate_interval_and_send_threshold (state_data, &it);
}

static config_err_t 
_get_sol_sol_retry (bmc_config_state_data_t *state_data, 
                    struct sol_retry *sr)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;

  assert(state_data);
  assert(sr);
  
  if (!(obj_cmd_rs = Fiid_obj_create(tmpl_cmd_get_sol_configuration_parameters_sol_retry_rs)))
    goto cleanup;

  if ((ret = get_sol_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_get_sol_configuration_parameters_sol_retry (state_data->dev, 
							   channel_number, 
							   IPMI_GET_SOL_PARAMETER, 
							   SET_SELECTOR, 
							   BLOCK_SELECTOR, 
							   obj_cmd_rs) < 0)
    {
      if (state_data->prog_data->args->common.flags & IPMI_FLAGS_DEBUG_DUMP)
        fprintf(stderr,
                "ipmi_cmd_get_sol_configuration_parameters_sol_retry: %s\n",
                ipmi_device_strerror(ipmi_device_errnum(state_data->dev)));
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }
  
  if (Fiid_obj_get (obj_cmd_rs, "retry_count", &val) < 0)
    goto cleanup;
  sr->retry_count = val;

  if (Fiid_obj_get (obj_cmd_rs, "retry_interval", &val) < 0)
    goto cleanup;
  sr->retry_interval = val;
  
  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  Fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

static config_err_t
_set_sol_sol_retry(bmc_config_state_data_t *state_data,
                   struct sol_retry *sr)
{
  fiid_obj_t obj_cmd_rs = NULL;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;
  
  if (!(obj_cmd_rs = Fiid_obj_create(tmpl_cmd_set_sol_configuration_parameters_rs)))
    goto cleanup;
  
  if ((ret = get_sol_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_set_sol_configuration_parameters_sol_retry (state_data->dev, 
							   channel_number,
							   sr->retry_count,
							   sr->retry_interval,
							   obj_cmd_rs) < 0)
    {
      if (state_data->prog_data->args->common.flags & IPMI_FLAGS_DEBUG_DUMP)
        fprintf(stderr,
                "ipmi_cmd_set_sol_configuration_parameters_sol_retry: %s\n",
                ipmi_device_strerror(ipmi_device_errnum(state_data->dev)));
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }
  
  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  Fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

static config_err_t
sol_retry_count_checkout (const char *section_name,
			  struct config_keyvalue *kv,
                          void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  struct sol_retry sr;
  config_err_t ret;

  if ((ret = _get_sol_sol_retry (state_data,
                                 &sr)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (config_section_update_keyvalue_output_int(kv, sr.retry_count) < 0)
    return CONFIG_ERR_FATAL_ERROR;

  return CONFIG_ERR_SUCCESS;
}


static config_err_t
sol_retry_count_commit (const char *section_name,
			const struct config_keyvalue *kv,
                        void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  struct sol_retry sr;
  config_err_t ret;
  
  if ((ret = _get_sol_sol_retry (state_data,
                                 &sr)) != CONFIG_ERR_SUCCESS)
    return ret;
  
  sr.retry_count = atoi (kv->value_input);
  
  return _set_sol_sol_retry (state_data, &sr);
}

static config_err_t
sol_retry_interval_checkout (const char *section_name,
			     struct config_keyvalue *kv,
                             void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  struct sol_retry sr;
  config_err_t ret;

  if ((ret = _get_sol_sol_retry (state_data,
                                 &sr)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (config_section_update_keyvalue_output_int(kv, sr.retry_interval) < 0)
    return CONFIG_ERR_FATAL_ERROR;

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
sol_retry_interval_commit (const char *section_name,
			   const struct config_keyvalue *kv,
                           void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  struct sol_retry sr;
  config_err_t ret;

  if ((ret = _get_sol_sol_retry (state_data,
                                 &sr)) != CONFIG_ERR_SUCCESS)
    return ret;
  
  sr.retry_interval = atoi (kv->value_input);
  
  return _set_sol_sol_retry (state_data, &sr);
}

static config_err_t
non_volatile_bit_rate_checkout (const char *section_name,
				struct config_keyvalue *kv,
                                void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;
  
  if (!(obj_cmd_rs = Fiid_obj_create(tmpl_cmd_get_sol_configuration_parameters_sol_non_volatile_bit_rate_rs)))
    goto cleanup;

  if ((ret = get_sol_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_get_sol_configuration_parameters_sol_non_volatile_bit_rate (state_data->dev, 
                                                                           channel_number, 
                                                                           IPMI_GET_SOL_PARAMETER, 
                                                                           SET_SELECTOR, 
                                                                           BLOCK_SELECTOR, 
                                                                           obj_cmd_rs) < 0)
    {
      if (state_data->prog_data->args->common.flags & IPMI_FLAGS_DEBUG_DUMP)
        fprintf(stderr,
                "ipmi_cmd_get_sol_configuration_parameters_sol_non_volatile_bit_rate: %s\n",
                ipmi_device_strerror(ipmi_device_errnum(state_data->dev)));
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }
  
  if (Fiid_obj_get (obj_cmd_rs, "bit_rate", &val) < 0)
    goto cleanup;

  if (config_section_update_keyvalue_output(kv, sol_bit_rate_string (val)) < 0)
    return CONFIG_ERR_FATAL_ERROR;
  
  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  Fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

static config_err_t
non_volatile_bit_rate_commit (const char *section_name,
			      const struct config_keyvalue *kv,
                              void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  fiid_obj_t obj_cmd_rs = NULL;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;
  
  if (!(obj_cmd_rs = Fiid_obj_create(tmpl_cmd_set_sol_configuration_parameters_rs)))
    goto cleanup;
  
  if ((ret = get_sol_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_set_sol_configuration_parameters_sol_non_volatile_bit_rate (state_data->dev, 
									   channel_number,
									   sol_bit_rate_number (kv->value_input),
									   obj_cmd_rs) < 0)
    {
      if (state_data->prog_data->args->common.flags & IPMI_FLAGS_DEBUG_DUMP)
        fprintf(stderr,
                "ipmi_cmd_set_sol_configuration_parameters_sol_non_volatile_bit_rate: %s\n",
                ipmi_device_strerror(ipmi_device_errnum(state_data->dev)));
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }
  
  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  Fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

static config_err_t
volatile_bit_rate_checkout (const char *section_name,
			    struct config_keyvalue *kv,
                            void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;
  
  if (!(obj_cmd_rs = Fiid_obj_create(tmpl_cmd_get_sol_configuration_parameters_sol_volatile_bit_rate_rs)))
    goto cleanup;

  if ((ret = get_sol_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_get_sol_configuration_parameters_sol_volatile_bit_rate (state_data->dev, 
								       channel_number, 
								       IPMI_GET_SOL_PARAMETER, 
								       SET_SELECTOR, 
								       BLOCK_SELECTOR, 
								       obj_cmd_rs) < 0)
    {
      if (state_data->prog_data->args->common.flags & IPMI_FLAGS_DEBUG_DUMP)
        fprintf(stderr,
                "ipmi_cmd_get_sol_configuration_parameters_sol_volatile_bit_rate: %s\n",
                ipmi_device_strerror(ipmi_device_errnum(state_data->dev)));
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }
  
  if (Fiid_obj_get (obj_cmd_rs, "bit_rate", &val) < 0)
    goto cleanup;

  if (config_section_update_keyvalue_output(kv, sol_bit_rate_string (val)) < 0)
    return CONFIG_ERR_FATAL_ERROR;
  
  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  Fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

static config_err_t
volatile_bit_rate_commit (const char *section_name,
			  const struct config_keyvalue *kv,
                          void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  fiid_obj_t obj_cmd_rs = NULL;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;
  
  if (!(obj_cmd_rs = Fiid_obj_create(tmpl_cmd_set_sol_configuration_parameters_rs)))
    goto cleanup;
  
  if ((ret = get_sol_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_set_sol_configuration_parameters_sol_volatile_bit_rate (state_data->dev, 
								       channel_number,
								       sol_bit_rate_number (kv->value_input),
								       obj_cmd_rs) < 0)
    {
      if (state_data->prog_data->args->common.flags & IPMI_FLAGS_DEBUG_DUMP)
        fprintf(stderr,
                "ipmi_cmd_set_sol_configuration_parameters_sol_volatile_bit_rate: %s\n",
                ipmi_device_strerror(ipmi_device_errnum(state_data->dev)));
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }
  
  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  Fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

static config_err_t
sol_payload_port_checkout (const char *section_name,
                           struct config_keyvalue *kv,
                           void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;
  
  if (!(obj_cmd_rs = Fiid_obj_create(tmpl_cmd_get_sol_configuration_parameters_sol_payload_port_number_rs)))
    goto cleanup;

  if ((ret = get_sol_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_get_sol_configuration_parameters_sol_payload_port_number (state_data->dev, 
									 channel_number, 
									 IPMI_GET_SOL_PARAMETER, 
									 SET_SELECTOR, 
									 BLOCK_SELECTOR, 
									 obj_cmd_rs) < 0)
    {
      if (state_data->prog_data->args->common.flags & IPMI_FLAGS_DEBUG_DUMP)
        fprintf(stderr,
                "ipmi_cmd_get_sol_configuration_parameters_sol_payload_port_number: %s\n",
                ipmi_device_strerror(ipmi_device_errnum(state_data->dev)));
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }
  
  if (Fiid_obj_get (obj_cmd_rs, "port_number", &val) < 0)
    goto cleanup;

  if (config_section_update_keyvalue_output_int(kv, val) < 0)
    return CONFIG_ERR_FATAL_ERROR;

  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  Fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

static config_err_t
sol_payload_port_commit (const char *section_name,
                         const struct config_keyvalue *kv,
                         void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  fiid_obj_t obj_cmd_rs = NULL;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;  
  config_err_t ret;
  uint8_t channel_number;

  if (!(obj_cmd_rs = Fiid_obj_create(tmpl_cmd_set_sol_configuration_parameters_rs)))
    goto cleanup;
  
  if ((ret = get_sol_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_set_sol_configuration_parameters_sol_payload_port_number (state_data->dev, 
									 channel_number,
                                                                         atoi (kv->value_input),
									 obj_cmd_rs) < 0)
    {
      if (state_data->prog_data->args->common.flags & IPMI_FLAGS_DEBUG_DUMP)
        fprintf(stderr,
                "ipmi_cmd_set_sol_configuration_parameters_sol_payload_port_number: %s\n",
                ipmi_device_strerror(ipmi_device_errnum(state_data->dev)));
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }
  
  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  Fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

struct config_section *
bmc_config_sol_conf_section_get (bmc_config_state_data_t *state_data)
{
  struct config_section * sol_conf_section = NULL;
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

  if (!(sol_conf_section = config_section_create("SOL_Conf",
                                                 "SOL_Conf",
                                                 section_comment,
                                                 0)))
    goto cleanup;

  if (config_section_add_key (sol_conf_section,
                              "Enable_SOL",
                              "Possible values: Yes/No",
                              0,
                              enable_sol_checkout,
                              enable_sol_commit,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (sol_conf_section,
                              "SOL_Privilege_Level",
                              "Possible values: Callback/User/Operator/Administrator/OEM_Proprietary",
                              0,
                              sol_privilege_level_checkout,
                              sol_privilege_level_commit,
                              privilege_level_number_validate) < 0)
    goto cleanup;

  if (config_section_add_key (sol_conf_section,
                              "Force_SOL_Payload_Authentication",
                              "Possible values: Yes/No",
                              0,
                              force_sol_payload_authentication_checkout,
                              force_sol_payload_authentication_commit,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (sol_conf_section,
                              "Force_SOL_Payload_Encryption",
                              "Possible values: Yes/No",
                              0,
                              force_sol_payload_encryption_checkout,
                              force_sol_payload_encryption_commit,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (sol_conf_section,
                              "Character_Accumulate_Interval",
                              "Give a non-zero valid integer. Each unit is 5ms",
                              0,
                              character_accumulate_interval_checkout,
                              character_accumulate_interval_commit,
                              config_number_range_one_byte_non_zero) < 0)
    goto cleanup;

  if (config_section_add_key (sol_conf_section,
                              "Character_Send_Threshold",
                              "Give a valid number",
                              0,
                              character_send_threshold_checkout,
                              character_send_threshold_commit,
                              config_number_range_one_byte) < 0)
    goto cleanup;

  if (config_section_add_key (sol_conf_section,
                              "SOL_Retry_Count",
                              "Give a valid integer",
                              0,
                              sol_retry_count_checkout,
                              sol_retry_count_commit,
                              config_number_range_one_byte) < 0)
    goto cleanup;

  if (config_section_add_key (sol_conf_section,
                              "SOL_Retry_Interval",
                              "Give a valid integer. Interval unit is 10ms",
                              0,
                              sol_retry_interval_checkout,
                              sol_retry_interval_commit,
                              config_number_range_one_byte) < 0)
    goto cleanup;

  if (config_section_add_key (sol_conf_section,
                              "Non_Volatile_Bit_Rate",
                              "Possible values: Serial/9600/19200/38400/57600/115200",
                              0,
                              non_volatile_bit_rate_checkout,
                              non_volatile_bit_rate_commit,
                              sol_bit_rate_number_validate) < 0)
    goto cleanup;

  if (config_section_add_key (sol_conf_section,
                              "Volatile_Bit_Rate",
                              "Possible values: Serial/9600/19200/38400/57600/115200",
                              0,
                              volatile_bit_rate_checkout,
                              volatile_bit_rate_commit,
                              sol_bit_rate_number_validate) < 0)
    goto cleanup;

  if (config_section_add_key (sol_conf_section,
                              "SOL_Payload_Port_Number",
                              "Give a valid port number",
                              CONFIG_CHECKOUT_KEY_COMMENTED_OUT,
                              sol_payload_port_checkout,
                              sol_payload_port_commit,
                              config_number_range_two_bytes) < 0)
    goto cleanup;

  return sol_conf_section;

 cleanup:
  if (sol_conf_section)
    config_section_destroy(sol_conf_section);
  return NULL;
}
