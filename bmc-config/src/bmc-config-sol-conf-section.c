#if HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */

#include "bmc-config.h"
#include "bmc-config-wrapper.h"
#include "bmc-config-map.h"
#include "bmc-config-validate.h"

static config_err_t
sol_auth_checkout (bmc_config_state_data_t *state_data,
		   uint8_t *sol_privilege_level,
		   uint8_t *force_sol_payload_authentication,
		   uint8_t *force_sol_payload_encryption)
{
  uint8_t tmp_sol_privilege_level;
  uint8_t tmp_force_sol_payload_authentication;
  uint8_t tmp_force_sol_payload_encryption;
  config_err_t ret;

  if ((ret = get_sol_sol_authentication (state_data,
                                         &tmp_sol_privilege_level,
                                         &tmp_force_sol_payload_authentication,
                                         &tmp_force_sol_payload_encryption)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (sol_privilege_level)
    *sol_privilege_level = tmp_sol_privilege_level;

  if (force_sol_payload_authentication)
    *force_sol_payload_authentication = tmp_force_sol_payload_authentication;

  if (force_sol_payload_encryption)
    *force_sol_payload_encryption = tmp_force_sol_payload_encryption;

  return CONFIG_ERR_SUCCESS;
}


static config_err_t
sol_auth_commit (bmc_config_state_data_t *state_data,
		 uint8_t *sol_privilege_level,
		 uint8_t *force_sol_payload_authentication,
		 uint8_t *force_sol_payload_encryption)
{
  uint8_t tmp_sol_privilege_level;
  uint8_t tmp_force_sol_payload_authentication;
  uint8_t tmp_force_sol_payload_encryption;
  config_err_t ret;

  if ((ret = get_sol_sol_authentication (state_data,
                                         &tmp_sol_privilege_level,
                                         &tmp_force_sol_payload_authentication,
                                         &tmp_force_sol_payload_encryption)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (sol_privilege_level)
    tmp_sol_privilege_level = *sol_privilege_level;

  if (force_sol_payload_authentication)
    tmp_force_sol_payload_authentication = *force_sol_payload_authentication;

  if (force_sol_payload_encryption)
    tmp_force_sol_payload_encryption = *force_sol_payload_encryption;

  return set_sol_sol_authentication (state_data,
				     tmp_sol_privilege_level,
				     tmp_force_sol_payload_authentication,
				     tmp_force_sol_payload_encryption);
}

static config_err_t
enable_sol_checkout (const char *section_name,
		     struct config_keyvalue *kv,
                     void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  uint8_t enable;
  config_err_t ret;

  if ((ret = get_sol_sol_enable (state_data,
                                 &enable)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (!(kv->value_output = strdup (enable ? "Yes" : "No")))
    {
      perror("strdup");
      return CONFIG_ERR_FATAL_ERROR;
    }

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
enable_sol_commit (const char *section_name,
		   const struct config_keyvalue *kv,
                   void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  return set_sol_sol_enable (state_data,
			     same (kv->value_input, "yes"));
}

static config_err_t
sol_privilege_level_checkout (const char *section_name,
			      struct config_keyvalue *kv,
                              void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  uint8_t value;
  config_err_t ret;

  if ((ret = sol_auth_checkout (state_data,
                                &value,
                                NULL,
                                NULL)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (!(kv->value_output = strdup (privilege_level_string (value))))
    {
      perror("strdup");
      return CONFIG_ERR_FATAL_ERROR;
    }
  return CONFIG_ERR_SUCCESS;
}

static config_err_t
sol_privilege_level_commit (const char *section_name,
			    const struct config_keyvalue *kv,
                            void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  uint8_t value = privilege_level_number (kv->value_input);
  return sol_auth_commit (state_data,
			  &value,
			  NULL,
			  NULL);
}

static config_err_t
force_sol_payload_authentication_checkout (const char *section_name,
					   struct config_keyvalue *kv,
                                           void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  uint8_t value;
  config_err_t ret;

  if ((ret = sol_auth_checkout (state_data,
                                NULL,
                                &value,
                                NULL)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (!(kv->value_output = strdup (value ? "Yes" : "No")))
    {
      perror("strdup");
      return CONFIG_ERR_FATAL_ERROR;
    }
  
  return CONFIG_ERR_SUCCESS;
}

static config_err_t
force_sol_payload_authentication_commit (const char *section_name,
					 const struct config_keyvalue *kv,
                                         void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  uint8_t value = same (kv->value_input, "yes") ? 1 : 0;
  return sol_auth_commit (state_data,
			  NULL,
			  &value,
			  NULL);
}

static config_err_t
force_sol_payload_encryption_checkout (const char *section_name,
                                       struct config_keyvalue *kv,
                                       void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  uint8_t value;
  config_err_t ret;

  if ((ret = sol_auth_checkout (state_data,
                                NULL,
                                NULL,
                                &value)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (!(kv->value_output = strdup (value ? "Yes" : "No")))
    {
      perror("strdup");
      return CONFIG_ERR_FATAL_ERROR;
    }
  
  return CONFIG_ERR_SUCCESS;
}

static config_err_t
force_sol_payload_encryption_commit (const char *section_name,
				     const struct config_keyvalue *kv,
                                     void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  uint8_t value = same (kv->value_input, "yes") ? 1 : 0;
  return sol_auth_commit (state_data,
			  NULL,
			  NULL,
			  &value);
}

static config_err_t
character_accumulate_interval_checkout (const char *section_name,
					struct config_keyvalue *kv,
                                        void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  uint8_t interval;
  uint8_t threshold;
  config_err_t ret;

  if ((ret = get_sol_character_accumulate_interval_and_send_threshold (state_data,
                                                                       &interval,
                                                                       &threshold)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (asprintf (&kv->value_output, "%d", interval) < 0)
    {
      perror("asprintf");
      return CONFIG_ERR_FATAL_ERROR;
    }
  return CONFIG_ERR_SUCCESS;
}

static config_err_t
character_accumulate_interval_commit (const char *section_name,
				      const struct config_keyvalue *kv,
                                      void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  uint8_t interval;
  uint8_t threshold;
  config_err_t ret;

  if ((ret = get_sol_character_accumulate_interval_and_send_threshold (state_data,
                                                                       &interval,
                                                                       &threshold)) != CONFIG_ERR_SUCCESS)
    return ret;

  interval = atoi (kv->value_input);

  return set_sol_character_accumulate_interval_and_send_threshold (state_data,
								   interval,
								   threshold);
}

static config_err_t
character_send_threshold_checkout (const char *section_name,
				   struct config_keyvalue *kv,
                                   void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  uint8_t interval;
  uint8_t threshold;
  config_err_t ret;

  if ((ret = get_sol_character_accumulate_interval_and_send_threshold (state_data,
                                                                       &interval,
                                                                       &threshold)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (asprintf (&kv->value_output, "%d", threshold) < 0)
    {
      perror("asprintf");
      return CONFIG_ERR_FATAL_ERROR;
    }
  return CONFIG_ERR_SUCCESS;
}

static config_err_t
character_send_threshold_commit (const char *section_name,
				 const struct config_keyvalue *kv,
                                 void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  uint8_t interval;
  uint8_t threshold;
  config_err_t ret;

  if ((ret = get_sol_character_accumulate_interval_and_send_threshold (state_data,
                                                                       &interval,
                                                                       &threshold)) != CONFIG_ERR_SUCCESS)
    return ret;

  threshold = atoi (kv->value_input);

  return set_sol_character_accumulate_interval_and_send_threshold (state_data,
								   interval,
								   threshold);
}

static config_err_t
sol_retry_count_checkout (const char *section_name,
			  struct config_keyvalue *kv,
                          void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  uint8_t count;
  uint8_t interval;
  config_err_t ret;

  if ((ret = get_sol_sol_retry (state_data,
                                &count,
                                &interval)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (asprintf (&kv->value_output, "%d", count) < 0)
    {
      perror("asprintf");
      return CONFIG_ERR_FATAL_ERROR;
    }
  return CONFIG_ERR_SUCCESS;
}


static config_err_t
sol_retry_count_commit (const char *section_name,
			const struct config_keyvalue *kv,
                        void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  uint8_t count;
  uint8_t interval;
  config_err_t ret;

  if ((ret = get_sol_sol_retry (state_data,
                                &count,
                                &interval)) != CONFIG_ERR_SUCCESS)
    return ret;

  count = atoi (kv->value_input);

  return set_sol_sol_retry (state_data,
			    count,
			    interval);
}

static config_err_t
sol_retry_interval_checkout (const char *section_name,
			     struct config_keyvalue *kv,
                             void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  uint8_t count;
  uint8_t interval;
  config_err_t ret;

  if ((ret = get_sol_sol_retry (state_data,
                                &count,
                                &interval)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (asprintf (&kv->value_output, "%d", interval) < 0)
    {
      perror("asprintf");
      return CONFIG_ERR_FATAL_ERROR;
    }
  return CONFIG_ERR_SUCCESS;
}


static config_err_t
sol_retry_interval_commit (const char *section_name,
			   const struct config_keyvalue *kv,
                           void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  uint8_t count;
  uint8_t interval;
  config_err_t ret;

  if ((ret = get_sol_sol_retry (state_data,
                                &count,
                                &interval)) != CONFIG_ERR_SUCCESS)
    return ret;

  interval = atoi (kv->value_input);

  return set_sol_sol_retry (state_data,
			    count,
			    interval);
}

static config_err_t
non_volatile_bit_rate_checkout (const char *section_name,
				struct config_keyvalue *kv,
                                void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  uint8_t bitrate;
  config_err_t ret;

  if ((ret = get_sol_sol_non_volatile_bit_rate (state_data,
                                                &bitrate)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (!(kv->value_output = strdup (sol_bit_rate_string (bitrate))))
    {
      perror("strdup");
      return CONFIG_ERR_FATAL_ERROR;
    }
  return CONFIG_ERR_SUCCESS;
}

static config_err_t
non_volatile_bit_rate_commit (const char *section_name,
			      const struct config_keyvalue *kv,
                              void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  return set_sol_sol_non_volatile_bit_rate (state_data,
					    sol_bit_rate_number (kv->value_input));
}

static config_err_t
volatile_bit_rate_checkout (const char *section_name,
			    struct config_keyvalue *kv,
                            void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  uint8_t bitrate;
  config_err_t ret;

  if ((ret = get_sol_sol_volatile_bit_rate (state_data,
                                            &bitrate)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (!(kv->value_output = strdup (sol_bit_rate_string (bitrate))))
    {
      perror("strdup");
      return CONFIG_ERR_FATAL_ERROR;
    }
  return CONFIG_ERR_SUCCESS;
}

static config_err_t
volatile_bit_rate_commit (const char *section_name,
			  const struct config_keyvalue *kv,
                          void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  return set_sol_sol_volatile_bit_rate (state_data,
					sol_bit_rate_number (kv->value_input));
}

static config_err_t
port_checkout (const char *section_name,
	       struct config_keyvalue *kv,
               void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  uint16_t port;
  config_err_t ret;

  if ((ret = get_sol_sol_payload_port_number (state_data,
                                              &port)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (asprintf (&kv->value_output, "%d", port) < 0)
    {
      perror("asprintf");
      return CONFIG_ERR_FATAL_ERROR;
    }
  return CONFIG_ERR_SUCCESS;
}


static config_err_t
port_commit (const char *section_name,
	     const struct config_keyvalue *kv,
             void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  return set_sol_sol_payload_port_number (state_data,
					  atoi (kv->value_input));
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
                              port_checkout,
                              port_commit,
                              config_number_range_two_bytes) < 0)
    goto cleanup;

  return sol_conf_section;

 cleanup:
  if (sol_conf_section)
    config_section_destroy(sol_conf_section);
  return NULL;
}
