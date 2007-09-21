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
#include "bmc-config-common.h"
#include "bmc-config-wrapper.h"
#include "bmc-config-map.h"
#include "bmc-config-validate.h"

#include "config-common.h"
#include "config-section.h"
#include "config-validate.h"

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
enable_sol_checkout (bmc_config_state_data_t *state_data,
		     const struct config_section *sect,
		     struct config_keyvalue *kv)
{
  uint8_t enable;
  config_err_t ret;

  if ((ret = get_sol_sol_enable (state_data,
                                 &enable)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (enable)
    {
      if (!(kv->value_output = strdup ("Yes")))
        {
          perror("strdup");
          return CONFIG_ERR_FATAL_ERROR;
        }
    }
  else
    {
      if (!(kv->value_output = strdup ("No")))
        {
          perror("strdup");
          return CONFIG_ERR_FATAL_ERROR;
        }
    }

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
enable_sol_commit (bmc_config_state_data_t *state_data,
		   const struct config_section *sect,
		   const struct config_keyvalue *kv)
{
  return set_sol_sol_enable (state_data,
			     same (kv->value_input, "yes"));
}

static bmc_diff_t
enable_sol_diff (bmc_config_state_data_t *state_data,
		 const struct config_section *sect,
		 const struct config_keyvalue *kv)
{
  uint8_t got_value;
  uint8_t passed_value;
  config_err_t rc;
  bmc_diff_t ret;
  
  if ((rc = get_sol_sol_enable (state_data,
                                &got_value)) != CONFIG_ERR_SUCCESS)
    {
      if (rc == CONFIG_ERR_NON_FATAL_ERROR)
        return BMC_DIFF_NON_FATAL_ERROR;
      return BMC_DIFF_FATAL_ERROR;
    }

  passed_value = same (kv->value_input, "yes") ? 1 : 0;

  if (passed_value == got_value)
    ret = BMC_DIFF_SAME;
  else 
    {
      ret = BMC_DIFF_DIFFERENT;
      report_diff (sect->section_name,
                   kv->key,
                   kv->value_input,
                   got_value ? "Yes" : "No");
    }
  return ret;
}

static config_err_t
sol_privilege_level_checkout (bmc_config_state_data_t *state_data,
			      const struct config_section *sect,
			      struct config_keyvalue *kv)
{
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
sol_privilege_level_commit (bmc_config_state_data_t *state_data,
			    const struct config_section *sect,
			    const struct config_keyvalue *kv)
{
  uint8_t value = privilege_level_number (kv->value_input);
  return sol_auth_commit (state_data,
			  &value,
			  NULL,
			  NULL);
}

static bmc_diff_t
sol_privilege_level_diff (bmc_config_state_data_t *state_data,
			  const struct config_section *sect,
			  const struct config_keyvalue *kv)
{
  uint8_t passed_value;
  uint8_t got_value;
  config_err_t rc;
  bmc_diff_t ret;

  if ((rc = sol_auth_checkout (state_data,
                               &got_value,
                               NULL,
                               NULL)) != CONFIG_ERR_SUCCESS)
    {
      if (rc == CONFIG_ERR_NON_FATAL_ERROR)
        return BMC_DIFF_NON_FATAL_ERROR;
      return BMC_DIFF_FATAL_ERROR;
    }

  passed_value = privilege_level_number (kv->value_input);

  if (passed_value == got_value)
    ret = BMC_DIFF_SAME;
  else 
    {
      ret = BMC_DIFF_DIFFERENT;
      report_diff (sect->section_name,
                   kv->key,
                   kv->value_input,
                   privilege_level_string (got_value));
    }
  return ret;
}


/* force_sol_payload_authentication */
static config_err_t
force_sol_payload_authentication_checkout (bmc_config_state_data_t *state_data,
					   const struct config_section *sect,
					   struct config_keyvalue *kv)
{
  uint8_t value;
  config_err_t ret;

  if ((ret = sol_auth_checkout (state_data,
                                NULL,
                                &value,
                                NULL)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (value)
    {
      if (!(kv->value_output = strdup ("Yes")))
        {
          perror("strdup");
          return CONFIG_ERR_FATAL_ERROR;
        }
    }
  else
    {
      if (!(kv->value_output = strdup ("No")))
        {
          perror("strdup");
          return CONFIG_ERR_FATAL_ERROR;
        }
    }
  
  return CONFIG_ERR_SUCCESS;
}

static config_err_t
force_sol_payload_authentication_commit (bmc_config_state_data_t *state_data,
					 const struct config_section *sect,
					 const struct config_keyvalue *kv)
{
  uint8_t value = same (kv->value_input, "yes") ? 1 : 0;
  return sol_auth_commit (state_data,
			  NULL,
			  &value,
			  NULL);
}

static bmc_diff_t
force_sol_payload_authentication_diff (bmc_config_state_data_t *state_data,
				       const struct config_section *sect,
				       const struct config_keyvalue *kv)
{
  uint8_t passed_value;
  uint8_t got_value;
  config_err_t rc;
  bmc_diff_t ret;

  if ((rc = sol_auth_checkout (state_data,
                               NULL,
                               &got_value,
                               NULL)) != CONFIG_ERR_SUCCESS)
    {
      if (rc == CONFIG_ERR_NON_FATAL_ERROR)
        return BMC_DIFF_NON_FATAL_ERROR;
      return BMC_DIFF_FATAL_ERROR;
    }

  passed_value = same (kv->value_input, "yes") ? 1 : 0;

  if (passed_value == got_value)
    ret = BMC_DIFF_SAME;
  else 
    {
      ret = BMC_DIFF_DIFFERENT;
      report_diff (sect->section_name,
                   kv->key,
                   kv->value_input,
                   got_value ? "Yes" : "No");
    }
  return ret;
}

/* force_sol_payload_encryption */

static config_err_t
force_sol_payload_encryption_checkout (bmc_config_state_data_t *state_data,
					   const struct config_section *sect,
					   struct config_keyvalue *kv)
{
  uint8_t value;
  config_err_t ret;

  if ((ret = sol_auth_checkout (state_data,
                                NULL,
                                NULL,
                                &value)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (value)
    {
      if (!(kv->value_output = strdup ("Yes")))
        {
          perror("strdup");
          return CONFIG_ERR_FATAL_ERROR;
        }
    }
  else
    {
      if (!(kv->value_output = strdup ("No")))
        {
          perror("strdup");
          return CONFIG_ERR_FATAL_ERROR;
        }
    }
  
  return CONFIG_ERR_SUCCESS;
}

static config_err_t
force_sol_payload_encryption_commit (bmc_config_state_data_t *state_data,
				     const struct config_section *sect,
				     const struct config_keyvalue *kv)
{
  uint8_t value = same (kv->value_input, "yes") ? 1 : 0;
  return sol_auth_commit (state_data,
			  NULL,
			  NULL,
			  &value);
}

static bmc_diff_t
force_sol_payload_encryption_diff (bmc_config_state_data_t *state_data,
				   const struct config_section *sect,
				   const struct config_keyvalue *kv)
{
  uint8_t passed_value;
  uint8_t got_value;
  config_err_t rc;
  bmc_diff_t ret;

  if ((rc = sol_auth_checkout (state_data,
                               NULL,
                               NULL,
                               &got_value)) != CONFIG_ERR_SUCCESS)
    {
      if (rc == CONFIG_ERR_NON_FATAL_ERROR)
        return BMC_DIFF_NON_FATAL_ERROR;
      return BMC_DIFF_FATAL_ERROR;
    }

  passed_value = same (kv->value_input, "yes") ? 1 : 0;

  if (passed_value == got_value)
    ret = BMC_DIFF_SAME;
  else 
    {
      ret = BMC_DIFF_DIFFERENT;
      report_diff (sect->section_name,
                   kv->key,
                   kv->value_input,
                   got_value ? "Yes" : "No");
    }
  return ret;
}

/* character_accumulate_interval */

static config_err_t
character_accumulate_interval_checkout (bmc_config_state_data_t *state_data,
					const struct config_section *sect,
					struct config_keyvalue *kv)
{
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
character_accumulate_interval_commit (bmc_config_state_data_t *state_data,
				      const struct config_section *sect,
				      const struct config_keyvalue *kv)
{
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

static bmc_diff_t
character_accumulate_interval_diff (bmc_config_state_data_t *state_data,
				    const struct config_section *sect,
				    const struct config_keyvalue *kv)
{
  uint8_t got_value;
  uint8_t passed_value;
  uint8_t interval;
  uint8_t threshold;
  config_err_t rc;
  bmc_diff_t ret;

  if ((rc = get_sol_character_accumulate_interval_and_send_threshold (state_data,
                                                                      &interval,
                                                                      &threshold)) != CONFIG_ERR_SUCCESS)
    {
      if (rc == CONFIG_ERR_NON_FATAL_ERROR)
        return BMC_DIFF_NON_FATAL_ERROR;
      return BMC_DIFF_FATAL_ERROR;
    }

  got_value = interval;
  passed_value = atoi (kv->value_input);

  if (passed_value == got_value)
    ret = BMC_DIFF_SAME;
  else 
    {
      char num[32];
      ret = BMC_DIFF_DIFFERENT;
      sprintf (num, "%d", got_value);
      report_diff (sect->section_name,
                   kv->key,
                   kv->value_input,
                   num);
    }
  return ret;
}

/* character_send_threshold */

static config_err_t
character_send_threshold_checkout (bmc_config_state_data_t *state_data,
				   const struct config_section *sect,
				   struct config_keyvalue *kv)
{
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
character_send_threshold_commit (bmc_config_state_data_t *state_data,
				 const struct config_section *sect,
				 const struct config_keyvalue *kv)
{
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

static bmc_diff_t
character_send_threshold_diff (bmc_config_state_data_t *state_data,
			       const struct config_section *sect,
			       const struct config_keyvalue *kv)
{
  uint8_t got_value;
  uint8_t passed_value;
  uint8_t interval;
  uint8_t threshold;
  config_err_t rc;
  bmc_diff_t ret;

  if ((rc = get_sol_character_accumulate_interval_and_send_threshold (state_data,
                                                                      &interval,
                                                                      &threshold)) != CONFIG_ERR_SUCCESS)
    {
      if (rc == CONFIG_ERR_NON_FATAL_ERROR)
        return BMC_DIFF_NON_FATAL_ERROR;
      return BMC_DIFF_FATAL_ERROR;
    }

  got_value = threshold;
  passed_value = atoi (kv->value_input);

  if (passed_value == got_value)
    ret = BMC_DIFF_SAME;
  else 
    {
      char num[32];
      ret = BMC_DIFF_DIFFERENT;
      sprintf (num, "%d", got_value);
      report_diff (sect->section_name,
                   kv->key,
                   kv->value_input,
                   num);
    }
  return ret;
}

/* sol_retry_count */

static config_err_t
sol_retry_count_checkout (bmc_config_state_data_t *state_data,
			  const struct config_section *sect,
			  struct config_keyvalue *kv)
{
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
sol_retry_count_commit (bmc_config_state_data_t *state_data,
			const struct config_section *sect,
			const struct config_keyvalue *kv)
{
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

static bmc_diff_t
sol_retry_count_diff (bmc_config_state_data_t *state_data,
		      const struct config_section *sect,
		      const struct config_keyvalue *kv)
{
  uint8_t passed_value;
  uint8_t got_value;
  uint8_t count;
  uint8_t interval;
  config_err_t rc;
  bmc_diff_t ret;

  if ((rc = get_sol_sol_retry (state_data,
                               &count,
                               &interval)) != CONFIG_ERR_SUCCESS)
    {
      if (rc == CONFIG_ERR_NON_FATAL_ERROR)
        return BMC_DIFF_NON_FATAL_ERROR;
      return BMC_DIFF_FATAL_ERROR;
    }

  got_value = count;
  passed_value = atoi (kv->value_input);

  if (passed_value == got_value)
    ret = BMC_DIFF_SAME;
  else 
    {
      char num[32];
      ret = BMC_DIFF_DIFFERENT;
      sprintf (num, "%d", got_value);
      report_diff (sect->section_name,
                   kv->key,
                   kv->value_input,
                   num);
    }
  return ret;
}

/* sol_retry_interval */

static config_err_t
sol_retry_interval_checkout (bmc_config_state_data_t *state_data,
			     const struct config_section *sect,
			     struct config_keyvalue *kv)
{
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
sol_retry_interval_commit (bmc_config_state_data_t *state_data,
			   const struct config_section *sect,
			   const struct config_keyvalue *kv)
{
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

static bmc_diff_t
sol_retry_interval_diff (bmc_config_state_data_t *state_data,
			 const struct config_section *sect,
			 const struct config_keyvalue *kv)
{
  uint8_t passed_value;
  uint8_t got_value;
  uint8_t count;
  uint8_t interval;
  config_err_t rc;
  bmc_diff_t ret;

  if ((rc = get_sol_sol_retry (state_data,
                               &count,
                               &interval)) != CONFIG_ERR_SUCCESS)
    {
      if (rc == CONFIG_ERR_NON_FATAL_ERROR)
        return BMC_DIFF_NON_FATAL_ERROR;
      return BMC_DIFF_FATAL_ERROR;
    }
       
  got_value = interval;
  passed_value = atoi (kv->value_input);

  if (passed_value == got_value)
    ret = BMC_DIFF_SAME;
  else 
    {
      char num[32];
      ret = BMC_DIFF_DIFFERENT;
      sprintf (num, "%d", got_value);
      report_diff (sect->section_name,
                   kv->key,
                   kv->value_input,
                   num);
    }
  return ret;
}

static config_err_t
non_volatile_bit_rate_checkout (bmc_config_state_data_t *state_data,
				const struct config_section *sect,
				struct config_keyvalue *kv)
{
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
non_volatile_bit_rate_commit (bmc_config_state_data_t *state_data,
			      const struct config_section *sect,
			      const struct config_keyvalue *kv)
{
  return set_sol_sol_non_volatile_bit_rate (state_data,
					    sol_bit_rate_number (kv->value_input));
}

static bmc_diff_t
non_volatile_bit_rate_diff (bmc_config_state_data_t *state_data,
			    const struct config_section *sect,
			    const struct config_keyvalue *kv)
{
  uint8_t got_value;
  uint8_t passed_value;
  config_err_t rc;
  bmc_diff_t ret;

  if ((rc = get_sol_sol_non_volatile_bit_rate (state_data,
                                               &got_value)) != CONFIG_ERR_SUCCESS)
    {
      if (rc == CONFIG_ERR_NON_FATAL_ERROR)
        return BMC_DIFF_NON_FATAL_ERROR;
      return BMC_DIFF_FATAL_ERROR;
    }

  passed_value = sol_bit_rate_number (kv->value_input);

  if (passed_value == got_value)
    ret = BMC_DIFF_SAME;
  else 
    {
      ret = BMC_DIFF_DIFFERENT;
      report_diff (sect->section_name,
                   kv->key,
                   kv->value_input,
                   sol_bit_rate_string (got_value));
    }
  return ret;
}

/* volatile_bit_rate */

static config_err_t
volatile_bit_rate_checkout (bmc_config_state_data_t *state_data,
			    const struct config_section *sect,
			    struct config_keyvalue *kv)
{
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
volatile_bit_rate_commit (bmc_config_state_data_t *state_data,
			  const struct config_section *sect,
			  const struct config_keyvalue *kv)
{
  return set_sol_sol_volatile_bit_rate (state_data,
					sol_bit_rate_number (kv->value_input));
}

static bmc_diff_t
volatile_bit_rate_diff (bmc_config_state_data_t *state_data,
			const struct config_section *sect,
			const struct config_keyvalue *kv)
{
  uint8_t got_value;
  uint8_t passed_value;
  config_err_t rc;
  bmc_diff_t ret;

  if ((rc = get_sol_sol_volatile_bit_rate (state_data,
					   &got_value)) != CONFIG_ERR_SUCCESS)
    {
      if (rc == CONFIG_ERR_NON_FATAL_ERROR)
        return BMC_DIFF_NON_FATAL_ERROR;
      return BMC_DIFF_FATAL_ERROR;
    }

  passed_value = sol_bit_rate_number (kv->value_input);


  if (passed_value == got_value)
    ret = BMC_DIFF_SAME;
  else 
    {
      ret = BMC_DIFF_DIFFERENT;
      report_diff (sect->section_name,
                   kv->key,
                   kv->value_input,
                   sol_bit_rate_string (got_value));
    }
  return ret;
}

static config_err_t
port_checkout (bmc_config_state_data_t *state_data,
	       const struct config_section *sect,
	       struct config_keyvalue *kv)
{
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
port_commit (bmc_config_state_data_t *state_data,
	     const struct config_section *sect,
	     const struct config_keyvalue *kv)
{
  return set_sol_sol_payload_port_number (state_data,
					  atoi (kv->value_input));
}

static bmc_diff_t
port_diff (bmc_config_state_data_t *state_data,
	   const struct config_section *sect,
	   const struct config_keyvalue *kv)
{
  uint16_t got_value;
  uint16_t passed_value;
  config_err_t rc;
  bmc_diff_t ret;

  if ((rc = get_sol_sol_payload_port_number (state_data,
                                             &got_value)) != CONFIG_ERR_SUCCESS)
    {
      if (rc == CONFIG_ERR_NON_FATAL_ERROR)
        return BMC_DIFF_NON_FATAL_ERROR;
      return BMC_DIFF_FATAL_ERROR;
    }

  passed_value = atoi (kv->value_input);

  if (passed_value == got_value)
    ret = BMC_DIFF_SAME;
  else 
    {
      char num[32];
      ret = BMC_DIFF_DIFFERENT;
      sprintf (num, "%d", got_value);
      report_diff (sect->section_name,
                   kv->key,
                   kv->value_input,
                   num);
    }
  return ret;
}

static config_err_t
_sol_conf_checkout(const char *section_name,
                   struct config_keyvalue *keyvalues,
                   int debug,
                   void *arg)
{
  bmc_config_state_data_t *state_data;
  struct config_keyvalue *kv;

  assert(section_name);
  assert(keyvalues);
  assert(arg);

  state_data = (bmc_config_state_data_t *)arg;

  kv = keyvalues;
  while (kv)
    {
      assert(!kv->value_output);

      kv = kv->next;
    }
}

static config_err_t
_sol_conf_commit(const char *section_name,
                 struct config_keyvalue *keyvalues,
                 int debug,
                 void *arg)
{
  bmc_config_state_data_t *state_data;
  struct config_keyvalue *kv;

  assert(section_name);
  assert(keyvalues);
  assert(arg);

  state_data = (bmc_config_state_data_t *)arg;

  kv = keyvalues;
  while (kv)
    {
      assert(kv->value_input);

      kv = kv->next;
    }
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
                                                 0,
                                                 _sol_conf_checkout,
                                                 _sol_conf_commit)))
    goto cleanup;

  if (config_section_add_key (sol_conf_section,
                              "Enable_SOL",
                              "Possible values: Yes/No",
                              0,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (sol_conf_section,
                              "SOL_Privilege_Level",
                              "Possible values: Callback/User/Operator/Administrator/OEM_Proprietary",
                              0,
                              privilege_level_number_validate) < 0)
    goto cleanup;

  if (config_section_add_key (sol_conf_section,
                              "Force_SOL_Payload_Authentication",
                              "Possible values: Yes/No",
                              0,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (sol_conf_section,
                              "Force_SOL_Payload_Encryption",
                              "Possible values: Yes/No",
                              0,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (sol_conf_section,
                              "Character_Accumulate_Interval",
                              "Give a non-zero valid integer. Each unit is 5ms",
                              0,
                              config_number_range_one_byte_non_zero) < 0)
    goto cleanup;

  if (config_section_add_key (sol_conf_section,
                              "Character_Send_Threshold",
                              "Give a valid number",
                              0,
                              config_number_range_one_byte) < 0)
    goto cleanup;

  if (config_section_add_key (sol_conf_section,
                              "SOL_Retry_Count",
                              "Give a valid integer",
                              0,
                              config_number_range_one_byte) < 0)
    goto cleanup;

  if (config_section_add_key (sol_conf_section,
                              "SOL_Retry_Interval",
                              "Give a valid integer. Interval unit is 10ms",
                              0,
                              config_number_range_one_byte) < 0)
    goto cleanup;

  if (config_section_add_key (sol_conf_section,
                              "Non_Volatile_Bit_Rate",
                              "Possible values: Serial/9600/19200/38400/57600/115200",
                              0,
                              sol_bit_rate_number_validate) < 0)
    goto cleanup;

  if (config_section_add_key (sol_conf_section,
                              "Volatile_Bit_Rate",
                              "Possible values: Serial/9600/19200/38400/57600/115200",
                              0,
                              sol_bit_rate_number_validate) < 0)
    goto cleanup;

  if (config_section_add_key (sol_conf_section,
                              "SOL_Payload_Port_Number",
                              "Give a valid port number",
                              CONFIG_CHECKOUT_KEY_COMMENTED_OUT,
                              config_number_range_two_bytes) < 0)
    goto cleanup;

  return sol_conf_section;

 cleanup:
  if (sol_conf_section)
    config_section_destroy(sol_conf_section);
  return NULL;
}
