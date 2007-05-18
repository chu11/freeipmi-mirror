#include "bmc-config.h"
#include "bmc-config-common.h"
#include "bmc-config-wrapper.h"
#include "bmc-config-diff.h"
#include "bmc-config-map.h"
#include "bmc-config-sections.h"
#include "bmc-config-validate.h"

static bmc_err_t
sol_auth_checkout (bmc_config_state_data_t *state_data,
		   uint8_t *sol_privilege_level,
		   uint8_t *force_sol_payload_authentication,
		   uint8_t *force_sol_payload_encryption)
{
  uint8_t tmp_sol_privilege_level;
  uint8_t tmp_force_sol_payload_authentication;
  uint8_t tmp_force_sol_payload_encryption;
  bmc_err_t ret;

  if ((ret = get_sol_sol_authentication (state_data,
                                         &tmp_sol_privilege_level,
                                         &tmp_force_sol_payload_authentication,
                                         &tmp_force_sol_payload_encryption)) != BMC_ERR_SUCCESS)
    return ret;

  if (sol_privilege_level)
    *sol_privilege_level = tmp_sol_privilege_level;

  if (force_sol_payload_authentication)
    *force_sol_payload_authentication = tmp_force_sol_payload_authentication;

  if (force_sol_payload_encryption)
    *force_sol_payload_encryption = tmp_force_sol_payload_encryption;

  return BMC_ERR_SUCCESS;
}


static bmc_err_t
sol_auth_commit (bmc_config_state_data_t *state_data,
		 uint8_t *sol_privilege_level,
		 uint8_t *force_sol_payload_authentication,
		 uint8_t *force_sol_payload_encryption)
{
  uint8_t tmp_sol_privilege_level;
  uint8_t tmp_force_sol_payload_authentication;
  uint8_t tmp_force_sol_payload_encryption;
  bmc_err_t ret;

  if ((ret = get_sol_sol_authentication (state_data,
                                         &tmp_sol_privilege_level,
                                         &tmp_force_sol_payload_authentication,
                                         &tmp_force_sol_payload_encryption)) != BMC_ERR_SUCCESS)
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

static bmc_err_t
enable_sol_checkout (bmc_config_state_data_t *state_data,
		     const struct section *sect,
		     struct keyvalue *kv)
{
  uint8_t enable;
  bmc_err_t ret;

  if ((ret = get_sol_sol_enable (state_data,
                                 &enable)) != BMC_ERR_SUCCESS)
    return ret;

  if (kv->value)
    free (kv->value);

  if (enable)
    {
      if (!(kv->value = strdup ("Yes")))
        {
          perror("strdup");
          return BMC_ERR_FATAL_ERROR;
        }
    }
  else
    {
      if (!(kv->value = strdup ("No")))
        {
          perror("strdup");
          return BMC_ERR_FATAL_ERROR;
        }
    }

  return BMC_ERR_SUCCESS;
}

static bmc_err_t
enable_sol_commit (bmc_config_state_data_t *state_data,
		   const struct section *sect,
		   const struct keyvalue *kv)
{
  return set_sol_sol_enable (state_data,
			     same (kv->value, "yes"));
}

static bmc_diff_t
enable_sol_diff (bmc_config_state_data_t *state_data,
		 const struct section *sect,
		 const struct keyvalue *kv)
{
  uint8_t got_value;
  uint8_t passed_value;
  bmc_err_t rc;
  bmc_diff_t ret;
  
  if ((rc = get_sol_sol_enable (state_data,
                                &got_value)) != BMC_ERR_SUCCESS)
    {
      if (rc == BMC_ERR_NON_FATAL_ERROR)
        return BMC_DIFF_NON_FATAL_ERROR;
      return BMC_DIFF_FATAL_ERROR;
    }

  passed_value = same (kv->value, "yes") ? 1 : 0;

  if (passed_value == got_value)
    ret = BMC_DIFF_SAME;
  else 
    {
      ret = BMC_DIFF_DIFFERENT;
      report_diff (sect->section_name,
                   kv->key,
                   kv->value,
                   got_value ? "Yes" : "No");
    }
  return ret;
}

static bmc_err_t
sol_privilege_level_checkout (bmc_config_state_data_t *state_data,
			      const struct section *sect,
			      struct keyvalue *kv)
{
  uint8_t value;
  bmc_err_t ret;

  if ((ret = sol_auth_checkout (state_data,
                                &value,
                                NULL,
                                NULL)) != BMC_ERR_SUCCESS)
    return ret;

  if (kv->value)
    free (kv->value);

  if (!(kv->value = strdup (privilege_level_string (value))))
    {
      perror("strdup");
      return BMC_ERR_FATAL_ERROR;
    }
  return BMC_ERR_SUCCESS;
}

static bmc_err_t
sol_privilege_level_commit (bmc_config_state_data_t *state_data,
			    const struct section *sect,
			    const struct keyvalue *kv)
{
  uint8_t value = privilege_level_number (kv->value);
  return sol_auth_commit (state_data,
			  &value,
			  NULL,
			  NULL);
}

static bmc_diff_t
sol_privilege_level_diff (bmc_config_state_data_t *state_data,
			  const struct section *sect,
			  const struct keyvalue *kv)
{
  uint8_t passed_value;
  uint8_t got_value;
  bmc_err_t rc;
  bmc_diff_t ret;

  if ((rc = sol_auth_checkout (state_data,
                               &got_value,
                               NULL,
                               NULL)) != BMC_ERR_SUCCESS)
    {
      if (rc == BMC_ERR_NON_FATAL_ERROR)
        return BMC_DIFF_NON_FATAL_ERROR;
      return BMC_DIFF_FATAL_ERROR;
    }

  passed_value = privilege_level_number (kv->value);

  if (passed_value == got_value)
    ret = BMC_DIFF_SAME;
  else 
    {
      ret = BMC_DIFF_DIFFERENT;
      report_diff (sect->section_name,
                   kv->key,
                   kv->value,
                   privilege_level_string (got_value));
    }
  return ret;
}


/* force_sol_payload_authentication */
static bmc_err_t
force_sol_payload_authentication_checkout (bmc_config_state_data_t *state_data,
					   const struct section *sect,
					   struct keyvalue *kv)
{
  uint8_t value;
  bmc_err_t ret;

  if ((ret = sol_auth_checkout (state_data,
                                NULL,
                                &value,
                                NULL)) != BMC_ERR_SUCCESS)
    return ret;

  if (kv->value)
    free (kv->value);

  if (value)
    {
      if (!(kv->value = strdup ("Yes")))
        {
          perror("strdup");
          return BMC_ERR_FATAL_ERROR;
        }
    }
  else
    {
      if (!(kv->value = strdup ("No")))
        {
          perror("strdup");
          return BMC_ERR_FATAL_ERROR;
        }
    }
  
  return BMC_ERR_SUCCESS;
}

static bmc_err_t
force_sol_payload_authentication_commit (bmc_config_state_data_t *state_data,
					 const struct section *sect,
					 const struct keyvalue *kv)
{
  uint8_t value = same (kv->value, "yes") ? 1 : 0;
  return sol_auth_commit (state_data,
			  NULL,
			  &value,
			  NULL);
}

static bmc_diff_t
force_sol_payload_authentication_diff (bmc_config_state_data_t *state_data,
				       const struct section *sect,
				       const struct keyvalue *kv)
{
  uint8_t passed_value;
  uint8_t got_value;
  bmc_err_t rc;
  bmc_diff_t ret;

  if ((rc = sol_auth_checkout (state_data,
                               NULL,
                               &got_value,
                               NULL)) != BMC_ERR_SUCCESS)
    {
      if (rc == BMC_ERR_NON_FATAL_ERROR)
        return BMC_DIFF_NON_FATAL_ERROR;
      return BMC_DIFF_FATAL_ERROR;
    }

  passed_value = same (kv->value, "yes") ? 1 : 0;

  if (passed_value == got_value)
    ret = BMC_DIFF_SAME;
  else 
    {
      ret = BMC_DIFF_DIFFERENT;
      report_diff (sect->section_name,
                   kv->key,
                   kv->value,
                   got_value ? "Yes" : "No");
    }
  return ret;
}

/* force_sol_payload_encryption */

static bmc_err_t
force_sol_payload_encryption_checkout (bmc_config_state_data_t *state_data,
					   const struct section *sect,
					   struct keyvalue *kv)
{
  uint8_t value;
  bmc_err_t ret;

  if ((ret = sol_auth_checkout (state_data,
                                NULL,
                                NULL,
                                &value)) != BMC_ERR_SUCCESS)
    return ret;

  if (kv->value)
    free (kv->value);

  if (value)
    {
      if (!(kv->value = strdup ("Yes")))
        {
          perror("strdup");
          return BMC_ERR_FATAL_ERROR;
        }
    }
  else
    {
      if (!(kv->value = strdup ("No")))
        {
          perror("strdup");
          return BMC_ERR_FATAL_ERROR;
        }
    }
  
  return BMC_ERR_SUCCESS;
}

static bmc_err_t
force_sol_payload_encryption_commit (bmc_config_state_data_t *state_data,
				     const struct section *sect,
				     const struct keyvalue *kv)
{
  uint8_t value = same (kv->value, "yes") ? 1 : 0;
  return sol_auth_commit (state_data,
			  NULL,
			  NULL,
			  &value);
}

static bmc_diff_t
force_sol_payload_encryption_diff (bmc_config_state_data_t *state_data,
				   const struct section *sect,
				   const struct keyvalue *kv)
{
  uint8_t passed_value;
  uint8_t got_value;
  bmc_err_t rc;
  bmc_diff_t ret;

  if ((rc = sol_auth_checkout (state_data,
                               NULL,
                               NULL,
                               &got_value)) != BMC_ERR_SUCCESS)
    {
      if (rc == BMC_ERR_NON_FATAL_ERROR)
        return BMC_DIFF_NON_FATAL_ERROR;
      return BMC_DIFF_FATAL_ERROR;
    }

  passed_value = same (kv->value, "yes") ? 1 : 0;

  if (passed_value == got_value)
    ret = BMC_DIFF_SAME;
  else 
    {
      ret = BMC_DIFF_DIFFERENT;
      report_diff (sect->section_name,
                   kv->key,
                   kv->value,
                   got_value ? "Yes" : "No");
    }
  return ret;
}

/* character_accumulate_interval */

static bmc_err_t
character_accumulate_interval_checkout (bmc_config_state_data_t *state_data,
					const struct section *sect,
					struct keyvalue *kv)
{
  uint8_t interval;
  uint8_t threshold;
  bmc_err_t ret;

  if ((ret = get_sol_character_accumulate_interval_and_send_threshold (state_data,
                                                                       &interval,
                                                                       &threshold)) != BMC_ERR_SUCCESS)
    return ret;

  if (kv->value)
    free (kv->value);

  if (asprintf (&kv->value, "%d", interval) < 0)
    {
      perror("asprintf");
      return BMC_ERR_FATAL_ERROR;
    }
  return BMC_ERR_SUCCESS;
}

static bmc_err_t
character_accumulate_interval_commit (bmc_config_state_data_t *state_data,
				      const struct section *sect,
				      const struct keyvalue *kv)
{
  uint8_t interval;
  uint8_t threshold;
  bmc_err_t ret;

  if ((ret = get_sol_character_accumulate_interval_and_send_threshold (state_data,
                                                                       &interval,
                                                                       &threshold)) != BMC_ERR_SUCCESS)
    return ret;

  interval = atoi (kv->value);

  return set_sol_character_accumulate_interval_and_send_threshold (state_data,
								   interval,
								   threshold);
}

static bmc_diff_t
character_accumulate_interval_diff (bmc_config_state_data_t *state_data,
				    const struct section *sect,
				    const struct keyvalue *kv)
{
  uint8_t got_value;
  uint8_t passed_value;
  uint8_t interval;
  uint8_t threshold;
  bmc_err_t rc;
  bmc_diff_t ret;

  if ((rc = get_sol_character_accumulate_interval_and_send_threshold (state_data,
                                                                      &interval,
                                                                      &threshold)) != BMC_ERR_SUCCESS)
    {
      if (rc == BMC_ERR_NON_FATAL_ERROR)
        return BMC_DIFF_NON_FATAL_ERROR;
      return BMC_DIFF_FATAL_ERROR;
    }

  got_value = interval;
  passed_value = atoi (kv->value);

  if (passed_value == got_value)
    ret = BMC_DIFF_SAME;
  else 
    {
      char num[32];
      ret = BMC_DIFF_DIFFERENT;
      sprintf (num, "%d", got_value);
      report_diff (sect->section_name,
                   kv->key,
                   kv->value,
                   num);
    }
  return ret;
}

/* character_send_threshold */

static bmc_err_t
character_send_threshold_checkout (bmc_config_state_data_t *state_data,
				   const struct section *sect,
				   struct keyvalue *kv)
{
  uint8_t interval;
  uint8_t threshold;
  bmc_err_t ret;

  if ((ret = get_sol_character_accumulate_interval_and_send_threshold (state_data,
                                                                       &interval,
                                                                       &threshold)) != BMC_ERR_SUCCESS)
    return ret;

  if (kv->value)
    free (kv->value);

  if (asprintf (&kv->value, "%d", threshold) < 0)
    {
      perror("asprintf");
      return BMC_ERR_FATAL_ERROR;
    }
  return BMC_ERR_SUCCESS;
}

static bmc_err_t
character_send_threshold_commit (bmc_config_state_data_t *state_data,
				 const struct section *sect,
				 const struct keyvalue *kv)
{
  uint8_t interval;
  uint8_t threshold;
  bmc_err_t ret;

  if ((ret = get_sol_character_accumulate_interval_and_send_threshold (state_data,
                                                                       &interval,
                                                                       &threshold)) != BMC_ERR_SUCCESS)
    return ret;

  threshold = atoi (kv->value);

  return set_sol_character_accumulate_interval_and_send_threshold (state_data,
								   interval,
								   threshold);
}

static bmc_diff_t
character_send_threshold_diff (bmc_config_state_data_t *state_data,
			       const struct section *sect,
			       const struct keyvalue *kv)
{
  uint8_t got_value;
  uint8_t passed_value;
  uint8_t interval;
  uint8_t threshold;
  bmc_err_t rc;
  bmc_diff_t ret;

  if ((rc = get_sol_character_accumulate_interval_and_send_threshold (state_data,
                                                                      &interval,
                                                                      &threshold)) != BMC_ERR_SUCCESS)
    {
      if (rc == BMC_ERR_NON_FATAL_ERROR)
        return BMC_DIFF_NON_FATAL_ERROR;
      return BMC_DIFF_FATAL_ERROR;
    }

  got_value = threshold;
  passed_value = atoi (kv->value);

  if (passed_value == got_value)
    ret = BMC_DIFF_SAME;
  else 
    {
      char num[32];
      ret = BMC_DIFF_DIFFERENT;
      sprintf (num, "%d", got_value);
      report_diff (sect->section_name,
                   kv->key,
                   kv->value,
                   num);
    }
  return ret;
}

/* sol_retry_count */

static bmc_err_t
sol_retry_count_checkout (bmc_config_state_data_t *state_data,
			  const struct section *sect,
			  struct keyvalue *kv)
{
  uint8_t count;
  uint8_t interval;
  bmc_err_t ret;

  if ((ret = get_sol_sol_retry (state_data,
                                &count,
                                &interval)) != BMC_ERR_SUCCESS)
    return ret;

  if (kv->value)
    free (kv->value);

  if (asprintf (&kv->value, "%d", count) < 0)
    {
      perror("asprintf");
      return BMC_ERR_FATAL_ERROR;
    }
  return BMC_ERR_SUCCESS;
}


static bmc_err_t
sol_retry_count_commit (bmc_config_state_data_t *state_data,
			const struct section *sect,
			const struct keyvalue *kv)
{
  uint8_t count;
  uint8_t interval;
  bmc_err_t ret;

  if ((ret = get_sol_sol_retry (state_data,
                                &count,
                                &interval)) != BMC_ERR_SUCCESS)
    return ret;

  count = atoi (kv->value);

  return set_sol_sol_retry (state_data,
			    count,
			    interval);
}

static bmc_diff_t
sol_retry_count_diff (bmc_config_state_data_t *state_data,
		      const struct section *sect,
		      const struct keyvalue *kv)
{
  uint8_t passed_value;
  uint8_t got_value;
  uint8_t count;
  uint8_t interval;
  bmc_err_t rc;
  bmc_diff_t ret;

  if ((rc = get_sol_sol_retry (state_data,
                               &count,
                               &interval)) != BMC_ERR_SUCCESS)
    {
      if (rc == BMC_ERR_NON_FATAL_ERROR)
        return BMC_DIFF_NON_FATAL_ERROR;
      return BMC_DIFF_FATAL_ERROR;
    }

  got_value = count;
  passed_value = atoi (kv->value);

  if (passed_value == got_value)
    ret = BMC_DIFF_SAME;
  else 
    {
      char num[32];
      ret = BMC_DIFF_DIFFERENT;
      sprintf (num, "%d", got_value);
      report_diff (sect->section_name,
                   kv->key,
                   kv->value,
                   num);
    }
  return ret;
}

/* sol_retry_interval */

static bmc_err_t
sol_retry_interval_checkout (bmc_config_state_data_t *state_data,
			     const struct section *sect,
			     struct keyvalue *kv)
{
  uint8_t count;
  uint8_t interval;
  bmc_err_t ret;

  if ((ret = get_sol_sol_retry (state_data,
                                &count,
                                &interval)) != BMC_ERR_SUCCESS)
    return ret;

  if (kv->value)
    free (kv->value);

  if (asprintf (&kv->value, "%d", interval) < 0)
    {
      perror("asprintf");
      return BMC_ERR_FATAL_ERROR;
    }
  return BMC_ERR_SUCCESS;
}


static bmc_err_t
sol_retry_interval_commit (bmc_config_state_data_t *state_data,
			   const struct section *sect,
			   const struct keyvalue *kv)
{
  uint8_t count;
  uint8_t interval;
  bmc_err_t ret;

  if ((ret = get_sol_sol_retry (state_data,
                                &count,
                                &interval)) != BMC_ERR_SUCCESS)
    return ret;

  interval = atoi (kv->value);

  return set_sol_sol_retry (state_data,
			    count,
			    interval);
}

static bmc_diff_t
sol_retry_interval_diff (bmc_config_state_data_t *state_data,
			 const struct section *sect,
			 const struct keyvalue *kv)
{
  uint8_t passed_value;
  uint8_t got_value;
  uint8_t count;
  uint8_t interval;
  bmc_err_t rc;
  bmc_diff_t ret;

  if ((rc = get_sol_sol_retry (state_data,
                               &count,
                               &interval)) != BMC_ERR_SUCCESS)
    {
      if (rc == BMC_ERR_NON_FATAL_ERROR)
        return BMC_DIFF_NON_FATAL_ERROR;
      return BMC_DIFF_FATAL_ERROR;
    }
       
  got_value = interval;
  passed_value = atoi (kv->value);

  if (passed_value == got_value)
    ret = BMC_DIFF_SAME;
  else 
    {
      char num[32];
      ret = BMC_DIFF_DIFFERENT;
      sprintf (num, "%d", got_value);
      report_diff (sect->section_name,
                   kv->key,
                   kv->value,
                   num);
    }
  return ret;
}

static bmc_err_t
non_volatile_bit_rate_checkout (bmc_config_state_data_t *state_data,
				const struct section *sect,
				struct keyvalue *kv)
{
  uint8_t bitrate;
  bmc_err_t ret;

  if ((ret = get_sol_sol_non_volatile_bit_rate (state_data,
                                                &bitrate)) != BMC_ERR_SUCCESS)
    return ret;

  if (kv->value)
    free (kv->value);

  if (!(kv->value = strdup (sol_bit_rate_string (bitrate))))
    {
      perror("strdup");
      return BMC_ERR_FATAL_ERROR;
    }
  return BMC_ERR_SUCCESS;
}

static bmc_err_t
non_volatile_bit_rate_commit (bmc_config_state_data_t *state_data,
			      const struct section *sect,
			      const struct keyvalue *kv)
{
  return set_sol_sol_non_volatile_bit_rate (state_data,
					    sol_bit_rate_number (kv->value));
}

static bmc_diff_t
non_volatile_bit_rate_diff (bmc_config_state_data_t *state_data,
			    const struct section *sect,
			    const struct keyvalue *kv)
{
  uint8_t got_value;
  uint8_t passed_value;
  bmc_err_t rc;
  bmc_diff_t ret;

  if ((rc = get_sol_sol_non_volatile_bit_rate (state_data,
                                               &got_value)) != BMC_ERR_SUCCESS)
    {
      if (rc == BMC_ERR_NON_FATAL_ERROR)
        return BMC_DIFF_NON_FATAL_ERROR;
      return BMC_DIFF_FATAL_ERROR;
    }

  passed_value = sol_bit_rate_number (kv->value);

  if (passed_value == got_value)
    ret = BMC_DIFF_SAME;
  else 
    {
      ret = BMC_DIFF_DIFFERENT;
      report_diff (sect->section_name,
                   kv->key,
                   kv->value,
                   sol_bit_rate_string (got_value));
    }
  return ret;
}

/* volatile_bit_rate */

static bmc_err_t
volatile_bit_rate_checkout (bmc_config_state_data_t *state_data,
			    const struct section *sect,
			    struct keyvalue *kv)
{
  uint8_t bitrate;
  bmc_err_t ret;

  if ((ret = get_sol_sol_volatile_bit_rate (state_data,
                                            &bitrate)) != BMC_ERR_SUCCESS)
    return ret;

  if (kv->value)
    free (kv->value);

  if (!(kv->value = strdup (sol_bit_rate_string (bitrate))))
    {
      perror("strdup");
      return BMC_ERR_FATAL_ERROR;
    }
  return BMC_ERR_SUCCESS;
}

static bmc_err_t
volatile_bit_rate_commit (bmc_config_state_data_t *state_data,
			  const struct section *sect,
			  const struct keyvalue *kv)
{
  return set_sol_sol_volatile_bit_rate (state_data,
					sol_bit_rate_number (kv->value));
}

static bmc_diff_t
volatile_bit_rate_diff (bmc_config_state_data_t *state_data,
			const struct section *sect,
			const struct keyvalue *kv)
{
  uint8_t got_value;
  uint8_t passed_value;
  bmc_err_t rc;
  bmc_diff_t ret;

  if ((rc = get_sol_sol_volatile_bit_rate (state_data,
					   &got_value)) != BMC_ERR_SUCCESS)
    {
      if (rc == BMC_ERR_NON_FATAL_ERROR)
        return BMC_DIFF_NON_FATAL_ERROR;
      return BMC_DIFF_FATAL_ERROR;
    }

  passed_value = sol_bit_rate_number (kv->value);


  if (passed_value == got_value)
    ret = BMC_DIFF_SAME;
  else 
    {
      ret = BMC_DIFF_DIFFERENT;
      report_diff (sect->section_name,
                   kv->key,
                   kv->value,
                   sol_bit_rate_string (got_value));
    }
  return ret;
}

static bmc_err_t
port_checkout (bmc_config_state_data_t *state_data,
	       const struct section *sect,
	       struct keyvalue *kv)
{
  uint16_t port;
  bmc_err_t ret;

  if ((ret = get_sol_sol_payload_port_number (state_data,
                                              &port)) != BMC_ERR_SUCCESS)
    return ret;

  if (kv->value)
    free (kv->value);

  if (asprintf (&kv->value, "%d", port) < 0)
    {
      perror("asprintf");
      return BMC_ERR_FATAL_ERROR;
    }
  return BMC_ERR_SUCCESS;
}


static bmc_err_t
port_commit (bmc_config_state_data_t *state_data,
	     const struct section *sect,
	     const struct keyvalue *kv)
{
  return set_sol_sol_payload_port_number (state_data,
					  atoi (kv->value));
}

static bmc_diff_t
port_diff (bmc_config_state_data_t *state_data,
	   const struct section *sect,
	   const struct keyvalue *kv)
{
  uint16_t got_value;
  uint16_t passed_value;
  bmc_err_t rc;
  bmc_diff_t ret;

  if ((rc = get_sol_sol_payload_port_number (state_data,
                                             &got_value)) != BMC_ERR_SUCCESS)
    {
      if (rc == BMC_ERR_NON_FATAL_ERROR)
        return BMC_DIFF_NON_FATAL_ERROR;
      return BMC_DIFF_FATAL_ERROR;
    }

  passed_value = atoi (kv->value);

  if (passed_value == got_value)
    ret = BMC_DIFF_SAME;
  else 
    {
      char num[32];
      ret = BMC_DIFF_DIFFERENT;
      sprintf (num, "%d", got_value);
      report_diff (sect->section_name,
                   kv->key,
                   kv->value,
                   num);
    }
  return ret;
}

static bmc_validate_t
port_validate (bmc_config_state_data_t *state_data,
	       const struct section *sect,
	       const char *value)
{
  long int num;
  char *endptr;

  num = strtol (value, &endptr, 0);

  if (*endptr)
    return BMC_VALIDATE_INVALID_VALUE;

  if (num < 0 || num > 65535)
    return BMC_VALIDATE_INVALID_VALUE;

  return BMC_VALIDATE_VALID_VALUE;
}

struct section *
bmc_sol_conf_section_get (bmc_config_state_data_t *state_data)
{
  struct section * sol_conf_section = NULL;

  if (!(sol_conf_section = bmc_config_section_create(state_data, "SOL_Conf")))
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       sol_conf_section,
                                       "Enable_SOL",
                                       "Possible values: Yes/No",
                                       0,
                                       enable_sol_checkout,
                                       enable_sol_commit,
                                       enable_sol_diff,
                                       yes_no_validate) < 0)
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       sol_conf_section,
                                       "SOL_Privilege_Level",
                                       "Possible values: Callback/User/Operator/Administrator/OEM_Proprietary",
                                       0,
                                       sol_privilege_level_checkout,
                                       sol_privilege_level_commit,
                                       sol_privilege_level_diff,
                                       privilege_level_number_validate) < 0)
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       sol_conf_section,
                                       "Force_SOL_Payload_Authentication",
                                       "Possible values: Yes/No",
                                       0,
                                       force_sol_payload_authentication_checkout,
                                       force_sol_payload_authentication_commit,
                                       force_sol_payload_authentication_diff,
                                       yes_no_validate) < 0)
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       sol_conf_section,
                                       "Force_SOL_Payload_Encryption",
                                       "Possible values: Yes/No",
                                       0,
                                       force_sol_payload_encryption_checkout,
                                       force_sol_payload_encryption_commit,
                                       force_sol_payload_encryption_diff,
                                       yes_no_validate) < 0)
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       sol_conf_section,
                                       "Character_Accumulate_Interval",
                                       "Give a valid integer. Each unit is 5ms",
                                       0,
                                       character_accumulate_interval_checkout,
                                       character_accumulate_interval_commit,
                                       character_accumulate_interval_diff,
                                       number_range_one_byte) < 0)
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       sol_conf_section,
                                       "Character_Send_Threshold",
                                       "Give a valid number",
                                       0,
                                       character_send_threshold_checkout,
                                       character_send_threshold_commit,
                                       character_send_threshold_diff,
                                       number_range_one_byte) < 0)
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       sol_conf_section,
                                       "SOL_Retry_Count",
                                       "Give a valid integer",
                                       0,
                                       sol_retry_count_checkout,
                                       sol_retry_count_commit,
                                       sol_retry_count_diff,
                                       number_range_one_byte) < 0)
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       sol_conf_section,
                                       "SOL_Retry_Interval",
                                       "Give a valid integer. Interval unit is 10ms",
                                       0,
                                       sol_retry_interval_checkout,
                                       sol_retry_interval_commit,
                                       sol_retry_interval_diff,
                                       number_range_one_byte) < 0)
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       sol_conf_section,
                                       "Non_Volatile_Bit_Rate",
                                       "Possible values: Serial/9600/19200/38400/57600/115200",
                                       0,
                                       non_volatile_bit_rate_checkout,
                                       non_volatile_bit_rate_commit,
                                       non_volatile_bit_rate_diff,
                                       sol_bit_rate_number_validate) < 0)
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       sol_conf_section,
                                       "Volatile_Bit_Rate",
                                       "Possible values: Serial/9600/19200/38400/57600/115200",
                                       0,
                                       volatile_bit_rate_checkout,
                                       volatile_bit_rate_commit,
                                       volatile_bit_rate_diff,
                                       sol_bit_rate_number_validate) < 0)
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       sol_conf_section,
                                       "SOL_Payload_Port_Number",
                                       "Give a valid port number",
                                       BMC_CHECKOUT_KEY_COMMENTED_OUT,
                                       port_checkout,
                                       port_commit,
                                       port_diff,
                                       port_validate) < 0)
    goto cleanup;

  return sol_conf_section;

 cleanup:
  if (sol_conf_section)
    bmc_config_section_destroy(state_data, sol_conf_section);
  return NULL;
}
