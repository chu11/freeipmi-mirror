#include "bmc-config.h"
#include "bmc-common.h"
#include "bmc-config-api.h"
#include "bmc-diff.h"
#include "bmc-sections.h"

static bmc_err_t
pef_control_checkout (ipmi_device_t dev,
		      uint8_t *pef,
		      uint8_t *pef_event_messages,
		      uint8_t *pef_startup_delay,
		      uint8_t *pef_alert_startup_delay)
{
  uint8_t tmp_pef;
  uint8_t tmp_pef_event_messages;
  uint8_t tmp_pef_startup_delay;
  uint8_t tmp_pef_alert_startup_delay;
  bmc_err_t ret;

  if ((ret = get_pef_control (dev,
                              &tmp_pef,
                              &tmp_pef_event_messages,
                              &tmp_pef_startup_delay,
                              &tmp_pef_alert_startup_delay)) != BMC_ERR_SUCCESS)
    return ret;

  if (pef)
    *pef = tmp_pef;

  if (pef_event_messages)
    *pef_event_messages = tmp_pef_event_messages;

  if (pef_startup_delay)
    *pef_startup_delay = tmp_pef_startup_delay;

  if (pef_alert_startup_delay)
    *pef_alert_startup_delay = tmp_pef_alert_startup_delay;

  return BMC_ERR_SUCCESS;
}

static bmc_err_t
pef_control_commit (ipmi_device_t dev,
		    uint8_t *pef,
		    uint8_t *pef_event_messages,
		    uint8_t *pef_startup_delay,
		    uint8_t *pef_alert_startup_delay)
{
  uint8_t tmp_pef;
  uint8_t tmp_pef_event_messages;
  uint8_t tmp_pef_startup_delay;
  uint8_t tmp_pef_alert_startup_delay;
  bmc_err_t ret;

  if ((ret = get_pef_control (dev,
                              &tmp_pef,
                              &tmp_pef_event_messages,
                              &tmp_pef_startup_delay,
                              &tmp_pef_alert_startup_delay)) != BMC_ERR_SUCCESS)
    return ret;

  if (pef)
    tmp_pef = *pef;

  if (pef_event_messages)
    tmp_pef_event_messages = *pef_event_messages;

  if (pef_startup_delay)
    tmp_pef_startup_delay = *pef_startup_delay;

  if (pef_alert_startup_delay)
    tmp_pef_alert_startup_delay = *pef_alert_startup_delay;

  return set_pef_control (dev,
			  tmp_pef,
			  tmp_pef_event_messages,
			  tmp_pef_startup_delay,
			  tmp_pef_alert_startup_delay);
}

static bmc_err_t
enable_pef_checkout (const struct bmc_config_arguments *args,
		     const struct section *sect,
		     struct keyvalue *kv)
{
  uint8_t value;
  bmc_err_t ret;
  
  if ((ret = pef_control_checkout (args->dev,
                                   &value,
                                   NULL,
                                   NULL,
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
enable_pef_commit (const struct bmc_config_arguments *args,
		   const struct section *sect,
		   const struct keyvalue *kv)
{
  uint8_t value = same (kv->value, "yes");
  return pef_control_commit (args->dev,
			     &value,
			     NULL,
			     NULL,
			     NULL);
}

static bmc_diff_t
enable_pef_diff (const struct bmc_config_arguments *args,
		 const struct section *sect,
		 const struct keyvalue *kv)
{
  uint8_t got_value;
  uint8_t passed_value;
  bmc_err_t rc;
  bmc_diff_t ret;

  if ((rc = pef_control_checkout (args->dev,
                                  &got_value,
                                  NULL,
                                  NULL,
                                  NULL)) != BMC_ERR_SUCCESS)
    {
      if (rc == BMC_ERR_NON_FATAL_ERROR)
        return BMC_DIFF_NON_FATAL_ERROR;
      return BMC_DIFF_FATAL_ERROR;
    }

  passed_value = same (kv->value, "yes");

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

static bmc_validate_t
enable_pef_validate (const struct bmc_config_arguments *args,
		     const struct section *sect,
		     const char *value)
{
  if (value && (same (value, "yes") || same (value, "no")))
    return BMC_VALIDATE_VALID_VALUE;
  return BMC_VALIDATE_INVALID_VALUE;
}

/* event_messages */

static bmc_err_t
enable_pef_event_messages_checkout (const struct bmc_config_arguments *args,
				    const struct section *sect,
				    struct keyvalue *kv)
{
  uint8_t value;
  bmc_err_t ret;
  
  if ((ret = pef_control_checkout (args->dev,
                                   NULL,
                                   &value,
                                   NULL,
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
enable_pef_event_messages_commit (const struct bmc_config_arguments *args,
				  const struct section *sect,
				  const struct keyvalue *kv)
{
  uint8_t value = same (kv->value, "yes");
  return pef_control_commit (args->dev,
			     NULL,
			     &value,
			     NULL,
			     NULL);
}

static bmc_diff_t
enable_pef_event_messages_diff (const struct bmc_config_arguments *args,
				const struct section *sect,
				const struct keyvalue *kv)
{
  uint8_t got_value;
  uint8_t passed_value;
  bmc_err_t rc;
  bmc_diff_t ret;

  if ((rc = pef_control_checkout (args->dev,
                                  NULL,
                                  &got_value,
                                  NULL,
                                  NULL)) != BMC_ERR_SUCCESS)
    {
      if (rc == BMC_ERR_NON_FATAL_ERROR)
        return BMC_DIFF_NON_FATAL_ERROR;
      return BMC_DIFF_FATAL_ERROR;
    }

  passed_value = same (kv->value, "yes");

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

static bmc_validate_t
enable_pef_event_messages_validate (const struct bmc_config_arguments *args,
				    const struct section *sect,
				    const char *value)
{
  if (value && (same (value, "yes") || same (value, "no")))
    return BMC_VALIDATE_VALID_VALUE;
  return BMC_VALIDATE_INVALID_VALUE;
}

/* startup_delay */


static bmc_err_t
enable_pef_startup_delay_checkout (const struct bmc_config_arguments *args,
				   const struct section *sect,
				   struct keyvalue *kv)
{
  uint8_t value;
  bmc_err_t ret;
  
  if ((ret = pef_control_checkout (args->dev,
                                   NULL,
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
enable_pef_startup_delay_commit (const struct bmc_config_arguments *args,
				 const struct section *sect,
				 const struct keyvalue *kv)
{
  uint8_t value = same (kv->value, "yes");
  return pef_control_commit (args->dev,
			     NULL,
			     NULL,
			     &value,
			     NULL);
}

static bmc_diff_t
enable_pef_startup_delay_diff (const struct bmc_config_arguments *args,
			       const struct section *sect,
			       const struct keyvalue *kv)
{
  uint8_t got_value;
  uint8_t passed_value;
  bmc_err_t rc;
  bmc_diff_t ret;

  if ((rc = pef_control_checkout (args->dev,
                                  NULL,
                                  NULL,
                                  &got_value,
                                  NULL)) != BMC_ERR_SUCCESS)
    {
      if (rc == BMC_ERR_NON_FATAL_ERROR)
        return BMC_DIFF_NON_FATAL_ERROR;
      return BMC_DIFF_FATAL_ERROR;
    }

  passed_value = same (kv->value, "yes");

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

static bmc_validate_t
enable_pef_startup_delay_validate (const struct bmc_config_arguments *args,
				   const struct section *sect,
				   const char *value)
{
  if (value && (same (value, "yes") || same (value, "no")))
    return BMC_VALIDATE_VALID_VALUE;
  return BMC_VALIDATE_INVALID_VALUE;
}

/* alert_startup_delay */


static bmc_err_t
enable_pef_alert_startup_delay_checkout (const struct bmc_config_arguments *args,
					 const struct section *sect,
					 struct keyvalue *kv)
{
  uint8_t value;
  bmc_err_t ret;
  
  if ((ret = pef_control_checkout (args->dev,
                                   NULL,
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
enable_pef_alert_startup_delay_commit (const struct bmc_config_arguments *args,
				       const struct section *sect,
				       const struct keyvalue *kv)
{
  uint8_t value = same (kv->value, "yes");
  return pef_control_commit (args->dev,
			     NULL,
			     NULL,
			     NULL,
			     &value);
}

static bmc_diff_t
enable_pef_alert_startup_delay_diff (const struct bmc_config_arguments *args,
				     const struct section *sect,
				     const struct keyvalue *kv)
{
  uint8_t got_value;
  uint8_t passed_value;
  bmc_err_t rc;
  bmc_diff_t ret;

  if ((rc = pef_control_checkout (args->dev,
                                  NULL,
                                  NULL,
                                  NULL,
                                  &got_value)) != BMC_ERR_SUCCESS)
    {
      if (rc == BMC_ERR_NON_FATAL_ERROR)
        return BMC_DIFF_NON_FATAL_ERROR;
      return BMC_DIFF_FATAL_ERROR;
    }

  passed_value = same (kv->value, "yes");

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

static bmc_validate_t
enable_pef_alert_startup_delay_validate (const struct bmc_config_arguments *args,
					 const struct section *sect,
					 const char *value)
{
  if (value && (same (value, "yes") || same (value, "no")))
    return BMC_VALIDATE_VALID_VALUE;
  return BMC_VALIDATE_INVALID_VALUE;
}


static bmc_err_t
pef_global_control_checkout (ipmi_device_t dev,
			     uint8_t *alert_action,
			     uint8_t *power_down_action,
			     uint8_t *reset_action,
			     uint8_t *power_cycle_action,
			     uint8_t *oem_action,
			     uint8_t *diagnostic_interrupt)
{
  uint8_t tmp_alert_action;
  uint8_t tmp_power_down_action;
  uint8_t tmp_reset_action;
  uint8_t tmp_power_cycle_action;
  uint8_t tmp_oem_action;
  uint8_t tmp_diagnostic_interrupt;
  bmc_err_t ret;

  if ((ret = get_pef_action_global_control (dev,
                                            &tmp_alert_action,
                                            &tmp_power_down_action,
                                            &tmp_reset_action,
                                            &tmp_power_cycle_action,
                                            &tmp_oem_action,
                                            &tmp_diagnostic_interrupt)) != BMC_ERR_SUCCESS)
    return ret;

  if (alert_action)
    *alert_action = tmp_alert_action;
  if (power_down_action)
    *power_down_action = tmp_power_down_action;
  if (reset_action)
    *reset_action = tmp_reset_action;
  if (power_cycle_action)
    *power_cycle_action = tmp_power_cycle_action;
  if (oem_action)
    *oem_action = tmp_oem_action;
  if (diagnostic_interrupt)
    *diagnostic_interrupt = tmp_diagnostic_interrupt;

  return BMC_ERR_SUCCESS;
}


static bmc_err_t
pef_global_control_commit (ipmi_device_t dev,
			   uint8_t *alert_action,
			   uint8_t *power_down_action,
			   uint8_t *reset_action,
			   uint8_t *power_cycle_action,
			   uint8_t *oem_action,
			   uint8_t *diagnostic_interrupt)
{
  uint8_t tmp_alert_action;
  uint8_t tmp_power_down_action;
  uint8_t tmp_reset_action;
  uint8_t tmp_power_cycle_action;
  uint8_t tmp_oem_action;
  uint8_t tmp_diagnostic_interrupt;
  bmc_err_t ret;

  if ((ret = get_pef_action_global_control (dev,
                                            &tmp_alert_action,
                                            &tmp_power_down_action,
                                            &tmp_reset_action,
                                            &tmp_power_cycle_action,
                                            &tmp_oem_action,
                                            &tmp_diagnostic_interrupt)) != BMC_ERR_SUCCESS)
    return ret;

  if (alert_action)
    tmp_alert_action = *alert_action;
  if (power_down_action)
    tmp_power_down_action = *power_down_action;
  if (reset_action)
    tmp_reset_action = *reset_action;
  if (power_cycle_action)
    tmp_power_cycle_action = *power_cycle_action;
  if (oem_action)
    tmp_oem_action = *oem_action;
  if (diagnostic_interrupt)
    tmp_diagnostic_interrupt = *diagnostic_interrupt;

  return set_pef_action_global_control (dev,
					tmp_alert_action,
					tmp_power_down_action,
					tmp_reset_action,
					tmp_power_cycle_action,
					tmp_oem_action,
					tmp_diagnostic_interrupt);
}

/* alert_action */
			   
static bmc_err_t
enable_alert_action_checkout (const struct bmc_config_arguments *args,
			      const struct section *sect,
			      struct keyvalue *kv)
{
  uint8_t value;
  bmc_err_t ret;

  if ((ret = pef_global_control_checkout (args->dev,
                                          &value,
                                          NULL,
                                          NULL,
                                          NULL,
                                          NULL,
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
enable_alert_action_commit (const struct bmc_config_arguments *args,
			    const struct section *sect,
			    const struct keyvalue *kv)
{
  uint8_t value = (same (kv->value, "yes") ? 1 : 0);
  return pef_global_control_commit (args->dev,
				    &value,
				    NULL,
				    NULL,
				    NULL,
				    NULL,
				    NULL);
}

static bmc_diff_t
enable_alert_action_diff (const struct bmc_config_arguments *args,
			  const struct section *sect,
			  const struct keyvalue *kv)
{
  uint8_t passed_value;
  uint8_t got_value;
  bmc_err_t rc;
  bmc_diff_t ret;

  if ((rc = pef_global_control_checkout (args->dev,
                                         &got_value,
                                         NULL,
                                         NULL,
                                         NULL,
                                         NULL,
                                         NULL)) != BMC_ERR_SUCCESS)
    {
      if (rc == BMC_ERR_NON_FATAL_ERROR)
        return BMC_DIFF_NON_FATAL_ERROR;
      return BMC_DIFF_FATAL_ERROR;
    }

  passed_value = (same (kv->value, "yes") ? 1 : 0);

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

static bmc_validate_t
enable_alert_action_validate (const struct bmc_config_arguments *args,
			      const struct section *sect,
			      const char *value)
{
  if (value && (same (value, "yes") || same (value, "no")))
    return BMC_VALIDATE_VALID_VALUE;
  return BMC_VALIDATE_INVALID_VALUE;
}

/* power_down_action */

static bmc_err_t
enable_power_down_action_checkout (const struct bmc_config_arguments *args,
				   const struct section *sect,
				   struct keyvalue *kv)
{
  uint8_t value;
  bmc_err_t ret;

  if ((ret = pef_global_control_checkout (args->dev,
                                          NULL,
                                          &value,
                                          NULL,
                                          NULL,
                                          NULL,
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
enable_power_down_action_commit (const struct bmc_config_arguments *args,
				 const struct section *sect,
				 const struct keyvalue *kv)
{
  uint8_t value = (same (kv->value, "yes") ? 1 : 0);
  return pef_global_control_commit (args->dev,
				    NULL,
				    &value,
				    NULL,
				    NULL,
				    NULL,
				    NULL);
}

static bmc_diff_t
enable_power_down_action_diff (const struct bmc_config_arguments *args,
			       const struct section *sect,
			       const struct keyvalue *kv)
{
  uint8_t passed_value;
  uint8_t got_value;
  bmc_err_t rc;
  bmc_diff_t ret;
  
  if ((rc = pef_global_control_checkout (args->dev,
                                         NULL,
                                         &got_value,
                                         NULL,
                                         NULL,
                                         NULL,
                                         NULL)) != BMC_ERR_SUCCESS)
    {
      if (rc == BMC_ERR_NON_FATAL_ERROR)
        return BMC_DIFF_NON_FATAL_ERROR;
      return BMC_DIFF_FATAL_ERROR;
    }

  passed_value = (same (kv->value, "yes") ? 1 : 0);
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

static bmc_validate_t
enable_power_down_action_validate (const struct bmc_config_arguments *args,
				   const struct section *sect,
				   const char *value)
{
  if (value && (same (value, "yes") || same (value, "no")))
    return BMC_VALIDATE_VALID_VALUE;
  return BMC_VALIDATE_INVALID_VALUE;
}

/* reset_action */

static bmc_err_t
enable_reset_action_checkout (const struct bmc_config_arguments *args,
			      const struct section *sect,
			      struct keyvalue *kv)
{
  uint8_t value;
  bmc_err_t ret;

  if ((ret = pef_global_control_checkout (args->dev,
                                          NULL,
                                          NULL,
                                          &value,
                                          NULL,
                                          NULL,
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
enable_reset_action_commit (const struct bmc_config_arguments *args,
			    const struct section *sect,
			    const struct keyvalue *kv)
{
  uint8_t value = (same (kv->value, "yes") ? 1 : 0);
  return pef_global_control_commit (args->dev,
				    NULL,
				    NULL,
				    &value,
				    NULL,
				    NULL,
				    NULL);
}

static bmc_diff_t
enable_reset_action_diff (const struct bmc_config_arguments *args,
			  const struct section *sect,
			  const struct keyvalue *kv)
{
  uint8_t passed_value;
  uint8_t got_value;
  bmc_err_t rc;
  bmc_diff_t ret;
  
  if ((rc = pef_global_control_checkout (args->dev,
                                         NULL,
                                         NULL,
                                         &got_value,
                                         NULL,
                                         NULL,
                                         NULL)) != BMC_ERR_SUCCESS)
    {
      if (rc == BMC_ERR_NON_FATAL_ERROR)
        return BMC_DIFF_NON_FATAL_ERROR;
      return BMC_DIFF_FATAL_ERROR;
    }

  passed_value = (same (kv->value, "yes") ? 1 : 0);
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

static bmc_validate_t
enable_reset_action_validate (const struct bmc_config_arguments *args,
			      const struct section *sect,
			      const char *value)
{
  if (value && (same (value, "yes") || same (value, "no")))
    return BMC_VALIDATE_VALID_VALUE;
  return BMC_VALIDATE_INVALID_VALUE;
}

/* power_cycle_action */

static bmc_err_t
enable_power_cycle_action_checkout (const struct bmc_config_arguments *args,
				    const struct section *sect,
				    struct keyvalue *kv)
{
  uint8_t value;
  bmc_err_t ret;

  if ((ret = pef_global_control_checkout (args->dev,
                                          NULL,
                                          NULL,
                                          NULL,
                                          &value,
                                          NULL,
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
enable_power_cycle_action_commit (const struct bmc_config_arguments *args,
				  const struct section *sect,
				  const struct keyvalue *kv)
{
  uint8_t value = (same (kv->value, "yes") ? 1 : 0);
  return pef_global_control_commit (args->dev,
				    NULL,
				    NULL,
				    NULL,
				    &value,
				    NULL,
				    NULL);
}

static bmc_diff_t
enable_power_cycle_action_diff (const struct bmc_config_arguments *args,
				const struct section *sect,
				const struct keyvalue *kv)
{
  uint8_t passed_value;
  uint8_t got_value;
  bmc_err_t rc;
  bmc_diff_t ret;
  
  if ((rc = pef_global_control_checkout (args->dev,
                                         NULL,
                                         NULL,
                                         NULL,
                                         &got_value,
                                         NULL,
                                         NULL)) != BMC_ERR_SUCCESS)
    {
      if (rc == BMC_ERR_NON_FATAL_ERROR)
        return BMC_DIFF_NON_FATAL_ERROR;
      return BMC_DIFF_FATAL_ERROR;
    }

  passed_value = (same (kv->value, "yes") ? 1 : 0);
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

static bmc_validate_t
enable_power_cycle_action_validate (const struct bmc_config_arguments *args,
				    const struct section *sect,
				    const char *value)
{
  if (value && (same (value, "yes") || same (value, "no")))
    return BMC_VALIDATE_VALID_VALUE;
  return BMC_VALIDATE_INVALID_VALUE;
}

/* OEM_action */

static bmc_err_t
enable_oem_action_checkout (const struct bmc_config_arguments *args,
			    const struct section *sect,
			    struct keyvalue *kv)
{
  uint8_t value;
  bmc_err_t ret;

  if ((ret = pef_global_control_checkout (args->dev,
                                          NULL,
                                          NULL,
                                          NULL,
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
enable_oem_action_commit (const struct bmc_config_arguments *args,
			  const struct section *sect,
			  const struct keyvalue *kv)
{
  uint8_t value = (same (kv->value, "yes") ? 1 : 0);
  return pef_global_control_commit (args->dev,
				    NULL,
				    NULL,
				    NULL,
				    NULL,
				    &value,
				    NULL);
}

static bmc_diff_t
enable_oem_action_diff (const struct bmc_config_arguments *args,
			const struct section *sect,
			const struct keyvalue *kv)
{
  uint8_t passed_value;
  uint8_t got_value;
  bmc_err_t rc;
  bmc_diff_t ret;
  
  if ((rc = pef_global_control_checkout (args->dev,
                                         NULL,
                                         NULL,
                                         NULL,
                                         NULL,
                                         &got_value,
                                         NULL)) != BMC_ERR_SUCCESS)
    {
      if (rc == BMC_ERR_NON_FATAL_ERROR)
        return BMC_DIFF_NON_FATAL_ERROR;
      return BMC_DIFF_FATAL_ERROR;
    }

  passed_value = (same (kv->value, "yes") ? 1 : 0);

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

static bmc_validate_t
enable_oem_action_validate (const struct bmc_config_arguments *args,
			    const struct section *sect,
			    const char *value)
{
  if (value && (same (value, "yes") || same (value, "no")))
    return BMC_VALIDATE_VALID_VALUE;
  return BMC_VALIDATE_INVALID_VALUE;
}

/* diagnostic_interrupt */

static bmc_err_t
enable_diagnostic_interrupt_checkout (const struct bmc_config_arguments *args,
                                      const struct section *sect,
                                      struct keyvalue *kv)
{
  uint8_t value;
  bmc_err_t ret;

  if ((ret = pef_global_control_checkout (args->dev,
                                          NULL,
                                          NULL,
                                          NULL,
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
enable_diagnostic_interrupt_commit (const struct bmc_config_arguments *args,
				    const struct section *sect,
				    const struct keyvalue *kv)
{
  uint8_t value = (same (kv->value, "yes") ? 1 : 0);
  return pef_global_control_commit (args->dev,
				    NULL,
				    NULL,
				    NULL,
				    NULL,
				    NULL,
				    &value);
}

static bmc_diff_t
enable_diagnostic_interrupt_diff (const struct bmc_config_arguments *args,
				  const struct section *sect,
				  const struct keyvalue *kv)
{
  uint8_t passed_value;
  uint8_t got_value;
  bmc_err_t rc;
  bmc_diff_t ret;
  
  if ((rc = pef_global_control_checkout (args->dev,
                                         NULL,
                                         NULL,
                                         NULL,
                                         NULL,
                                         NULL,
                                         &got_value)) != BMC_ERR_SUCCESS)
    {
      if (rc == BMC_ERR_NON_FATAL_ERROR)
        return BMC_DIFF_NON_FATAL_ERROR;
      return BMC_DIFF_FATAL_ERROR;
    }

  passed_value = (same (kv->value, "yes") ? 1 : 0);

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


static bmc_validate_t
enable_diagnostic_interrupt_validate (const struct bmc_config_arguments *args,
				      const struct section *sect,
				      const char *value)
{
  if (value && (same (value, "yes") || same (value, "no")))
    return BMC_VALIDATE_VALID_VALUE;
  return BMC_VALIDATE_INVALID_VALUE;
}

/* pef_startup_delay */
static bmc_err_t
pef_startup_delay_checkout (const struct bmc_config_arguments *args,
			    const struct section *sect,
			    struct keyvalue *kv)
{
  uint8_t delay;
  bmc_err_t ret;
  
  if ((ret = get_pef_startup_delay (args->dev,
                                    &delay)) != BMC_ERR_SUCCESS)
    return ret;

  if (kv->value)
    free (kv->value);

  if (asprintf (&kv->value, "%d", delay) < 0)
    {
      perror("asprintf");
      return BMC_ERR_FATAL_ERROR;
    }
  return BMC_ERR_SUCCESS;
}

static bmc_err_t
pef_startup_delay_commit (const struct bmc_config_arguments *args,
			  const struct section *sect,
			  const struct keyvalue *kv)
{
  uint8_t value = atoi (kv->value);
  return set_pef_startup_delay (args->dev,
				value);
}

static bmc_diff_t
pef_startup_delay_diff (const struct bmc_config_arguments *args,
			const struct section *sect,
			const struct keyvalue *kv)
{
  uint8_t got_value;
  uint8_t passed_value;
  bmc_err_t rc;
  bmc_diff_t ret;
  
  if ((rc = get_pef_startup_delay (args->dev,
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
pef_startup_delay_validate (const struct bmc_config_arguments *args,
			    const struct section *sect,
			    const char *value)
{
  long int num;
  char *endptr;

  num = strtol (value, &endptr, 0);

  if (*endptr)
    return BMC_VALIDATE_INVALID_VALUE;

  if (num < 0 || num > 255)
    return BMC_VALIDATE_INVALID_VALUE;

  return BMC_VALIDATE_VALID_VALUE;
}

/* alert_startup_delay */

static bmc_err_t
pef_alert_startup_delay_checkout (const struct bmc_config_arguments *args,
				  const struct section *sect,
				  struct keyvalue *kv)
{
  uint8_t delay;
  bmc_err_t ret;
  
  if ((ret = get_pef_alert_startup_delay (args->dev,
                                          &delay)) != BMC_ERR_SUCCESS)
    return ret;

  if (kv->value)
    free (kv->value);

  if (asprintf (&kv->value, "%d", delay) < 0)
    {
      perror("asprintf");
      return BMC_ERR_FATAL_ERROR;
    }
  return BMC_ERR_SUCCESS;
}

static bmc_err_t
pef_alert_startup_delay_commit (const struct bmc_config_arguments *args,
				const struct section *sect,
				const struct keyvalue *kv)
{
  uint8_t value = atoi (kv->value);
  return set_pef_alert_startup_delay (args->dev,
				      value);
}

static bmc_diff_t
pef_alert_startup_delay_diff (const struct bmc_config_arguments *args,
			      const struct section *sect,
			      const struct keyvalue *kv)
{
  uint8_t got_value;
  uint8_t passed_value;
  bmc_err_t rc;
  bmc_diff_t ret;
  
  if ((rc = get_pef_alert_startup_delay (args->dev,
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
pef_alert_startup_delay_validate (const struct bmc_config_arguments *args,
				  const struct section *sect,
				  const char *value)
{
  long int num;
  char *endptr;

  num = strtol (value, &endptr, 0);

  if (*endptr)
    return BMC_VALIDATE_INVALID_VALUE;

  if (num < 0 || num > 255)
    return BMC_VALIDATE_INVALID_VALUE;

  return BMC_VALIDATE_VALID_VALUE;
}


struct section *
bmc_pef_conf_section_get (struct bmc_config_arguments *args)
{
  struct section *pef_section;

  if (!(pef_section = bmc_section_create ("PEF_Conf")))
    goto cleanup;

  if (bmc_section_add_keyvalue (pef_section,
				"Enable_PEF",
				"Possible values: Yes/No",
				0,
				enable_pef_checkout,
				enable_pef_commit,
				enable_pef_diff,
				enable_pef_validate) < 0)
    goto cleanup;

  if (bmc_section_add_keyvalue (pef_section,
				"Enable_PEF_Event_Messages",
				"Possible values: Yes/No",
				0,
				enable_pef_event_messages_checkout,
				enable_pef_event_messages_commit,
				enable_pef_event_messages_diff,
				enable_pef_event_messages_validate) < 0)
    goto cleanup;

  if (bmc_section_add_keyvalue (pef_section,
				"Enable_PEF_Startup_Delay",
				"Possible values: Yes/No",
				0,
				enable_pef_startup_delay_checkout,
				enable_pef_startup_delay_commit,
				enable_pef_startup_delay_diff,
				enable_pef_startup_delay_validate) < 0)
    goto cleanup;

  if (bmc_section_add_keyvalue (pef_section,
				"Enable_PEF_Alert_Startup_Delay",
				"Possible values: Yes/No",
				0,
				enable_pef_alert_startup_delay_checkout,
				enable_pef_alert_startup_delay_commit,
				enable_pef_alert_startup_delay_diff,
				enable_pef_alert_startup_delay_validate) < 0)
    goto cleanup;
  
  if (bmc_section_add_keyvalue (pef_section,
				"Enable_Alert_Action",
				"Possible values: Yes/No",
				0,
				enable_alert_action_checkout,
				enable_alert_action_commit,
				enable_alert_action_diff,
				enable_alert_action_validate) < 0)
    goto cleanup;

  if (bmc_section_add_keyvalue (pef_section,
				"Enable_Power_Down_Action",
				"Possible values: Yes/No",
				0,
				enable_power_down_action_checkout,
				enable_power_down_action_commit,
				enable_power_down_action_diff,
				enable_power_down_action_validate) < 0)
    goto cleanup;

  if (bmc_section_add_keyvalue (pef_section,
				"Enable_Reset_Action",
				"Possible values: Yes/No",
				0,
				enable_reset_action_checkout,
				enable_reset_action_commit,
				enable_reset_action_diff,
				enable_reset_action_validate) < 0)
    goto cleanup;

  if (bmc_section_add_keyvalue (pef_section,
				"Enable_Power_Cycle_Action",
				"Possible values: Yes/No",
				0,
				enable_power_cycle_action_checkout,
				enable_power_cycle_action_commit,
				enable_power_cycle_action_diff,
				enable_power_cycle_action_validate) < 0)
    goto cleanup;

  if (bmc_section_add_keyvalue (pef_section,
				"Enable_OEM_Action",
				"Possible values: Yes/No",
				0,
				enable_oem_action_checkout,
				enable_oem_action_commit,
				enable_oem_action_diff,
				enable_oem_action_validate) < 0)
    goto cleanup;

  if (bmc_section_add_keyvalue (pef_section,
				"Enable_Diagnostic_Interrupt",
				"Possible values: Yes/No",
				0,
				enable_diagnostic_interrupt_checkout,
				enable_diagnostic_interrupt_commit,
				enable_diagnostic_interrupt_diff,
				enable_diagnostic_interrupt_validate) < 0)
    goto cleanup;

  if (bmc_section_add_keyvalue (pef_section,
				"PEF_Startup_Delay",
				"Give value in seconds",
				0,
				pef_startup_delay_checkout,
				pef_startup_delay_commit,
				pef_startup_delay_diff,
				pef_startup_delay_validate) < 0)
    goto cleanup;

  if (bmc_section_add_keyvalue (pef_section,
				"PEF_Alert_Startup_Delay",
				"Give value in seconds",
				0,
				pef_alert_startup_delay_checkout,
				pef_alert_startup_delay_commit,
				pef_alert_startup_delay_diff,
				pef_alert_startup_delay_validate) < 0)
    goto cleanup;

  return pef_section;

 cleanup:
  if (pef_section)
    bmc_section_destroy(pef_section);
  return NULL;
}

