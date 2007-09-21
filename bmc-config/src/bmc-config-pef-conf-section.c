#if HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */

#include "bmc-config.h"
#include "bmc-config-common.h"
#include "bmc-config-wrapper.h"
#include "bmc-config-map.h"
#include "bmc-config-validate.h"

#include "config-common.h"
#include "config-section.h"
#include "config-validate.h"

static config_err_t
pef_control_checkout (bmc_config_state_data_t *state_data,
		      uint8_t *pef,
		      uint8_t *pef_event_messages,
		      uint8_t *pef_startup_delay,
		      uint8_t *pef_alert_startup_delay)
{
  uint8_t tmp_pef;
  uint8_t tmp_pef_event_messages;
  uint8_t tmp_pef_startup_delay;
  uint8_t tmp_pef_alert_startup_delay;
  config_err_t ret;

  if ((ret = get_pef_control (state_data,
                              &tmp_pef,
                              &tmp_pef_event_messages,
                              &tmp_pef_startup_delay,
                              &tmp_pef_alert_startup_delay)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (pef)
    *pef = tmp_pef;

  if (pef_event_messages)
    *pef_event_messages = tmp_pef_event_messages;

  if (pef_startup_delay)
    *pef_startup_delay = tmp_pef_startup_delay;

  if (pef_alert_startup_delay)
    *pef_alert_startup_delay = tmp_pef_alert_startup_delay;

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
pef_control_commit (bmc_config_state_data_t *state_data,
		    uint8_t *pef,
		    uint8_t *pef_event_messages,
		    uint8_t *pef_startup_delay,
		    uint8_t *pef_alert_startup_delay)
{
  uint8_t tmp_pef;
  uint8_t tmp_pef_event_messages;
  uint8_t tmp_pef_startup_delay;
  uint8_t tmp_pef_alert_startup_delay;
  config_err_t ret;

  if ((ret = get_pef_control (state_data,
                              &tmp_pef,
                              &tmp_pef_event_messages,
                              &tmp_pef_startup_delay,
                              &tmp_pef_alert_startup_delay)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (pef)
    tmp_pef = *pef;

  if (pef_event_messages)
    tmp_pef_event_messages = *pef_event_messages;

  if (pef_startup_delay)
    tmp_pef_startup_delay = *pef_startup_delay;

  if (pef_alert_startup_delay)
    tmp_pef_alert_startup_delay = *pef_alert_startup_delay;

  return set_pef_control (state_data,
			  tmp_pef,
			  tmp_pef_event_messages,
			  tmp_pef_startup_delay,
			  tmp_pef_alert_startup_delay);
}

static config_err_t
enable_pef_checkout (bmc_config_state_data_t *state_data,
		     const struct config_section *sect,
		     struct config_keyvalue *kv)
{
  uint8_t value;
  config_err_t ret;
  
  if ((ret = pef_control_checkout (state_data,
                                   &value,
                                   NULL,
                                   NULL,
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
enable_pef_commit (bmc_config_state_data_t *state_data,
		   const struct config_section *sect,
		   const struct config_keyvalue *kv)
{
  uint8_t value = same (kv->value_input, "yes");
  return pef_control_commit (state_data,
			     &value,
			     NULL,
			     NULL,
			     NULL);
}

static bmc_diff_t
enable_pef_diff (bmc_config_state_data_t *state_data,
		 const struct config_section *sect,
		 const struct config_keyvalue *kv)
{
  uint8_t got_value;
  uint8_t passed_value;
  config_err_t rc;
  bmc_diff_t ret;

  if ((rc = pef_control_checkout (state_data,
                                  &got_value,
                                  NULL,
                                  NULL,
                                  NULL)) != CONFIG_ERR_SUCCESS)
    {
      if (rc == CONFIG_ERR_NON_FATAL_ERROR)
        return BMC_DIFF_NON_FATAL_ERROR;
      return BMC_DIFF_FATAL_ERROR;
    }

  passed_value = same (kv->value_input, "yes");

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

/* event_messages */

static config_err_t
enable_pef_event_messages_checkout (bmc_config_state_data_t *state_data,
				    const struct config_section *sect,
				    struct config_keyvalue *kv)
{
  uint8_t value;
  config_err_t ret;
  
  if ((ret = pef_control_checkout (state_data,
                                   NULL,
                                   &value,
                                   NULL,
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
enable_pef_event_messages_commit (bmc_config_state_data_t *state_data,
				  const struct config_section *sect,
				  const struct config_keyvalue *kv)
{
  uint8_t value = same (kv->value_input, "yes");
  return pef_control_commit (state_data,
			     NULL,
			     &value,
			     NULL,
			     NULL);
}

static bmc_diff_t
enable_pef_event_messages_diff (bmc_config_state_data_t *state_data,
				const struct config_section *sect,
				const struct config_keyvalue *kv)
{
  uint8_t got_value;
  uint8_t passed_value;
  config_err_t rc;
  bmc_diff_t ret;

  if ((rc = pef_control_checkout (state_data,
                                  NULL,
                                  &got_value,
                                  NULL,
                                  NULL)) != CONFIG_ERR_SUCCESS)
    {
      if (rc == CONFIG_ERR_NON_FATAL_ERROR)
        return BMC_DIFF_NON_FATAL_ERROR;
      return BMC_DIFF_FATAL_ERROR;
    }

  passed_value = same (kv->value_input, "yes");

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

/* startup_delay */

static config_err_t
enable_pef_startup_delay_checkout (bmc_config_state_data_t *state_data,
				   const struct config_section *sect,
				   struct config_keyvalue *kv)
{
  uint8_t value;
  config_err_t ret;
  
  if ((ret = pef_control_checkout (state_data,
                                   NULL,
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
enable_pef_startup_delay_commit (bmc_config_state_data_t *state_data,
				 const struct config_section *sect,
				 const struct config_keyvalue *kv)
{
  uint8_t value = same (kv->value_input, "yes");
  return pef_control_commit (state_data,
			     NULL,
			     NULL,
			     &value,
			     NULL);
}

static bmc_diff_t
enable_pef_startup_delay_diff (bmc_config_state_data_t *state_data,
			       const struct config_section *sect,
			       const struct config_keyvalue *kv)
{
  uint8_t got_value;
  uint8_t passed_value;
  config_err_t rc;
  bmc_diff_t ret;

  if ((rc = pef_control_checkout (state_data,
                                  NULL,
                                  NULL,
                                  &got_value,
                                  NULL)) != CONFIG_ERR_SUCCESS)
    {
      if (rc == CONFIG_ERR_NON_FATAL_ERROR)
        return BMC_DIFF_NON_FATAL_ERROR;
      return BMC_DIFF_FATAL_ERROR;
    }

  passed_value = same (kv->value_input, "yes");

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

/* alert_startup_delay */

static config_err_t
enable_pef_alert_startup_delay_checkout (bmc_config_state_data_t *state_data,
					 const struct config_section *sect,
					 struct config_keyvalue *kv)
{
  uint8_t value;
  config_err_t ret;
  
  if ((ret = pef_control_checkout (state_data,
                                   NULL,
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
enable_pef_alert_startup_delay_commit (bmc_config_state_data_t *state_data,
				       const struct config_section *sect,
				       const struct config_keyvalue *kv)
{
  uint8_t value = same (kv->value_input, "yes");
  return pef_control_commit (state_data,
			     NULL,
			     NULL,
			     NULL,
			     &value);
}

static bmc_diff_t
enable_pef_alert_startup_delay_diff (bmc_config_state_data_t *state_data,
				     const struct config_section *sect,
				     const struct config_keyvalue *kv)
{
  uint8_t got_value;
  uint8_t passed_value;
  config_err_t rc;
  bmc_diff_t ret;

  if ((rc = pef_control_checkout (state_data,
                                  NULL,
                                  NULL,
                                  NULL,
                                  &got_value)) != CONFIG_ERR_SUCCESS)
    {
      if (rc == CONFIG_ERR_NON_FATAL_ERROR)
        return BMC_DIFF_NON_FATAL_ERROR;
      return BMC_DIFF_FATAL_ERROR;
    }

  passed_value = same (kv->value_input, "yes");

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
pef_global_control_checkout (bmc_config_state_data_t *state_data,
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
  config_err_t ret;

  if ((ret = get_pef_action_global_control (state_data,
                                            &tmp_alert_action,
                                            &tmp_power_down_action,
                                            &tmp_reset_action,
                                            &tmp_power_cycle_action,
                                            &tmp_oem_action,
                                            &tmp_diagnostic_interrupt)) != CONFIG_ERR_SUCCESS)
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

  return CONFIG_ERR_SUCCESS;
}


static config_err_t
pef_global_control_commit (bmc_config_state_data_t *state_data,
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
  config_err_t ret;

  if ((ret = get_pef_action_global_control (state_data,
                                            &tmp_alert_action,
                                            &tmp_power_down_action,
                                            &tmp_reset_action,
                                            &tmp_power_cycle_action,
                                            &tmp_oem_action,
                                            &tmp_diagnostic_interrupt)) != CONFIG_ERR_SUCCESS)
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

  return set_pef_action_global_control (state_data,
					tmp_alert_action,
					tmp_power_down_action,
					tmp_reset_action,
					tmp_power_cycle_action,
					tmp_oem_action,
					tmp_diagnostic_interrupt);
}

/* alert_action */
			   
static config_err_t
enable_alert_action_checkout (bmc_config_state_data_t *state_data,
			      const struct config_section *sect,
			      struct config_keyvalue *kv)
{
  uint8_t value;
  config_err_t ret;

  if ((ret = pef_global_control_checkout (state_data,
                                          &value,
                                          NULL,
                                          NULL,
                                          NULL,
                                          NULL,
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
enable_alert_action_commit (bmc_config_state_data_t *state_data,
			    const struct config_section *sect,
			    const struct config_keyvalue *kv)
{
  uint8_t value = (same (kv->value_input, "yes") ? 1 : 0);
  return pef_global_control_commit (state_data,
				    &value,
				    NULL,
				    NULL,
				    NULL,
				    NULL,
				    NULL);
}

static bmc_diff_t
enable_alert_action_diff (bmc_config_state_data_t *state_data,
			  const struct config_section *sect,
			  const struct config_keyvalue *kv)
{
  uint8_t passed_value;
  uint8_t got_value;
  config_err_t rc;
  bmc_diff_t ret;

  if ((rc = pef_global_control_checkout (state_data,
                                         &got_value,
                                         NULL,
                                         NULL,
                                         NULL,
                                         NULL,
                                         NULL)) != CONFIG_ERR_SUCCESS)
    {
      if (rc == CONFIG_ERR_NON_FATAL_ERROR)
        return BMC_DIFF_NON_FATAL_ERROR;
      return BMC_DIFF_FATAL_ERROR;
    }

  passed_value = (same (kv->value_input, "yes") ? 1 : 0);

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

/* power_down_action */

static config_err_t
enable_power_down_action_checkout (bmc_config_state_data_t *state_data,
				   const struct config_section *sect,
				   struct config_keyvalue *kv)
{
  uint8_t value;
  config_err_t ret;

  if ((ret = pef_global_control_checkout (state_data,
                                          NULL,
                                          &value,
                                          NULL,
                                          NULL,
                                          NULL,
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
enable_power_down_action_commit (bmc_config_state_data_t *state_data,
				 const struct config_section *sect,
				 const struct config_keyvalue *kv)
{
  uint8_t value = (same (kv->value_input, "yes") ? 1 : 0);
  return pef_global_control_commit (state_data,
				    NULL,
				    &value,
				    NULL,
				    NULL,
				    NULL,
				    NULL);
}

static bmc_diff_t
enable_power_down_action_diff (bmc_config_state_data_t *state_data,
			       const struct config_section *sect,
			       const struct config_keyvalue *kv)
{
  uint8_t passed_value;
  uint8_t got_value;
  config_err_t rc;
  bmc_diff_t ret;
  
  if ((rc = pef_global_control_checkout (state_data,
                                         NULL,
                                         &got_value,
                                         NULL,
                                         NULL,
                                         NULL,
                                         NULL)) != CONFIG_ERR_SUCCESS)
    {
      if (rc == CONFIG_ERR_NON_FATAL_ERROR)
        return BMC_DIFF_NON_FATAL_ERROR;
      return BMC_DIFF_FATAL_ERROR;
    }

  passed_value = (same (kv->value_input, "yes") ? 1 : 0);
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

/* reset_action */

static config_err_t
enable_reset_action_checkout (bmc_config_state_data_t *state_data,
			      const struct config_section *sect,
			      struct config_keyvalue *kv)
{
  uint8_t value;
  config_err_t ret;

  if ((ret = pef_global_control_checkout (state_data,
                                          NULL,
                                          NULL,
                                          &value,
                                          NULL,
                                          NULL,
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
enable_reset_action_commit (bmc_config_state_data_t *state_data,
			    const struct config_section *sect,
			    const struct config_keyvalue *kv)
{
  uint8_t value = (same (kv->value_input, "yes") ? 1 : 0);
  return pef_global_control_commit (state_data,
				    NULL,
				    NULL,
				    &value,
				    NULL,
				    NULL,
				    NULL);
}

static bmc_diff_t
enable_reset_action_diff (bmc_config_state_data_t *state_data,
			  const struct config_section *sect,
			  const struct config_keyvalue *kv)
{
  uint8_t passed_value;
  uint8_t got_value;
  config_err_t rc;
  bmc_diff_t ret;
  
  if ((rc = pef_global_control_checkout (state_data,
                                         NULL,
                                         NULL,
                                         &got_value,
                                         NULL,
                                         NULL,
                                         NULL)) != CONFIG_ERR_SUCCESS)
    {
      if (rc == CONFIG_ERR_NON_FATAL_ERROR)
        return BMC_DIFF_NON_FATAL_ERROR;
      return BMC_DIFF_FATAL_ERROR;
    }

  passed_value = (same (kv->value_input, "yes") ? 1 : 0);
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

/* power_cycle_action */

static config_err_t
enable_power_cycle_action_checkout (bmc_config_state_data_t *state_data,
				    const struct config_section *sect,
				    struct config_keyvalue *kv)
{
  uint8_t value;
  config_err_t ret;

  if ((ret = pef_global_control_checkout (state_data,
                                          NULL,
                                          NULL,
                                          NULL,
                                          &value,
                                          NULL,
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
enable_power_cycle_action_commit (bmc_config_state_data_t *state_data,
				  const struct config_section *sect,
				  const struct config_keyvalue *kv)
{
  uint8_t value = (same (kv->value_input, "yes") ? 1 : 0);
  return pef_global_control_commit (state_data,
				    NULL,
				    NULL,
				    NULL,
				    &value,
				    NULL,
				    NULL);
}

static bmc_diff_t
enable_power_cycle_action_diff (bmc_config_state_data_t *state_data,
				const struct config_section *sect,
				const struct config_keyvalue *kv)
{
  uint8_t passed_value;
  uint8_t got_value;
  config_err_t rc;
  bmc_diff_t ret;
  
  if ((rc = pef_global_control_checkout (state_data,
                                         NULL,
                                         NULL,
                                         NULL,
                                         &got_value,
                                         NULL,
                                         NULL)) != CONFIG_ERR_SUCCESS)
    {
      if (rc == CONFIG_ERR_NON_FATAL_ERROR)
        return BMC_DIFF_NON_FATAL_ERROR;
      return BMC_DIFF_FATAL_ERROR;
    }

  passed_value = (same (kv->value_input, "yes") ? 1 : 0);
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

/* OEM_action */

static config_err_t
enable_oem_action_checkout (bmc_config_state_data_t *state_data,
			    const struct config_section *sect,
			    struct config_keyvalue *kv)
{
  uint8_t value;
  config_err_t ret;

  if ((ret = pef_global_control_checkout (state_data,
                                          NULL,
                                          NULL,
                                          NULL,
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
enable_oem_action_commit (bmc_config_state_data_t *state_data,
			  const struct config_section *sect,
			  const struct config_keyvalue *kv)
{
  uint8_t value = (same (kv->value_input, "yes") ? 1 : 0);
  return pef_global_control_commit (state_data,
				    NULL,
				    NULL,
				    NULL,
				    NULL,
				    &value,
				    NULL);
}

static bmc_diff_t
enable_oem_action_diff (bmc_config_state_data_t *state_data,
			const struct config_section *sect,
			const struct config_keyvalue *kv)
{
  uint8_t passed_value;
  uint8_t got_value;
  config_err_t rc;
  bmc_diff_t ret;
  
  if ((rc = pef_global_control_checkout (state_data,
                                         NULL,
                                         NULL,
                                         NULL,
                                         NULL,
                                         &got_value,
                                         NULL)) != CONFIG_ERR_SUCCESS)
    {
      if (rc == CONFIG_ERR_NON_FATAL_ERROR)
        return BMC_DIFF_NON_FATAL_ERROR;
      return BMC_DIFF_FATAL_ERROR;
    }

  passed_value = (same (kv->value_input, "yes") ? 1 : 0);

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

/* diagnostic_interrupt */

static config_err_t
enable_diagnostic_interrupt_checkout (bmc_config_state_data_t *state_data,
                                      const struct config_section *sect,
                                      struct config_keyvalue *kv)
{
  uint8_t value;
  config_err_t ret;

  if ((ret = pef_global_control_checkout (state_data,
                                          NULL,
                                          NULL,
                                          NULL,
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
enable_diagnostic_interrupt_commit (bmc_config_state_data_t *state_data,
				    const struct config_section *sect,
				    const struct config_keyvalue *kv)
{
  uint8_t value = (same (kv->value_input, "yes") ? 1 : 0);
  return pef_global_control_commit (state_data,
				    NULL,
				    NULL,
				    NULL,
				    NULL,
				    NULL,
				    &value);
}

static bmc_diff_t
enable_diagnostic_interrupt_diff (bmc_config_state_data_t *state_data,
				  const struct config_section *sect,
				  const struct config_keyvalue *kv)
{
  uint8_t passed_value;
  uint8_t got_value;
  config_err_t rc;
  bmc_diff_t ret;
  
  if ((rc = pef_global_control_checkout (state_data,
                                         NULL,
                                         NULL,
                                         NULL,
                                         NULL,
                                         NULL,
                                         &got_value)) != CONFIG_ERR_SUCCESS)
    {
      if (rc == CONFIG_ERR_NON_FATAL_ERROR)
        return BMC_DIFF_NON_FATAL_ERROR;
      return BMC_DIFF_FATAL_ERROR;
    }

  passed_value = (same (kv->value_input, "yes") ? 1 : 0);

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


/* pef_startup_delay */

static config_err_t
pef_startup_delay_checkout (bmc_config_state_data_t *state_data,
			    const struct config_section *sect,
			    struct config_keyvalue *kv)
{
  uint8_t delay;
  config_err_t ret;
  
  if ((ret = get_pef_startup_delay (state_data,
                                    &delay)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (asprintf (&kv->value_output, "%d", delay) < 0)
    {
      perror("asprintf");
      return CONFIG_ERR_FATAL_ERROR;
    }
  return CONFIG_ERR_SUCCESS;
}

static config_err_t
pef_startup_delay_commit (bmc_config_state_data_t *state_data,
			  const struct config_section *sect,
			  const struct config_keyvalue *kv)
{
  uint8_t value = atoi (kv->value_input);
  return set_pef_startup_delay (state_data,
				value);
}

static bmc_diff_t
pef_startup_delay_diff (bmc_config_state_data_t *state_data,
			const struct config_section *sect,
			const struct config_keyvalue *kv)
{
  uint8_t got_value;
  uint8_t passed_value;
  config_err_t rc;
  bmc_diff_t ret;
  
  if ((rc = get_pef_startup_delay (state_data,
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

/* alert_startup_delay */

static config_err_t
pef_alert_startup_delay_checkout (bmc_config_state_data_t *state_data,
				  const struct config_section *sect,
				  struct config_keyvalue *kv)
{
  uint8_t delay;
  config_err_t ret;
  
  if ((ret = get_pef_alert_startup_delay (state_data,
                                          &delay)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (asprintf (&kv->value_output, "%d", delay) < 0)
    {
      perror("asprintf");
      return CONFIG_ERR_FATAL_ERROR;
    }
  return CONFIG_ERR_SUCCESS;
}

static config_err_t
pef_alert_startup_delay_commit (bmc_config_state_data_t *state_data,
				const struct config_section *sect,
				const struct config_keyvalue *kv)
{
  uint8_t value = atoi (kv->value_input);
  return set_pef_alert_startup_delay (state_data,
				      value);
}

static bmc_diff_t
pef_alert_startup_delay_diff (bmc_config_state_data_t *state_data,
			      const struct config_section *sect,
			      const struct config_keyvalue *kv)
{
  uint8_t got_value;
  uint8_t passed_value;
  config_err_t rc;
  bmc_diff_t ret;
  
  if ((rc = get_pef_alert_startup_delay (state_data,
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

struct config_section *
bmc_config_pef_conf_section_get (bmc_config_state_data_t *state_data)
{
  struct config_section *pef_section;

  if (!(pef_section = config_section_create ("PEF_Conf",
                                             NULL,
                                             NULL,
                                             CONFIG_DO_NOT_CHECKOUT,
                                             NULL, /* XXX */
                                             NULL)))
    goto cleanup;

  if (config_section_add_key (pef_section,
                              "Enable_PEF",
                              "Possible values: Yes/No",
                              CONFIG_DO_NOT_CHECKOUT,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (pef_section,
                              "Enable_PEF_Event_Messages",
                              "Possible values: Yes/No",
                              CONFIG_DO_NOT_CHECKOUT,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (pef_section,
                              "Enable_PEF_Startup_Delay",
                              "Possible values: Yes/No",
                              CONFIG_DO_NOT_CHECKOUT,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (pef_section,
                              "Enable_PEF_Alert_Startup_Delay",
                              "Possible values: Yes/No",
                              CONFIG_DO_NOT_CHECKOUT,
                              config_yes_no_validate) < 0)
    goto cleanup;
  
  if (config_section_add_key (pef_section,
                              "Enable_Alert_Action",
                              "Possible values: Yes/No",
                              CONFIG_DO_NOT_CHECKOUT,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (pef_section,
                              "Enable_Power_Down_Action",
                              "Possible values: Yes/No",
                              CONFIG_DO_NOT_CHECKOUT,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (pef_section,
                              "Enable_Reset_Action",
                              "Possible values: Yes/No",
                              CONFIG_DO_NOT_CHECKOUT,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (pef_section,
                              "Enable_Power_Cycle_Action",
                              "Possible values: Yes/No",
                              CONFIG_DO_NOT_CHECKOUT,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (pef_section,
                              "Enable_OEM_Action",
                              "Possible values: Yes/No",
                              CONFIG_DO_NOT_CHECKOUT,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (pef_section,
                              "Enable_Diagnostic_Interrupt",
                              "Possible values: Yes/No",
                              CONFIG_DO_NOT_CHECKOUT,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (pef_section,
                              "PEF_Startup_Delay",
                              "Give value in seconds",
                              CONFIG_DO_NOT_CHECKOUT,
                              config_number_range_one_byte) < 0)
    goto cleanup;

  if (config_section_add_key (pef_section,
                              "PEF_Alert_Startup_Delay",
                              "Give value in seconds",
                              CONFIG_DO_NOT_CHECKOUT,
                              config_number_range_one_byte) < 0)
    goto cleanup;

  return pef_section;

 cleanup:
  if (pef_section)
    config_section_destroy(pef_section);
  return NULL;
}

