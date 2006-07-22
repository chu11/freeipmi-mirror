#include "bmc-common.h"
#include "bmc-diff.h"
#include "bmc-types.h"
#include "bmc-sections.h"
#include "bmc-config-api.h"

int
pef_control_checkout (ipmi_device_t *dev,
		      uint8_t *pef,
		      uint8_t *pef_event_messages,
		      uint8_t *pef_startup_delay,
		      uint8_t *pef_alert_startup_delay)
{
  int ret;
  uint8_t tmp_pef;
  uint8_t tmp_pef_event_messages;
  uint8_t tmp_pef_startup_delay;
  uint8_t tmp_pef_alert_startup_delay;

  ret = get_pef_control (dev,
			 &tmp_pef,
			 &tmp_pef_event_messages,
			 &tmp_pef_startup_delay,
			 &tmp_pef_alert_startup_delay);

  if (ret != 0)
    return -1;

  if (pef)
    *pef = tmp_pef;

  if (pef_event_messages)
    *pef_event_messages = tmp_pef_event_messages;

  if (pef_startup_delay)
    *pef_startup_delay = tmp_pef_startup_delay;

  if (pef_alert_startup_delay)
    *pef_alert_startup_delay = tmp_pef_alert_startup_delay;

  return 0;
}


int
pef_control_commit (ipmi_device_t *dev,
		    uint8_t *pef,
		    uint8_t *pef_event_messages,
		    uint8_t *pef_startup_delay,
		    uint8_t *pef_alert_startup_delay)
{
  int ret;
  uint8_t tmp_pef;
  uint8_t tmp_pef_event_messages;
  uint8_t tmp_pef_startup_delay;
  uint8_t tmp_pef_alert_startup_delay;

  ret = get_pef_control (dev,
			 &tmp_pef,
			 &tmp_pef_event_messages,
			 &tmp_pef_startup_delay,
			 &tmp_pef_alert_startup_delay);

  if (ret != 0)
    return -1;

  if (pef)
    *pef = tmp_pef;

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

static int
enable_pef_checkout (const struct arguments *args,
		     const struct section *sect,
		     struct keyvalue *kv)
{
  int ret;
  uint8_t value;
  
  ret = pef_control_checkout ((ipmi_device_t *)&args->dev,
			      &value,
			      NULL,
			      NULL,
			      NULL);
  if (ret != 0)
    return -1;

  if (kv->value)
    free (kv->value);

  kv->value = strdup (value ? "Yes" : "No");
  return 0;
}

static int
enable_pef_commit (const struct arguments *args,
		   const struct section *sect,
		   const struct keyvalue *kv)
{
  uint8_t value = same (kv->value, "yes");
  return pef_control_commit ((ipmi_device_t *)&args->dev,
			     &value,
			     NULL,
			     NULL,
			     NULL);
}

static int
enable_pef_diff (const struct arguments *args,
		 const struct section *sect,
		 const struct keyvalue *kv)
{
  uint8_t got_value;
  uint8_t passed_value;
  int ret;

  ret = pef_control_checkout ((ipmi_device_t *)&args->dev,
			      &got_value,
			      NULL,
			      NULL,
			      NULL);

  if (ret != 0)
    return -1;

  passed_value = same (kv->value, "yes");

  if (passed_value == got_value)
    ret = 0;
  else 
    {
      ret = 1;
      report_diff (sect->section,
                   kv->key,
                   kv->value,
                   got_value ? "Yes" : "No");
    }
  return ret;
}

static int
enable_pef_validate (const struct arguments *args,
		     const struct section *sect,
		     const char *value)
{
  return (value && (same (value, "yes") || same (value, "no"))) ? 0 : 1;
}

/* event_messages */

static int
enable_pef_event_messages_checkout (const struct arguments *args,
				    const struct section *sect,
				    struct keyvalue *kv)
{
  int ret;
  uint8_t value;
  
  ret = pef_control_checkout ((ipmi_device_t *)&args->dev,
			      NULL,
			      &value,
			      NULL,
			      NULL);
  if (ret != 0)
    return -1;

  if (kv->value)
    free (kv->value);

  kv->value = strdup (value ? "Yes" : "No");
  return 0;
}

static int
enable_pef_event_messages_commit (const struct arguments *args,
				  const struct section *sect,
				  const struct keyvalue *kv)
{
  uint8_t value = same (kv->value, "yes");
  return pef_control_commit ((ipmi_device_t *)&args->dev,
			     NULL,
			     &value,
			     NULL,
			     NULL);
}

static int
enable_pef_event_messages_diff (const struct arguments *args,
				const struct section *sect,
				const struct keyvalue *kv)
{
  uint8_t got_value;
  uint8_t passed_value;
  int ret;

  ret = pef_control_checkout ((ipmi_device_t *)&args->dev,
			      NULL,
			      &got_value,
			      NULL,
			      NULL);

  if (ret != 0)
    return -1;

  passed_value = same (kv->value, "yes");

  if (passed_value == got_value)
    ret = 0;
  else 
    {
      ret = 1;
      report_diff (sect->section,
                   kv->key,
                   kv->value,
                   got_value ? "Yes" : "No");
    }
  return ret;
}

static int
enable_pef_event_messages_validate (const struct arguments *args,
				    const struct section *sect,
				    const char *value)
{
  return (value && (same (value, "yes") || same (value, "no"))) ? 0 : 1;
}

/* startup_delay */


static int
enable_pef_startup_delay_checkout (const struct arguments *args,
				   const struct section *sect,
				   struct keyvalue *kv)
{
  int ret;
  uint8_t value;
  
  ret = pef_control_checkout ((ipmi_device_t *)&args->dev,
			      NULL,
			      NULL,
			      &value,
			      NULL);
  if (ret != 0)
    return -1;

  if (kv->value)
    free (kv->value);

  kv->value = strdup (value ? "Yes" : "No");
  return 0;
}

static int
enable_pef_startup_delay_commit (const struct arguments *args,
				 const struct section *sect,
				 const struct keyvalue *kv)
{
  uint8_t value = same (kv->value, "yes");
  return pef_control_commit ((ipmi_device_t *)&args->dev,
			     NULL,
			     NULL,
			     &value,
			     NULL);
}

static int
enable_pef_startup_delay_diff (const struct arguments *args,
			       const struct section *sect,
			       const struct keyvalue *kv)
{
  uint8_t got_value;
  uint8_t passed_value;
  int ret;

  ret = pef_control_checkout ((ipmi_device_t *)&args->dev,
			      NULL,
			      NULL,
			      &got_value,
			      NULL);

  if (ret != 0)
    return -1;

  passed_value = same (kv->value, "yes");

  if (passed_value == got_value)
    ret = 0;
  else 
    {
      ret = 1;
      report_diff (sect->section,
                   kv->key,
                   kv->value,
                   got_value ? "Yes" : "No");
    }
  return ret;
}

static int
enable_pef_startup_delay_validate (const struct arguments *args,
				   const struct section *sect,
				   const char *value)
{
  return (value && (same (value, "yes") || same (value, "no"))) ? 0 : 1;
}

/* alert_startup_delay */


static int
enable_pef_alert_startup_delay_checkout (const struct arguments *args,
					 const struct section *sect,
					 struct keyvalue *kv)
{
  int ret;
  uint8_t value;
  
  ret = pef_control_checkout ((ipmi_device_t *)&args->dev,
			      NULL,
			      NULL,
			      NULL,
			      &value);
  if (ret != 0)
    return -1;

  if (kv->value)
    free (kv->value);

  kv->value = strdup (value ? "Yes" : "No");
  return 0;
}

static int
enable_pef_alert_startup_delay_commit (const struct arguments *args,
				       const struct section *sect,
				       const struct keyvalue *kv)
{
  uint8_t value = same (kv->value, "yes");
  return pef_control_commit ((ipmi_device_t *)&args->dev,
			     NULL,
			     NULL,
			     NULL,
			     &value);
}

static int
enable_pef_alert_startup_delay_diff (const struct arguments *args,
				     const struct section *sect,
				     const struct keyvalue *kv)
{
  uint8_t got_value;
  uint8_t passed_value;
  int ret;

  ret = pef_control_checkout ((ipmi_device_t *)&args->dev,
			      NULL,
			      NULL,
			      NULL,
			      &got_value);

  if (ret != 0)
    return -1;

  passed_value = same (kv->value, "yes");

  if (passed_value == got_value)
    ret = 0;
  else 
    {
      ret = 1;
      report_diff (sect->section,
                   kv->key,
                   kv->value,
                   got_value ? "Yes" : "No");
    }
  return ret;
}

static int
enable_pef_alert_startup_delay_validate (const struct arguments *args,
					 const struct section *sect,
					 const char *value)
{
  return (value && (same (value, "yes") || same (value, "no"))) ? 0 : 1;
}


static int
pef_global_control_checkout (ipmi_device_t *dev,
			     uint8_t *alert_action,
			     uint8_t *power_down_action,
			     uint8_t *reset_action,
			     uint8_t *power_cycle_action,
			     uint8_t *oem_action,
			     uint8_t *diagnostic_interrupt)
{
  int ret;

  uint8_t tmp_alert_action;
  uint8_t tmp_power_down_action;
  uint8_t tmp_reset_action;
  uint8_t tmp_power_cycle_action;
  uint8_t tmp_oem_action;
  uint8_t tmp_diagnostic_interrupt;

  ret = get_pef_action_global_control (dev,
				       &tmp_alert_action,
				       &tmp_power_down_action,
				       &tmp_reset_action,
				       &tmp_power_cycle_action,
				       &tmp_oem_action,
				       &tmp_diagnostic_interrupt);

  if (ret != 0)
    return -1;

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

  return 0;
}


static int
pef_global_control_commit (ipmi_device_t *dev,
			   uint8_t *alert_action,
			   uint8_t *power_down_action,
			   uint8_t *reset_action,
			   uint8_t *power_cycle_action,
			   uint8_t *oem_action,
			   uint8_t *diagnostic_interrupt)
{
  int ret;

  uint8_t tmp_alert_action;
  uint8_t tmp_power_down_action;
  uint8_t tmp_reset_action;
  uint8_t tmp_power_cycle_action;
  uint8_t tmp_oem_action;
  uint8_t tmp_diagnostic_interrupt;

  ret = get_pef_action_global_control (dev,
				       &tmp_alert_action,
				       &tmp_power_down_action,
				       &tmp_reset_action,
				       &tmp_power_cycle_action,
				       &tmp_oem_action,
				       &tmp_diagnostic_interrupt);

  if (ret != 0)
    return -1;

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
			   
static int
enable_alert_action_checkout (const struct arguments *args,
			      const struct section *sect,
			      struct keyvalue *kv)
{
  int ret;
  uint8_t value;

  ret = pef_global_control_checkout ((ipmi_device_t *)&args->dev,
				     &value,
				     NULL,
				     NULL,
				     NULL,
				     NULL,
				     NULL);

  if (ret != 0)
    return -1;

  if (kv->value)
    free (kv->value);

  kv->value = strdup (value ? "Yes" : "No");
  return 0;
}

static int
enable_alert_action_commit (const struct arguments *args,
			    const struct section *sect,
			    const struct keyvalue *kv)
{
  uint8_t value = (same (kv->value, "yes") ? 1 : 0);
  return pef_global_control_commit ((ipmi_device_t *)&args->dev,
				    &value,
				    NULL,
				    NULL,
				    NULL,
				    NULL,
				    NULL);
}

static int
enable_alert_action_diff (const struct arguments *args,
			  const struct section *sect,
			  const struct keyvalue *kv)
{
  uint8_t passed_value;
  uint8_t got_value;
  int ret;
  
  ret = pef_global_control_checkout ((ipmi_device_t *)&args->dev,
				     &got_value,
				     NULL,
				     NULL,
				     NULL,
				     NULL,
				     NULL);

  if (ret != 0)
    return -1;

  passed_value = (same (kv->value, "yes") ? 1 : 0);

  if (passed_value == got_value)
    ret = 0;
  else 
    {
      ret = 1;
      report_diff (sect->section,
                   kv->key,
                   kv->value,
                   got_value ? "Yes" : "No");
    }
  return ret;
}

static int
enable_alert_action_validate (const struct arguments *args,
			      const struct section *sect,
			      const char *value)
{
  return (value && (same (value, "yes") || same (value, "no"))) ? 0 : 1;
}

/* power_down_action */

static int
enable_power_down_action_checkout (const struct arguments *args,
				   const struct section *sect,
				   struct keyvalue *kv)
{
  int ret;
  uint8_t value;

  ret = pef_global_control_checkout ((ipmi_device_t *)&args->dev,
				     NULL,
				     &value,
				     NULL,
				     NULL,
				     NULL,
				     NULL);

  if (ret != 0)
    return -1;

  if (kv->value)
    free (kv->value);

  kv->value = strdup (value ? "Yes" : "No");
  return 0;
}

static int
enable_power_down_action_commit (const struct arguments *args,
				 const struct section *sect,
				 const struct keyvalue *kv)
{
  uint8_t value = (same (kv->value, "yes") ? 1 : 0);
  return pef_global_control_commit ((ipmi_device_t *)&args->dev,
				    NULL,
				    &value,
				    NULL,
				    NULL,
				    NULL,
				    NULL);
}

static int
enable_power_down_action_diff (const struct arguments *args,
			       const struct section *sect,
			       const struct keyvalue *kv)
{
  uint8_t passed_value;
  uint8_t got_value;
  int ret;
  
  ret = pef_global_control_checkout ((ipmi_device_t *)&args->dev,
				     NULL,
				     &got_value,
				     NULL,
				     NULL,
				     NULL,
				     NULL);

  if (ret != 0)
    return -1;

  passed_value = (same (kv->value, "yes") ? 1 : 0);
  if (passed_value == got_value)
    ret = 0;
  else 
    {
      ret = 1;
      report_diff (sect->section,
                   kv->key,
                   kv->value,
                   got_value ? "Yes" : "No");
    }
  return ret;
}

static int
enable_power_down_action_validate (const struct arguments *args,
				   const struct section *sect,
				   const char *value)
{
  return (value && (same (value, "yes") || same (value, "no"))) ? 0 : 1;
}

/* reset_action */

static int
enable_reset_action_checkout (const struct arguments *args,
			      const struct section *sect,
			      struct keyvalue *kv)
{
  int ret;
  uint8_t value;

  ret = pef_global_control_checkout ((ipmi_device_t *)&args->dev,
				     NULL,
				     NULL,
				     &value,
				     NULL,
				     NULL,
				     NULL);

  if (ret != 0)
    return -1;

  if (kv->value)
    free (kv->value);

  kv->value = strdup (value ? "Yes" : "No");
  return 0;
}

static int
enable_reset_action_commit (const struct arguments *args,
			    const struct section *sect,
			    const struct keyvalue *kv)
{
  uint8_t value = (same (kv->value, "yes") ? 1 : 0);
  return pef_global_control_commit ((ipmi_device_t *)&args->dev,
				    NULL,
				    NULL,
				    &value,
				    NULL,
				    NULL,
				    NULL);
}

static int
enable_reset_action_diff (const struct arguments *args,
			  const struct section *sect,
			  const struct keyvalue *kv)
{
  uint8_t passed_value;
  uint8_t got_value;
  int ret;
  
  ret = pef_global_control_checkout ((ipmi_device_t *)&args->dev,
				     NULL,
				     NULL,
				     &got_value,
				     NULL,
				     NULL,
				     NULL);

  if (ret != 0)
    return -1;

  passed_value = (same (kv->value, "yes") ? 1 : 0);
  if (passed_value == got_value)
    ret = 0;
  else 
    {
      ret = 1;
      report_diff (sect->section,
                   kv->key,
                   kv->value,
                   got_value ? "Yes" : "No");
    }
  return ret;
}

static int
enable_reset_action_validate (const struct arguments *args,
			      const struct section *sect,
			      const char *value)
{
  return (value && (same (value, "yes") || same (value, "no"))) ? 0 : 1;
}

/* power_cycle_action */

static int
enable_power_cycle_action_checkout (const struct arguments *args,
				    const struct section *sect,
				    struct keyvalue *kv)
{
  int ret;
  uint8_t value;

  ret = pef_global_control_checkout ((ipmi_device_t *)&args->dev,
				     NULL,
				     NULL,
				     NULL,
				     &value,
				     NULL,
				     NULL);

  if (ret != 0)
    return -1;

  if (kv->value)
    free (kv->value);

  kv->value = strdup (value ? "Yes" : "No");
  return 0;
}

static int
enable_power_cycle_action_commit (const struct arguments *args,
				  const struct section *sect,
				  const struct keyvalue *kv)
{
  uint8_t value = (same (kv->value, "yes") ? 1 : 0);
  return pef_global_control_commit ((ipmi_device_t *)&args->dev,
				    NULL,
				    NULL,
				    NULL,
				    &value,
				    NULL,
				    NULL);
}

static int
enable_power_cycle_action_diff (const struct arguments *args,
				const struct section *sect,
				const struct keyvalue *kv)
{
  uint8_t passed_value;
  uint8_t got_value;
  int ret;
  
  ret = pef_global_control_checkout ((ipmi_device_t *)&args->dev,
				     NULL,
				     NULL,
				     NULL,
				     &got_value,
				     NULL,
				     NULL);

  if (ret != 0)
    return -1;

  passed_value = (same (kv->value, "yes") ? 1 : 0);
  if (passed_value == got_value)
    ret = 0;
  else 
    {
      ret = 1;
      report_diff (sect->section,
                   kv->key,
                   kv->value,
                   got_value ? "Yes" : "No");
    }
  return ret;
}

static int
enable_power_cycle_action_validate (const struct arguments *args,
				    const struct section *sect,
				    const char *value)
{
  return (value && (same (value, "yes") || same (value, "no"))) ? 0 : 1;
}

/* OEM_action */

static int
enable_oem_action_checkout (const struct arguments *args,
			    const struct section *sect,
			    struct keyvalue *kv)
{
  int ret;
  uint8_t value;

  ret = pef_global_control_checkout ((ipmi_device_t *)&args->dev,
				     NULL,
				     NULL,
				     NULL,
				     NULL,
				     &value,
				     NULL);

  if (ret != 0)
    return -1;

  if (kv->value)
    free (kv->value);

  kv->value = strdup (value ? "Yes" : "No");
  return 0;
}

static int
enable_oem_action_commit (const struct arguments *args,
			  const struct section *sect,
			  const struct keyvalue *kv)
{
  uint8_t value = (same (kv->value, "yes") ? 1 : 0);
  return pef_global_control_commit ((ipmi_device_t *)&args->dev,
				    NULL,
				    NULL,
				    NULL,
				    NULL,
				    &value,
				    NULL);
}

static int
enable_oem_action_diff (const struct arguments *args,
			const struct section *sect,
			const struct keyvalue *kv)
{
  uint8_t passed_value;
  uint8_t got_value;
  int ret;
  
  ret = pef_global_control_checkout ((ipmi_device_t *)&args->dev,
				     NULL,
				     NULL,
				     NULL,
				     NULL,
				     &got_value,
				     NULL);

  if (ret != 0)
    return -1;

  passed_value = (same (kv->value, "yes") ? 1 : 0);

  if (passed_value == got_value)
    ret = 0;
  else 
    {
      ret = 1;
      report_diff (sect->section,
                   kv->key,
                   kv->value,
                   got_value ? "Yes" : "No");
    }
  return ret;
}

static int
enable_oem_action_validate (const struct arguments *args,
			    const struct section *sect,
			    const char *value)
{
  return (value && (same (value, "yes") || same (value, "no"))) ? 0 : 1;
}

/* diagnostic_interrupt */

static int
enable_diagnostic_interrupt_checkout (const struct arguments *args,
			    const struct section *sect,
			    struct keyvalue *kv)
{
  int ret;
  uint8_t value;

  ret = pef_global_control_checkout ((ipmi_device_t *)&args->dev,
				     NULL,
				     NULL,
				     NULL,
				     NULL,
				     NULL,
				     &value);

  if (ret != 0)
    return -1;

  if (kv->value)
    free (kv->value);

  kv->value = strdup (value ? "Yes" : "No");
  return 0;
}

static int
enable_diagnostic_interrupt_commit (const struct arguments *args,
				    const struct section *sect,
				    const struct keyvalue *kv)
{
  uint8_t value = (same (kv->value, "yes") ? 1 : 0);
  return pef_global_control_commit ((ipmi_device_t *)&args->dev,
				    NULL,
				    NULL,
				    NULL,
				    NULL,
				    NULL,
				    &value);
}

static int
enable_diagnostic_interrupt_diff (const struct arguments *args,
				  const struct section *sect,
				  const struct keyvalue *kv)
{
  uint8_t passed_value;
  uint8_t got_value;
  int ret;
  
  ret = pef_global_control_checkout ((ipmi_device_t *)&args->dev,
				     NULL,
				     NULL,
				     NULL,
				     NULL,
				     NULL,
				     &got_value);

  if (ret != 0)
    return -1;

  passed_value = (same (kv->value, "yes") ? 1 : 0);

  if (passed_value == got_value)
    ret = 0;
  else 
    {
      ret = 1;
      report_diff (sect->section,
                   kv->key,
                   kv->value,
                   got_value ? "Yes" : "No");
    }
  return ret;
}


static int
enable_diagnostic_interrupt_validate (const struct arguments *args,
				      const struct section *sect,
				      const char *value)
{
  return (value && (same (value, "yes") || same (value, "no"))) ? 0 : 1;
}

/* pef_startup_delay */
static int
pef_startup_delay_checkout (const struct arguments *args,
			    const struct section *sect,
			    struct keyvalue *kv)
{
  int ret;
  uint8_t delay;
  
  ret = get_pef_startup_delay ((ipmi_device_t *)&args->dev,
			       &delay);
  if (ret != 0)
    return -1;

  if (kv->value)
    free (kv->value);

  asprintf (&kv->value, "%d", delay);
  return 0;
}

static int
pef_startup_delay_commit (const struct arguments *args,
			  const struct section *sect,
			  const struct keyvalue *kv)
{
  uint8_t value = atoi (kv->value);
  return set_pef_startup_delay ((ipmi_device_t *)&args->dev,
				value);
}

static int
pef_startup_delay_diff (const struct arguments *args,
			const struct section *sect,
			const struct keyvalue *kv)
{
  int ret;
  uint8_t got_value;
  uint8_t passed_value;
  
  ret = get_pef_startup_delay ((ipmi_device_t *)&args->dev,
			       &got_value);
  if (ret != 0)
    return -1;

  passed_value = atoi (kv->value);

  if (passed_value == got_value)
    ret = 0;
  else 
    {
      char num[32];
      ret = 1;
      sprintf (num, "%d", got_value);
      report_diff (sect->section,
                   kv->key,
                   kv->value,
                   num);
    }
  return ret;
}

static int
pef_startup_delay_validate (const struct arguments *args,
			    const struct section *sect,
			    const char *value)
{
  int num;
  char *endptr;

  num = strtol (value, &endptr, 0);

  if (*endptr)
    return -1;
  if (num < 0 || num > 255)
    return 1;

  return 0;
}

/* alert_startup_delay */

static int
pef_alert_startup_delay_checkout (const struct arguments *args,
				  const struct section *sect,
				  struct keyvalue *kv)
{
  int ret;
  uint8_t delay;
  
  ret = get_pef_alert_startup_delay ((ipmi_device_t *)&args->dev,
				     &delay);
  if (ret != 0)
    return -1;

  if (kv->value)
    free (kv->value);

  asprintf (&kv->value, "%d", delay);
  return 0;
}

static int
pef_alert_startup_delay_commit (const struct arguments *args,
				const struct section *sect,
				const struct keyvalue *kv)
{
  uint8_t value = atoi (kv->value);
  return set_pef_alert_startup_delay ((ipmi_device_t *)&args->dev,
				      value);
}

static int
pef_alert_startup_delay_diff (const struct arguments *args,
			      const struct section *sect,
			      const struct keyvalue *kv)
{
  int ret;
  uint8_t got_value;
  uint8_t passed_value;
  
  ret = get_pef_alert_startup_delay ((ipmi_device_t *)&args->dev,
			       &got_value);
  if (ret != 0)
    return -1;

  passed_value = atoi (kv->value);

  if (passed_value == got_value)
    ret = 0;
  else 
    {
      char num[32];
      ret = 1;
      sprintf (num, "%d", got_value);
      report_diff (sect->section,
                   kv->key,
                   kv->value,
                   num);
    }
  return ret;
}

static int
pef_alert_startup_delay_validate (const struct arguments *args,
				  const struct section *sect,
				  const char *value)
{
  int num;
  char *endptr;

  num = strtol (value, &endptr, 0);

  if (*endptr)
    return -1;
  if (num < 0 || num > 255)
    return 1;

  return 0;
}


struct section *
bmc_pef_conf_section_get (struct arguments *args)
{
  struct section *pef_section;
  pef_section = (void *) calloc (1, sizeof (struct section));
  pef_section->section = strdup ("PEF_Conf");

  add_keyvalue (pef_section,
		"Enable_PEF",
		"Possible values: Yes/No",
                0,
		enable_pef_checkout,
		enable_pef_commit,
		enable_pef_diff,
		enable_pef_validate);

  add_keyvalue (pef_section,
		"Enable_PEF_Event_Messages",
		"Possible values: Yes/No",
                0,
		enable_pef_event_messages_checkout,
		enable_pef_event_messages_commit,
		enable_pef_event_messages_diff,
		enable_pef_event_messages_validate);

  add_keyvalue (pef_section,
		"Enable_PEF_Startup_Delay",
		"Possible values: Yes/No",
                0,
		enable_pef_startup_delay_checkout,
		enable_pef_startup_delay_commit,
		enable_pef_startup_delay_diff,
		enable_pef_startup_delay_validate);

  add_keyvalue (pef_section,
		"Enable_PEF_Alert_Startup_Delay",
		"Possible values: Yes/No",
                0,
		enable_pef_alert_startup_delay_checkout,
		enable_pef_alert_startup_delay_commit,
		enable_pef_alert_startup_delay_diff,
		enable_pef_alert_startup_delay_validate);
  
  add_keyvalue (pef_section,
		"Enable_Alert_Action",
		"Possible values: Yes/No",
                0,
		enable_alert_action_checkout,
		enable_alert_action_commit,
		enable_alert_action_diff,
		enable_alert_action_validate);

  add_keyvalue (pef_section,
		"Enable_Power_Down_Action",
		"Possible values: Yes/No",
                0,
		enable_power_down_action_checkout,
		enable_power_down_action_commit,
		enable_power_down_action_diff,
		enable_power_down_action_validate);

  add_keyvalue (pef_section,
		"Enable_Reset_Action",
		"Possible values: Yes/No",
                0,
		enable_reset_action_checkout,
		enable_reset_action_commit,
		enable_reset_action_diff,
		enable_reset_action_validate);

  add_keyvalue (pef_section,
		"Enable_Power_Cycle_Action",
		"Possible values: Yes/No",
                0,
		enable_power_cycle_action_checkout,
		enable_power_cycle_action_commit,
		enable_power_cycle_action_diff,
		enable_power_cycle_action_validate);

  add_keyvalue (pef_section,
		"Enable_OEM_Action",
		"Possible values: Yes/No",
                0,
		enable_oem_action_checkout,
		enable_oem_action_commit,
		enable_oem_action_diff,
		enable_oem_action_validate);

  add_keyvalue (pef_section,
		"Enable_Diagnostic_Interrupt",
		"Possible values: Yes/No",
                0,
		enable_diagnostic_interrupt_checkout,
		enable_diagnostic_interrupt_commit,
		enable_diagnostic_interrupt_diff,
		enable_diagnostic_interrupt_validate);

  add_keyvalue (pef_section,
		"PEF_Startup_Delay",
		"Give value in seconds",
                0,
		pef_startup_delay_checkout,
		pef_startup_delay_commit,
		pef_startup_delay_diff,
		pef_startup_delay_validate);

  add_keyvalue (pef_section,
		"PEF_Alert_Startup_Delay",
		"Give value in seconds",
                0,
		pef_alert_startup_delay_checkout,
		pef_alert_startup_delay_commit,
		pef_alert_startup_delay_diff,
		pef_alert_startup_delay_validate);


  return pef_section;
}

