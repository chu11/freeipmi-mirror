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

/* convenience structs */

struct pef_control
{
  uint8_t enable_pef;
  uint8_t enable_pef_event_messages;
  uint8_t enable_pef_startup_delay;
  uint8_t enable_pef_alert_startup_delay;
};

struct pef_action_global_control
{
  uint8_t enable_alert_action;
  uint8_t enable_power_down_action;
  uint8_t enable_reset_action;
  uint8_t enable_power_cycle_action;
  uint8_t enable_oem_action;
  uint8_t enable_diagnostic_interrupt;
};

static config_err_t
_get_pef_control (bmc_config_state_data_t *state_data,
                  struct pef_control *pc)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val = 0;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;

  assert(state_data);
  assert(pc);

  if (!(obj_cmd_rs = Fiid_obj_create(tmpl_cmd_get_pef_configuration_parameters_pef_control_rs)))
    goto cleanup;

  if (ipmi_cmd_get_pef_configuration_parameters_pef_control (state_data->dev,
                                                             IPMI_GET_PEF_PARAMETER,
                                                             SET_SELECTOR,
                                                             BLOCK_SELECTOR,
                                                             obj_cmd_rs) < 0)
    {
      if (state_data->prog_data->args->common.flags & IPMI_FLAGS_DEBUG_DUMP)
        fprintf(stderr,
                "ipmi_cmd_get_pef_configuration_parameters_pef_control: %s\n",
                ipmi_device_strerror(ipmi_device_errnum(state_data->dev)));
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  if (Fiid_obj_get (obj_cmd_rs, "pef", &val) < 0)
    goto cleanup;
  pc->enable_pef = val;

  if (Fiid_obj_get (obj_cmd_rs, "pef_event_messages", &val) < 0)
    goto cleanup;
  pc->enable_pef_event_messages = val;

  if (Fiid_obj_get (obj_cmd_rs, "pef_startup_delay", &val) < 0)
    goto cleanup;
  pc->enable_pef_startup_delay = val;

  if (Fiid_obj_get (obj_cmd_rs, "pef_alert_startup_delay", &val) < 0)
    goto cleanup;
  pc->enable_pef_alert_startup_delay = val;

  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  Fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

static config_err_t
_set_pef_control (bmc_config_state_data_t *state_data,
                  struct pef_control *pc)
{
  fiid_obj_t obj_cmd_rs = NULL;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;

  assert(state_data);
  assert(pc);

  if (!(obj_cmd_rs = Fiid_obj_create(tmpl_cmd_set_pef_configuration_parameters_rs)))
    goto cleanup;

  if (ipmi_cmd_set_pef_configuration_parameters_pef_control (state_data->dev,
                                                             pc->enable_pef,
                                                             pc->enable_pef_event_messages,
                                                             pc->enable_pef_startup_delay,
                                                             pc->enable_pef_alert_startup_delay,
                                                             obj_cmd_rs) < 0)
    {
      if (state_data->prog_data->args->common.flags & IPMI_FLAGS_DEBUG_DUMP)
        fprintf(stderr,
                "ipmi_cmd_set_pef_configuration_parameters_pef_control: %s\n",
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
enable_pef_checkout (const char *section_name,
		     struct config_keyvalue *kv,
                     void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  struct pef_control pc;
  config_err_t ret;
  
  if ((ret = _get_pef_control (state_data, &pc)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (config_section_update_keyvalue_output(kv, pc.enable_pef ? "Yes" : "No") < 0)
    return CONFIG_ERR_FATAL_ERROR;

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
enable_pef_commit (const char *section_name,
		   const struct config_keyvalue *kv,
                   void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  struct pef_control pc;
  config_err_t ret;

  if ((ret = _get_pef_control (state_data, &pc)) != CONFIG_ERR_SUCCESS)
    return ret;

  pc.enable_pef = same (kv->value_input, "yes");

  return _set_pef_control (state_data, &pc);
}

static config_err_t
enable_pef_event_messages_checkout (const char *section_name,
				    struct config_keyvalue *kv,
                                    void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  struct pef_control pc;
  config_err_t ret;
  
  if ((ret = _get_pef_control (state_data, &pc)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (config_section_update_keyvalue_output(kv, pc.enable_pef_event_messages ? "Yes" : "No") < 0)
    return CONFIG_ERR_FATAL_ERROR;

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
enable_pef_event_messages_commit (const char *section_name,
				  const struct config_keyvalue *kv,
                                  void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  struct pef_control pc;
  config_err_t ret;

  if ((ret = _get_pef_control (state_data, &pc)) != CONFIG_ERR_SUCCESS)
    return ret;

  pc.enable_pef_event_messages = same (kv->value_input, "yes");

  return _set_pef_control (state_data, &pc);
}

static config_err_t
enable_pef_startup_delay_checkout (const char *section_name,
				   struct config_keyvalue *kv,
                                   void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  struct pef_control pc;
  config_err_t ret;
  
  if ((ret = _get_pef_control (state_data, &pc)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (config_section_update_keyvalue_output(kv, pc.enable_pef_startup_delay ? "Yes" : "No") < 0)
    return CONFIG_ERR_FATAL_ERROR;

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
enable_pef_startup_delay_commit (const char *section_name,
				 const struct config_keyvalue *kv,
                                 void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  struct pef_control pc;
  config_err_t ret;

  if ((ret = _get_pef_control (state_data, &pc)) != CONFIG_ERR_SUCCESS)
    return ret;

  pc.enable_pef_startup_delay = same (kv->value_input, "yes");

  return _set_pef_control (state_data, &pc);
}

static config_err_t
enable_pef_alert_startup_delay_checkout (const char *section_name,
					 struct config_keyvalue *kv,
                                         void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  struct pef_control pc;
  config_err_t ret;
  
  if ((ret = _get_pef_control (state_data, &pc)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (config_section_update_keyvalue_output(kv, pc.enable_pef_alert_startup_delay ? "Yes" : "No") < 0)
    return CONFIG_ERR_FATAL_ERROR;

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
enable_pef_alert_startup_delay_commit (const char *section_name,
				       const struct config_keyvalue *kv,
                                       void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  struct pef_control pc;
  config_err_t ret;

  if ((ret = _get_pef_control (state_data, &pc)) != CONFIG_ERR_SUCCESS)
    return ret;

  pc.enable_pef_alert_startup_delay = same (kv->value_input, "yes");

  return _set_pef_control (state_data, &pc);
}


static config_err_t
_get_pef_action_global_control (bmc_config_state_data_t *state_data,
                                struct pef_action_global_control *gc)
                               
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val = 0;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;

  assert(state_data);
  assert(gc);

  if (!(obj_cmd_rs = Fiid_obj_create(tmpl_cmd_get_pef_configuration_parameters_pef_action_global_control_rs)))
    goto cleanup;

  if (ipmi_cmd_get_pef_configuration_parameters_pef_action_global_control (state_data->dev,
                                                                           IPMI_GET_PEF_PARAMETER,
                                                                           SET_SELECTOR,
                                                                           BLOCK_SELECTOR,
                                                                           obj_cmd_rs) < 0)
    {
      if (state_data->prog_data->args->common.flags & IPMI_FLAGS_DEBUG_DUMP)
        fprintf(stderr,
                "ipmi_cmd_get_pef_configuration_parameters_pef_action_global_control: %s\n",
                ipmi_device_strerror(ipmi_device_errnum(state_data->dev)));
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  if (Fiid_obj_get (obj_cmd_rs, "alert_action", &val) < 0)
    goto cleanup;
  gc->enable_alert_action = val;

  if (Fiid_obj_get (obj_cmd_rs, "power_down_action", &val) < 0)
    goto cleanup;
  gc->enable_power_down_action = val;

  if (Fiid_obj_get (obj_cmd_rs, "reset_action", &val) < 0)
    goto cleanup;
  gc->enable_reset_action = val;

  if (Fiid_obj_get (obj_cmd_rs, "power_cycle_action", &val) < 0)
    goto cleanup;
  gc->enable_power_cycle_action = val;

  if (Fiid_obj_get (obj_cmd_rs, "oem_action", &val) < 0)
    goto cleanup;
  gc->enable_oem_action = val;

  if (Fiid_obj_get (obj_cmd_rs, "diagnostic_interrupt", &val) < 0)
    goto cleanup;
  gc->enable_diagnostic_interrupt = val;

  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  Fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

static config_err_t
_set_pef_action_global_control (bmc_config_state_data_t *state_data,
                                struct pef_action_global_control *gc)
{
  fiid_obj_t obj_cmd_rs = NULL;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;

  assert(state_data);
  assert(gc);

  if (!(obj_cmd_rs = Fiid_obj_create(tmpl_cmd_set_pef_configuration_parameters_rs)))
    goto cleanup;

  if (ipmi_cmd_set_pef_configuration_parameters_pef_action_global_control (state_data->dev,
                                                                           gc->enable_alert_action,
                                                                           gc->enable_power_down_action,
                                                                           gc->enable_reset_action,
                                                                           gc->enable_power_cycle_action,
                                                                           gc->enable_oem_action,
                                                                           gc->enable_diagnostic_interrupt,
                                                                           obj_cmd_rs) < 0)
    {
      if (state_data->prog_data->args->common.flags & IPMI_FLAGS_DEBUG_DUMP)
        fprintf(stderr,
                "ipmi_cmd_set_pef_configuration_parameters_pef_action_global_control: %s\n",
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
enable_alert_action_checkout (const char *section_name,
			      struct config_keyvalue *kv,
                              void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  struct pef_action_global_control gc;
  config_err_t ret;
  
  if ((ret = _get_pef_action_global_control (state_data, &gc)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (config_section_update_keyvalue_output(kv, gc.enable_alert_action ? "Yes" : "No") < 0)
    return CONFIG_ERR_FATAL_ERROR;

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
enable_alert_action_commit (const char *section_name,
			    const struct config_keyvalue *kv,
                            void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  struct pef_action_global_control gc;
  config_err_t ret;

  if ((ret = _get_pef_action_global_control (state_data, &gc)) != CONFIG_ERR_SUCCESS)
    return ret;

  gc.enable_alert_action = same (kv->value_input, "yes");

  return _set_pef_action_global_control (state_data, &gc);
}

static config_err_t
enable_power_down_action_checkout (const char *section_name,
				   struct config_keyvalue *kv,
                                   void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  struct pef_action_global_control gc;
  config_err_t ret;

  if ((ret = _get_pef_action_global_control (state_data, &gc)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (config_section_update_keyvalue_output(kv, gc.enable_power_down_action ? "Yes" : "No") < 0)
    return CONFIG_ERR_FATAL_ERROR;

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
enable_power_down_action_commit (const char *section_name,
				 const struct config_keyvalue *kv,
                                 void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  struct pef_action_global_control gc;
  config_err_t ret;

  if ((ret = _get_pef_action_global_control (state_data, &gc)) != CONFIG_ERR_SUCCESS)
    return ret;

  gc.enable_power_down_action = same (kv->value_input, "yes");

  return _set_pef_action_global_control (state_data, &gc);
}

static config_err_t
enable_reset_action_checkout (const char *section_name,
			      struct config_keyvalue *kv,
                              void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  struct pef_action_global_control gc;
  config_err_t ret;

  if ((ret = _get_pef_action_global_control (state_data, &gc)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (config_section_update_keyvalue_output(kv, gc.enable_reset_action ? "Yes" : "No") < 0)
    return CONFIG_ERR_FATAL_ERROR;

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
enable_reset_action_commit (const char *section_name,
			    const struct config_keyvalue *kv,
                            void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  struct pef_action_global_control gc;
  config_err_t ret;

  if ((ret = _get_pef_action_global_control (state_data, &gc)) != CONFIG_ERR_SUCCESS)
    return ret;

  gc.enable_reset_action = same (kv->value_input, "yes");

  return _set_pef_action_global_control (state_data, &gc);
}

static config_err_t
enable_power_cycle_action_checkout (const char *section_name,
				    struct config_keyvalue *kv,
                                    void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  struct pef_action_global_control gc;
  config_err_t ret;

  if ((ret = _get_pef_action_global_control (state_data, &gc)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (config_section_update_keyvalue_output(kv, gc.enable_power_cycle_action ? "Yes" : "No") < 0)
    return CONFIG_ERR_FATAL_ERROR;

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
enable_power_cycle_action_commit (const char *section_name,
				  const struct config_keyvalue *kv,
                                  void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  struct pef_action_global_control gc;
  config_err_t ret;

  if ((ret = _get_pef_action_global_control (state_data, &gc)) != CONFIG_ERR_SUCCESS)
    return ret;

  gc.enable_power_cycle_action = same (kv->value_input, "yes");

  return _set_pef_action_global_control (state_data, &gc);
}

static config_err_t
enable_oem_action_checkout (const char *section_name,
			    struct config_keyvalue *kv,
                            void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  struct pef_action_global_control gc;
  config_err_t ret;

  if ((ret = _get_pef_action_global_control (state_data, &gc)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (config_section_update_keyvalue_output(kv, gc.enable_oem_action ? "Yes" : "No") < 0)
    return CONFIG_ERR_FATAL_ERROR;

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
enable_oem_action_commit (const char *section_name,
			  const struct config_keyvalue *kv,
                          void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  struct pef_action_global_control gc;
  config_err_t ret;

  if ((ret = _get_pef_action_global_control (state_data, &gc)) != CONFIG_ERR_SUCCESS)
    return ret;

  gc.enable_oem_action = same (kv->value_input, "yes");

  return _set_pef_action_global_control (state_data, &gc);
}

static config_err_t
enable_diagnostic_interrupt_checkout (const char *section_name,
                                      struct config_keyvalue *kv,
                                      void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  struct pef_action_global_control gc;
  config_err_t ret;

  if ((ret = _get_pef_action_global_control (state_data, &gc)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (config_section_update_keyvalue_output(kv, gc.enable_diagnostic_interrupt ? "Yes" : "No") < 0)
    return CONFIG_ERR_FATAL_ERROR;

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
enable_diagnostic_interrupt_commit (const char *section_name,
				    const struct config_keyvalue *kv,
                                    void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  struct pef_action_global_control gc;
  config_err_t ret;

  if ((ret = _get_pef_action_global_control (state_data, &gc)) != CONFIG_ERR_SUCCESS)
    return ret;

  gc.enable_diagnostic_interrupt = same (kv->value_input, "yes");

  return _set_pef_action_global_control (state_data, &gc);
}

static config_err_t
pef_startup_delay_checkout (const char *section_name,
			    struct config_keyvalue *kv,
                            void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val = 0;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;

  if (!(obj_cmd_rs = Fiid_obj_create(tmpl_cmd_get_pef_configuration_parameters_pef_startup_delay_rs)))
    goto cleanup;

  if (ipmi_cmd_get_pef_configuration_parameters_pef_startup_delay (state_data->dev,
                                                                   IPMI_GET_PEF_PARAMETER,
                                                                   SET_SELECTOR,
                                                                   BLOCK_SELECTOR,
                                                                   obj_cmd_rs) < 0)
    {
      if (state_data->prog_data->args->common.flags & IPMI_FLAGS_DEBUG_DUMP)
        fprintf(stderr,
                "ipmi_cmd_get_pef_configuration_parameters_pef_startup_delay: %s\n",
                ipmi_device_strerror(ipmi_device_errnum(state_data->dev)));
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  if (Fiid_obj_get (obj_cmd_rs, "pef_startup_delay", &val) < 0)
    goto cleanup;

  if (config_section_update_keyvalue_output_int(kv, val) < 0)
    return CONFIG_ERR_FATAL_ERROR;

  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  Fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

static config_err_t
pef_startup_delay_commit (const char *section_name,
			  const struct config_keyvalue *kv,
                          void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  fiid_obj_t obj_cmd_rs = NULL;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  
  if (!(obj_cmd_rs = Fiid_obj_create(tmpl_cmd_set_pef_configuration_parameters_rs)))
    goto cleanup;
  
  if (ipmi_cmd_set_pef_configuration_parameters_pef_startup_delay (state_data->dev,
                                                                   atoi (kv->value_input),
                                                                   obj_cmd_rs) < 0)
    {
      if (state_data->prog_data->args->common.flags & IPMI_FLAGS_DEBUG_DUMP)
        fprintf(stderr,
                "ipmi_cmd_set_pef_configuration_parameters_pef_startup_delay: %s\n",
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
pef_alert_startup_delay_checkout (const char *section_name,
				  struct config_keyvalue *kv,
                                  void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val = 0;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;

  if (!(obj_cmd_rs = Fiid_obj_create(tmpl_cmd_get_pef_configuration_parameters_pef_alert_startup_delay_rs)))
    goto cleanup;

  if (ipmi_cmd_get_pef_configuration_parameters_pef_alert_startup_delay (state_data->dev,
                                                                         IPMI_GET_PEF_PARAMETER,
                                                                         SET_SELECTOR,
                                                                         BLOCK_SELECTOR,
                                                                         obj_cmd_rs) < 0)
    {
      if (state_data->prog_data->args->common.flags & IPMI_FLAGS_DEBUG_DUMP)
        fprintf(stderr,
                "ipmi_cmd_get_pef_configuration_parameters_pef_alert_startup_delay: %s\n",
                ipmi_device_strerror(ipmi_device_errnum(state_data->dev)));
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  if (Fiid_obj_get (obj_cmd_rs, "pef_alert_startup_delay", &val) < 0)
    goto cleanup;

  if (config_section_update_keyvalue_output_int(kv, val) < 0)
    return CONFIG_ERR_FATAL_ERROR;

  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  Fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

static config_err_t
pef_alert_startup_delay_commit (const char *section_name,
				const struct config_keyvalue *kv,
                                void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  fiid_obj_t obj_cmd_rs = NULL;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;

  if (!(obj_cmd_rs = Fiid_obj_create(tmpl_cmd_set_pef_configuration_parameters_rs)))
    goto cleanup;

  if (ipmi_cmd_set_pef_configuration_parameters_pef_alert_startup_delay (state_data->dev,
                                                                         atoi (kv->value_input),
                                                                         obj_cmd_rs) < 0)
    {
      if (state_data->prog_data->args->common.flags & IPMI_FLAGS_DEBUG_DUMP)
        fprintf(stderr,
                "ipmi_cmd_set_pef_configuration_parameters_pef_alert_startup_delay: %s\n",
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
bmc_config_pef_conf_section_get (bmc_config_state_data_t *state_data)
{
  struct config_section *pef_section;

  if (!(pef_section = config_section_create ("PEF_Conf",
                                             NULL,
                                             NULL,
                                             CONFIG_DO_NOT_CHECKOUT)))
    goto cleanup;

  if (config_section_add_key (pef_section,
                              "Enable_PEF",
                              "Possible values: Yes/No",
                              CONFIG_DO_NOT_CHECKOUT,
                              enable_pef_checkout,
                              enable_pef_commit,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (pef_section,
                              "Enable_PEF_Event_Messages",
                              "Possible values: Yes/No",
                              CONFIG_DO_NOT_CHECKOUT,
                              enable_pef_event_messages_checkout,
                              enable_pef_event_messages_commit,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (pef_section,
                              "Enable_PEF_Startup_Delay",
                              "Possible values: Yes/No",
                              CONFIG_DO_NOT_CHECKOUT,
                              enable_pef_startup_delay_checkout,
                              enable_pef_startup_delay_commit,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (pef_section,
                              "Enable_PEF_Alert_Startup_Delay",
                              "Possible values: Yes/No",
                              CONFIG_DO_NOT_CHECKOUT,
                              enable_pef_alert_startup_delay_checkout,
                              enable_pef_alert_startup_delay_commit,
                              config_yes_no_validate) < 0)
    goto cleanup;
  
  if (config_section_add_key (pef_section,
                              "Enable_Alert_Action",
                              "Possible values: Yes/No",
                              CONFIG_DO_NOT_CHECKOUT,
                              enable_alert_action_checkout,
                              enable_alert_action_commit,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (pef_section,
                              "Enable_Power_Down_Action",
                              "Possible values: Yes/No",
                              CONFIG_DO_NOT_CHECKOUT,
                              enable_power_down_action_checkout,
                              enable_power_down_action_commit,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (pef_section,
                              "Enable_Reset_Action",
                              "Possible values: Yes/No",
                              CONFIG_DO_NOT_CHECKOUT,
                              enable_reset_action_checkout,
                              enable_reset_action_commit,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (pef_section,
                              "Enable_Power_Cycle_Action",
                              "Possible values: Yes/No",
                              CONFIG_DO_NOT_CHECKOUT,
                              enable_power_cycle_action_checkout,
                              enable_power_cycle_action_commit,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (pef_section,
                              "Enable_OEM_Action",
                              "Possible values: Yes/No",
                              CONFIG_DO_NOT_CHECKOUT,
                              enable_oem_action_checkout,
                              enable_oem_action_commit,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (pef_section,
                              "Enable_Diagnostic_Interrupt",
                              "Possible values: Yes/No",
                              CONFIG_DO_NOT_CHECKOUT,
                              enable_diagnostic_interrupt_checkout,
                              enable_diagnostic_interrupt_commit,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (pef_section,
                              "PEF_Startup_Delay",
                              "Give value in seconds",
                              CONFIG_DO_NOT_CHECKOUT,
                              pef_startup_delay_checkout,
                              pef_startup_delay_commit,
                              config_number_range_one_byte) < 0)
    goto cleanup;

  if (config_section_add_key (pef_section,
                              "PEF_Alert_Startup_Delay",
                              "Give value in seconds",
                              CONFIG_DO_NOT_CHECKOUT,
                              pef_alert_startup_delay_checkout,
                              pef_alert_startup_delay_commit,
                              config_number_range_one_byte) < 0)
    goto cleanup;

  return pef_section;

 cleanup:
  if (pef_section)
    config_section_destroy(pef_section);
  return NULL;
}

