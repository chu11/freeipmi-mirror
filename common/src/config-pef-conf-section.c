#if HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <errno.h>
#include <assert.h>

#include "config-pef-conf-section.h"
#include "config-fiid.h"
#include "config-section.h"

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
_get_pef_control (ipmi_device_t dev,
                  struct config_arguments *cmd_args,
                  struct pef_control *pc)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val = 0;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;

  assert(dev);
  assert(cmd_args);
  assert(pc);

  if (!(obj_cmd_rs = Fiid_obj_create(tmpl_cmd_get_pef_configuration_parameters_pef_control_rs)))
    goto cleanup;

  if (ipmi_cmd_get_pef_configuration_parameters_pef_control (dev,
                                                             IPMI_GET_PEF_PARAMETER,
                                                             SET_SELECTOR,
                                                             BLOCK_SELECTOR,
                                                             obj_cmd_rs) < 0)
    {
      if (cmd_args->common.flags & IPMI_FLAGS_DEBUG_DUMP)
        fprintf(stderr,
                "ipmi_cmd_get_pef_configuration_parameters_pef_control: %s\n",
                ipmi_device_strerror(ipmi_device_errnum(dev)));
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
_set_pef_control (ipmi_device_t dev,
                  struct config_arguments *cmd_args,
                  struct pef_control *pc)
{
  fiid_obj_t obj_cmd_rs = NULL;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;

  assert(dev);
  assert(cmd_args);
  assert(pc);

  if (!(obj_cmd_rs = Fiid_obj_create(tmpl_cmd_set_pef_configuration_parameters_rs)))
    goto cleanup;

  if (ipmi_cmd_set_pef_configuration_parameters_pef_control (dev,
                                                             pc->enable_pef,
                                                             pc->enable_pef_event_messages,
                                                             pc->enable_pef_startup_delay,
                                                             pc->enable_pef_alert_startup_delay,
                                                             obj_cmd_rs) < 0)
    {
      if (cmd_args->common.flags & IPMI_FLAGS_DEBUG_DUMP)
        fprintf(stderr,
                "ipmi_cmd_set_pef_configuration_parameters_pef_control: %s\n",
                ipmi_device_strerror(ipmi_device_errnum(dev)));
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  Fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

config_err_t
enable_pef_checkout (struct config_keyvalue *kv,
                     ipmi_device_t dev,
                     struct config_arguments *cmd_args)
{
  struct pef_control pc;
  config_err_t ret;
  
  if ((ret = _get_pef_control (dev, cmd_args, &pc)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (config_section_update_keyvalue_output(kv, pc.enable_pef ? "Yes" : "No") < 0)
    return CONFIG_ERR_FATAL_ERROR;

  return CONFIG_ERR_SUCCESS;
}

config_err_t
enable_pef_commit (const struct config_keyvalue *kv,
                   ipmi_device_t dev,
                   struct config_arguments *cmd_args)
{
  struct pef_control pc;
  config_err_t ret;

  if ((ret = _get_pef_control (dev, cmd_args, &pc)) != CONFIG_ERR_SUCCESS)
    return ret;

  pc.enable_pef = same (kv->value_input, "yes");

  return _set_pef_control (dev, cmd_args, &pc);
}

config_err_t
enable_pef_event_messages_checkout (struct config_keyvalue *kv,
                                    ipmi_device_t dev,
                                    struct config_arguments *cmd_args)
{
  struct pef_control pc;
  config_err_t ret;
  
  if ((ret = _get_pef_control (dev, cmd_args, &pc)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (config_section_update_keyvalue_output(kv, pc.enable_pef_event_messages ? "Yes" : "No") < 0)
    return CONFIG_ERR_FATAL_ERROR;

  return CONFIG_ERR_SUCCESS;
}

config_err_t
enable_pef_event_messages_commit (const struct config_keyvalue *kv,
                                  ipmi_device_t dev,
                                  struct config_arguments *cmd_args)
{
  struct pef_control pc;
  config_err_t ret;

  if ((ret = _get_pef_control (dev, cmd_args, &pc)) != CONFIG_ERR_SUCCESS)
    return ret;

  pc.enable_pef_event_messages = same (kv->value_input, "yes");

  return _set_pef_control (dev, cmd_args, &pc);
}

config_err_t
enable_pef_startup_delay_checkout (struct config_keyvalue *kv,
                                   ipmi_device_t dev,
                                   struct config_arguments *cmd_args)
{
  struct pef_control pc;
  config_err_t ret;
  
  if ((ret = _get_pef_control (dev, cmd_args, &pc)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (config_section_update_keyvalue_output(kv, pc.enable_pef_startup_delay ? "Yes" : "No") < 0)
    return CONFIG_ERR_FATAL_ERROR;

  return CONFIG_ERR_SUCCESS;
}

config_err_t
enable_pef_startup_delay_commit (const struct config_keyvalue *kv,
                                 ipmi_device_t dev,
                                 struct config_arguments *cmd_args)
{
  struct pef_control pc;
  config_err_t ret;

  if ((ret = _get_pef_control (dev, cmd_args, &pc)) != CONFIG_ERR_SUCCESS)
    return ret;

  pc.enable_pef_startup_delay = same (kv->value_input, "yes");

  return _set_pef_control (dev, cmd_args, &pc);
}

config_err_t
enable_pef_alert_startup_delay_checkout (struct config_keyvalue *kv,
                                         ipmi_device_t dev,
                                         struct config_arguments *cmd_args)
{
  struct pef_control pc;
  config_err_t ret;
  
  if ((ret = _get_pef_control (dev, cmd_args, &pc)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (config_section_update_keyvalue_output(kv, pc.enable_pef_alert_startup_delay ? "Yes" : "No") < 0)
    return CONFIG_ERR_FATAL_ERROR;

  return CONFIG_ERR_SUCCESS;
}

config_err_t
enable_pef_alert_startup_delay_commit (const struct config_keyvalue *kv,
                                       ipmi_device_t dev,
                                       struct config_arguments *cmd_args)
{
  struct pef_control pc;
  config_err_t ret;

  if ((ret = _get_pef_control (dev, cmd_args, &pc)) != CONFIG_ERR_SUCCESS)
    return ret;

  pc.enable_pef_alert_startup_delay = same (kv->value_input, "yes");

  return _set_pef_control (dev, cmd_args, &pc);
}

static config_err_t
_get_pef_action_global_control (ipmi_device_t dev,
                                struct config_arguments *cmd_args,
                                struct pef_action_global_control *gc)
                               
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val = 0;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;

  assert(dev);
  assert(cmd_args);
  assert(gc);

  if (!(obj_cmd_rs = Fiid_obj_create(tmpl_cmd_get_pef_configuration_parameters_pef_action_global_control_rs)))
    goto cleanup;

  if (ipmi_cmd_get_pef_configuration_parameters_pef_action_global_control (dev,
                                                                           IPMI_GET_PEF_PARAMETER,
                                                                           SET_SELECTOR,
                                                                           BLOCK_SELECTOR,
                                                                           obj_cmd_rs) < 0)
    {
      if (cmd_args->common.flags & IPMI_FLAGS_DEBUG_DUMP)
        fprintf(stderr,
                "ipmi_cmd_get_pef_configuration_parameters_pef_action_global_control: %s\n",
                ipmi_device_strerror(ipmi_device_errnum(dev)));
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
_set_pef_action_global_control (ipmi_device_t dev,
                                struct config_arguments *cmd_args,
                                struct pef_action_global_control *gc)
{
  fiid_obj_t obj_cmd_rs = NULL;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;

  assert(dev);
  assert(cmd_args);
  assert(gc);

  if (!(obj_cmd_rs = Fiid_obj_create(tmpl_cmd_set_pef_configuration_parameters_rs)))
    goto cleanup;

  if (ipmi_cmd_set_pef_configuration_parameters_pef_action_global_control (dev,
                                                                           gc->enable_alert_action,
                                                                           gc->enable_power_down_action,
                                                                           gc->enable_reset_action,
                                                                           gc->enable_power_cycle_action,
                                                                           gc->enable_oem_action,
                                                                           gc->enable_diagnostic_interrupt,
                                                                           obj_cmd_rs) < 0)
    {
      if (cmd_args->common.flags & IPMI_FLAGS_DEBUG_DUMP)
        fprintf(stderr,
                "ipmi_cmd_set_pef_configuration_parameters_pef_action_global_control: %s\n",
                ipmi_device_strerror(ipmi_device_errnum(dev)));
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  Fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

config_err_t
enable_alert_action_checkout (struct config_keyvalue *kv,
                              ipmi_device_t dev,
                              struct config_arguments *cmd_args)
{
  struct pef_action_global_control gc;
  config_err_t ret;
  
  if ((ret = _get_pef_action_global_control (dev, cmd_args, &gc)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (config_section_update_keyvalue_output(kv, gc.enable_alert_action ? "Yes" : "No") < 0)
    return CONFIG_ERR_FATAL_ERROR;

  return CONFIG_ERR_SUCCESS;
}

config_err_t
enable_alert_action_commit (const struct config_keyvalue *kv,
                            ipmi_device_t dev,
                            struct config_arguments *cmd_args)
{
  struct pef_action_global_control gc;
  config_err_t ret;

  if ((ret = _get_pef_action_global_control (dev, cmd_args, &gc)) != CONFIG_ERR_SUCCESS)
    return ret;

  gc.enable_alert_action = same (kv->value_input, "yes");

  return _set_pef_action_global_control (dev, cmd_args, &gc);
}

config_err_t
enable_power_down_action_checkout (struct config_keyvalue *kv,
                                   ipmi_device_t dev,
                                   struct config_arguments *cmd_args)
{
  struct pef_action_global_control gc;
  config_err_t ret;

  if ((ret = _get_pef_action_global_control (dev, cmd_args, &gc)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (config_section_update_keyvalue_output(kv, gc.enable_power_down_action ? "Yes" : "No") < 0)
    return CONFIG_ERR_FATAL_ERROR;

  return CONFIG_ERR_SUCCESS;
}

config_err_t
enable_power_down_action_commit (const struct config_keyvalue *kv,
                                 ipmi_device_t dev,
                                 struct config_arguments *cmd_args)
{
  struct pef_action_global_control gc;
  config_err_t ret;

  if ((ret = _get_pef_action_global_control (dev, cmd_args, &gc)) != CONFIG_ERR_SUCCESS)
    return ret;

  gc.enable_power_down_action = same (kv->value_input, "yes");

  return _set_pef_action_global_control (dev, cmd_args, &gc);
}

config_err_t
enable_reset_action_checkout (struct config_keyvalue *kv,
                              ipmi_device_t dev,
                              struct config_arguments *cmd_args)
{
  struct pef_action_global_control gc;
  config_err_t ret;

  if ((ret = _get_pef_action_global_control (dev, cmd_args, &gc)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (config_section_update_keyvalue_output(kv, gc.enable_reset_action ? "Yes" : "No") < 0)
    return CONFIG_ERR_FATAL_ERROR;

  return CONFIG_ERR_SUCCESS;
}

config_err_t
enable_reset_action_commit (const struct config_keyvalue *kv,
                            ipmi_device_t dev,
                            struct config_arguments *cmd_args)
{
  struct pef_action_global_control gc;
  config_err_t ret;

  if ((ret = _get_pef_action_global_control (dev, cmd_args, &gc)) != CONFIG_ERR_SUCCESS)
    return ret;

  gc.enable_reset_action = same (kv->value_input, "yes");

  return _set_pef_action_global_control (dev, cmd_args, &gc);
}

config_err_t
enable_power_cycle_action_checkout (struct config_keyvalue *kv,
                                    ipmi_device_t dev,
                                    struct config_arguments *cmd_args)
{
  struct pef_action_global_control gc;
  config_err_t ret;

  if ((ret = _get_pef_action_global_control (dev, cmd_args, &gc)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (config_section_update_keyvalue_output(kv, gc.enable_power_cycle_action ? "Yes" : "No") < 0)
    return CONFIG_ERR_FATAL_ERROR;

  return CONFIG_ERR_SUCCESS;
}

config_err_t
enable_power_cycle_action_commit (const struct config_keyvalue *kv,
                                  ipmi_device_t dev,
                                  struct config_arguments *cmd_args)
{
  struct pef_action_global_control gc;
  config_err_t ret;

  if ((ret = _get_pef_action_global_control (dev, cmd_args, &gc)) != CONFIG_ERR_SUCCESS)
    return ret;

  gc.enable_power_cycle_action = same (kv->value_input, "yes");

  return _set_pef_action_global_control (dev, cmd_args, &gc);
}

config_err_t
enable_oem_action_checkout (struct config_keyvalue *kv,
                            ipmi_device_t dev,
                            struct config_arguments *cmd_args)
{
  struct pef_action_global_control gc;
  config_err_t ret;

  if ((ret = _get_pef_action_global_control (dev, cmd_args, &gc)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (config_section_update_keyvalue_output(kv, gc.enable_oem_action ? "Yes" : "No") < 0)
    return CONFIG_ERR_FATAL_ERROR;

  return CONFIG_ERR_SUCCESS;
}

config_err_t
enable_oem_action_commit (const struct config_keyvalue *kv,
                          ipmi_device_t dev,
                          struct config_arguments *cmd_args)
{
  struct pef_action_global_control gc;
  config_err_t ret;

  if ((ret = _get_pef_action_global_control (dev, cmd_args, &gc)) != CONFIG_ERR_SUCCESS)
    return ret;

  gc.enable_oem_action = same (kv->value_input, "yes");

  return _set_pef_action_global_control (dev, cmd_args, &gc);
}

config_err_t
enable_diagnostic_interrupt_checkout (struct config_keyvalue *kv,
                                      ipmi_device_t dev,
                                      struct config_arguments *cmd_args)
{
  struct pef_action_global_control gc;
  config_err_t ret;

  if ((ret = _get_pef_action_global_control (dev, cmd_args, &gc)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (config_section_update_keyvalue_output(kv, gc.enable_diagnostic_interrupt ? "Yes" : "No") < 0)
    return CONFIG_ERR_FATAL_ERROR;

  return CONFIG_ERR_SUCCESS;
}

config_err_t
enable_diagnostic_interrupt_commit (const struct config_keyvalue *kv,
                                    ipmi_device_t dev,
                                    struct config_arguments *cmd_args)
{
  struct pef_action_global_control gc;
  config_err_t ret;

  if ((ret = _get_pef_action_global_control (dev, cmd_args, &gc)) != CONFIG_ERR_SUCCESS)
    return ret;

  gc.enable_diagnostic_interrupt = same (kv->value_input, "yes");

  return _set_pef_action_global_control (dev, cmd_args, &gc);
}

config_err_t
pef_startup_delay_checkout (struct config_keyvalue *kv,
                            ipmi_device_t dev,
                            struct config_arguments *cmd_args)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val = 0;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;

  assert(kv);
  assert(dev);
  assert(cmd_args);

  if (!(obj_cmd_rs = Fiid_obj_create(tmpl_cmd_get_pef_configuration_parameters_pef_startup_delay_rs)))
    goto cleanup;

  if (ipmi_cmd_get_pef_configuration_parameters_pef_startup_delay (dev,
                                                                   IPMI_GET_PEF_PARAMETER,
                                                                   SET_SELECTOR,
                                                                   BLOCK_SELECTOR,
                                                                   obj_cmd_rs) < 0)
    {
      if (cmd_args->common.flags & IPMI_FLAGS_DEBUG_DUMP)
        fprintf(stderr,
                "ipmi_cmd_get_pef_configuration_parameters_pef_startup_delay: %s\n",
                ipmi_device_strerror(ipmi_device_errnum(dev)));
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

config_err_t
pef_startup_delay_commit (const struct config_keyvalue *kv,
                          ipmi_device_t dev,
                          struct config_arguments *cmd_args)
{
  fiid_obj_t obj_cmd_rs = NULL;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  
  assert(kv);
  assert(dev);
  assert(cmd_args);

  if (!(obj_cmd_rs = Fiid_obj_create(tmpl_cmd_set_pef_configuration_parameters_rs)))
    goto cleanup;
  
  if (ipmi_cmd_set_pef_configuration_parameters_pef_startup_delay (dev,
                                                                   atoi (kv->value_input),
                                                                   obj_cmd_rs) < 0)
    {
      if (cmd_args->common.flags & IPMI_FLAGS_DEBUG_DUMP)
        fprintf(stderr,
                "ipmi_cmd_set_pef_configuration_parameters_pef_startup_delay: %s\n",
                ipmi_device_strerror(ipmi_device_errnum(dev)));
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }
  
  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  Fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

config_err_t
pef_alert_startup_delay_checkout (struct config_keyvalue *kv,
                                  ipmi_device_t dev,
                                  struct config_arguments *cmd_args)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val = 0;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;

  assert(kv);
  assert(dev);
  assert(cmd_args);

  if (!(obj_cmd_rs = Fiid_obj_create(tmpl_cmd_get_pef_configuration_parameters_pef_alert_startup_delay_rs)))
    goto cleanup;

  if (ipmi_cmd_get_pef_configuration_parameters_pef_alert_startup_delay (dev,
                                                                         IPMI_GET_PEF_PARAMETER,
                                                                         SET_SELECTOR,
                                                                         BLOCK_SELECTOR,
                                                                         obj_cmd_rs) < 0)
    {
      if (cmd_args->common.flags & IPMI_FLAGS_DEBUG_DUMP)
        fprintf(stderr,
                "ipmi_cmd_get_pef_configuration_parameters_pef_alert_startup_delay: %s\n",
                ipmi_device_strerror(ipmi_device_errnum(dev)));
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

config_err_t
pef_alert_startup_delay_commit (const struct config_keyvalue *kv,
                                ipmi_device_t dev,
                                struct config_arguments *cmd_args)
{
  fiid_obj_t obj_cmd_rs = NULL;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;

  assert(kv);
  assert(dev);
  assert(cmd_args);

  if (!(obj_cmd_rs = Fiid_obj_create(tmpl_cmd_set_pef_configuration_parameters_rs)))
    goto cleanup;

  if (ipmi_cmd_set_pef_configuration_parameters_pef_alert_startup_delay (dev,
                                                                         atoi (kv->value_input),
                                                                         obj_cmd_rs) < 0)
    {
      if (cmd_args->common.flags & IPMI_FLAGS_DEBUG_DUMP)
        fprintf(stderr,
                "ipmi_cmd_set_pef_configuration_parameters_pef_alert_startup_delay: %s\n",
                ipmi_device_strerror(ipmi_device_errnum(dev)));
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }
  
  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  Fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}


