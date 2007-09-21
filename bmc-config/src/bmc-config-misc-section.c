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

#define KEY_NAME_POWER_RESTORE_POLICY "Power_Restore_Policy"

static config_err_t
_get_bmc_power_restore_policy (bmc_config_state_data_t *state_data,
                               int debug,
                               uint8_t *power_restore_policy)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;

  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_get_chassis_status_rs)))
    goto cleanup;

  if (ipmi_cmd_get_chassis_status (state_data->dev, obj_cmd_rs) < 0)
    {
      if (debug)
        fprintf(stderr,
                "ipmi_cmd_get_chassis_status: %s\n",
                ipmi_device_strerror(ipmi_device_errnum(state_data->dev)));
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  if (fiid_obj_get (obj_cmd_rs, "current_power_state.power_restore_policy", &val) < 0)
    goto cleanup;
  *power_restore_policy = val;

  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}


static config_err_t
_set_bmc_power_restore_policy (bmc_config_state_data_t *state_data,
                               int debug,
                               uint8_t power_restore_policy)
{
  fiid_obj_t obj_cmd_rs = NULL;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_set_power_restore_policy_rs)))
    goto cleanup;
  
  if (ipmi_cmd_set_power_restore_policy (state_data->dev,
                                         power_restore_policy,
                                         obj_cmd_rs) < 0)
    {
      if (debug)
        fprintf(stderr,
                "ipmi_cmd_set_power_restore_policy: %s\n",
                ipmi_device_strerror(ipmi_device_errnum(state_data->dev)));
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

static config_err_t
_misc_section_checkout(const char *section_name,
                       struct config_keyvalue *keyvalues,
                       int debug,
                       void *arg)
{
  bmc_config_state_data_t *state_data;
  struct config_keyvalue *kv;
  config_err_t ret;
  uint8_t power_restore_policy;

  assert(section_name);
  assert(keyvalues);
  assert(arg);

  state_data = (bmc_config_state_data_t *)arg;
  
  kv = keyvalues;
  while (kv)
    {
      assert(!kv->value_output);

      if (!strcasecmp(kv->key->key_name, KEY_NAME_POWER_RESTORE_POLICY))
        {
          if ((ret = _get_bmc_power_restore_policy (state_data,
                                                    debug,
                                                    &power_restore_policy)) != CONFIG_ERR_SUCCESS)
            return ret;

          if (config_section_update_keyvalue(kv,
                                             NULL,
                                             power_restore_policy_string(power_restore_policy)) < 0)
            {
              if (debug)
                fprintf(stderr, "config_section_update_keyvalue error\n");
              return CONFIG_ERR_FATAL_ERROR;
            }
        }
      else
        {
          if (debug)
            fprintf(stderr,
                    "ERROR: Unknown key '%s' in '%s'\n",
                    kv->key->key_name,
                    section_name);
        }
      
      kv = kv->next;
    }

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
_misc_section_commit(const char *section_name,
                     struct config_keyvalue *keyvalues,
                     int debug,
                     void *arg)
{
  bmc_config_state_data_t *state_data;
  struct config_keyvalue *kv;
  config_err_t ret;
  uint8_t power_restore_policy;

  assert(section_name);
  assert(keyvalues);
  assert(arg);

  state_data = (bmc_config_state_data_t *)arg;
  
  kv = keyvalues;
  while (kv)
    {
      assert(kv->value_input);

      if (!strcasecmp(kv->key->key_name, KEY_NAME_POWER_RESTORE_POLICY))
        {
          power_restore_policy = power_restore_policy_number (kv->value_input);

          if ((ret = _set_bmc_power_restore_policy (state_data,
                                                    debug,
                                                    power_restore_policy)) != CONFIG_ERR_SUCCESS)
            return ret;
        }
      else
        {
          if (debug)
            fprintf(stderr,
                    "ERROR: Unknown key '%s' in '%s'\n",
                    kv->key->key_name,
                    section_name);
        }
      
      kv = kv->next;
    }

  return CONFIG_ERR_SUCCESS;
}

struct config_section *
bmc_config_misc_section_get (bmc_config_state_data_t *state_data)
{
  struct config_section *misc_section = NULL;
  char *section_comment = 
    "The following miscellaneous configuration options are optionally "
    "implemented by the vendor.  They may not be available your system and "
    "may not be visible below."
    "\n"
    "The \"Power_Restore_Policy\" determines the behavior of the machine "
    "when AC power returns after a power loss.  The behavior can be set to "
    "always power on the machine (\"On_State_AC_Apply\"), power off the "
    "machine (\"Off_State_AC_Apply\"), or return the power to the state that "
    "existed before the power loss (\"Restore_State_AC_Apply\").";

  if (!(misc_section = config_section_create ("Misc",
                                              "Misc",
                                              section_comment,
                                              0,
                                              _misc_section_checkout,
                                              _misc_section_commit)))
    goto cleanup;

  if (config_section_add_key (misc_section,
                              KEY_NAME_POWER_RESTORE_POLICY,
                              "Possible values: Off_State_AC_Apply/Restore_State_AC_Apply/On_State_AC_Apply",
                              CONFIG_CHECKOUT_KEY_COMMENTED_OUT_IF_VALUE_EMPTY,
                              power_restore_policy_number_validate) < 0)
    goto cleanup;

  return misc_section;

 cleanup:
  if (misc_section)
    config_section_destroy(misc_section);
  return NULL;
}
