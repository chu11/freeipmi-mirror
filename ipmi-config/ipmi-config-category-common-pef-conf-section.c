/*
 * Copyright (C) 2003-2014 FreeIPMI Core Team
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 * 
 */

#if HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <errno.h>
#include <assert.h>
#include <freeipmi/freeipmi.h>

#include "ipmi-config-category-common-pef-conf-section.h"
#include "ipmi-config-section.h"
#include "ipmi-config-utils.h"

#include "freeipmi-portability.h"
#include "pstdout.h"

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

static ipmi_config_err_t
_get_pef_control (ipmi_config_state_data_t *state_data,
                  struct pef_control *pc)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val = 0;
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;

  assert (state_data);
  assert (pc);

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_get_pef_configuration_parameters_pef_control_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s",
                       strerror (errno));
      goto cleanup;
    }

  if (ipmi_cmd_get_pef_configuration_parameters_pef_control (state_data->ipmi_ctx,
                                                             IPMI_GET_PEF_PARAMETER,
                                                             IPMI_PEF_CONFIGURATION_PARAMETERS_NO_SET_SELECTOR,
                                                             IPMI_PEF_CONFIGURATION_PARAMETERS_NO_BLOCK_SELECTOR,
                                                             obj_cmd_rs) < 0)
    {
      ipmi_config_err_t ret;

      if (ipmi_config_param_errnum_is_non_fatal (state_data,
                                                 obj_cmd_rs,
                                                 &ret))
        rv = ret;

      if (rv == IPMI_CONFIG_ERR_FATAL_ERROR
	  || state_data->prog_data->args->common_args.debug)
        pstdout_fprintf (state_data->pstate,
                         stderr,
                         "ipmi_cmd_get_pef_configuration_parameters_pef_control: %s\n",
                         ipmi_ctx_errormsg (state_data->ipmi_ctx));

      goto cleanup;
    }

  if (FIID_OBJ_GET (obj_cmd_rs, "pef", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'pef': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  pc->enable_pef = val;

  if (FIID_OBJ_GET (obj_cmd_rs, "pef_event_messages", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'pef_event_messages': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  pc->enable_pef_event_messages = val;

  if (FIID_OBJ_GET (obj_cmd_rs, "pef_startup_delay", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'pef_startup_delay': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  pc->enable_pef_startup_delay = val;

  if (FIID_OBJ_GET (obj_cmd_rs, "pef_alert_startup_delay", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'pef_alert_startup_delay': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  pc->enable_pef_alert_startup_delay = val;

  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static ipmi_config_err_t
_set_pef_control (ipmi_config_state_data_t *state_data,
                  struct pef_control *pc)
{
  fiid_obj_t obj_cmd_rs = NULL;
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;

  assert (state_data);
  assert (pc);

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_set_pef_configuration_parameters_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s",
                       strerror (errno));
      goto cleanup;
    }

  if (ipmi_cmd_set_pef_configuration_parameters_pef_control (state_data->ipmi_ctx,
                                                             pc->enable_pef,
                                                             pc->enable_pef_event_messages,
                                                             pc->enable_pef_startup_delay,
                                                             pc->enable_pef_alert_startup_delay,
                                                             obj_cmd_rs) < 0)
    {
      ipmi_config_err_t ret;

      if (ipmi_config_param_errnum_is_non_fatal (state_data,
                                                 obj_cmd_rs,
                                                 &ret))
        rv = ret;

      if (rv == IPMI_CONFIG_ERR_FATAL_ERROR
	  || state_data->prog_data->args->common_args.debug)
        pstdout_fprintf (state_data->pstate,
                         stderr,
                         "ipmi_cmd_set_pef_configuration_parameters_pef_control: %s\n",
                         ipmi_ctx_errormsg (state_data->ipmi_ctx));

      goto cleanup;
    }

  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

ipmi_config_err_t
enable_pef_checkout (ipmi_config_state_data_t *state_data,
		     const char *section_name,
                     struct ipmi_config_keyvalue *kv)
{
  struct pef_control pc;
  ipmi_config_err_t ret;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if ((ret = _get_pef_control (state_data, &pc)) != IPMI_CONFIG_ERR_SUCCESS)
    return (ret);

  if (ipmi_config_section_update_keyvalue_output (state_data,
                                                  kv,
                                                  pc.enable_pef ? "Yes" : "No") < 0)
    return (IPMI_CONFIG_ERR_FATAL_ERROR);

  return (IPMI_CONFIG_ERR_SUCCESS);
}

ipmi_config_err_t
enable_pef_commit (ipmi_config_state_data_t *state_data,
		   const char *section_name,
                   const struct ipmi_config_keyvalue *kv)
{
  struct pef_control pc;
  ipmi_config_err_t ret;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if ((ret = _get_pef_control (state_data, &pc)) != IPMI_CONFIG_ERR_SUCCESS)
    return (ret);

  pc.enable_pef = same (kv->value_input, "yes");

  return (_set_pef_control (state_data, &pc));
}

ipmi_config_err_t
enable_pef_event_messages_checkout (ipmi_config_state_data_t *state_data,
				    const char *section_name,
                                    struct ipmi_config_keyvalue *kv)
{
  struct pef_control pc;
  ipmi_config_err_t ret;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if ((ret = _get_pef_control (state_data, &pc)) != IPMI_CONFIG_ERR_SUCCESS)
    return (ret);

  if (ipmi_config_section_update_keyvalue_output (state_data,
                                                  kv,
                                                  pc.enable_pef_event_messages ? "Yes" : "No") < 0)
    return (IPMI_CONFIG_ERR_FATAL_ERROR);

  return (IPMI_CONFIG_ERR_SUCCESS);
}

ipmi_config_err_t
enable_pef_event_messages_commit (ipmi_config_state_data_t *state_data,
				  const char *section_name,
                                  const struct ipmi_config_keyvalue *kv)
{
  struct pef_control pc;
  ipmi_config_err_t ret;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if ((ret = _get_pef_control (state_data, &pc)) != IPMI_CONFIG_ERR_SUCCESS)
    return (ret);

  pc.enable_pef_event_messages = same (kv->value_input, "yes");

  return (_set_pef_control (state_data, &pc));
}

ipmi_config_err_t
enable_pef_startup_delay_checkout (ipmi_config_state_data_t *state_data,
				   const char *section_name,
                                   struct ipmi_config_keyvalue *kv)
{
  struct pef_control pc;
  ipmi_config_err_t ret;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if ((ret = _get_pef_control (state_data, &pc)) != IPMI_CONFIG_ERR_SUCCESS)
    return (ret);

  if (ipmi_config_section_update_keyvalue_output (state_data,
                                                  kv,
                                                  pc.enable_pef_startup_delay ? "Yes" : "No") < 0)
    return (IPMI_CONFIG_ERR_FATAL_ERROR);

  return (IPMI_CONFIG_ERR_SUCCESS);
}

ipmi_config_err_t
enable_pef_startup_delay_commit (ipmi_config_state_data_t *state_data,
				 const char *section_name,
                                 const struct ipmi_config_keyvalue *kv)
{
  struct pef_control pc;
  ipmi_config_err_t ret;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if ((ret = _get_pef_control (state_data, &pc)) != IPMI_CONFIG_ERR_SUCCESS)
    return (ret);

  pc.enable_pef_startup_delay = same (kv->value_input, "yes");

  return (_set_pef_control (state_data, &pc));
}

ipmi_config_err_t
enable_pef_alert_startup_delay_checkout (ipmi_config_state_data_t *state_data,
					 const char *section_name,
                                         struct ipmi_config_keyvalue *kv)
{
  struct pef_control pc;
  ipmi_config_err_t ret;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if ((ret = _get_pef_control (state_data, &pc)) != IPMI_CONFIG_ERR_SUCCESS)
    return (ret);

  if (ipmi_config_section_update_keyvalue_output (state_data,
                                                  kv,
                                                  pc.enable_pef_alert_startup_delay ? "Yes" : "No") < 0)
    return (IPMI_CONFIG_ERR_FATAL_ERROR);

  return (IPMI_CONFIG_ERR_SUCCESS);
}

ipmi_config_err_t
enable_pef_alert_startup_delay_commit (ipmi_config_state_data_t *state_data,
				       const char *section_name,
                                       const struct ipmi_config_keyvalue *kv)
{
  struct pef_control pc;
  ipmi_config_err_t ret;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if ((ret = _get_pef_control (state_data, &pc)) != IPMI_CONFIG_ERR_SUCCESS)
    return (ret);

  pc.enable_pef_alert_startup_delay = same (kv->value_input, "yes");

  return (_set_pef_control (state_data, &pc));
}

static ipmi_config_err_t
_get_pef_action_global_control (ipmi_config_state_data_t *state_data,
                                struct pef_action_global_control *gc)

{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val = 0;
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;

  assert (state_data);
  assert (gc);

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_get_pef_configuration_parameters_pef_action_global_control_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s",
                       strerror (errno));
      goto cleanup;
    }

  if (ipmi_cmd_get_pef_configuration_parameters_pef_action_global_control (state_data->ipmi_ctx,
                                                                           IPMI_GET_PEF_PARAMETER,
                                                                           IPMI_PEF_CONFIGURATION_PARAMETERS_NO_SET_SELECTOR,
                                                                           IPMI_PEF_CONFIGURATION_PARAMETERS_NO_BLOCK_SELECTOR,
                                                                           obj_cmd_rs) < 0)
    {
      ipmi_config_err_t ret;

      if (ipmi_config_param_errnum_is_non_fatal (state_data,
                                                 obj_cmd_rs,
                                                 &ret))
        rv = ret;

      if (rv == IPMI_CONFIG_ERR_FATAL_ERROR
	  || state_data->prog_data->args->common_args.debug)
        pstdout_fprintf (state_data->pstate,
                         stderr,
                         "ipmi_cmd_get_pef_configuration_parameters_pef_action_global_control: %s\n",
                         ipmi_ctx_errormsg (state_data->ipmi_ctx));

      goto cleanup;
    }

  if (FIID_OBJ_GET (obj_cmd_rs, "alert_action", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'alert_action': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  gc->enable_alert_action = val;

  if (FIID_OBJ_GET (obj_cmd_rs, "power_down_action", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'power_down_action': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  gc->enable_power_down_action = val;

  if (FIID_OBJ_GET (obj_cmd_rs, "reset_action", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'reset_action': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  gc->enable_reset_action = val;

  if (FIID_OBJ_GET (obj_cmd_rs, "power_cycle_action", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'power_cycle_action': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  gc->enable_power_cycle_action = val;

  if (FIID_OBJ_GET (obj_cmd_rs, "oem_action", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'oem_action': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  gc->enable_oem_action = val;

  if (FIID_OBJ_GET (obj_cmd_rs, "diagnostic_interrupt", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'diagnostic_interrupt': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  gc->enable_diagnostic_interrupt = val;

  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static ipmi_config_err_t
_set_pef_action_global_control (ipmi_config_state_data_t *state_data,
                                struct pef_action_global_control *gc)
{
  fiid_obj_t obj_cmd_rs = NULL;
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;

  assert (state_data);
  assert (gc);

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_set_pef_configuration_parameters_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s",
                       strerror (errno));
      goto cleanup;
    }

  if (ipmi_cmd_set_pef_configuration_parameters_pef_action_global_control (state_data->ipmi_ctx,
                                                                           gc->enable_alert_action,
                                                                           gc->enable_power_down_action,
                                                                           gc->enable_reset_action,
                                                                           gc->enable_power_cycle_action,
                                                                           gc->enable_oem_action,
                                                                           gc->enable_diagnostic_interrupt,
                                                                           obj_cmd_rs) < 0)
    {
      ipmi_config_err_t ret;

      if (ipmi_config_param_errnum_is_non_fatal (state_data,
                                                 obj_cmd_rs,
                                                 &ret))
        rv = ret;

      if (rv == IPMI_CONFIG_ERR_FATAL_ERROR
	  || state_data->prog_data->args->common_args.debug)
        pstdout_fprintf (state_data->pstate,
                         stderr,
                         "ipmi_cmd_set_pef_configuration_parameters_pef_action_global_control: %s\n",
                         ipmi_ctx_errormsg (state_data->ipmi_ctx));

      goto cleanup;
    }

  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

ipmi_config_err_t
enable_alert_action_checkout (ipmi_config_state_data_t *state_data,
			      const char *section_name,
                              struct ipmi_config_keyvalue *kv)
{
  struct pef_action_global_control gc;
  ipmi_config_err_t ret;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if ((ret = _get_pef_action_global_control (state_data, &gc)) != IPMI_CONFIG_ERR_SUCCESS)
    return (ret);

  if (ipmi_config_section_update_keyvalue_output (state_data,
                                                  kv,
                                                  gc.enable_alert_action ? "Yes" : "No") < 0)
    return (IPMI_CONFIG_ERR_FATAL_ERROR);

  return (IPMI_CONFIG_ERR_SUCCESS);
}

ipmi_config_err_t
enable_alert_action_commit (ipmi_config_state_data_t *state_data,
			    const char *section_name,
                            const struct ipmi_config_keyvalue *kv)
{
  struct pef_action_global_control gc;
  ipmi_config_err_t ret;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if ((ret = _get_pef_action_global_control (state_data, &gc)) != IPMI_CONFIG_ERR_SUCCESS)
    return (ret);

  gc.enable_alert_action = same (kv->value_input, "yes");

  return (_set_pef_action_global_control (state_data, &gc));
}

ipmi_config_err_t
enable_power_down_action_checkout (ipmi_config_state_data_t *state_data,
				   const char *section_name,
                                   struct ipmi_config_keyvalue *kv)
{
  struct pef_action_global_control gc;
  ipmi_config_err_t ret;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if ((ret = _get_pef_action_global_control (state_data, &gc)) != IPMI_CONFIG_ERR_SUCCESS)
    return (ret);

  if (ipmi_config_section_update_keyvalue_output (state_data,
                                                  kv,
                                                  gc.enable_power_down_action ? "Yes" : "No") < 0)
    return (IPMI_CONFIG_ERR_FATAL_ERROR);

  return (IPMI_CONFIG_ERR_SUCCESS);
}

ipmi_config_err_t
enable_power_down_action_commit (ipmi_config_state_data_t *state_data,
				 const char *section_name,
                                 const struct ipmi_config_keyvalue *kv)
{
  struct pef_action_global_control gc;
  ipmi_config_err_t ret;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if ((ret = _get_pef_action_global_control (state_data, &gc)) != IPMI_CONFIG_ERR_SUCCESS)
    return (ret);

  gc.enable_power_down_action = same (kv->value_input, "yes");

  return (_set_pef_action_global_control (state_data, &gc));
}

ipmi_config_err_t
enable_reset_action_checkout (ipmi_config_state_data_t *state_data,
			      const char *section_name,
                              struct ipmi_config_keyvalue *kv)
{
  struct pef_action_global_control gc;
  ipmi_config_err_t ret;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if ((ret = _get_pef_action_global_control (state_data, &gc)) != IPMI_CONFIG_ERR_SUCCESS)
    return (ret);

  if (ipmi_config_section_update_keyvalue_output (state_data,
                                                  kv,
                                                  gc.enable_reset_action ? "Yes" : "No") < 0)
    return (IPMI_CONFIG_ERR_FATAL_ERROR);

  return (IPMI_CONFIG_ERR_SUCCESS);
}

ipmi_config_err_t
enable_reset_action_commit (ipmi_config_state_data_t *state_data,
			    const char *section_name,
                            const struct ipmi_config_keyvalue *kv)
{
  struct pef_action_global_control gc;
  ipmi_config_err_t ret;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if ((ret = _get_pef_action_global_control (state_data, &gc)) != IPMI_CONFIG_ERR_SUCCESS)
    return (ret);

  gc.enable_reset_action = same (kv->value_input, "yes");

  return (_set_pef_action_global_control (state_data, &gc));
}

ipmi_config_err_t
enable_power_cycle_action_checkout (ipmi_config_state_data_t *state_data,
				    const char *section_name,
                                    struct ipmi_config_keyvalue *kv)
{
  struct pef_action_global_control gc;
  ipmi_config_err_t ret;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if ((ret = _get_pef_action_global_control (state_data, &gc)) != IPMI_CONFIG_ERR_SUCCESS)
    return (ret);

  if (ipmi_config_section_update_keyvalue_output (state_data,
                                                  kv,
                                                  gc.enable_power_cycle_action ? "Yes" : "No") < 0)
    return (IPMI_CONFIG_ERR_FATAL_ERROR);

  return (IPMI_CONFIG_ERR_SUCCESS);
}

ipmi_config_err_t
enable_power_cycle_action_commit (ipmi_config_state_data_t *state_data,
				  const char *section_name,
                                  const struct ipmi_config_keyvalue *kv)
{
  struct pef_action_global_control gc;
  ipmi_config_err_t ret;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if ((ret = _get_pef_action_global_control (state_data, &gc)) != IPMI_CONFIG_ERR_SUCCESS)
    return (ret);

  gc.enable_power_cycle_action = same (kv->value_input, "yes");

  return (_set_pef_action_global_control (state_data, &gc));
}

ipmi_config_err_t
enable_oem_action_checkout (ipmi_config_state_data_t *state_data,
			    const char *section_name,
                            struct ipmi_config_keyvalue *kv)
{
  struct pef_action_global_control gc;
  ipmi_config_err_t ret;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if ((ret = _get_pef_action_global_control (state_data, &gc)) != IPMI_CONFIG_ERR_SUCCESS)
    return (ret);

  if (ipmi_config_section_update_keyvalue_output (state_data,
                                                  kv,
                                                  gc.enable_oem_action ? "Yes" : "No") < 0)
    return (IPMI_CONFIG_ERR_FATAL_ERROR);

  return (IPMI_CONFIG_ERR_SUCCESS);
}

ipmi_config_err_t
enable_oem_action_commit (ipmi_config_state_data_t *state_data,
			  const char *section_name,
                          const struct ipmi_config_keyvalue *kv)
{
  struct pef_action_global_control gc;
  ipmi_config_err_t ret;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if ((ret = _get_pef_action_global_control (state_data, &gc)) != IPMI_CONFIG_ERR_SUCCESS)
    return (ret);

  gc.enable_oem_action = same (kv->value_input, "yes");

  return (_set_pef_action_global_control (state_data, &gc));
}

ipmi_config_err_t
enable_diagnostic_interrupt_checkout (ipmi_config_state_data_t *state_data,
				      const char *section_name,
                                      struct ipmi_config_keyvalue *kv)
{
  struct pef_action_global_control gc;
  ipmi_config_err_t ret;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if ((ret = _get_pef_action_global_control (state_data, &gc)) != IPMI_CONFIG_ERR_SUCCESS)
    return (ret);

  if (ipmi_config_section_update_keyvalue_output (state_data,
                                                  kv,
                                                  gc.enable_diagnostic_interrupt ? "Yes" : "No") < 0)
    return (IPMI_CONFIG_ERR_FATAL_ERROR);

  return (IPMI_CONFIG_ERR_SUCCESS);
}

ipmi_config_err_t
enable_diagnostic_interrupt_commit (ipmi_config_state_data_t *state_data,
				    const char *section_name,
                                    const struct ipmi_config_keyvalue *kv)
{
  struct pef_action_global_control gc;
  ipmi_config_err_t ret;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if ((ret = _get_pef_action_global_control (state_data, &gc)) != IPMI_CONFIG_ERR_SUCCESS)
    return (ret);

  gc.enable_diagnostic_interrupt = same (kv->value_input, "yes");

  return (_set_pef_action_global_control (state_data, &gc));
}

ipmi_config_err_t
pef_startup_delay_checkout (ipmi_config_state_data_t *state_data,
			    const char *section_name,
                            struct ipmi_config_keyvalue *kv)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint8_t pef_startup_delay;
  uint64_t val = 0;
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_get_pef_configuration_parameters_pef_startup_delay_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s",
                       strerror (errno));
      goto cleanup;
    }

  if (ipmi_cmd_get_pef_configuration_parameters_pef_startup_delay (state_data->ipmi_ctx,
                                                                   IPMI_GET_PEF_PARAMETER,
                                                                   IPMI_PEF_CONFIGURATION_PARAMETERS_NO_SET_SELECTOR,
                                                                   IPMI_PEF_CONFIGURATION_PARAMETERS_NO_BLOCK_SELECTOR,
                                                                   obj_cmd_rs) < 0)
    {
      ipmi_config_err_t ret;

      if (ipmi_config_param_errnum_is_non_fatal (state_data,
                                                 obj_cmd_rs,
                                                 &ret))
        rv = ret;

      if (rv == IPMI_CONFIG_ERR_FATAL_ERROR
	  || state_data->prog_data->args->common_args.debug)
        pstdout_fprintf (state_data->pstate,
                         stderr,
                         "ipmi_cmd_get_pef_configuration_parameters_pef_startup_delay: %s\n",
                         ipmi_ctx_errormsg (state_data->ipmi_ctx));

      goto cleanup;
    }

  if (FIID_OBJ_GET (obj_cmd_rs, "pef_startup_delay", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'pef_startup_delay': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  pef_startup_delay = val;

  if (ipmi_config_section_update_keyvalue_output_unsigned_int (state_data,
                                                               kv,
                                                               pef_startup_delay) < 0)
    return (IPMI_CONFIG_ERR_FATAL_ERROR);

  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

ipmi_config_err_t
pef_startup_delay_commit (ipmi_config_state_data_t *state_data,
			  const char *section_name,
                          const struct ipmi_config_keyvalue *kv)
{
  fiid_obj_t obj_cmd_rs = NULL;
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_set_pef_configuration_parameters_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s",
                       strerror (errno));
      goto cleanup;
    }

  if (ipmi_cmd_set_pef_configuration_parameters_pef_startup_delay (state_data->ipmi_ctx,
                                                                   atoi (kv->value_input),
                                                                   obj_cmd_rs) < 0)
    {
      ipmi_config_err_t ret;

      if (ipmi_config_param_errnum_is_non_fatal (state_data,
                                                 obj_cmd_rs,
                                                 &ret))
        rv = ret;

      if (rv == IPMI_CONFIG_ERR_FATAL_ERROR
	  || state_data->prog_data->args->common_args.debug)
        pstdout_fprintf (state_data->pstate,
                         stderr,
                         "ipmi_cmd_set_pef_configuration_parameters_pef_startup_delay: %s\n",
                         ipmi_ctx_errormsg (state_data->ipmi_ctx));

      goto cleanup;
    }

  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

ipmi_config_err_t
pef_alert_startup_delay_checkout (ipmi_config_state_data_t *state_data,
				  const char *section_name,
                                  struct ipmi_config_keyvalue *kv)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint8_t pef_alert_startup_delay;
  uint64_t val = 0;
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_get_pef_configuration_parameters_pef_alert_startup_delay_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s",
                       strerror (errno));
      goto cleanup;
    }

  if (ipmi_cmd_get_pef_configuration_parameters_pef_alert_startup_delay (state_data->ipmi_ctx,
                                                                         IPMI_GET_PEF_PARAMETER,
                                                                         IPMI_PEF_CONFIGURATION_PARAMETERS_NO_SET_SELECTOR,
                                                                         IPMI_PEF_CONFIGURATION_PARAMETERS_NO_BLOCK_SELECTOR,
                                                                         obj_cmd_rs) < 0)
    {
      ipmi_config_err_t ret;

      if (ipmi_config_param_errnum_is_non_fatal (state_data,
                                                 obj_cmd_rs,
                                                 &ret))
        rv = ret;

      if (rv == IPMI_CONFIG_ERR_FATAL_ERROR
	  || state_data->prog_data->args->common_args.debug)
        pstdout_fprintf (state_data->pstate,
                         stderr,
                         "ipmi_cmd_get_pef_configuration_parameters_pef_alert_startup_delay: %s\n",
                         ipmi_ctx_errormsg (state_data->ipmi_ctx));

      goto cleanup;
    }

  if (FIID_OBJ_GET (obj_cmd_rs, "pef_alert_startup_delay", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'pef_alert_startup_delay': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  pef_alert_startup_delay = val;

  if (ipmi_config_section_update_keyvalue_output_unsigned_int (state_data,
                                                               kv,
                                                               val) < 0)
    return (IPMI_CONFIG_ERR_FATAL_ERROR);

  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

ipmi_config_err_t
pef_alert_startup_delay_commit (ipmi_config_state_data_t *state_data,
				const char *section_name,
                                const struct ipmi_config_keyvalue *kv)
{
  fiid_obj_t obj_cmd_rs = NULL;
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_set_pef_configuration_parameters_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s",
                       strerror (errno));
      goto cleanup;
    }

  if (ipmi_cmd_set_pef_configuration_parameters_pef_alert_startup_delay (state_data->ipmi_ctx,
                                                                         atoi (kv->value_input),
                                                                         obj_cmd_rs) < 0)
    {
      ipmi_config_err_t ret;

      if (ipmi_config_param_errnum_is_non_fatal (state_data,
                                                 obj_cmd_rs,
                                                 &ret))
        rv = ret;

      if (rv == IPMI_CONFIG_ERR_FATAL_ERROR
	  || state_data->prog_data->args->common_args.debug)
        pstdout_fprintf (state_data->pstate,
                         stderr,
                         "ipmi_cmd_set_pef_configuration_parameters_pef_alert_startup_delay: %s\n",
                         ipmi_ctx_errormsg (state_data->ipmi_ctx));

      goto cleanup;
    }

  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}
