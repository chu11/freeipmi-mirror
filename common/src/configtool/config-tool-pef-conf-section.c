/* 
   Copyright (C) 2003-2008 FreeIPMI Core Team

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software Foundation,
   Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA.  
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

#include "config-tool-pef-conf-section.h"
#include "config-tool-section.h"

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

static config_err_t
_get_pef_control (pstdout_state_t pstate,
                  ipmi_ctx_t ipmi_ctx,
                  struct config_arguments *cmd_args,
                  struct pef_control *pc)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val = 0;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;

  assert(ipmi_ctx);
  assert(cmd_args);
  assert(pc);

  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_get_pef_configuration_parameters_pef_control_rs)))
    {
      PSTDOUT_FPRINTF(pstate,
                      stderr,
                      "fiid_obj_create: %s",
                      strerror(errno));
      goto cleanup;
    }

  if (ipmi_cmd_get_pef_configuration_parameters_pef_control (ipmi_ctx,
                                                             IPMI_GET_PEF_PARAMETER,
                                                             SET_SELECTOR,
                                                             BLOCK_SELECTOR,
                                                             obj_cmd_rs) < 0)
    {
      if (cmd_args->common.debug)
        PSTDOUT_FPRINTF(pstate,
                        stderr,
                        "ipmi_cmd_get_pef_configuration_parameters_pef_control: %s\n",
                        ipmi_ctx_strerror(ipmi_ctx_errnum(ipmi_ctx)));
      if (!IPMI_CTX_ERRNUM_IS_FATAL_ERROR(ipmi_ctx))
        rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  if (fiid_obj_get (obj_cmd_rs, "pef", &val) < 0)
    {
      PSTDOUT_FPRINTF(pstate,
                      stderr,
                      "fiid_obj_get: %s\n",
                      fiid_strerror(fiid_obj_errnum(obj_cmd_rs)));
      goto cleanup;
    }
  pc->enable_pef = val;

  if (fiid_obj_get (obj_cmd_rs, "pef_event_messages", &val) < 0)
    {
      PSTDOUT_FPRINTF(pstate,
                      stderr,
                      "fiid_obj_get: %s\n",
                      fiid_strerror(fiid_obj_errnum(obj_cmd_rs)));
      goto cleanup;
    }
  pc->enable_pef_event_messages = val;

  if (fiid_obj_get (obj_cmd_rs, "pef_startup_delay", &val) < 0)
    {
      PSTDOUT_FPRINTF(pstate,
                      stderr,
                      "fiid_obj_get: %s\n",
                      fiid_strerror(fiid_obj_errnum(obj_cmd_rs)));
      goto cleanup;
    }
  pc->enable_pef_startup_delay = val;

  if (fiid_obj_get (obj_cmd_rs, "pef_alert_startup_delay", &val) < 0)
    {
      PSTDOUT_FPRINTF(pstate,
                      stderr,
                      "fiid_obj_get: %s\n",
                      fiid_strerror(fiid_obj_errnum(obj_cmd_rs)));
      goto cleanup;
    }
  pc->enable_pef_alert_startup_delay = val;

  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

static config_err_t
_set_pef_control (pstdout_state_t pstate,
                  ipmi_ctx_t ipmi_ctx,
                  struct config_arguments *cmd_args,
                  struct pef_control *pc)
{
  fiid_obj_t obj_cmd_rs = NULL;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;

  assert(ipmi_ctx);
  assert(cmd_args);
  assert(pc);

  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_set_pef_configuration_parameters_rs)))
    {
      PSTDOUT_FPRINTF(pstate,
                      stderr,
                      "fiid_obj_create: %s",
                      strerror(errno));
      goto cleanup;
    }

  if (ipmi_cmd_set_pef_configuration_parameters_pef_control (ipmi_ctx,
                                                             pc->enable_pef,
                                                             pc->enable_pef_event_messages,
                                                             pc->enable_pef_startup_delay,
                                                             pc->enable_pef_alert_startup_delay,
                                                             obj_cmd_rs) < 0)
    {
      if (cmd_args->common.debug)
        PSTDOUT_FPRINTF(pstate,
                        stderr,
                        "ipmi_cmd_set_pef_configuration_parameters_pef_control: %s\n",
                        ipmi_ctx_strerror(ipmi_ctx_errnum(ipmi_ctx)));
      if (!IPMI_CTX_ERRNUM_IS_FATAL_ERROR(ipmi_ctx))
        rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

config_err_t
enable_pef_checkout (pstdout_state_t pstate,
                     struct config_keyvalue *kv,
                     ipmi_ctx_t ipmi_ctx,
                     struct config_arguments *cmd_args)
{
  struct pef_control pc;
  config_err_t ret;
  
  if ((ret = _get_pef_control (pstate,
                               ipmi_ctx, 
                               cmd_args, 
                               &pc)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (config_section_update_keyvalue_output(pstate,
                                            kv,
                                            pc.enable_pef ? "Yes" : "No") < 0)
    return CONFIG_ERR_FATAL_ERROR;

  return CONFIG_ERR_SUCCESS;
}

config_err_t
enable_pef_commit (pstdout_state_t pstate,
                   const struct config_keyvalue *kv,
                   ipmi_ctx_t ipmi_ctx,
                   struct config_arguments *cmd_args)
{
  struct pef_control pc;
  config_err_t ret;

  if ((ret = _get_pef_control (pstate, 
                               ipmi_ctx, 
                               cmd_args, 
                               &pc)) != CONFIG_ERR_SUCCESS)
    return ret;

  pc.enable_pef = same (kv->value_input, "yes");

  return _set_pef_control (pstate, ipmi_ctx, cmd_args, &pc);
}

config_err_t
enable_pef_event_messages_checkout (pstdout_state_t pstate,
                                    struct config_keyvalue *kv,
                                    ipmi_ctx_t ipmi_ctx,
                                    struct config_arguments *cmd_args)
{
  struct pef_control pc;
  config_err_t ret;
  
  if ((ret = _get_pef_control (pstate, 
                               ipmi_ctx, 
                               cmd_args, 
                               &pc)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (config_section_update_keyvalue_output(pstate,
                                            kv,
                                            pc.enable_pef_event_messages ? "Yes" : "No") < 0)
    return CONFIG_ERR_FATAL_ERROR;

  return CONFIG_ERR_SUCCESS;
}

config_err_t
enable_pef_event_messages_commit (pstdout_state_t pstate,
                                  const struct config_keyvalue *kv,
                                  ipmi_ctx_t ipmi_ctx,
                                  struct config_arguments *cmd_args)
{
  struct pef_control pc;
  config_err_t ret;

  if ((ret = _get_pef_control (pstate, 
                               ipmi_ctx, 
                               cmd_args, 
                               &pc)) != CONFIG_ERR_SUCCESS)
    return ret;

  pc.enable_pef_event_messages = same (kv->value_input, "yes");

  return _set_pef_control (pstate, ipmi_ctx, cmd_args, &pc);
}

config_err_t
enable_pef_startup_delay_checkout (pstdout_state_t pstate,
                                   struct config_keyvalue *kv,
                                   ipmi_ctx_t ipmi_ctx,
                                   struct config_arguments *cmd_args)
{
  struct pef_control pc;
  config_err_t ret;
  
  if ((ret = _get_pef_control (pstate, 
                               ipmi_ctx, 
                               cmd_args,
                               &pc)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (config_section_update_keyvalue_output(pstate,
                                            kv,
                                            pc.enable_pef_startup_delay ? "Yes" : "No") < 0)
    return CONFIG_ERR_FATAL_ERROR;

  return CONFIG_ERR_SUCCESS;
}

config_err_t
enable_pef_startup_delay_commit (pstdout_state_t pstate,
                                 const struct config_keyvalue *kv,
                                 ipmi_ctx_t ipmi_ctx,
                                 struct config_arguments *cmd_args)
{
  struct pef_control pc;
  config_err_t ret;

  if ((ret = _get_pef_control (pstate, 
                               ipmi_ctx,
                               cmd_args, 
                               &pc)) != CONFIG_ERR_SUCCESS)
    return ret;

  pc.enable_pef_startup_delay = same (kv->value_input, "yes");

  return _set_pef_control (pstate, ipmi_ctx, cmd_args, &pc);
}

config_err_t
enable_pef_alert_startup_delay_checkout (pstdout_state_t pstate,
                                         struct config_keyvalue *kv,
                                         ipmi_ctx_t ipmi_ctx,
                                         struct config_arguments *cmd_args)
{
  struct pef_control pc;
  config_err_t ret;
  
  if ((ret = _get_pef_control (pstate, 
                               ipmi_ctx, 
                               cmd_args,
                               &pc)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (config_section_update_keyvalue_output(pstate,
                                            kv,
                                            pc.enable_pef_alert_startup_delay ? "Yes" : "No") < 0)
    return CONFIG_ERR_FATAL_ERROR;

  return CONFIG_ERR_SUCCESS;
}

config_err_t
enable_pef_alert_startup_delay_commit (pstdout_state_t pstate,
                                       const struct config_keyvalue *kv,
                                       ipmi_ctx_t ipmi_ctx,
                                       struct config_arguments *cmd_args)
{
  struct pef_control pc;
  config_err_t ret;

  if ((ret = _get_pef_control (pstate,
                               ipmi_ctx, 
                               cmd_args, 
                               &pc)) != CONFIG_ERR_SUCCESS)
    return ret;

  pc.enable_pef_alert_startup_delay = same (kv->value_input, "yes");

  return _set_pef_control (pstate, ipmi_ctx, cmd_args, &pc);
}

static config_err_t
_get_pef_action_global_control (pstdout_state_t pstate,
                                ipmi_ctx_t ipmi_ctx,
                                struct config_arguments *cmd_args,
                                struct pef_action_global_control *gc)
                               
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val = 0;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;

  assert(ipmi_ctx);
  assert(cmd_args);
  assert(gc);

  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_get_pef_configuration_parameters_pef_action_global_control_rs)))
    {
      PSTDOUT_FPRINTF(pstate,
                      stderr,
                      "fiid_obj_create: %s",
                      strerror(errno));
      goto cleanup;
    }

  if (ipmi_cmd_get_pef_configuration_parameters_pef_action_global_control (ipmi_ctx,
                                                                           IPMI_GET_PEF_PARAMETER,
                                                                           SET_SELECTOR,
                                                                           BLOCK_SELECTOR,
                                                                           obj_cmd_rs) < 0)
    {
      if (cmd_args->common.debug)
        PSTDOUT_FPRINTF(pstate,
                        stderr,
                        "ipmi_cmd_get_pef_configuration_parameters_pef_action_global_control: %s\n",
                        ipmi_ctx_strerror(ipmi_ctx_errnum(ipmi_ctx)));
      if (!IPMI_CTX_ERRNUM_IS_FATAL_ERROR(ipmi_ctx))
        rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  if (fiid_obj_get (obj_cmd_rs, "alert_action", &val) < 0)
    {
      PSTDOUT_FPRINTF(pstate,
                      stderr,
                      "fiid_obj_get: %s\n",
                      fiid_strerror(fiid_obj_errnum(obj_cmd_rs)));
      goto cleanup;
    }
  gc->enable_alert_action = val;

  if (fiid_obj_get (obj_cmd_rs, "power_down_action", &val) < 0)
    {
      PSTDOUT_FPRINTF(pstate,
                      stderr,
                      "fiid_obj_get: %s\n",
                      fiid_strerror(fiid_obj_errnum(obj_cmd_rs)));
      goto cleanup;
    }
  gc->enable_power_down_action = val;

  if (fiid_obj_get (obj_cmd_rs, "reset_action", &val) < 0)
    {
      PSTDOUT_FPRINTF(pstate,
                      stderr,
                      "fiid_obj_get: %s\n",
                      fiid_strerror(fiid_obj_errnum(obj_cmd_rs)));
      goto cleanup;
    }
  gc->enable_reset_action = val;

  if (fiid_obj_get (obj_cmd_rs, "power_cycle_action", &val) < 0)
    {
      PSTDOUT_FPRINTF(pstate,
                      stderr,
                      "fiid_obj_get: %s\n",
                      fiid_strerror(fiid_obj_errnum(obj_cmd_rs)));
      goto cleanup;
    }
  gc->enable_power_cycle_action = val;

  if (fiid_obj_get (obj_cmd_rs, "oem_action", &val) < 0)
    {
      PSTDOUT_FPRINTF(pstate,
                      stderr,
                      "fiid_obj_get: %s\n",
                      fiid_strerror(fiid_obj_errnum(obj_cmd_rs)));
      goto cleanup;
    }
  gc->enable_oem_action = val;

  if (fiid_obj_get (obj_cmd_rs, "diagnostic_interrupt", &val) < 0)
    {
      PSTDOUT_FPRINTF(pstate,
                      stderr,
                      "fiid_obj_get: %s\n",
                      fiid_strerror(fiid_obj_errnum(obj_cmd_rs)));
      goto cleanup;
    }
  gc->enable_diagnostic_interrupt = val;

  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

static config_err_t
_set_pef_action_global_control (pstdout_state_t pstate,
                                ipmi_ctx_t ipmi_ctx,
                                struct config_arguments *cmd_args,
                                struct pef_action_global_control *gc)
{
  fiid_obj_t obj_cmd_rs = NULL;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;

  assert(ipmi_ctx);
  assert(cmd_args);
  assert(gc);

  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_set_pef_configuration_parameters_rs)))
    {
      PSTDOUT_FPRINTF(pstate,
                      stderr,
                      "fiid_obj_create: %s",
                      strerror(errno));
      goto cleanup;
    }

  if (ipmi_cmd_set_pef_configuration_parameters_pef_action_global_control (ipmi_ctx,
                                                                           gc->enable_alert_action,
                                                                           gc->enable_power_down_action,
                                                                           gc->enable_reset_action,
                                                                           gc->enable_power_cycle_action,
                                                                           gc->enable_oem_action,
                                                                           gc->enable_diagnostic_interrupt,
                                                                           obj_cmd_rs) < 0)
    {
      if (cmd_args->common.debug)
        PSTDOUT_FPRINTF(pstate,
                        stderr,
                        "ipmi_cmd_set_pef_configuration_parameters_pef_action_global_control: %s\n",
                        ipmi_ctx_strerror(ipmi_ctx_errnum(ipmi_ctx)));
      if (!IPMI_CTX_ERRNUM_IS_FATAL_ERROR(ipmi_ctx))
        rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

config_err_t
enable_alert_action_checkout (pstdout_state_t pstate,
                              struct config_keyvalue *kv,
                              ipmi_ctx_t ipmi_ctx,
                              struct config_arguments *cmd_args)
{
  struct pef_action_global_control gc;
  config_err_t ret;
  
  if ((ret = _get_pef_action_global_control (pstate,
                                             ipmi_ctx, 
                                             cmd_args,
                                             &gc)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (config_section_update_keyvalue_output(pstate,
                                            kv,
                                            gc.enable_alert_action ? "Yes" : "No") < 0)
    return CONFIG_ERR_FATAL_ERROR;

  return CONFIG_ERR_SUCCESS;
}

config_err_t
enable_alert_action_commit (pstdout_state_t pstate,
                            const struct config_keyvalue *kv,
                            ipmi_ctx_t ipmi_ctx,
                            struct config_arguments *cmd_args)
{
  struct pef_action_global_control gc;
  config_err_t ret;

  if ((ret = _get_pef_action_global_control (pstate,
                                             ipmi_ctx, 
                                             cmd_args, 
                                             &gc)) != CONFIG_ERR_SUCCESS)
    return ret;

  gc.enable_alert_action = same (kv->value_input, "yes");

  return _set_pef_action_global_control (pstate, ipmi_ctx, cmd_args, &gc);
}

config_err_t
enable_power_down_action_checkout (pstdout_state_t pstate,
                                   struct config_keyvalue *kv,
                                   ipmi_ctx_t ipmi_ctx,
                                   struct config_arguments *cmd_args)
{
  struct pef_action_global_control gc;
  config_err_t ret;

  if ((ret = _get_pef_action_global_control (pstate, 
                                             ipmi_ctx, 
                                             cmd_args, 
                                             &gc)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (config_section_update_keyvalue_output(pstate,
                                            kv,
                                            gc.enable_power_down_action ? "Yes" : "No") < 0)
    return CONFIG_ERR_FATAL_ERROR;

  return CONFIG_ERR_SUCCESS;
}

config_err_t
enable_power_down_action_commit (pstdout_state_t pstate,
                                 const struct config_keyvalue *kv,
                                 ipmi_ctx_t ipmi_ctx,
                                 struct config_arguments *cmd_args)
{
  struct pef_action_global_control gc;
  config_err_t ret;

  if ((ret = _get_pef_action_global_control (pstate, 
                                             ipmi_ctx, 
                                             cmd_args, 
                                             &gc)) != CONFIG_ERR_SUCCESS)
    return ret;

  gc.enable_power_down_action = same (kv->value_input, "yes");

  return _set_pef_action_global_control (pstate, ipmi_ctx, cmd_args, &gc);
}

config_err_t
enable_reset_action_checkout (pstdout_state_t pstate,
                              struct config_keyvalue *kv,
                              ipmi_ctx_t ipmi_ctx,
                              struct config_arguments *cmd_args)
{
  struct pef_action_global_control gc;
  config_err_t ret;

  if ((ret = _get_pef_action_global_control (pstate, 
                                             ipmi_ctx, 
                                             cmd_args,
                                             &gc)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (config_section_update_keyvalue_output(pstate,
                                            kv,
                                            gc.enable_reset_action ? "Yes" : "No") < 0)
    return CONFIG_ERR_FATAL_ERROR;

  return CONFIG_ERR_SUCCESS;
}

config_err_t
enable_reset_action_commit (pstdout_state_t pstate,
                            const struct config_keyvalue *kv,
                            ipmi_ctx_t ipmi_ctx,
                            struct config_arguments *cmd_args)
{
  struct pef_action_global_control gc;
  config_err_t ret;

  if ((ret = _get_pef_action_global_control (pstate, 
                                             ipmi_ctx, 
                                             cmd_args,
                                             &gc)) != CONFIG_ERR_SUCCESS)
    return ret;

  gc.enable_reset_action = same (kv->value_input, "yes");

  return _set_pef_action_global_control (pstate, ipmi_ctx, cmd_args, &gc);
}

config_err_t
enable_power_cycle_action_checkout (pstdout_state_t pstate,
                                    struct config_keyvalue *kv,
                                    ipmi_ctx_t ipmi_ctx,
                                    struct config_arguments *cmd_args)
{
  struct pef_action_global_control gc;
  config_err_t ret;

  if ((ret = _get_pef_action_global_control (pstate, 
                                             ipmi_ctx,
                                             cmd_args,
                                             &gc)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (config_section_update_keyvalue_output(pstate,
                                            kv,
                                            gc.enable_power_cycle_action ? "Yes" : "No") < 0)
    return CONFIG_ERR_FATAL_ERROR;

  return CONFIG_ERR_SUCCESS;
}

config_err_t
enable_power_cycle_action_commit (pstdout_state_t pstate,
                                  const struct config_keyvalue *kv,
                                  ipmi_ctx_t ipmi_ctx,
                                  struct config_arguments *cmd_args)
{
  struct pef_action_global_control gc;
  config_err_t ret;

  if ((ret = _get_pef_action_global_control (pstate, 
                                             ipmi_ctx, 
                                             cmd_args,
                                             &gc)) != CONFIG_ERR_SUCCESS)
    return ret;

  gc.enable_power_cycle_action = same (kv->value_input, "yes");

  return _set_pef_action_global_control (pstate, ipmi_ctx, cmd_args, &gc);
}

config_err_t
enable_oem_action_checkout (pstdout_state_t pstate,
                            struct config_keyvalue *kv,
                            ipmi_ctx_t ipmi_ctx,
                            struct config_arguments *cmd_args)
{
  struct pef_action_global_control gc;
  config_err_t ret;

  if ((ret = _get_pef_action_global_control (pstate, 
                                             ipmi_ctx,
                                             cmd_args,
                                             &gc)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (config_section_update_keyvalue_output(pstate,
                                            kv, 
                                            gc.enable_oem_action ? "Yes" : "No") < 0)
    return CONFIG_ERR_FATAL_ERROR;

  return CONFIG_ERR_SUCCESS;
}

config_err_t
enable_oem_action_commit (pstdout_state_t pstate,
                          const struct config_keyvalue *kv,
                          ipmi_ctx_t ipmi_ctx,
                          struct config_arguments *cmd_args)
{
  struct pef_action_global_control gc;
  config_err_t ret;

  if ((ret = _get_pef_action_global_control (pstate,
                                             ipmi_ctx, 
                                             cmd_args,
                                             &gc)) != CONFIG_ERR_SUCCESS)
    return ret;

  gc.enable_oem_action = same (kv->value_input, "yes");

  return _set_pef_action_global_control (pstate, ipmi_ctx, cmd_args, &gc);
}

config_err_t
enable_diagnostic_interrupt_checkout (pstdout_state_t pstate,
                                      struct config_keyvalue *kv,
                                      ipmi_ctx_t ipmi_ctx,
                                      struct config_arguments *cmd_args)
{
  struct pef_action_global_control gc;
  config_err_t ret;

  if ((ret = _get_pef_action_global_control (pstate, 
                                             ipmi_ctx, 
                                             cmd_args, 
                                             &gc)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (config_section_update_keyvalue_output(pstate,
                                            kv, 
                                            gc.enable_diagnostic_interrupt ? "Yes" : "No") < 0)
    return CONFIG_ERR_FATAL_ERROR;

  return CONFIG_ERR_SUCCESS;
}

config_err_t
enable_diagnostic_interrupt_commit (pstdout_state_t pstate,
                                    const struct config_keyvalue *kv,
                                    ipmi_ctx_t ipmi_ctx,
                                    struct config_arguments *cmd_args)
{
  struct pef_action_global_control gc;
  config_err_t ret;

  if ((ret = _get_pef_action_global_control (pstate, 
                                             ipmi_ctx, 
                                             cmd_args,
                                             &gc)) != CONFIG_ERR_SUCCESS)
    return ret;

  gc.enable_diagnostic_interrupt = same (kv->value_input, "yes");

  return _set_pef_action_global_control (pstate, ipmi_ctx, cmd_args, &gc);
}

config_err_t
pef_startup_delay_checkout (pstdout_state_t pstate,
                            struct config_keyvalue *kv,
                            ipmi_ctx_t ipmi_ctx,
                            struct config_arguments *cmd_args)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val = 0;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;

  assert(kv);
  assert(ipmi_ctx);
  assert(cmd_args);

  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_get_pef_configuration_parameters_pef_startup_delay_rs)))
    {
      PSTDOUT_FPRINTF(pstate,
                      stderr,
                      "fiid_obj_create: %s",
                      strerror(errno));
      goto cleanup;
    }

  if (ipmi_cmd_get_pef_configuration_parameters_pef_startup_delay (ipmi_ctx,
                                                                   IPMI_GET_PEF_PARAMETER,
                                                                   SET_SELECTOR,
                                                                   BLOCK_SELECTOR,
                                                                   obj_cmd_rs) < 0)
    {
      if (cmd_args->common.debug)
        PSTDOUT_FPRINTF(pstate,
                        stderr,
                        "ipmi_cmd_get_pef_configuration_parameters_pef_startup_delay: %s\n",
                        ipmi_ctx_strerror(ipmi_ctx_errnum(ipmi_ctx)));
      if (!IPMI_CTX_ERRNUM_IS_FATAL_ERROR(ipmi_ctx))
        rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  if (fiid_obj_get (obj_cmd_rs, "pef_startup_delay", &val) < 0)
    {
      PSTDOUT_FPRINTF(pstate,
                      stderr,
                      "fiid_obj_get: %s\n",
                      fiid_strerror(fiid_obj_errnum(obj_cmd_rs)));
      goto cleanup;
    }

  if (config_section_update_keyvalue_output_int(pstate,
                                                kv,
                                                val) < 0)
    return CONFIG_ERR_FATAL_ERROR;

  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

config_err_t
pef_startup_delay_commit (pstdout_state_t pstate,
                          const struct config_keyvalue *kv,
                          ipmi_ctx_t ipmi_ctx,
                          struct config_arguments *cmd_args)
{
  fiid_obj_t obj_cmd_rs = NULL;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  
  assert(kv);
  assert(ipmi_ctx);
  assert(cmd_args);

  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_set_pef_configuration_parameters_rs)))
    {
      PSTDOUT_FPRINTF(pstate,
                      stderr,
                      "fiid_obj_create: %s",
                      strerror(errno));
      goto cleanup;
    }
  
  if (ipmi_cmd_set_pef_configuration_parameters_pef_startup_delay (ipmi_ctx,
                                                                   atoi (kv->value_input),
                                                                   obj_cmd_rs) < 0)
    {
      if (cmd_args->common.debug)
        PSTDOUT_FPRINTF(pstate,
                        stderr,
                        "ipmi_cmd_set_pef_configuration_parameters_pef_startup_delay: %s\n",
                        ipmi_ctx_strerror(ipmi_ctx_errnum(ipmi_ctx)));
      if (!IPMI_CTX_ERRNUM_IS_FATAL_ERROR(ipmi_ctx))
        rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }
  
  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

config_err_t
pef_alert_startup_delay_checkout (pstdout_state_t pstate,
                                  struct config_keyvalue *kv,
                                  ipmi_ctx_t ipmi_ctx,
                                  struct config_arguments *cmd_args)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val = 0;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;

  assert(kv);
  assert(ipmi_ctx);
  assert(cmd_args);

  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_get_pef_configuration_parameters_pef_alert_startup_delay_rs)))
    {
      PSTDOUT_FPRINTF(pstate,
                      stderr,
                      "fiid_obj_create: %s",
                      strerror(errno));
      goto cleanup;
    }

  if (ipmi_cmd_get_pef_configuration_parameters_pef_alert_startup_delay (ipmi_ctx,
                                                                         IPMI_GET_PEF_PARAMETER,
                                                                         SET_SELECTOR,
                                                                         BLOCK_SELECTOR,
                                                                         obj_cmd_rs) < 0)
    {
      if (cmd_args->common.debug)
        PSTDOUT_FPRINTF(pstate,
                        stderr,
                        "ipmi_cmd_get_pef_configuration_parameters_pef_alert_startup_delay: %s\n",
                        ipmi_ctx_strerror(ipmi_ctx_errnum(ipmi_ctx)));
      if (!IPMI_CTX_ERRNUM_IS_FATAL_ERROR(ipmi_ctx))
        rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  if (fiid_obj_get (obj_cmd_rs, "pef_alert_startup_delay", &val) < 0)
    {
      PSTDOUT_FPRINTF(pstate,
                      stderr,
                      "fiid_obj_get: %s\n",
                      fiid_strerror(fiid_obj_errnum(obj_cmd_rs)));
      goto cleanup;
    }

  if (config_section_update_keyvalue_output_int(pstate,
                                                kv,
                                                val) < 0)
    return CONFIG_ERR_FATAL_ERROR;

  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

config_err_t
pef_alert_startup_delay_commit (pstdout_state_t pstate,
                                const struct config_keyvalue *kv,
                                ipmi_ctx_t ipmi_ctx,
                                struct config_arguments *cmd_args)
{
  fiid_obj_t obj_cmd_rs = NULL;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;

  assert(kv);
  assert(ipmi_ctx);
  assert(cmd_args);

  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_set_pef_configuration_parameters_rs)))
    {
      PSTDOUT_FPRINTF(pstate,
                      stderr,
                      "fiid_obj_create: %s",
                      strerror(errno));
      goto cleanup;
    }

  if (ipmi_cmd_set_pef_configuration_parameters_pef_alert_startup_delay (ipmi_ctx,
                                                                         atoi (kv->value_input),
                                                                         obj_cmd_rs) < 0)
    {
      if (cmd_args->common.debug)
        PSTDOUT_FPRINTF(pstate,
                        stderr,
                        "ipmi_cmd_set_pef_configuration_parameters_pef_alert_startup_delay: %s\n",
                        ipmi_ctx_strerror(ipmi_ctx_errnum(ipmi_ctx)));
      if (!IPMI_CTX_ERRNUM_IS_FATAL_ERROR(ipmi_ctx))
        rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }
  
  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}


