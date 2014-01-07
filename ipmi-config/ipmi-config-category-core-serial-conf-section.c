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
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <assert.h>

#include "ipmi-config.h"
#include "ipmi-config-map.h"
#include "ipmi-config-section.h"
#include "ipmi-config-utils.h"
#include "ipmi-config-validate.h"

#include "freeipmi-portability.h"
#include "pstdout.h"

/* convenience structs */

struct connection_mode {
  uint8_t basic_mode;
  uint8_t ppp_mode;
  uint8_t terminal_mode;
  uint8_t connect_mode;
};

struct ipmi_messaging_comm_settings {
  uint8_t dtr_hangup;
  uint8_t flow_control;
  uint8_t bit_rate;
};

static ipmi_config_err_t
_get_connection_mode (ipmi_config_state_data_t *state_data,
                      const char *section_name,
                      struct connection_mode *cm)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;
  ipmi_config_err_t ret;
  uint8_t channel_number;

  assert (state_data);
  assert (section_name);
  assert (cm);

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_get_serial_modem_configuration_connection_mode_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if ((ret = get_serial_channel_number (state_data, section_name, &channel_number)) != IPMI_CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_get_serial_modem_configuration_connection_mode (state_data->ipmi_ctx,
                                                               channel_number,
                                                               IPMI_GET_SERIAL_MODEM_PARAMETER,
                                                               IPMI_SERIAL_MODEM_CONFIGURATION_NO_SET_SELECTOR,
                                                               IPMI_SERIAL_MODEM_CONFIGURATION_NO_BLOCK_SELECTOR,
                                                               obj_cmd_rs) < 0)
    {
      if (ipmi_config_param_errnum_is_non_fatal (state_data,
                                                 obj_cmd_rs,
                                                 &ret))
        rv = ret;

      if (rv == IPMI_CONFIG_ERR_FATAL_ERROR
	  || state_data->prog_data->args->common_args.debug)
        pstdout_fprintf (state_data->pstate,
                         stderr,
                         "ipmi_cmd_get_serial_modem_configuration_connection_mode: %s\n",
                         ipmi_ctx_errormsg (state_data->ipmi_ctx));

      goto cleanup;
    }

  if (FIID_OBJ_GET (obj_cmd_rs, "basic_mode", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'basic_mode': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  cm->basic_mode = val;

  if (FIID_OBJ_GET (obj_cmd_rs, "ppp_mode", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'ppp_mode': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  cm->ppp_mode = val;

  if (FIID_OBJ_GET (obj_cmd_rs, "terminal_mode", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'terminal_mode': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  cm->terminal_mode = val;

  if (FIID_OBJ_GET (obj_cmd_rs, "connect_mode", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'connect_mode': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  cm->connect_mode = val;

  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static ipmi_config_err_t
_set_connection_mode (ipmi_config_state_data_t *state_data,
                      const char *section_name,
                      struct connection_mode *cm)
{
  fiid_obj_t obj_cmd_rs = NULL;
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;
  ipmi_config_err_t ret;
  uint8_t channel_number;

  assert (state_data);
  assert (section_name);
  assert (cm);

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_set_serial_modem_configuration_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if ((ret = get_serial_channel_number (state_data, section_name, &channel_number)) != IPMI_CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_set_serial_modem_configuration_connection_mode (state_data->ipmi_ctx,
                                                               channel_number,
                                                               cm->basic_mode,
                                                               cm->ppp_mode,
                                                               cm->terminal_mode,
                                                               cm->connect_mode,
                                                               obj_cmd_rs) < 0)
    {
      if (ipmi_config_param_errnum_is_non_fatal (state_data,
                                                 obj_cmd_rs,
                                                 &ret))
        rv = ret;

      if (rv == IPMI_CONFIG_ERR_FATAL_ERROR
	  || state_data->prog_data->args->common_args.debug)
        pstdout_fprintf (state_data->pstate,
                         stderr,
                         "ipmi_cmd_set_serial_modem_configuration_connection_mode: %s\n",
                         ipmi_ctx_errormsg (state_data->ipmi_ctx));

      goto cleanup;
    }

  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static ipmi_config_err_t
enable_basic_mode_checkout (ipmi_config_state_data_t *state_data,
			    const char *section_name,
                            struct ipmi_config_keyvalue *kv)
{
  struct connection_mode cm;
  ipmi_config_err_t ret;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if ((ret = _get_connection_mode (state_data, section_name, &cm)) != IPMI_CONFIG_ERR_SUCCESS)
    return (ret);

  if (ipmi_config_section_update_keyvalue_output (state_data,
                                                  kv,
                                                  cm.basic_mode ? "Yes" : "No") < 0)
    return (IPMI_CONFIG_ERR_FATAL_ERROR);

  return (IPMI_CONFIG_ERR_SUCCESS);
}

static ipmi_config_err_t
enable_basic_mode_commit (ipmi_config_state_data_t *state_data,
			  const char *section_name,
                          const struct ipmi_config_keyvalue *kv)
{
  struct connection_mode cm;
  ipmi_config_err_t ret;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if ((ret = _get_connection_mode (state_data, section_name, &cm)) != IPMI_CONFIG_ERR_SUCCESS)
    return (ret);

  cm.basic_mode = same (kv->value_input, "yes");

  return (_set_connection_mode (state_data, section_name, &cm));
}

static ipmi_config_err_t
enable_ppp_mode_checkout (ipmi_config_state_data_t *state_data,
			  const char *section_name,
                          struct ipmi_config_keyvalue *kv)
{
  struct connection_mode cm;
  ipmi_config_err_t ret;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if ((ret = _get_connection_mode (state_data, section_name, &cm)) != IPMI_CONFIG_ERR_SUCCESS)
    return (ret);

  if (ipmi_config_section_update_keyvalue_output (state_data,
                                                  kv,
                                                  cm.ppp_mode ? "Yes" : "No") < 0)
    return (IPMI_CONFIG_ERR_FATAL_ERROR);

  return (IPMI_CONFIG_ERR_SUCCESS);
}

static ipmi_config_err_t
enable_ppp_mode_commit (ipmi_config_state_data_t *state_data,
			const char *section_name,
                        const struct ipmi_config_keyvalue *kv)
{
  struct connection_mode cm;
  ipmi_config_err_t ret;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if ((ret = _get_connection_mode (state_data, section_name, &cm)) != IPMI_CONFIG_ERR_SUCCESS)
    return (ret);

  cm.ppp_mode = same (kv->value_input, "yes");

  return (_set_connection_mode (state_data, section_name, &cm));
}

static ipmi_config_err_t
enable_terminal_mode_checkout (ipmi_config_state_data_t *state_data,
			       const char *section_name,
                               struct ipmi_config_keyvalue *kv)
{
  struct connection_mode cm;
  ipmi_config_err_t ret;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if ((ret = _get_connection_mode (state_data, section_name, &cm)) != IPMI_CONFIG_ERR_SUCCESS)
    return (ret);

  if (ipmi_config_section_update_keyvalue_output (state_data,
                                                  kv,
                                                  cm.terminal_mode ? "Yes" : "No") < 0)
    return (IPMI_CONFIG_ERR_FATAL_ERROR);

  return (IPMI_CONFIG_ERR_SUCCESS);
}

static ipmi_config_err_t
enable_terminal_mode_commit (ipmi_config_state_data_t *state_data,
			     const char *section_name,
                             const struct ipmi_config_keyvalue *kv)
{
  struct connection_mode cm;
  ipmi_config_err_t ret;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if ((ret = _get_connection_mode (state_data, section_name, &cm)) != IPMI_CONFIG_ERR_SUCCESS)
    return (ret);

  cm.terminal_mode = same (kv->value_input, "yes");

  return (_set_connection_mode (state_data, section_name, &cm));
}

static ipmi_config_err_t
connect_mode_checkout (ipmi_config_state_data_t *state_data,
		       const char *section_name,
                       struct ipmi_config_keyvalue *kv)
{
  struct connection_mode cm;
  ipmi_config_err_t ret;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if ((ret = _get_connection_mode (state_data, section_name, &cm)) != IPMI_CONFIG_ERR_SUCCESS)
    return (ret);

  if (ipmi_config_section_update_keyvalue_output (state_data,
                                                  kv,
                                                  connect_mode_string (cm.connect_mode)) < 0)
    return (IPMI_CONFIG_ERR_FATAL_ERROR);

  return (IPMI_CONFIG_ERR_SUCCESS);
}

static ipmi_config_err_t
connect_mode_commit (ipmi_config_state_data_t *state_data,
		     const char *section_name,
                     const struct ipmi_config_keyvalue *kv)
{
  struct connection_mode cm;
  ipmi_config_err_t ret;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if ((ret = _get_connection_mode (state_data, section_name, &cm)) != IPMI_CONFIG_ERR_SUCCESS)
    return (ret);

  cm.connect_mode = connect_mode_number (kv->value_input);

  return (_set_connection_mode (state_data, section_name, &cm));
}

static ipmi_config_err_t
page_blackout_interval_checkout (ipmi_config_state_data_t *state_data,
				 const char *section_name,
                                 struct ipmi_config_keyvalue *kv)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint8_t page_blackout_interval;
  uint64_t val;
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;
  ipmi_config_err_t ret;
  uint8_t channel_number;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_get_serial_modem_configuration_page_blackout_interval_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if ((ret = get_serial_channel_number (state_data, section_name, &channel_number)) != IPMI_CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_get_serial_modem_configuration_page_blackout_interval (state_data->ipmi_ctx,
                                                                      channel_number,
                                                                      IPMI_GET_SERIAL_MODEM_PARAMETER,
                                                                      IPMI_SERIAL_MODEM_CONFIGURATION_NO_SET_SELECTOR,
                                                                      IPMI_SERIAL_MODEM_CONFIGURATION_NO_BLOCK_SELECTOR,
                                                                      obj_cmd_rs) < 0)
    {
      if (ipmi_config_param_errnum_is_non_fatal (state_data,
                                                 obj_cmd_rs,
                                                 &ret))
        rv = ret;

      if (rv == IPMI_CONFIG_ERR_FATAL_ERROR
	  || state_data->prog_data->args->common_args.debug)
        pstdout_fprintf (state_data->pstate,
                         stderr,
                         "ipmi_cmd_get_serial_modem_configuration_page_blackout_interval: %s\n",
                         ipmi_ctx_errormsg (state_data->ipmi_ctx));

      goto cleanup;
    }

  if (FIID_OBJ_GET (obj_cmd_rs, "page_blackout_interval", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'page_blackout_interval': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  page_blackout_interval = val;

  if (ipmi_config_section_update_keyvalue_output_unsigned_int (state_data,
                                                               kv,
                                                               page_blackout_interval) < 0)
    return (IPMI_CONFIG_ERR_FATAL_ERROR);

  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);

}

static ipmi_config_err_t
page_blackout_interval_commit (ipmi_config_state_data_t *state_data,
			       const char *section_name,
                               const struct ipmi_config_keyvalue *kv)
{
  fiid_obj_t obj_cmd_rs = NULL;
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;
  ipmi_config_err_t ret;
  uint8_t channel_number;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_set_serial_modem_configuration_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if ((ret = get_serial_channel_number (state_data, section_name, &channel_number)) != IPMI_CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_set_serial_modem_configuration_page_blackout_interval (state_data->ipmi_ctx,
                                                                      channel_number,
                                                                      atoi (kv->value_input),
                                                                      obj_cmd_rs) < 0)
    {
      if (ipmi_config_param_errnum_is_non_fatal (state_data,
                                                 obj_cmd_rs,
                                                 &ret))
        rv = ret;

      if (rv == IPMI_CONFIG_ERR_FATAL_ERROR
	  || state_data->prog_data->args->common_args.debug)
        pstdout_fprintf (state_data->pstate,
                         stderr,
                         "ipmi_cmd_set_serial_modem_configuration_page_blackout_interval: %s\n",
                         ipmi_ctx_errormsg (state_data->ipmi_ctx));

      goto cleanup;
    }

  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static ipmi_config_err_t
call_retry_interval_checkout (ipmi_config_state_data_t *state_data,
			      const char *section_name,
                              struct ipmi_config_keyvalue *kv)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint8_t call_retry_interval;
  uint64_t val;
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;
  ipmi_config_err_t ret;
  uint8_t channel_number;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_get_serial_modem_configuration_call_retry_interval_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if ((ret = get_serial_channel_number (state_data, section_name, &channel_number)) != IPMI_CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_get_serial_modem_configuration_call_retry_interval (state_data->ipmi_ctx,
                                                                   channel_number,
                                                                   IPMI_GET_SERIAL_MODEM_PARAMETER,
                                                                   IPMI_SERIAL_MODEM_CONFIGURATION_NO_SET_SELECTOR,
                                                                   IPMI_SERIAL_MODEM_CONFIGURATION_NO_BLOCK_SELECTOR,
                                                                   obj_cmd_rs) < 0)
    {
      if (ipmi_config_param_errnum_is_non_fatal (state_data,
                                                 obj_cmd_rs,
                                                 &ret))
        rv = ret;

      if (rv == IPMI_CONFIG_ERR_FATAL_ERROR
	  || state_data->prog_data->args->common_args.debug)
        pstdout_fprintf (state_data->pstate,
                         stderr,
                         "ipmi_cmd_get_serial_modem_configuration_call_retry_interval: %s\n",
                         ipmi_ctx_errormsg (state_data->ipmi_ctx));

      goto cleanup;
    }

  if (FIID_OBJ_GET (obj_cmd_rs, "call_retry_interval", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'call_retry_interval': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  call_retry_interval = val;

  if (ipmi_config_section_update_keyvalue_output_unsigned_int (state_data,
                                                               kv,
                                                               call_retry_interval) < 0)
    return (IPMI_CONFIG_ERR_FATAL_ERROR);

  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static ipmi_config_err_t
call_retry_interval_commit (ipmi_config_state_data_t *state_data,
			    const char *section_name,
                            const struct ipmi_config_keyvalue *kv)
{
  fiid_obj_t obj_cmd_rs = NULL;
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;
  ipmi_config_err_t ret;
  uint8_t channel_number;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_set_serial_modem_configuration_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if ((ret = get_serial_channel_number (state_data, section_name, &channel_number)) != IPMI_CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_set_serial_modem_configuration_call_retry_interval (state_data->ipmi_ctx,
                                                                   channel_number,
                                                                   atoi (kv->value_input),
                                                                   obj_cmd_rs) < 0)
    {
      if (ipmi_config_param_errnum_is_non_fatal (state_data,
                                                 obj_cmd_rs,
                                                 &ret))
        rv = ret;

      if (rv == IPMI_CONFIG_ERR_FATAL_ERROR
	  || state_data->prog_data->args->common_args.debug)
        pstdout_fprintf (state_data->pstate,
                         stderr,
                         "ipmi_cmd_set_serial_modem_configuration_call_retry_interval: %s\n",
                         ipmi_ctx_errormsg (state_data->ipmi_ctx));

      goto cleanup;
    }

  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static ipmi_config_err_t
_get_ipmi_messaging_comm_settings (ipmi_config_state_data_t *state_data,
                                   const char *section_name,
                                   struct ipmi_messaging_comm_settings *cs)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;
  ipmi_config_err_t ret;
  uint8_t channel_number;

  assert (state_data);
  assert (section_name);
  assert (cs);

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_get_serial_modem_configuration_ipmi_messaging_comm_settings_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if ((ret = get_serial_channel_number (state_data, section_name, &channel_number)) != IPMI_CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_get_serial_modem_configuration_ipmi_messaging_comm_settings (state_data->ipmi_ctx,
                                                                            channel_number,
                                                                            IPMI_GET_SERIAL_MODEM_PARAMETER,
                                                                            IPMI_SERIAL_MODEM_CONFIGURATION_NO_SET_SELECTOR,
                                                                            IPMI_SERIAL_MODEM_CONFIGURATION_NO_BLOCK_SELECTOR,
                                                                            obj_cmd_rs) < 0)
    {
      if (ipmi_config_param_errnum_is_non_fatal (state_data,
                                                 obj_cmd_rs,
                                                 &ret))
        rv = ret;

      if (rv == IPMI_CONFIG_ERR_FATAL_ERROR
	  || state_data->prog_data->args->common_args.debug)
        pstdout_fprintf (state_data->pstate,
                         stderr,
                         "ipmi_cmd_get_serial_modem_configuration_ipmi_messaging_comm_settings: %s\n",
                         ipmi_ctx_errormsg (state_data->ipmi_ctx));

      goto cleanup;
    }

  if (FIID_OBJ_GET (obj_cmd_rs, "dtr_hangup", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'dtr_hangup': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  cs->dtr_hangup = val;

  if (FIID_OBJ_GET (obj_cmd_rs, "flow_control", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'flow_control': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  cs->flow_control = val;

  if (FIID_OBJ_GET (obj_cmd_rs, "bit_rate", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'bit_rate': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  cs->bit_rate = val;

  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static ipmi_config_err_t
_set_ipmi_messaging_comm_settings (ipmi_config_state_data_t *state_data,
                                   const char *section_name,
                                   struct ipmi_messaging_comm_settings *cs)
{
  fiid_obj_t obj_cmd_rs = NULL;
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;
  ipmi_config_err_t ret;
  uint8_t channel_number;

  assert (state_data);
  assert (section_name);
  assert (cs);

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_set_serial_modem_configuration_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if ((ret = get_serial_channel_number (state_data, section_name, &channel_number)) != IPMI_CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_set_serial_modem_configuration_ipmi_messaging_comm_settings (state_data->ipmi_ctx,
                                                                            channel_number,
                                                                            cs->dtr_hangup,
                                                                            cs->flow_control,
                                                                            cs->bit_rate,
                                                                            obj_cmd_rs) < 0)
    {
      if (ipmi_config_param_errnum_is_non_fatal (state_data,
                                                 obj_cmd_rs,
                                                 &ret))
        rv = ret;

      if (rv == IPMI_CONFIG_ERR_FATAL_ERROR
	  || state_data->prog_data->args->common_args.debug)
        pstdout_fprintf (state_data->pstate,
                         stderr,
                         "ipmi_cmd_set_serial_modem_configuration_ipmi_messaging_comm_settings: %s\n",
                         ipmi_ctx_errormsg (state_data->ipmi_ctx));

      goto cleanup;
    }

  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static ipmi_config_err_t
enable_dtr_hangup_checkout (ipmi_config_state_data_t *state_data,
			    const char *section_name,
                            struct ipmi_config_keyvalue *kv)
{
  struct ipmi_messaging_comm_settings cs;
  ipmi_config_err_t ret;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if ((ret = _get_ipmi_messaging_comm_settings (state_data, section_name, &cs)) != IPMI_CONFIG_ERR_SUCCESS)
    return (ret);

  if (ipmi_config_section_update_keyvalue_output (state_data,
                                                  kv,
                                                  cs.dtr_hangup ? "Yes" : "No") < 0)
    return (IPMI_CONFIG_ERR_FATAL_ERROR);

  return (IPMI_CONFIG_ERR_SUCCESS);
}

static ipmi_config_err_t
enable_dtr_hangup_commit (ipmi_config_state_data_t *state_data,
			  const char *section_name,
                          const struct ipmi_config_keyvalue *kv)
{
  struct ipmi_messaging_comm_settings cs;
  ipmi_config_err_t ret;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if ((ret = _get_ipmi_messaging_comm_settings (state_data, section_name, &cs)) != IPMI_CONFIG_ERR_SUCCESS)
    return (ret);

  cs.dtr_hangup = same (kv->value_input, "yes");

  return (_set_ipmi_messaging_comm_settings (state_data, section_name, &cs));
}

static ipmi_config_err_t
flow_control_checkout (ipmi_config_state_data_t *state_data,
		       const char *section_name,
                       struct ipmi_config_keyvalue *kv)
{
  struct ipmi_messaging_comm_settings cs;
  ipmi_config_err_t ret;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if ((ret = _get_ipmi_messaging_comm_settings (state_data, section_name, &cs)) != IPMI_CONFIG_ERR_SUCCESS)
    return (ret);

  if (ipmi_config_section_update_keyvalue_output (state_data,
                                                  kv,
                                                  flow_control_string (cs.flow_control)) < 0)
    return (IPMI_CONFIG_ERR_FATAL_ERROR);

  return (IPMI_CONFIG_ERR_SUCCESS);
}

static ipmi_config_err_t
flow_control_commit (ipmi_config_state_data_t *state_data,
		     const char *section_name,
                     const struct ipmi_config_keyvalue *kv)
{
  struct ipmi_messaging_comm_settings cs;
  ipmi_config_err_t ret;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if ((ret = _get_ipmi_messaging_comm_settings (state_data, section_name, &cs)) != IPMI_CONFIG_ERR_SUCCESS)
    return (ret);

  cs.flow_control = flow_control_number (kv->value_input);

  return (_set_ipmi_messaging_comm_settings (state_data, section_name, &cs));
}

static ipmi_config_err_t
bit_rate_checkout (ipmi_config_state_data_t *state_data,
		   const char *section_name,
                   struct ipmi_config_keyvalue *kv)
{
  struct ipmi_messaging_comm_settings cs;
  ipmi_config_err_t ret;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if ((ret = _get_ipmi_messaging_comm_settings (state_data, section_name, &cs)) != IPMI_CONFIG_ERR_SUCCESS)
    return (ret);

  if (ipmi_config_section_update_keyvalue_output (state_data,
                                                  kv,
                                                  bit_rate_string (cs.bit_rate)) < 0)
    return (IPMI_CONFIG_ERR_FATAL_ERROR);

  return (IPMI_CONFIG_ERR_SUCCESS);
}

static ipmi_config_err_t
bit_rate_commit (ipmi_config_state_data_t *state_data,
		 const char *section_name,
                 const struct ipmi_config_keyvalue *kv)
{
  struct ipmi_messaging_comm_settings cs;
  ipmi_config_err_t ret;

  assert (state_data);
  assert (section_name);
  assert (kv);

  if ((ret = _get_ipmi_messaging_comm_settings (state_data, section_name, &cs)) != IPMI_CONFIG_ERR_SUCCESS)
    return (ret);

  cs.bit_rate = bit_rate_number (kv->value_input);

  return (_set_ipmi_messaging_comm_settings (state_data, section_name, &cs));
}

struct ipmi_config_section *
ipmi_config_core_serial_conf_section_get (ipmi_config_state_data_t *state_data,
					  unsigned int config_flags,
					  int channel_index)
{
  struct ipmi_config_section *section = NULL;
  char *section_comment =
    "In the Serial_Conf section, typical serial communication configuration "
    "is setup.  Most users will only be interested in IPMI over LAN, "
    "therefore this section can generally be ignored.";
  char *section_name_base_str = "Serial_Conf";

  assert (state_data);

  /*
   * achu: section not checked out by default.
   */

  if (!state_data->prog_data->args->verbose_count)
    config_flags |= IPMI_CONFIG_DO_NOT_CHECKOUT;

  if (!(section = ipmi_config_section_multi_channel_create (state_data,
                                                            section_name_base_str,
                                                            section_comment,
                                                            NULL,
                                                            NULL,
                                                            config_flags,
                                                            channel_index,
                                                            state_data->serial_channel_numbers,
                                                            state_data->serial_channel_numbers_count)))
    goto cleanup;

  if (ipmi_config_section_add_key (state_data,
                                   section,
                                   "Enable_Basic_Mode",
                                   "Possible values: Yes/No",
                                   0,
                                   enable_basic_mode_checkout,
                                   enable_basic_mode_commit,
                                   yes_no_validate) < 0)
    goto cleanup;

  if (ipmi_config_section_add_key (state_data,
                                   section,
                                   "Enable_PPP_Mode",
                                   "Possible values: Yes/No",
                                   0,
                                   enable_ppp_mode_checkout,
                                   enable_ppp_mode_commit,
                                   yes_no_validate) < 0)
    goto cleanup;

  if (ipmi_config_section_add_key (state_data,
                                   section,
                                   "Enable_Terminal_Mode",
                                   "Possible values: Yes/No",
                                   0,
                                   enable_terminal_mode_checkout,
                                   enable_terminal_mode_commit,
                                   yes_no_validate) < 0)
    goto cleanup;

  if (ipmi_config_section_add_key (state_data,
                                   section,
                                   "Connect_Mode",
                                   "Possible values: Modem_Connect/Direct_Connect",
                                   0,
                                   connect_mode_checkout,
                                   connect_mode_commit,
                                   connect_mode_number_validate) < 0)
    goto cleanup;

  if (ipmi_config_section_add_key (state_data,
                                   section,
                                   "Page_Blackout_Interval",
                                   "Give a valid number",
                                   0,
                                   page_blackout_interval_checkout,
                                   page_blackout_interval_commit,
                                   number_range_one_byte_validate) < 0)
    goto cleanup;

  if (ipmi_config_section_add_key (state_data,
                                   section,
                                   "Call_Retry_Interval",
                                   "Give a valid number",
                                   0,
                                   call_retry_interval_checkout,
                                   call_retry_interval_commit,
                                   number_range_one_byte_validate) < 0)
    goto cleanup;

  /* achu: For backwards compatability to ipmi-config in 0.2.0 */
  if (ipmi_config_section_add_key (state_data,
                                   section,
                                   "Call_Retry_Time",
                                   "Give a valid number",
                                   IPMI_CONFIG_DO_NOT_CHECKOUT,
                                   call_retry_interval_checkout,
                                   call_retry_interval_commit,
                                   number_range_one_byte_validate) < 0)
    goto cleanup;

  if (ipmi_config_section_add_key (state_data,
                                   section,
                                   "Enable_DTR_Hangup",
                                   "Possible values: Yes/No",
                                   0,
                                   enable_dtr_hangup_checkout,
                                   enable_dtr_hangup_commit,
                                   yes_no_validate) < 0)
    goto cleanup;

  if (ipmi_config_section_add_key (state_data,
                                   section,
                                   "Flow_Control",
                                   "Possible values: No_Flow_Control/RTS_CTS/XON_XOFF",
                                   0,
                                   flow_control_checkout,
                                   flow_control_commit,
                                   flow_control_number_validate) < 0)
    goto cleanup;

  if (ipmi_config_section_add_key (state_data,
                                   section,
                                   "Bit_Rate",
                                   "Possible values: 9600/19200/38400/57600/115200",
                                   0,
                                   bit_rate_checkout,
                                   bit_rate_commit,
                                   bit_rate_number_validate) < 0)
    goto cleanup;

  return (section);

 cleanup:
  if (section)
    ipmi_config_section_destroy (section);
  return (NULL);
}
