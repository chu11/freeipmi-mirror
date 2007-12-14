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
#include "bmc-config-map.h"
#include "bmc-config-validate.h"
#include "bmc-config-utils.h"

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

static config_err_t 
_get_connection_mode (bmc_config_state_data_t *state_data, 
                      struct connection_mode *cm)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;
  
  assert(state_data);
  assert(cm);
  
  if (!(obj_cmd_rs = Fiid_obj_create(tmpl_cmd_get_serial_modem_configuration_connection_mode_rs)))
    goto cleanup;

  if ((ret = get_serial_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_get_serial_modem_configuration_connection_mode (state_data->ipmi_ctx, 
							       channel_number, 
							       IPMI_GET_SERIAL_MODEM_PARAMETER, 
							       SET_SELECTOR, 
							       BLOCK_SELECTOR, 
							       obj_cmd_rs) < 0)
    {
      if (state_data->prog_data->args->common.flags & IPMI_FLAGS_DEBUG_DUMP)
        fprintf(stderr,
                "ipmi_cmd_get_serial_modem_configuration_connection_mode: %s\n",
                ipmi_ctx_strerror(ipmi_ctx_errnum(state_data->ipmi_ctx)));
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }
  
  if (Fiid_obj_get (obj_cmd_rs, "basic_mode", &val) < 0)
    goto cleanup;
  cm->basic_mode = val;
  
  if (Fiid_obj_get (obj_cmd_rs, "ppp_mode", &val) < 0)
    goto cleanup;
  cm->ppp_mode = val;
  
  if (Fiid_obj_get (obj_cmd_rs, "terminal_mode", &val) < 0)
    goto cleanup;
  cm->terminal_mode = val;
  
  if (Fiid_obj_get (obj_cmd_rs, "connect_mode", &val) < 0)
    goto cleanup;
  cm->connect_mode = val;
  
  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  Fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

static config_err_t 
_set_connection_mode (bmc_config_state_data_t *state_data, 
                      struct connection_mode *cm)
{
  fiid_obj_t obj_cmd_rs = NULL;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;
  
  assert(state_data);
  assert(cm);

  if (!(obj_cmd_rs = Fiid_obj_create(tmpl_cmd_set_serial_modem_configuration_rs)))
    goto cleanup;
  
  if ((ret = get_serial_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
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
      if (state_data->prog_data->args->common.flags & IPMI_FLAGS_DEBUG_DUMP)
        fprintf(stderr,
                "ipmi_cmd_set_serial_modem_configuration_connection_mode: %s\n",
                ipmi_ctx_strerror(ipmi_ctx_errnum(state_data->ipmi_ctx)));
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }
   
  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  Fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

static config_err_t
enable_basic_mode_checkout (const char *section_name,
			    struct config_keyvalue *kv,
                            void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  struct connection_mode cm;
  config_err_t ret;

  if ((ret = _get_connection_mode (state_data, &cm)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (config_section_update_keyvalue_output(kv, cm.basic_mode ? "Yes" : "No") < 0)
    return CONFIG_ERR_FATAL_ERROR;
  
  return CONFIG_ERR_SUCCESS;
}

static config_err_t
enable_basic_mode_commit (const char *section_name,
			  const struct config_keyvalue *kv,
                          void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  struct connection_mode cm;
  config_err_t ret;

  if ((ret = _get_connection_mode (state_data, &cm)) != CONFIG_ERR_SUCCESS)
    return ret;

  cm.basic_mode = same (kv->value_input, "yes");

  return _set_connection_mode (state_data, &cm);
}

static config_err_t
enable_ppp_mode_checkout (const char *section_name,
			  struct config_keyvalue *kv,
                          void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  struct connection_mode cm;
  config_err_t ret;

  if ((ret = _get_connection_mode (state_data, &cm)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (config_section_update_keyvalue_output(kv, cm.ppp_mode ? "Yes" : "No") < 0)
    return CONFIG_ERR_FATAL_ERROR;

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
enable_ppp_mode_commit (const char *section_name,
			const struct config_keyvalue *kv,
                        void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  struct connection_mode cm;
  config_err_t ret;

  if ((ret = _get_connection_mode (state_data, &cm)) != CONFIG_ERR_SUCCESS)
    return ret;

  cm.ppp_mode = same (kv->value_input, "yes");

  return _set_connection_mode (state_data, &cm);
}

static config_err_t
enable_terminal_mode_checkout (const char *section_name,
			       struct config_keyvalue *kv,
                               void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  struct connection_mode cm;
  config_err_t ret;

  if ((ret = _get_connection_mode (state_data, &cm)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (config_section_update_keyvalue_output(kv, cm.terminal_mode ? "Yes" : "No") < 0)
    return CONFIG_ERR_FATAL_ERROR;

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
enable_terminal_mode_commit (const char *section_name,
			     const struct config_keyvalue *kv,
                             void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  struct connection_mode cm;
  config_err_t ret;

  if ((ret = _get_connection_mode (state_data, &cm)) != CONFIG_ERR_SUCCESS)
    return ret;

  cm.terminal_mode = same (kv->value_input, "yes");

  return _set_connection_mode (state_data, &cm);
}

static config_err_t
connect_mode_checkout (const char *section_name,
		       struct config_keyvalue *kv,
                       void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  struct connection_mode cm;
  config_err_t ret;

  if ((ret = _get_connection_mode (state_data, &cm)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (config_section_update_keyvalue_output(kv, connect_mode_string (cm.connect_mode)) < 0)
    return CONFIG_ERR_FATAL_ERROR;

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
connect_mode_commit (const char *section_name,
		     const struct config_keyvalue *kv,
                     void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  struct connection_mode cm;
  config_err_t ret;

  if ((ret = _get_connection_mode (state_data, &cm)) != CONFIG_ERR_SUCCESS)
    return ret;

  cm.connect_mode = connect_mode_number (kv->value_input);
  
  return _set_connection_mode (state_data, &cm);
}

static config_err_t
page_blackout_interval_checkout (const char *section_name,
				 struct config_keyvalue *kv,
                                 void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;
  
  if (!(obj_cmd_rs = Fiid_obj_create(tmpl_cmd_get_serial_modem_configuration_page_blackout_interval_rs)))
    goto cleanup;

  if ((ret = get_serial_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_get_serial_modem_configuration_page_blackout_interval (state_data->ipmi_ctx, 
								      channel_number, 
								      IPMI_GET_SERIAL_MODEM_PARAMETER, 
								      SET_SELECTOR, 
								      BLOCK_SELECTOR, 
								      obj_cmd_rs) < 0)
    {
      if (state_data->prog_data->args->common.flags & IPMI_FLAGS_DEBUG_DUMP)
        fprintf(stderr,
                "ipmi_cmd_get_serial_modem_configuration_page_blackout_interval: %s\n",
                ipmi_ctx_strerror(ipmi_ctx_errnum(state_data->ipmi_ctx)));
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }
  
  if (Fiid_obj_get (obj_cmd_rs, "page_blackout_interval", &val) < 0)
    goto cleanup;

  if (config_section_update_keyvalue_output_int(kv, val) < 0)
    return CONFIG_ERR_FATAL_ERROR;
  
  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  Fiid_obj_destroy(obj_cmd_rs);
  return (rv);

}

static config_err_t
page_blackout_interval_commit (const char *section_name,
			       const struct config_keyvalue *kv,
                               void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  fiid_obj_t obj_cmd_rs = NULL;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;
  
  if (!(obj_cmd_rs = Fiid_obj_create(tmpl_cmd_set_serial_modem_configuration_rs)))
    goto cleanup;

  if ((ret = get_serial_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_set_serial_modem_configuration_page_blackout_interval (state_data->ipmi_ctx, 
								      channel_number, 
								      atoi (kv->value_input), 
								      obj_cmd_rs) < 0)
    {
      if (state_data->prog_data->args->common.flags & IPMI_FLAGS_DEBUG_DUMP)
        fprintf(stderr,
                "ipmi_cmd_set_serial_modem_configuration_page_blackout_interval: %s\n",
                ipmi_ctx_strerror(ipmi_ctx_errnum(state_data->ipmi_ctx)));
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }
  
  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  Fiid_obj_destroy(obj_cmd_rs);
  return (rv);	
}

static config_err_t
call_retry_interval_checkout (const char *section_name,
			      struct config_keyvalue *kv,
                              void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;
  
  if (!(obj_cmd_rs = Fiid_obj_create(tmpl_cmd_get_serial_modem_configuration_call_retry_interval_rs)))
    goto cleanup;

  if ((ret = get_serial_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_get_serial_modem_configuration_call_retry_interval (state_data->ipmi_ctx, 
								   channel_number, 
								   IPMI_GET_SERIAL_MODEM_PARAMETER, 
								   SET_SELECTOR, 
								   BLOCK_SELECTOR, 
								   obj_cmd_rs) < 0)
    {
      if (state_data->prog_data->args->common.flags & IPMI_FLAGS_DEBUG_DUMP)
        fprintf(stderr,
                "ipmi_cmd_get_serial_modem_configuration_call_retry_interval: %s\n",
                ipmi_ctx_strerror(ipmi_ctx_errnum(state_data->ipmi_ctx)));
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }
  
  if (Fiid_obj_get (obj_cmd_rs, "call_retry_interval", &val) < 0)
    goto cleanup;

  if (config_section_update_keyvalue_output_int(kv, val) < 0)
    return CONFIG_ERR_FATAL_ERROR;
  
  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  Fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

static config_err_t
call_retry_interval_commit (const char *section_name,
			    const struct config_keyvalue *kv,
                            void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  fiid_obj_t obj_cmd_rs = NULL;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;
  
  if (!(obj_cmd_rs = Fiid_obj_create(tmpl_cmd_set_serial_modem_configuration_rs)))
    goto cleanup;

  if ((ret = get_serial_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_set_serial_modem_configuration_call_retry_interval (state_data->ipmi_ctx, 
								   channel_number, 
								   atoi (kv->value_input), 
								   obj_cmd_rs) < 0)
    {
      if (state_data->prog_data->args->common.flags & IPMI_FLAGS_DEBUG_DUMP)
        fprintf(stderr,
                "ipmi_cmd_set_serial_modem_configuration_call_retry_interval: %s\n",
                ipmi_ctx_strerror(ipmi_ctx_errnum(state_data->ipmi_ctx)));
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }
  
  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  Fiid_obj_destroy(obj_cmd_rs);
  return (rv);	
}

static config_err_t 
_get_ipmi_messaging_comm_settings (bmc_config_state_data_t *state_data, 
                                   struct ipmi_messaging_comm_settings *cs)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;

  assert(state_data);
  assert(cs);
  
  if (!(obj_cmd_rs = Fiid_obj_create(tmpl_cmd_get_serial_modem_configuration_ipmi_messaging_comm_settings_rs)))
    goto cleanup;

  if ((ret = get_serial_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_get_serial_modem_configuration_ipmi_messaging_comm_settings (state_data->ipmi_ctx, 
									    channel_number, 
									    IPMI_GET_SERIAL_MODEM_PARAMETER, 
									    SET_SELECTOR, 
									    BLOCK_SELECTOR, 
									    obj_cmd_rs) < 0)
    {
      if (state_data->prog_data->args->common.flags & IPMI_FLAGS_DEBUG_DUMP)
        fprintf(stderr,
                "ipmi_cmd_get_serial_modem_configuration_ipmi_messaging_comm_settings: %s\n",
                ipmi_ctx_strerror(ipmi_ctx_errnum(state_data->ipmi_ctx)));
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }
  
  if (Fiid_obj_get (obj_cmd_rs, "dtr_hangup", &val) < 0)
    goto cleanup;
  cs->dtr_hangup = val;
  
  if (Fiid_obj_get (obj_cmd_rs, "flow_control", &val) < 0)
    goto cleanup;
  cs->flow_control = val;
  
  if (Fiid_obj_get (obj_cmd_rs, "bit_rate", &val) < 0)
    goto cleanup;
  cs->bit_rate = val;
  
  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  Fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

static config_err_t 
_set_ipmi_messaging_comm_settings (bmc_config_state_data_t *state_data, 
                                   struct ipmi_messaging_comm_settings *cs)
{
  fiid_obj_t obj_cmd_rs = NULL;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;
  
  assert(state_data);
  assert(cs);

  if (!(obj_cmd_rs = Fiid_obj_create(tmpl_cmd_set_serial_modem_configuration_rs)))
    goto cleanup;

  if ((ret = get_serial_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
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
      if (state_data->prog_data->args->common.flags & IPMI_FLAGS_DEBUG_DUMP)
        fprintf(stderr,
                "ipmi_cmd_set_serial_modem_configuration_ipmi_messaging_comm_settings: %s\n",
                ipmi_ctx_strerror(ipmi_ctx_errnum(state_data->ipmi_ctx)));
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }
  
  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  Fiid_obj_destroy(obj_cmd_rs);
  return (rv);	
}

static config_err_t
enable_dtr_hangup_checkout (const char *section_name,
			    struct config_keyvalue *kv,
                            void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  struct ipmi_messaging_comm_settings cs;
  config_err_t ret;
  
  if ((ret = _get_ipmi_messaging_comm_settings (state_data, &cs)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (config_section_update_keyvalue_output(kv, cs.dtr_hangup ? "Yes" : "No") < 0)
    return CONFIG_ERR_FATAL_ERROR;

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
enable_dtr_hangup_commit (const char *section_name,
			  const struct config_keyvalue *kv,
                          void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  struct ipmi_messaging_comm_settings cs;
  config_err_t ret;

  if ((ret = _get_ipmi_messaging_comm_settings (state_data, &cs)) != CONFIG_ERR_SUCCESS)
    return ret;

  cs.dtr_hangup = same (kv->value_input, "yes");

  return _set_ipmi_messaging_comm_settings (state_data, &cs);
}

static config_err_t
flow_control_checkout (const char *section_name,
		       struct config_keyvalue *kv,
                       void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  struct ipmi_messaging_comm_settings cs;
  config_err_t ret;
  
  if ((ret = _get_ipmi_messaging_comm_settings (state_data, &cs)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (config_section_update_keyvalue_output(kv, flow_control_string (cs.flow_control)) < 0)
    return CONFIG_ERR_FATAL_ERROR;

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
flow_control_commit (const char *section_name,
		     const struct config_keyvalue *kv,
                     void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  struct ipmi_messaging_comm_settings cs;
  config_err_t ret;

  if ((ret = _get_ipmi_messaging_comm_settings (state_data, &cs)) != CONFIG_ERR_SUCCESS)
    return ret;

  cs.flow_control = flow_control_number (kv->value_input);

  return _set_ipmi_messaging_comm_settings (state_data, &cs);
}

static config_err_t
bit_rate_checkout (const char *section_name,
		   struct config_keyvalue *kv,
                   void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  struct ipmi_messaging_comm_settings cs;
  config_err_t ret;
  
  if ((ret = _get_ipmi_messaging_comm_settings (state_data, &cs)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (config_section_update_keyvalue_output(kv, bit_rate_string (cs.bit_rate)) < 0)
    return CONFIG_ERR_FATAL_ERROR;

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
bit_rate_commit (const char *section_name,
		 const struct config_keyvalue *kv,
                 void *arg)
{
  bmc_config_state_data_t *state_data = (bmc_config_state_data_t *)arg;
  struct ipmi_messaging_comm_settings cs;
  config_err_t ret;

  if ((ret = _get_ipmi_messaging_comm_settings (state_data, &cs)) != CONFIG_ERR_SUCCESS)
    return ret;

  cs.bit_rate = bit_rate_number (kv->value_input);

  return _set_ipmi_messaging_comm_settings (state_data, &cs);
}

struct config_section *
bmc_config_serial_conf_section_get (bmc_config_state_data_t *state_data)
{
  struct config_section *bmc_serial_conf_section = NULL;
  char *section_comment = 
    "In the Serial_Conf section, typical serial communication configuration "
    "is setup.  Most users will only be interested in IPMI over LAN, "
    "therefore this section can generally be ignored.";

  if (!(bmc_serial_conf_section = config_section_create("Serial_Conf", 
                                                        "Serial_Conf", 
                                                        section_comment,
                                                        0)))
    goto cleanup;

  if (config_section_add_key (bmc_serial_conf_section,
                              "Enable_Basic_Mode",
                              "Possible values: Yes/No",
                              0,
                              enable_basic_mode_checkout,
                              enable_basic_mode_commit,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (bmc_serial_conf_section,
                              "Enable_PPP_Mode",
                              "Possible values: Yes/No",
                              0,
                              enable_ppp_mode_checkout,
                              enable_ppp_mode_commit,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (bmc_serial_conf_section,
                              "Enable_Terminal_Mode",
                              "Possible values: Yes/No",
                              0,
                              enable_terminal_mode_checkout,
                              enable_terminal_mode_commit,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (bmc_serial_conf_section,
                              "Connect_Mode",
                              "Possible values: Modem_Connect/Direct_Connect",
                              0,
                              connect_mode_checkout,
                              connect_mode_commit,
                              connect_mode_number_validate) < 0)
    goto cleanup;

  if (config_section_add_key (bmc_serial_conf_section,
                              "Page_Blackout_Interval",
                              "Give a valid number",
                              0,
                              page_blackout_interval_checkout,
                              page_blackout_interval_commit,
                              config_number_range_one_byte) < 0)
    goto cleanup;

  if (config_section_add_key (bmc_serial_conf_section,
                              "Call_Retry_Interval",
                              "Give a valid number",
                              0,
                              call_retry_interval_checkout,
                              call_retry_interval_commit,
                              config_number_range_one_byte) < 0)
    goto cleanup;

  /* achu: For backwards compatability to bmc-config in 0.2.0 */
  if (config_section_add_key (bmc_serial_conf_section,
                              "Call_Retry_Time",
                              "Give a valid number",
                              CONFIG_DO_NOT_CHECKOUT,
                              call_retry_interval_checkout,
                              call_retry_interval_commit,
                              config_number_range_one_byte) < 0)
    goto cleanup;

  if (config_section_add_key (bmc_serial_conf_section,
                              "Enable_DTR_Hangup",
                              "Possible values: Yes/No",
                              0,
                              enable_dtr_hangup_checkout,
                              enable_dtr_hangup_commit,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (bmc_serial_conf_section,
                              "Flow_Control",
                              "Possible values: No_Flow_Control/RTS_CTS/XON_XOFF",
                              0,
                              flow_control_checkout,
                              flow_control_commit,
                              flow_control_number_validate) < 0)
    goto cleanup;

  if (config_section_add_key (bmc_serial_conf_section,
                              "Bit_Rate",
                              "Possible values: 9600/19200/38400/57600/115200",
                              0,
                              bit_rate_checkout,
                              bit_rate_commit,
                              bit_rate_number_validate) < 0)
    goto cleanup;

  return bmc_serial_conf_section;

 cleanup:
  if (bmc_serial_conf_section)
    config_section_destroy(bmc_serial_conf_section);
  return NULL;
}
