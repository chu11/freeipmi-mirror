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
#include "bmc-config-map.h"
#include "bmc-config-validate.h"
#include "bmc-config-utils.h"

#include "config-common.h"
#include "config-section.h"
#include "config-validate.h"

#define KEY_NAME_ENABLE_BASIC_MODE      "Enable_Basic_Mode"
#define KEY_NAME_ENABLE_PPP_MODE        "Enable_PPP_Mode"
#define KEY_NAME_ENABLE_TERMINAL_MODE   "Enable_Terminal_Mode"
#define KEY_NAME_CONNECT_MODE           "Connect_Mode"
#define KEY_NAME_PAGE_BLACKOUT_INTERVAL "Page_Blackout_Interval"
#define KEY_NAME_CALL_RETRY_INTERVAL    "Call_Retry_Interval"
/* legacy */
#define KEY_NAME_CALL_RETRY_TIME        "Call_Retry_Time"
#define KEY_NAME_ENABLE_DTR_HANGUP      "Enable_DTR_Hangup"
#define KEY_NAME_FLOW_CONTROL           "Flow_Control"
#define KEY_NAME_BIT_RATE               "Bit_Rate"

static config_err_t 
_get_serial_conf_connection_mode(bmc_config_state_data_t *state_data, 
                                 int debug,
                                 uint8_t *basic_mode, 
                                 uint8_t *ppp_mode, 
                                 uint8_t *terminal_mode, 
                                 uint8_t *connect_mode)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_get_serial_modem_configuration_connection_mode_rs)))
    goto cleanup;

  if ((ret = get_serial_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_get_serial_modem_configuration_connection_mode (state_data->dev, 
							       channel_number, 
							       IPMI_GET_SERIAL_MODEM_PARAMETER, 
							       SET_SELECTOR, 
							       BLOCK_SELECTOR, 
							       obj_cmd_rs) < 0)
    {
      if (debug)
        fprintf(stderr,
                "ipmi_cmd_get_serial_modem_configuration_connection_mode: %s\n",
                ipmi_device_strerror(ipmi_device_errnum(state_data->dev)));
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }
  
  if (fiid_obj_get (obj_cmd_rs, "basic_mode", &val) < 0)
    goto cleanup;
  *basic_mode = val;
  
  if (fiid_obj_get (obj_cmd_rs, "ppp_mode", &val) < 0)
    goto cleanup;
  *ppp_mode = val;
  
  if (fiid_obj_get (obj_cmd_rs, "terminal_mode", &val) < 0)
    goto cleanup;
  *terminal_mode = val;
  
  if (fiid_obj_get (obj_cmd_rs, "connect_mode", &val) < 0)
    goto cleanup;
  *connect_mode = val;
  
  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}
   
static config_err_t 
_get_serial_conf_page_blackout_interval(bmc_config_state_data_t *state_data, 
                                        int debug,
                                        uint8_t *page_blackout_interval)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_get_serial_modem_configuration_page_blackout_interval_rs)))
    goto cleanup;

  if ((ret = get_serial_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_get_serial_modem_configuration_page_blackout_interval (state_data->dev, 
								      channel_number, 
								      IPMI_GET_SERIAL_MODEM_PARAMETER, 
								      SET_SELECTOR, 
								      BLOCK_SELECTOR, 
								      obj_cmd_rs) < 0)
    {
      if (debug)
        fprintf(stderr,
                "ipmi_cmd_get_serial_modem_configuration_page_blackout_interval: %s\n",
                ipmi_device_strerror(ipmi_device_errnum(state_data->dev)));
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }
  
  if (fiid_obj_get (obj_cmd_rs, "page_blackout_interval", &val) < 0)
    goto cleanup;
  *page_blackout_interval = val;
  
  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

static config_err_t 
_get_serial_conf_call_retry_interval(bmc_config_state_data_t *state_data, 
                                     int debug,
                                     uint8_t *call_retry_interval)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_get_serial_modem_configuration_call_retry_interval_rs)))
    goto cleanup;

  if ((ret = get_serial_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_get_serial_modem_configuration_call_retry_interval (state_data->dev, 
								   channel_number, 
								   IPMI_GET_SERIAL_MODEM_PARAMETER, 
								   SET_SELECTOR, 
								   BLOCK_SELECTOR, 
								   obj_cmd_rs) < 0)
    {
      if (debug)
        fprintf(stderr,
                "ipmi_cmd_get_serial_modem_configuration_call_retry_interval: %s\n",
                ipmi_device_strerror(ipmi_device_errnum(state_data->dev)));
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }
  
  if (fiid_obj_get (obj_cmd_rs, "call_retry_interval", &val) < 0)
    goto cleanup;
  *call_retry_interval = val;
  
  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

static config_err_t 
_get_serial_conf_ipmi_messaging_comm_settings(bmc_config_state_data_t *state_data, 
                                              int debug,
                                              uint8_t *dtr_hangup, 
                                              uint8_t *flow_control, 
                                              uint8_t *bit_rate)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_get_serial_modem_configuration_ipmi_messaging_comm_settings_rs)))
    goto cleanup;

  if ((ret = get_serial_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_get_serial_modem_configuration_ipmi_messaging_comm_settings (state_data->dev, 
									    channel_number, 
									    IPMI_GET_SERIAL_MODEM_PARAMETER, 
									    SET_SELECTOR, 
									    BLOCK_SELECTOR, 
									    obj_cmd_rs) < 0)
    {
      if (debug)
        fprintf(stderr,
                "ipmi_cmd_get_serial_modem_configuration_ipmi_messaging_comm_settings: %s\n",
                ipmi_device_strerror(ipmi_device_errnum(state_data->dev)));
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }
  
  if (fiid_obj_get (obj_cmd_rs, "dtr_hangup", &val) < 0)
    goto cleanup;
  *dtr_hangup = val;
  
  if (fiid_obj_get (obj_cmd_rs, "flow_control", &val) < 0)
    goto cleanup;
  *flow_control = val;
  
  if (fiid_obj_get (obj_cmd_rs, "bit_rate", &val) < 0)
    goto cleanup;
  *bit_rate = val;
  
  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

static config_err_t 
_set_serial_conf_connection_mode(bmc_config_state_data_t *state_data, 
                                 int debug,
                                 uint8_t basic_mode,
                                 uint8_t ppp_mode,
                                 uint8_t terminal_mode,
                                 uint8_t connect_mode)
{
  fiid_obj_t obj_cmd_rs = NULL;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;

  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_set_serial_modem_configuration_rs)))
    goto cleanup;

  if ((ret = get_serial_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_set_serial_modem_configuration_connection_mode (state_data->dev, 
							       channel_number, 
							       basic_mode,
							       ppp_mode,
							       terminal_mode,
							       connect_mode,
							       obj_cmd_rs) < 0)
    {
      if (debug)
        fprintf(stderr,
                "ipmi_cmd_set_serial_modem_configuration_connection_mode: %s\n",
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
_set_serial_conf_page_blackout_interval(bmc_config_state_data_t *state_data, 
                                        int debug,
                                        uint8_t page_blackout_interval)
{
  fiid_obj_t obj_cmd_rs = NULL;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_set_serial_modem_configuration_rs)))
    goto cleanup;

  if ((ret = get_serial_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_set_serial_modem_configuration_page_blackout_interval (state_data->dev, 
								      channel_number, 
								      page_blackout_interval, 
								      obj_cmd_rs) < 0)
    {
      if (debug)
        fprintf(stderr,
                "ipmi_cmd_set_serial_modem_configuration_page_blackout_interval: %s\n",
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
_set_serial_conf_call_retry_interval(bmc_config_state_data_t *state_data, 
                                     int debug,
                                     uint8_t call_retry_interval)
{
  fiid_obj_t obj_cmd_rs = NULL;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_set_serial_modem_configuration_rs)))
    goto cleanup;

  if ((ret = get_serial_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_set_serial_modem_configuration_call_retry_interval (state_data->dev, 
								   channel_number, 
								   call_retry_interval, 
								   obj_cmd_rs) < 0)
    {
      if (debug)
        fprintf(stderr,
                "ipmi_cmd_set_serial_modem_configuration_call_retry_interval: %s\n",
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
_set_serial_conf_ipmi_messaging_comm_settings(bmc_config_state_data_t *state_data, 
                                              int debug,
                                              uint8_t dtr_hangup, 
                                              uint8_t flow_control, 
                                              uint8_t bit_rate)
{
  fiid_obj_t obj_cmd_rs = NULL;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_set_serial_modem_configuration_rs)))
    goto cleanup;
  
  if ((ret = get_serial_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_set_serial_modem_configuration_ipmi_messaging_comm_settings (state_data->dev, 
									    channel_number, 
									    dtr_hangup, 
									    flow_control, 
									    bit_rate, 
									    obj_cmd_rs) < 0)
    {
      if (debug)
        fprintf(stderr,
                "ipmi_cmd_set_serial_modem_configuration_ipmi_messaging_comm_settings: %s\n",
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
_serial_conf_checkout(const char *section_name,
                      struct config_keyvalue *keyvalues,
                      int debug,
                      void *arg)
{
  bmc_config_state_data_t *state_data;
  struct config_keyvalue *kv;
  config_err_t rv = CONFIG_ERR_SUCCESS;
  config_err_t ret;
  uint8_t connection_mode_checkout = 0;
  uint8_t connection_mode_checkout_success = 0;
  uint8_t page_blackout_interval_checkout = 0;
  uint8_t page_blackout_interval_checkout_success = 0;
  uint8_t call_retry_interval_checkout = 0;
  uint8_t call_retry_interval_checkout_success = 0;
  uint8_t ipmi_messaging_comm_settings_checkout = 0;
  uint8_t ipmi_messaging_comm_settings_checkout_success = 0;
  uint8_t basic_mode; 
  uint8_t ppp_mode; 
  uint8_t terminal_mode; 
  uint8_t connect_mode;
  uint8_t page_blackout_interval;
  uint8_t call_retry_interval;
  uint8_t dtr_hangup;
  uint8_t flow_control;
  uint8_t bit_rate;
  char buf[BMC_CONFIG_BUFLEN];

  assert(section_name);
  assert(keyvalues);
  assert(arg);
  
  state_data = (bmc_config_state_data_t *)arg;
  
  /* two passes through the list is minimally slower but makes the
   * code far simpler
   */
  
  kv = keyvalues;
  while (kv)
    {
      assert(!kv->value_output);
      
      if (!strcasecmp(kv->key->key_name, KEY_NAME_ENABLE_BASIC_MODE)
          || !strcasecmp(kv->key->key_name, KEY_NAME_ENABLE_PPP_MODE)
          || !strcasecmp(kv->key->key_name, KEY_NAME_ENABLE_TERMINAL_MODE)
          || !strcasecmp(kv->key->key_name, KEY_NAME_CONNECT_MODE))
        connection_mode_checkout++;
      else if (!strcasecmp(kv->key->key_name, KEY_NAME_PAGE_BLACKOUT_INTERVAL))
        page_blackout_interval_checkout++;
      else if (!strcasecmp(kv->key->key_name, KEY_NAME_CALL_RETRY_INTERVAL))
        call_retry_interval_checkout++;
      else if (!strcasecmp(kv->key->key_name, KEY_NAME_CALL_RETRY_TIME))
        /* legacy */
        ;
      else if (!strcasecmp(kv->key->key_name, KEY_NAME_ENABLE_DTR_HANGUP)
               || !strcasecmp(kv->key->key_name, KEY_NAME_FLOW_CONTROL)
               || !strcasecmp(kv->key->key_name, KEY_NAME_BIT_RATE))
        ipmi_messaging_comm_settings_checkout++;
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

  if (connection_mode_checkout)
    {
      if ((ret = _get_serial_conf_connection_mode(state_data,
                                                  debug,
                                                  &basic_mode,
                                                  &ppp_mode,
                                                  &terminal_mode,
                                                  &connect_mode)) == CONFIG_ERR_FATAL_ERROR)
        return CONFIG_ERR_FATAL_ERROR;
      if (ret == CONFIG_ERR_SUCCESS)
        connection_mode_checkout_success++;
      else
        rv = ret;
    }

  if (page_blackout_interval_checkout)
    {
      if ((ret = _get_serial_conf_page_blackout_interval(state_data,
                                                         debug,
                                                         &page_blackout_interval)) == CONFIG_ERR_FATAL_ERROR)
        return CONFIG_ERR_FATAL_ERROR;
      if (ret == CONFIG_ERR_SUCCESS)
        page_blackout_interval_checkout_success++;
      else
        rv = ret;
    }

  if (call_retry_interval_checkout)
    {
      if ((ret = _get_serial_conf_call_retry_interval(state_data,
                                                      debug,
                                                      &call_retry_interval)) == CONFIG_ERR_FATAL_ERROR)
        return CONFIG_ERR_FATAL_ERROR;
      if (ret == CONFIG_ERR_SUCCESS)
        call_retry_interval_checkout_success++;
      else
        rv = ret;
    }

  if (ipmi_messaging_comm_settings_checkout)
    {
      if ((ret = _get_serial_conf_ipmi_messaging_comm_settings(state_data,
                                                               debug,
                                                               &dtr_hangup,
                                                               &flow_control,
                                                               &bit_rate)) == CONFIG_ERR_FATAL_ERROR)
        return CONFIG_ERR_FATAL_ERROR;
      if (ret == CONFIG_ERR_SUCCESS)
        ipmi_messaging_comm_settings_checkout_success++;
      else
        rv = ret;
    }

  kv = keyvalues;
  while (kv)
    {
      int temp = 0;

      assert(!kv->value_output);
      
      if (!strcasecmp(kv->key->key_name, KEY_NAME_ENABLE_BASIC_MODE)
          && connection_mode_checkout_success)
        temp = config_section_update_keyvalue(kv,
                                              NULL,
                                              basic_mode ? "Yes" : "No");
      else if (!strcasecmp(kv->key->key_name, KEY_NAME_ENABLE_PPP_MODE)
               && connection_mode_checkout_success)
        temp = config_section_update_keyvalue(kv,
                                              NULL,
                                              ppp_mode ? "Yes" : "No");
      else if (!strcasecmp(kv->key->key_name, KEY_NAME_ENABLE_TERMINAL_MODE)
               && connection_mode_checkout_success)
        temp = config_section_update_keyvalue(kv,
                                              NULL,
                                              terminal_mode ? "Yes" : "No");
      else if (!strcasecmp(kv->key->key_name, KEY_NAME_CONNECT_MODE)
               && connection_mode_checkout_success)
        temp = config_section_update_keyvalue(kv,
                                              NULL,
                                              connect_mode_string(connect_mode));
      else if (!strcasecmp(kv->key->key_name, KEY_NAME_PAGE_BLACKOUT_INTERVAL)
               && page_blackout_interval_checkout_success)
        {
          snprintf(buf, BMC_CONFIG_BUFLEN, "%d", page_blackout_interval);
          temp = config_section_update_keyvalue(kv,
                                                NULL,
                                                buf);
        }
      else if (!strcasecmp(kv->key->key_name, KEY_NAME_CALL_RETRY_INTERVAL)
               && call_retry_interval_checkout_success)
        {
          snprintf(buf, BMC_CONFIG_BUFLEN, "%d", call_retry_interval);
          temp = config_section_update_keyvalue(kv,
                                                NULL,
                                                buf);
        }
      else if (!strcasecmp(kv->key->key_name, KEY_NAME_CALL_RETRY_TIME))
        /* legacy */
        ;
      else if (!strcasecmp(kv->key->key_name, KEY_NAME_ENABLE_DTR_HANGUP)
               && ipmi_messaging_comm_settings_checkout_success)
        temp = config_section_update_keyvalue(kv,
                                              NULL,
                                              dtr_hangup ? "Yes" : "No");
      else if (!strcasecmp(kv->key->key_name, KEY_NAME_FLOW_CONTROL)
               && ipmi_messaging_comm_settings_checkout_success)
        temp = config_section_update_keyvalue(kv,
                                              NULL,
                                              flow_control_string(flow_control));
      else if (!strcasecmp(kv->key->key_name, KEY_NAME_BIT_RATE)
               && ipmi_messaging_comm_settings_checkout_success)
        temp = config_section_update_keyvalue(kv,
                                              NULL,
                                              bit_rate_string(bit_rate));
      else
        {
          if (debug)
            fprintf(stderr,
                    "ERROR: Unknown key '%s' in '%s'\n",
                    kv->key->key_name,
                    section_name);
        }

      if (temp < 0)
        {
          if (debug)
            fprintf(stderr, "config_section_update_keyvalue error\n");
          return CONFIG_ERR_FATAL_ERROR;
        }
      
      kv = kv->next;
    }

  return rv;
}

static config_err_t
_serial_conf_commit(const char *section_name,
                    struct config_keyvalue *keyvalues,
                    int debug,
                    void *arg)
{
  bmc_config_state_data_t *state_data;
  struct config_keyvalue *kv;
  config_err_t rv = CONFIG_ERR_SUCCESS;
  config_err_t ret;
  uint8_t connection_mode_checkout = 0;
  uint8_t connection_mode_checkout_success = 0;
  uint8_t ipmi_messaging_comm_settings_checkout = 0;
  uint8_t ipmi_messaging_comm_settings_checkout_success = 0;
  uint8_t basic_mode; 
  uint8_t ppp_mode; 
  uint8_t terminal_mode; 
  uint8_t connect_mode;
  uint8_t page_blackout_interval;
  uint8_t call_retry_interval;
  uint8_t dtr_hangup;
  uint8_t flow_control;
  uint8_t bit_rate;
  uint8_t connection_mode_commit = 0;
  uint8_t page_blackout_interval_commit = 0;
  uint8_t call_retry_interval_commit = 0;
  uint8_t ipmi_messaging_comm_settings_commit = 0;

  assert(section_name);
  assert(keyvalues);
  assert(arg);

  state_data = (bmc_config_state_data_t *)arg;

  /* two passes through the list is minimally slower but makes the
   * code far simpler
   */

  kv = keyvalues;
  while (kv)
    {
      assert(kv->value_input);

      if (!strcasecmp(kv->key->key_name, KEY_NAME_ENABLE_BASIC_MODE)
          || !strcasecmp(kv->key->key_name, KEY_NAME_ENABLE_PPP_MODE)
          || !strcasecmp(kv->key->key_name, KEY_NAME_ENABLE_TERMINAL_MODE)
          || !strcasecmp(kv->key->key_name, KEY_NAME_CONNECT_MODE))
        connection_mode_checkout++;
      else if (!strcasecmp(kv->key->key_name, KEY_NAME_PAGE_BLACKOUT_INTERVAL))
        /* no group checkout */
        ;
      else if (!strcasecmp(kv->key->key_name, KEY_NAME_CALL_RETRY_INTERVAL))
        /* no group checkout */
        ;
      else if (!strcasecmp(kv->key->key_name, KEY_NAME_CALL_RETRY_TIME))
        /* legacy */
        ;
      else if (!strcasecmp(kv->key->key_name, KEY_NAME_ENABLE_DTR_HANGUP)
               || !strcasecmp(kv->key->key_name, KEY_NAME_FLOW_CONTROL)
               || !strcasecmp(kv->key->key_name, KEY_NAME_BIT_RATE))
        ipmi_messaging_comm_settings_checkout++;
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

  if (connection_mode_checkout)
    {
      if ((ret = _get_serial_conf_connection_mode(state_data,
                                                  debug,
                                                  &basic_mode,
                                                  &ppp_mode,
                                                  &terminal_mode,
                                                  &connect_mode)) == CONFIG_ERR_FATAL_ERROR)
        return CONFIG_ERR_FATAL_ERROR;
      if (ret == CONFIG_ERR_SUCCESS)
        connection_mode_checkout_success++;
      else
        rv = ret;
    }

  if (ipmi_messaging_comm_settings_checkout)
    {
      if ((ret = _get_serial_conf_ipmi_messaging_comm_settings(state_data,
                                                               debug,
                                                               &dtr_hangup,
                                                               &flow_control,
                                                               &bit_rate)) == CONFIG_ERR_FATAL_ERROR)
        return CONFIG_ERR_FATAL_ERROR;
      if (ret == CONFIG_ERR_SUCCESS)
        ipmi_messaging_comm_settings_checkout_success++;
      else
        rv = ret;
    }


  kv = keyvalues;
  while (kv)
    {
      assert(kv->value_input);
      
      if (!strcasecmp(kv->key->key_name, KEY_NAME_ENABLE_BASIC_MODE)
          && connection_mode_checkout_success)
        {
          basic_mode = same (kv->value_input, "yes");
          connection_mode_commit++;
        }
      else if (!strcasecmp(kv->key->key_name, KEY_NAME_ENABLE_PPP_MODE)
               && connection_mode_checkout_success)
        {
          ppp_mode = same (kv->value_input, "yes");
          connection_mode_commit++;
        }
      else if (!strcasecmp(kv->key->key_name, KEY_NAME_ENABLE_TERMINAL_MODE)
               && connection_mode_checkout_success)
        {
          terminal_mode = same (kv->value_input, "yes");
          connection_mode_commit++;
        }
      else if (!strcasecmp(kv->key->key_name, KEY_NAME_CONNECT_MODE)
               && connection_mode_checkout_success)
        {
          connect_mode = connect_mode_number (kv->value_input);
          connection_mode_commit++;
        }
      else if (!strcasecmp(kv->key->key_name, KEY_NAME_PAGE_BLACKOUT_INTERVAL))
        {
          page_blackout_interval = atoi(kv->value_input);
          page_blackout_interval_commit++;
        }
      else if (!strcasecmp(kv->key->key_name, KEY_NAME_CALL_RETRY_INTERVAL))
        {
          /* checked earlier for validity */
          call_retry_interval = atoi(kv->value_input);
          call_retry_interval_commit++;
        }
      else if (!strcasecmp(kv->key->key_name, KEY_NAME_CALL_RETRY_TIME))
        /* legacy */
        ;
      else if (!strcasecmp(kv->key->key_name, KEY_NAME_ENABLE_DTR_HANGUP)
               && ipmi_messaging_comm_settings_checkout_success)
        {
          dtr_hangup = same (kv->value_input, "yes");
          ipmi_messaging_comm_settings_commit++;
        }
      else if (!strcasecmp(kv->key->key_name, KEY_NAME_FLOW_CONTROL)
               && ipmi_messaging_comm_settings_checkout_success)
        {
          flow_control = flow_control_number(kv->value_input);
          ipmi_messaging_comm_settings_commit++;
        }
      else if (!strcasecmp(kv->key->key_name, KEY_NAME_BIT_RATE)
               && ipmi_messaging_comm_settings_checkout_success)
        {
          bit_rate = bit_rate_number(kv->value_input);
          ipmi_messaging_comm_settings_commit++;
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

  if (connection_mode_commit)
    {
      if ((ret = _set_serial_conf_connection_mode(state_data,
                                                  debug,
                                                  basic_mode,
                                                  ppp_mode,
                                                  terminal_mode,
                                                  connect_mode)) == CONFIG_ERR_FATAL_ERROR)
        return CONFIG_ERR_FATAL_ERROR;
      if (ret != CONFIG_ERR_SUCCESS)
        rv = ret;
    }

  if (page_blackout_interval_commit)
    {
      if ((ret = _set_serial_conf_page_blackout_interval(state_data,
                                                         debug,
                                                         page_blackout_interval)) == CONFIG_ERR_FATAL_ERROR)
        return CONFIG_ERR_FATAL_ERROR;
      if (ret != CONFIG_ERR_SUCCESS)
        rv = ret;
    }

  if (call_retry_interval_commit)
    {
      if ((ret = _set_serial_conf_call_retry_interval(state_data,
                                                      debug,
                                                      call_retry_interval)) == CONFIG_ERR_FATAL_ERROR)
        return CONFIG_ERR_FATAL_ERROR;
      if (ret != CONFIG_ERR_SUCCESS)
        rv = ret;
    }

  if (ipmi_messaging_comm_settings_commit)
    {
      if ((ret = _set_serial_conf_ipmi_messaging_comm_settings(state_data,
                                                               debug,
                                                               dtr_hangup,
                                                               flow_control,
                                                               bit_rate)) == CONFIG_ERR_FATAL_ERROR)
        return CONFIG_ERR_FATAL_ERROR;
      if (ret != CONFIG_ERR_SUCCESS)
        rv = ret;
    }

  return rv;
}

struct config_section *
bmc_config_serial_conf_section_get(bmc_config_state_data_t *state_data)
{
  struct config_section *serial_conf_section = NULL;
  char *section_comment = 
    "In the Serial_Conf section, typical serial communication configuration "
    "is setup.  Most users will only be interested in IPMI over LAN, "
    "therefore this section can generally be ignored.";

  if (!(serial_conf_section = config_section_create("Serial_Conf", 
                                                    "Serial_Conf", 
                                                    section_comment,
                                                    0,
                                                    _serial_conf_checkout,
                                                    _serial_conf_commit)))
    goto cleanup;


  if (config_section_add_key (serial_conf_section,
                              KEY_NAME_ENABLE_BASIC_MODE,
                              "Possible values: Yes/No",
                              0,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (serial_conf_section,
                              KEY_NAME_ENABLE_PPP_MODE,
                              "Possible values: Yes/No",
                              0,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (serial_conf_section,
                              KEY_NAME_ENABLE_TERMINAL_MODE,
                              "Possible values: Yes/No",
                              0,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (serial_conf_section,
                              KEY_NAME_CONNECT_MODE,
                              "Possible values: Modem_Connect/Direct_Mode",
                              0,
                              connect_mode_number_validate) < 0)
    goto cleanup;

  if (config_section_add_key (serial_conf_section,
                              KEY_NAME_PAGE_BLACKOUT_INTERVAL,
                              "Give a valid number",
                              0,
                              config_number_range_one_byte) < 0)
    goto cleanup;

  if (config_section_add_key (serial_conf_section,
                              KEY_NAME_CALL_RETRY_INTERVAL,
                              "Give a valid number",
                              0,
                              config_number_range_one_byte) < 0)
    goto cleanup;

  /* achu: For backwards compatability to bmc-config in 0.2.0 */
  if (config_section_add_key (serial_conf_section,
                              KEY_NAME_CALL_RETRY_TIME,
                              "Give a valid number",
                              CONFIG_DO_NOT_CHECKOUT,
                              config_number_range_one_byte) < 0)
    goto cleanup;

  if (config_section_add_key (serial_conf_section,
                              KEY_NAME_ENABLE_DTR_HANGUP,
                              "Possible values: Yes/No",
                              0,
                              config_yes_no_validate) < 0)
    goto cleanup;

  if (config_section_add_key (serial_conf_section,
                              KEY_NAME_FLOW_CONTROL,
                              "Possible values: No_Flow_Control/RTS_CTS/XON_XOFF",
                              0,
                              flow_control_number_validate) < 0)
    goto cleanup;

  if (config_section_add_key (serial_conf_section,
                              KEY_NAME_BIT_RATE,
                              "Possible values: 9600/19200/38400/57600/115200",
                              0,
                              bit_rate_number_validate) < 0)
    goto cleanup;

  return serial_conf_section;

 cleanup:
  if (serial_conf_section)
    config_section_destroy(serial_conf_section);
  return NULL;
}
