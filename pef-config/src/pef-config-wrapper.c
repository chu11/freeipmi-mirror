#if HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#if HAVE_UNISTD_H
#include <unistd.h>
#endif	/* HAVE_UNISTD_H */
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <sys/resource.h>
#if TIME_WITH_SYS_TIME
#include <sys/time.h>
#include <time.h>
#else /* !TIME_WITH_SYS_TIME */
#if HAVE_SYS_TIME_H
#include <sys/time.h>
#else /* !HAVE_SYS_TIME_H */
#include <time.h>
#endif /* !HAVE_SYS_TIME_H */
#endif /* !TIME_WITH_SYS_TIME */
#include <assert.h>

#include <freeipmi/freeipmi.h>
#include <freeipmi/udm/udm.h>

#include "freeipmi-portability.h"

#include "pef-config.h"
#include "pef-config-map.h"
#include "pef-config-utils.h"
#include "pef-config-wrapper.h"

config_err_t
get_bmc_destination_type(pef_config_state_data_t *state_data,
                         uint8_t destination_selector,
                         uint8_t *alert_destination_type,
                         uint8_t *alert_acknowledge,
                         uint8_t *alert_acknowledge_timeout,
                         uint8_t *alert_retries)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;

  assert(state_data);
  assert(destination_selector);

  if (!(obj_cmd_rs = Fiid_obj_create(tmpl_cmd_get_lan_configuration_parameters_destination_type_rs)))
    goto cleanup;

  if ((ret = get_lan_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_get_lan_configuration_parameters_destination_type (state_data->dev,
                                                                  channel_number,
                                                                  IPMI_GET_LAN_PARAMETER,
                                                                  destination_selector,
                                                                  BLOCK_SELECTOR,
                                                                  obj_cmd_rs) < 0)
    {
      if (state_data->prog_data->args->common.flags & IPMI_FLAGS_DEBUG_DUMP)
        fprintf(stderr,
                "ipmi_cmd_get_lan_configuration_parameters_destination_type: %s\n",
                ipmi_device_strerror(ipmi_device_errnum(state_data->dev)));
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  if (Fiid_obj_get (obj_cmd_rs, "destination_type", &val) < 0)
    goto cleanup;
  *alert_destination_type = val;

  if (Fiid_obj_get (obj_cmd_rs, "alert_acknowledge", &val) < 0)
    goto cleanup;
  *alert_acknowledge = val;

  if (Fiid_obj_get (obj_cmd_rs, "alert_acknowledge_timeout", &val) < 0)
    goto cleanup;
  *alert_acknowledge_timeout = val;

  if (Fiid_obj_get (obj_cmd_rs, "retries", &val) < 0)
    goto cleanup;
  *alert_retries = val;

  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  Fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

config_err_t
set_bmc_destination_type(pef_config_state_data_t *state_data,
                         uint8_t destination_selector,
                         uint8_t alert_destination_type,
                         uint8_t alert_acknowledge,
                         uint8_t alert_acknowledge_timeout,
                         uint8_t alert_retries)
{
  fiid_obj_t obj_cmd_rs = NULL;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;

  assert(state_data);
  assert(destination_selector);

  if (!(obj_cmd_rs = Fiid_obj_create(tmpl_cmd_set_lan_configuration_parameters_rs)))
    goto cleanup;

  if ((ret = get_lan_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_set_lan_configuration_parameters_destination_type (state_data->dev,
                                                                  channel_number,
                                                                  destination_selector,
                                                                  alert_destination_type,
                                                                  alert_acknowledge,
                                                                  alert_acknowledge_timeout,
                                                                  alert_retries,
                                                                  obj_cmd_rs) < 0)
    {
      if (state_data->prog_data->args->common.flags & IPMI_FLAGS_DEBUG_DUMP)
        fprintf(stderr,
                "ipmi_cmd_set_lan_configuration_parameters_destination_type: %s\n",
                ipmi_device_strerror(ipmi_device_errnum(state_data->dev)));
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  Fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

config_err_t
get_bmc_destination_addresses(pef_config_state_data_t *state_data,
                              uint8_t destination_selector,
                              uint8_t *alert_gateway,
                              char *alert_ip_address,
                              unsigned int alert_ip_address_len,
                              char *alert_mac_address,
                              unsigned int alert_mac_address_len)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  uint8_t alert_ip_address_bytes[4];
  uint8_t alert_mac_address_bytes[6];
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;

  assert(state_data);
  assert(destination_selector);

  if (!(obj_cmd_rs = Fiid_obj_create(tmpl_cmd_get_lan_configuration_parameters_destination_addresses_rs)))
    goto cleanup;

  if ((ret = get_lan_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_get_lan_configuration_parameters_destination_addresses (state_data->dev,
                                                                       channel_number,
                                                                       IPMI_GET_LAN_PARAMETER,
                                                                       destination_selector,
                                                                       BLOCK_SELECTOR,
                                                                       obj_cmd_rs) < 0)
    {
      if (state_data->prog_data->args->common.flags & IPMI_FLAGS_DEBUG_DUMP)
        fprintf(stderr,
                "ipmi_cmd_get_lan_configuration_parameters_destination_addresses: %s\n",
                ipmi_device_strerror(ipmi_device_errnum(state_data->dev)));
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  if (Fiid_obj_get (obj_cmd_rs, "gateway_selector", &val) < 0)
    goto cleanup;
  *alert_gateway = val;

  if (alert_ip_address && alert_ip_address_len)
    {
      if (Fiid_obj_get_data (obj_cmd_rs,
                             "alerting_ip_address",
                             alert_ip_address_bytes,
                             4) < 0)
        goto cleanup;

      memset(alert_ip_address, '\0', alert_ip_address_len);
      snprintf (alert_ip_address,
                alert_ip_address_len - 1,
                "%u.%u.%u.%u",
                alert_ip_address_bytes[0],
                alert_ip_address_bytes[1],
                alert_ip_address_bytes[2],
                alert_ip_address_bytes[3]);
    }

  if (alert_mac_address && alert_mac_address_len)
    {
      if (Fiid_obj_get_data (obj_cmd_rs,
                             "alerting_mac_address",
                             alert_mac_address_bytes,
                             6) < 0)
        goto cleanup;

      memset(alert_mac_address, '\0', alert_mac_address_len);
      snprintf (alert_mac_address,
                alert_mac_address_len - 1,
                "%02X:%02X:%02X:%02X:%02X:%02X",
                alert_mac_address_bytes[0],
                alert_mac_address_bytes[1],
                alert_mac_address_bytes[2],
                alert_mac_address_bytes[3],
                alert_mac_address_bytes[4],
                alert_mac_address_bytes[5]);
    }

  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  Fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

config_err_t
set_bmc_destination_addresses(pef_config_state_data_t *state_data,
                              uint8_t destination_selector,
                              uint8_t alert_gateway,
                              char *alert_ip_address,
                              char *alert_mac_address)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint32_t alert_ip_address_val = 0;
  uint64_t alert_mac_address_val = 0;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;

  assert(state_data);
  assert(destination_selector);

  if (!(obj_cmd_rs = Fiid_obj_create(tmpl_cmd_set_lan_configuration_parameters_rs)))
    goto cleanup;

  if ((ret = get_lan_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_ipv4_address_string2int(alert_ip_address, &alert_ip_address_val) < 0)
    goto cleanup;

  if (ipmi_mac_address_string2int(alert_mac_address, &alert_mac_address_val) < 0)
    goto cleanup;

  if (ipmi_cmd_set_lan_configuration_parameters_destination_addresses (state_data->dev,
                                                                       channel_number,
                                                                       destination_selector,
                                                                       alert_gateway,
                                                                       alert_ip_address_val,
                                                                       alert_mac_address_val,
                                                                       obj_cmd_rs) < 0)
    {
      if (state_data->prog_data->args->common.flags & IPMI_FLAGS_DEBUG_DUMP)
        fprintf(stderr,
                "ipmi_cmd_set_lan_configuration_parameters_destination_addresses: %s\n",
                ipmi_device_strerror(ipmi_device_errnum(state_data->dev)));
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  Fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

config_err_t
get_bmc_pef_conf_alert_policy_table (struct pef_config_state_data *state_data, 
                                     uint8_t alert_policy_entry_number,
                                     uint8_t *policy_type,
                                     uint8_t *policy_enabled,
                                     uint8_t *policy_number,
                                     uint8_t *destination_selector,
                                     uint8_t *channel_number,
                                     uint8_t *alert_string_set_selector,
                                     uint8_t *event_specific_alert_string)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  
  assert(state_data);
  assert(alert_policy_entry_number);
  
  if (!(obj_cmd_rs = Fiid_obj_create(tmpl_cmd_get_pef_configuration_parameters_alert_policy_table_rs)))
    goto cleanup;

  if (ipmi_cmd_get_pef_configuration_parameters_alert_policy_table (state_data->dev, 
								    IPMI_GET_PEF_PARAMETER,
								    alert_policy_entry_number, 
								    BLOCK_SELECTOR, 
								    obj_cmd_rs) < 0)
    {
      if (state_data->prog_data->args->common.flags & IPMI_FLAGS_DEBUG_DUMP)
        fprintf(stderr,
                "ipmi_cmd_get_pef_configuration_parameters_alert_policy_table: %s\n",
                ipmi_device_strerror(ipmi_device_errnum(state_data->dev)));
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

#if 0
  if (Fiid_obj_get (obj_cmd_rs, "alert_policy_entry_number", &val) < 0)
    goto cleanup;
#endif
  if (Fiid_obj_get (obj_cmd_rs, "policy_number.policy_type", &val) < 0)
    goto cleanup;
  *policy_type = val;
  if (Fiid_obj_get (obj_cmd_rs, "policy_number.enabled", &val) < 0)
    goto cleanup;
  *policy_enabled = val;
  if (Fiid_obj_get (obj_cmd_rs, "policy_number.policy_number", &val) < 0)
    goto cleanup;
  *policy_number = val;
  if (Fiid_obj_get (obj_cmd_rs, "channel_destination.destination_selector", &val) < 0)
    goto cleanup;
  *destination_selector = val;
  if (Fiid_obj_get (obj_cmd_rs, "channel_destination.channel_number", &val) < 0)
    goto cleanup;
  *channel_number = val;
  if (Fiid_obj_get (obj_cmd_rs, "alert_string_key.alert_string_set_selector", &val) < 0)
    goto cleanup;
  *alert_string_set_selector = val;
  if (Fiid_obj_get (obj_cmd_rs, "alert_string_key.event_specific_alert_string", &val) < 0)
    goto cleanup;
  *event_specific_alert_string = val;
  
  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  Fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

config_err_t
set_bmc_pef_conf_alert_policy_table (struct pef_config_state_data *state_data, 
                                     uint8_t alert_policy_entry_number,
                                     uint8_t policy_type,
                                     uint8_t policy_enabled,
                                     uint8_t policy_number,
                                     uint8_t destination_selector,
                                     uint8_t channel_number,
                                     uint8_t alert_string_set_selector,
                                     uint8_t event_specific_alert_string)
{
  fiid_obj_t obj_cmd_rs = NULL;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  
  assert(state_data);
  assert(alert_policy_entry_number);

  if (!(obj_cmd_rs = Fiid_obj_create(tmpl_cmd_set_pef_configuration_parameters_rs)))
    goto cleanup;

  if (ipmi_cmd_set_pef_configuration_parameters_alert_policy_table (state_data->dev, 
								    alert_policy_entry_number, 
								    policy_type, 
								    policy_enabled, 
								    policy_number, 
								    destination_selector, 
								    channel_number, 
								    alert_string_set_selector, 
								    event_specific_alert_string, 
								    obj_cmd_rs) < 0)
    {
      if (state_data->prog_data->args->common.flags & IPMI_FLAGS_DEBUG_DUMP)
        fprintf(stderr,
                "ipmi_cmd_set_pef_configuration_parameters_alert_policy_table: %s\n",
                ipmi_device_strerror(ipmi_device_errnum(state_data->dev)));
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }
      
  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  Fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}


