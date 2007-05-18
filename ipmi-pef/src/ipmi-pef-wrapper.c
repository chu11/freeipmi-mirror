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
#ifdef HAVE_ERROR_H
#include <error.h>
#endif
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

#include "err-wrappers.h"
#include "fiid-wrappers.h"
#include "freeipmi-portability.h"
#include "ipmi-common.h"

#include "common-utils.h"

#include "ipmi-pef.h"
#include "ipmi-pef-common.h"
#include "ipmi-pef-keys.h"
#include "ipmi-pef-map.h"
#include "ipmi-pef-utils.h"
#include "ipmi-pef-wrapper.h"

pef_err_t
get_bmc_lan_conf_community_string (ipmi_pef_state_data_t *state_data,
                                   uint8_t *community_string,
                                   uint32_t community_string_len)
{
  fiid_obj_t obj_cmd_rs = NULL;
  pef_err_t rv = PEF_ERR_FATAL_ERROR;
  pef_err_t ret;
  int8_t channel_number;

  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_get_lan_configuration_parameters_community_string_rs)))
    goto cleanup;

  if ((ret = get_lan_channel_number (state_data, &channel_number)) != PEF_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_get_lan_configuration_parameters_community_string (state_data->dev,
                                                                  channel_number,
                                                                  IPMI_GET_LAN_PARAMETER,
                                                                  SET_SELECTOR,
                                                                  BLOCK_SELECTOR,
                                                                  obj_cmd_rs) < 0)
    {
      rv = PEF_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  if (fiid_obj_get_data (obj_cmd_rs,
                         "community_string",
                         community_string,
                         community_string_len) < 0)
    goto cleanup;

  rv = PEF_ERR_SUCCESS;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

pef_err_t
set_bmc_lan_conf_community_string (ipmi_pef_state_data_t *state_data,
                                   uint8_t *community_string)
{
  fiid_obj_t obj_cmd_rs = NULL;
  pef_err_t rv = PEF_ERR_FATAL_ERROR;
  pef_err_t ret;
  int8_t channel_number;

  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_set_lan_configuration_parameters_rs)))
    goto cleanup;

  if ((ret = get_lan_channel_number (state_data, &channel_number)) != PEF_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_set_lan_configuration_parameters_community_string (state_data->dev,
                                                                  channel_number,
                                                                  community_string,
                                                                  (community_string) ? strlen((char *)community_string) : 0,
                                                                  obj_cmd_rs) < 0)
    {
      rv = PEF_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  rv = PEF_ERR_SUCCESS;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

pef_err_t
get_bmc_lan_conf_destination_type(ipmi_pef_state_data_t *state_data,
                                  uint8_t destination_selector,
                                  uint8_t *alert_destination_type,
                                  uint8_t *alert_acknowledge,
                                  uint8_t *alert_acknowledge_timeout,
                                  uint8_t *alert_retries)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  pef_err_t rv = PEF_ERR_FATAL_ERROR;
  pef_err_t ret;
  int8_t channel_number;

  assert(state_data);
  assert(destination_selector);

  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_get_lan_configuration_parameters_destination_type_rs)))
    goto cleanup;

  if ((ret = get_lan_channel_number (state_data, &channel_number)) != PEF_ERR_SUCCESS)
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
      rv = PEF_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  if (fiid_obj_get (obj_cmd_rs, "destination_type", &val) < 0)
    goto cleanup;
  *alert_destination_type = val;

  if (fiid_obj_get (obj_cmd_rs, "alert_acknowledge", &val) < 0)
    goto cleanup;
  *alert_acknowledge = val;

  if (fiid_obj_get (obj_cmd_rs, "alert_acknowledge_timeout", &val) < 0)
    goto cleanup;
  *alert_acknowledge_timeout = val;

  if (fiid_obj_get (obj_cmd_rs, "retries", &val) < 0)
    goto cleanup;
  *alert_retries = val;

  rv = PEF_ERR_SUCCESS;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

pef_err_t
set_bmc_lan_conf_destination_type(ipmi_pef_state_data_t *state_data,
                                  uint8_t destination_selector,
                                  uint8_t alert_destination_type,
                                  uint8_t alert_acknowledge,
                                  uint8_t alert_acknowledge_timeout,
                                  uint8_t alert_retries)
{
  fiid_obj_t obj_cmd_rs = NULL;
  pef_err_t rv = PEF_ERR_FATAL_ERROR;
  pef_err_t ret;
  int8_t channel_number;

  assert(state_data);
  assert(destination_selector);

  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_set_lan_configuration_parameters_rs)))
    goto cleanup;

  if ((ret = get_lan_channel_number (state_data, &channel_number)) != PEF_ERR_SUCCESS)
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
      rv = PEF_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  rv = PEF_ERR_SUCCESS;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

pef_err_t
get_bmc_lan_conf_destination_addresses(ipmi_pef_state_data_t *state_data,
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
  pef_err_t rv = PEF_ERR_FATAL_ERROR;
  pef_err_t ret;
  int8_t channel_number;

  assert(state_data);
  assert(destination_selector);

  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_get_lan_configuration_parameters_destination_addresses_rs)))
    goto cleanup;

  if ((ret = get_lan_channel_number (state_data, &channel_number)) != PEF_ERR_SUCCESS)
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
      rv = PEF_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  if (fiid_obj_get (obj_cmd_rs, "gateway_selector", &val) < 0)
    goto cleanup;
  *alert_gateway = val;

  if (alert_ip_address && alert_ip_address_len)
    {
      if (fiid_obj_get_data (obj_cmd_rs,
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
      if (fiid_obj_get_data (obj_cmd_rs,
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

  rv = PEF_ERR_SUCCESS;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

pef_err_t
set_bmc_lan_conf_destination_addresses(ipmi_pef_state_data_t *state_data,
                                       uint8_t destination_selector,
                                       uint8_t alert_gateway,
                                       char *alert_ip_address,
                                       char *alert_mac_address)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint32_t alert_ip_address_val = 0;
  uint64_t alert_mac_address_val = 0;
  pef_err_t rv = PEF_ERR_FATAL_ERROR;
  pef_err_t ret;
  int8_t channel_number;

  assert(state_data);
  assert(destination_selector);

  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_set_lan_configuration_parameters_rs)))
    goto cleanup;

  if ((ret = get_lan_channel_number (state_data, &channel_number)) != PEF_ERR_SUCCESS)
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
      rv = PEF_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  rv = PEF_ERR_SUCCESS;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

pef_err_t
get_bmc_pef_conf_alert_policy_table (struct ipmi_pef_state_data *state_data, 
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
  pef_err_t rv = PEF_ERR_FATAL_ERROR;
  
  assert(state_data);
  assert(alert_policy_entry_number);
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_get_pef_configuration_parameters_alert_policy_table_rs)))
    goto cleanup;

  if (ipmi_cmd_get_pef_configuration_parameters_alert_policy_table (state_data->dev, 
								    IPMI_GET_PEF_PARAMETER,
								    alert_policy_entry_number, 
								    BLOCK_SELECTOR, 
								    obj_cmd_rs) < 0)
    {
      rv = PEF_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

#if 0
  if (fiid_obj_get (obj_cmd_rs, "alert_policy_entry_number", &val) < 0)
    goto cleanup;
#endif
  if (fiid_obj_get (obj_cmd_rs, "policy_number.policy_type", &val) < 0)
    goto cleanup;
  *policy_type = val;
  if (fiid_obj_get (obj_cmd_rs, "policy_number.enabled", &val) < 0)
    goto cleanup;
  *policy_enabled = val;
  if (fiid_obj_get (obj_cmd_rs, "policy_number.policy_number", &val) < 0)
    goto cleanup;
  *policy_number = val;
  if (fiid_obj_get (obj_cmd_rs, "channel_destination.destination_selector", &val) < 0)
    goto cleanup;
  *destination_selector = val;
  if (fiid_obj_get (obj_cmd_rs, "channel_destination.channel_number", &val) < 0)
    goto cleanup;
  *channel_number = val;
  if (fiid_obj_get (obj_cmd_rs, "alert_string_key.alert_string_set_selector", &val) < 0)
    goto cleanup;
  *alert_string_set_selector = val;
  if (fiid_obj_get (obj_cmd_rs, "alert_string_key.event_specific_alert_string", &val) < 0)
    goto cleanup;
  *event_specific_alert_string = val;
  
  rv = PEF_ERR_SUCCESS;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

pef_err_t
set_bmc_pef_conf_alert_policy_table (struct ipmi_pef_state_data *state_data, 
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
  pef_err_t rv = PEF_ERR_FATAL_ERROR;
  
  assert(state_data);
  assert(alert_policy_entry_number);

  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_set_pef_configuration_parameters_rs)))
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
      rv = PEF_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }
      
  rv = PEF_ERR_SUCCESS;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

pef_err_t
get_bmc_pef_conf_event_filter_table (struct ipmi_pef_state_data *state_data, 
                                     uint8_t filter_number,
                                     uint8_t *filter_type,
                                     uint8_t *enable_filter,
                                     uint8_t *event_filter_action_alert,
                                     uint8_t *event_filter_action_power_off,
                                     uint8_t *event_filter_action_reset,
                                     uint8_t *event_filter_action_power_cycle,
                                     uint8_t *event_filter_action_oem,
                                     uint8_t *event_filter_action_diagnostic_interrupt,
                                     uint8_t *event_filter_action_group_control_operation,
                                     uint8_t *alert_policy_number,
                                     uint8_t *group_control_selector,
                                     uint8_t *event_severity,
                                     uint8_t *generator_id_byte1,
                                     uint8_t *generator_id_byte2,
                                     uint8_t *sensor_type,
                                     uint8_t *sensor_number,
                                     uint8_t *event_trigger,
                                     uint8_t *event_data1_offset_mask,
                                     uint8_t *event_data1_AND_mask,
                                     uint8_t *event_data1_compare1,
                                     uint8_t *event_data1_compare2,
                                     uint8_t *event_data2_AND_mask,
                                     uint8_t *event_data2_compare1,
                                     uint8_t *event_data2_compare2,
                                     uint8_t *event_data3_AND_mask,
                                     uint8_t *event_data3_compare1,
                                     uint8_t *event_data3_compare2)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  pef_err_t rv = PEF_ERR_FATAL_ERROR;
  
  assert(state_data);
  assert(filter_number);

  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_get_pef_configuration_parameters_event_filter_table_rs)))
    goto cleanup;

  if (ipmi_cmd_get_pef_configuration_parameters_event_filter_table (state_data->dev,
								    IPMI_GET_PEF_PARAMETER,
								    filter_number,
								    BLOCK_SELECTOR,
								    obj_cmd_rs) < 0)
    {
      rv = PEF_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }
 
#if 0
  if (fiid_obj_get (obj_cmd_rs, "filter_number", &val) < 0)
    goto cleanup;
#endif
  if (fiid_obj_get (obj_cmd_rs, "filter_configuration.type", &val) < 0)
    goto cleanup;
  *filter_type = val;
  if (fiid_obj_get (obj_cmd_rs, "filter_configuration.filter", &val) < 0)
    goto cleanup;
  *enable_filter = val;
  if (fiid_obj_get (obj_cmd_rs, "event_filter_action.alert", &val) < 0)
    goto cleanup;
  *event_filter_action_alert = val;
  if (fiid_obj_get (obj_cmd_rs, "event_filter_action.power_off", &val) < 0)
    goto cleanup;
  *event_filter_action_power_off = val;
  if (fiid_obj_get (obj_cmd_rs, "event_filter_action.reset", &val) < 0)
    goto cleanup;
  *event_filter_action_reset = val;
  if (fiid_obj_get (obj_cmd_rs, "event_filter_action.power_cycle", &val) < 0)
    goto cleanup;
  *event_filter_action_power_cycle = val;
  if (fiid_obj_get (obj_cmd_rs, "event_filter_action.oem", &val) < 0)
    goto cleanup;
  *event_filter_action_oem = val;
  if (fiid_obj_get (obj_cmd_rs, "event_filter_action.diagnostic_interrupt", &val) < 0)
    goto cleanup;
  *event_filter_action_diagnostic_interrupt = val;
  if (fiid_obj_get (obj_cmd_rs, "event_filter_action.group_control_operation", &val) < 0)
    goto cleanup;
  *event_filter_action_group_control_operation = val;
  if (fiid_obj_get (obj_cmd_rs, "alert_policy_number.policy_number", &val) < 0)
    goto cleanup;
  *alert_policy_number = val;
  if (fiid_obj_get (obj_cmd_rs, "alert_policy_number.group_control_selector", &val) < 0)
    goto cleanup;
  *group_control_selector = val;
  if (fiid_obj_get (obj_cmd_rs, "event_severity", &val) < 0)
    goto cleanup;
  *event_severity = val;
  if (fiid_obj_get (obj_cmd_rs, "generator_id_byte1", &val) < 0)
    goto cleanup;
  *generator_id_byte1 = val;
  if (fiid_obj_get (obj_cmd_rs, "generator_id_byte2", &val) < 0)
    goto cleanup;
  *generator_id_byte2 = val;
  if (fiid_obj_get (obj_cmd_rs, "sensor_type", &val) < 0)
    goto cleanup;
  *sensor_type = val;
  if (fiid_obj_get (obj_cmd_rs, "sensor_number", &val) < 0)
    goto cleanup;
  *sensor_number = val;
  if (fiid_obj_get (obj_cmd_rs, "event_trigger", &val) < 0)
    goto cleanup;
  *event_trigger = val;
  if (fiid_obj_get (obj_cmd_rs, "event_data1_offset_mask", &val) < 0)
    goto cleanup;
  *event_data1_offset_mask = val;
  if (fiid_obj_get (obj_cmd_rs, "event_data1_AND_mask", &val) < 0)
    goto cleanup;
  *event_data1_AND_mask = val;
  if (fiid_obj_get (obj_cmd_rs, "event_data1_compare1", &val) < 0)
    goto cleanup;
  *event_data1_compare1 = val;
  if (fiid_obj_get (obj_cmd_rs, "event_data1_compare2", &val) < 0)
    goto cleanup;
  *event_data1_compare2 = val;
  if (fiid_obj_get (obj_cmd_rs, "event_data2_AND_mask", &val) < 0)
    goto cleanup;
  *event_data2_AND_mask = val;
  if (fiid_obj_get (obj_cmd_rs, "event_data2_compare1", &val) < 0)
    goto cleanup;
  *event_data2_compare1 = val;
  if (fiid_obj_get (obj_cmd_rs, "event_data2_compare2", &val) < 0)
    goto cleanup;
  *event_data2_compare2 = val;
  if (fiid_obj_get (obj_cmd_rs, "event_data3_AND_mask", &val) < 0)
    goto cleanup;
  *event_data3_AND_mask = val;
  if (fiid_obj_get (obj_cmd_rs, "event_data3_compare1", &val) < 0)
    goto cleanup;
  *event_data3_compare1 = val;
  if (fiid_obj_get (obj_cmd_rs, "event_data3_compare2", &val) < 0)
    goto cleanup;
  *event_data3_compare2 = val;

  rv = PEF_ERR_SUCCESS;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

pef_err_t
set_bmc_pef_conf_event_filter_table (struct ipmi_pef_state_data *state_data, 
                                     uint8_t filter_number,
                                     uint8_t filter_type,
                                     uint8_t enable_filter,
                                     uint8_t event_filter_action_alert,
                                     uint8_t event_filter_action_power_off,
                                     uint8_t event_filter_action_reset,
                                     uint8_t event_filter_action_power_cycle,
                                     uint8_t event_filter_action_oem,
                                     uint8_t event_filter_action_diagnostic_interrupt,
                                     uint8_t event_filter_action_group_control_operation,
                                     uint8_t alert_policy_number,
                                     uint8_t group_control_selector,
                                     uint8_t event_severity,
                                     uint8_t generator_id_byte1,
                                     uint8_t generator_id_byte2,
                                     uint8_t sensor_type,
                                     uint8_t sensor_number,
                                     uint8_t event_trigger,
                                     uint8_t event_data1_offset_mask,
                                     uint8_t event_data1_AND_mask,
                                     uint8_t event_data1_compare1,
                                     uint8_t event_data1_compare2,
                                     uint8_t event_data2_AND_mask,
                                     uint8_t event_data2_compare1,
                                     uint8_t event_data2_compare2,
                                     uint8_t event_data3_AND_mask,
                                     uint8_t event_data3_compare1,
                                     uint8_t event_data3_compare2)
{
  fiid_obj_t obj_cmd_rs = NULL;
  pef_err_t rv = PEF_ERR_FATAL_ERROR;
  
  assert(state_data);
  assert(filter_number);

  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_set_pef_configuration_parameters_rs)))
    goto cleanup;

  if (ipmi_cmd_set_pef_configuration_parameters_event_filter_table (state_data->dev, 
								    filter_number, 
								    filter_type, 
								    enable_filter, 
								    event_filter_action_alert, 
								    event_filter_action_power_off, 
								    event_filter_action_reset, 
								    event_filter_action_power_cycle, 
								    event_filter_action_oem, 
								    event_filter_action_diagnostic_interrupt, 
								    event_filter_action_group_control_operation, 
								    alert_policy_number, 
								    group_control_selector, 
								    event_severity, 
								    generator_id_byte1, 
								    generator_id_byte2, 
								    sensor_type, 
								    sensor_number, 
								    event_trigger, 
								    event_data1_offset_mask, 
								    event_data1_AND_mask, 
								    event_data1_compare1, 
								    event_data1_compare2, 
								    event_data2_AND_mask, 
								    event_data2_compare1, 
								    event_data2_compare2, 
								    event_data3_AND_mask, 
								    event_data3_compare1, 
								    event_data3_compare2, 
								    obj_cmd_rs) < 0)
    {
      rv = PEF_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  rv = PEF_ERR_SUCCESS;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

#define GET_VALUE_STRING_BY_KEY_RETURN(__cache_record, __key, __value)  \
  do									\
    {									\
      char *local_value_string = NULL;					\
      if (_get_value_string_by_key (__cache_record,			\
				    __key,				\
				    &local_value_string) == -1)		\
	{                                                               \
          fprintf (stderr, "Key '%s' not found\n", __key);	        \
	  return (-1);							\
	}								\
      if (!(__value = strdupa (local_value_string)))                    \
        {                                                               \
          free (local_value_string);					\
          perror("strdupa");                                            \
          return (-1);                                                  \
        }                                                               \
      free (local_value_string);					\
    }									\
  while (0)

static int 
_get_value_string_by_key (const char *cache_record, 
			  const char *key, 
			  char **value)
{
  const char *buf = NULL;
  
  assert(cache_record);
  assert(key);
  assert(value);

  buf = cache_record;
  
  while (buf)
    {
      char *line_pos = NULL;
      char *line = NULL;
      char *token = NULL;
      
      if ((line_pos = strchr (buf, '\n')) == NULL)
	{
	  if (!(line = strdupa (buf)))
            {
              perror("strdupa");
              return -1;
            }
	  buf = NULL;
	}
      else 
	{
	  int len = line_pos - buf;
	  if (!(line = strndupa (buf, len)))
            {
              perror("strdupa");
              return -1;
            }
	  buf += (len + 1);
	}
      
      if ((token = strsep_noempty (&line, STRING_WHITESPACES)) == NULL)
	continue;
      
      if (strcasecmp (token, key) == 0)
	{
	  if (!(*value = strdup (stripwhite (line))))
            {
              perror("strdup");
              return -1;
            }
	  return 0;
	}
    }
  
  return -1;
}

static int 
_fread_record (FILE *fp, char **cache_record)
{
  char *record;
  
  assert(fp);
  assert(cache_record);

  record = NULL;
  
  while (1)
    {
      char *line = NULL;
      size_t n = 0;
      
      if (getline (&line, &n, fp) == -1)
	{
	  return (-1);
	}
      else 
	{
	  char *lineptr = line;
	  char *tmp_lineptr = line;
	  if (!(line = strdupa (stripwhite (tmp_lineptr))))
            {
              free (lineptr);
              perror("strdupa");
              return -1;
            }
	  free (lineptr);
	}
      if (strlen (line) == 0)
	{
	  *cache_record = NULL;
	  if (record)
	    {
	      if (!(*cache_record = strdup (record)))
                {
                  perror("strdup");
                  return -1;
                }
	    }
	  return 0;
	}
      
      if (line[0] == COMMENT_CHAR)
	{
	  continue;
	}
      
      {
	char *l_record = NULL;
	int len = 0;
	
	if (record)
	  {
	    len = strlen (record) + strlen (line) + 2;
	    if (!(l_record = alloca (len)))
              {
                perror("alloca");
                return -1;
              }
	    strcpy (l_record, record);
	    strcat (l_record, line);
	    strcat (l_record, "\n");
	    record = l_record;
	  }
	else 
	  {
	    len = strlen (line) + 2;
	    if (!(l_record = alloca (len)))
              {
                perror("alloca");
                return -1;
              }
	    strcpy (l_record, line);
	    strcat (l_record, "\n");
	    record = l_record;
	  }
      }
    }
 

  return 0;
}

static int 
_get_record_count (FILE *fp, int *count)
{
  char *cache_record = NULL;
  int rcount = 0;
  
  while (1)
    {
      cache_record = NULL;
      if (_fread_record (fp, &cache_record) == -1)
	{
	  *count = rcount;
	  return 0;
	}
      rcount++;
      free (cache_record);
    }
  
  return (-1);
}

int
get_bmc_community_string (struct ipmi_pef_state_data *state_data,
                          uint8_t *community_string,
                          uint32_t community_string_len)
{
  fiid_obj_t obj_cmd_rs = NULL;
  int8_t channel_number;
  int rv = -1;

  assert(state_data);
  assert(community_string);
  assert(community_string_len);

  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_get_lan_configuration_parameters_community_string_rs)))
    goto cleanup;

  if (get_lan_channel_number (state_data, &channel_number) != PEF_ERR_SUCCESS)
    goto cleanup;

  if (ipmi_cmd_get_lan_configuration_parameters_community_string (state_data->dev,
                                                                  channel_number,
                                                                  IPMI_GET_LAN_PARAMETER,
                                                                  SET_SELECTOR,
                                                                  BLOCK_SELECTOR,
                                                                  obj_cmd_rs) < 0)
    goto cleanup;

  if (fiid_obj_get_data (obj_cmd_rs,
                         "community_string",
                         community_string,
                         community_string_len) < 0)
    goto cleanup;

  rv = 0;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

int 
get_community_string (struct ipmi_pef_state_data *state_data,
                      FILE *fp,
                      uint8_t *community_string, 
                      uint32_t community_string_len)
{
  char *record = NULL;
  char *value_string = NULL;

  assert(state_data);
  assert(fp);
  assert(community_string);
  assert(community_string_len);

  fseek (fp, 0, SEEK_SET);
  

  if (_fread_record (fp, &record) < 0)
    {
      /* XXX fix later */
      return -1;
    }

  GET_VALUE_STRING_BY_KEY_RETURN (record, 
                                  COMMUNITY_STRING_KEY_STRING,
				  value_string);
  if (strlen(value_string) > IPMI_MAX_COMMUNITY_STRING_LENGTH)
    {
      fprintf (stderr, 
	       "Invalid value %s for %s\n", 
	       value_string, 
	       COMMUNITY_STRING_KEY_STRING);
      return -1;
    }
  if (community_string_len < strlen(value_string))
    {
      fprintf (stderr, 
	       "Internal buffer length error: %s for %s\n", 
	       value_string, 
	       COMMUNITY_STRING_KEY_STRING);
      return -1;
    }
  memcpy(community_string, value_string, community_string_len);
  
  return 0;
}

int
set_bmc_community_string (struct ipmi_pef_state_data *state_data,
                          uint8_t *community_string)
{
  fiid_obj_t obj_cmd_rs = NULL;
  int8_t channel_number;
  int rv = -1;

  assert(state_data);
  assert(community_string);

  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_set_lan_configuration_parameters_rs)))
    goto cleanup;

  if (get_lan_channel_number (state_data, &channel_number) != PEF_ERR_SUCCESS)
    goto cleanup;

  if (ipmi_cmd_set_lan_configuration_parameters_community_string (state_data->dev,
                                                                  channel_number,
                                                                  community_string,
                                                                  (community_string) ? strlen((char *)community_string) : 0,
                                                                  obj_cmd_rs) < 0)
    goto cleanup;

  rv = 0;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

int 
get_lan_alert_destination (struct ipmi_pef_state_data *state_data, 
			   int destination_selector, 
			   lan_alert_destination_t *lad)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  int rv = -1;
  int8_t channel_number;
  uint8_t alert_ip_address_bytes[4];
  uint8_t alert_mac_address_bytes[6];
  
  assert(state_data);
  assert(lad);
  
  if (get_lan_channel_number (state_data, &channel_number) != PEF_ERR_SUCCESS)
    goto cleanup; 
  
  lad->destination_selector = destination_selector;
  
  FIID_OBJ_CREATE (obj_cmd_rs, 
		   tmpl_cmd_get_lan_configuration_parameters_destination_type_rs);
  
  if (ipmi_cmd_get_lan_configuration_parameters_destination_type (state_data->dev, 
                                                                  channel_number, 
                                                                  IPMI_GET_LAN_PARAMETER, 
                                                                  destination_selector, 
                                                                  BLOCK_SELECTOR, 
                                                                  obj_cmd_rs) != 0)
    goto cleanup;
  
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "destination_type", &val);
  lad->destination_type = val;
  
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "alert_acknowledge", &val);
  lad->alert_acknowledge = val;
  
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "alert_acknowledge_timeout", &val);
  lad->alert_acknowledge_timeout = val;
  
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "retries", &val);
  lad->alert_retries = val;
  
  FIID_OBJ_DESTROY_NO_RETURN (obj_cmd_rs);
  
  FIID_OBJ_CREATE (obj_cmd_rs, 
		   tmpl_cmd_get_lan_configuration_parameters_destination_addresses_rs);
  
  if (ipmi_cmd_get_lan_configuration_parameters_destination_addresses (state_data->dev, 
                                                                       channel_number, 
                                                                       IPMI_GET_LAN_PARAMETER, 
                                                                       destination_selector, 
                                                                       BLOCK_SELECTOR, 
                                                                       obj_cmd_rs) != 0)
    goto cleanup;
  
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "gateway_selector", &val);
  lad->gateway_selector = val;
  
  FIID_OBJ_GET_DATA_CLEANUP (obj_cmd_rs, 
			     "alerting_ip_address", 
			     alert_ip_address_bytes, 
			     4);
  snprintf (lad->alert_ip_address, 
	    16, 
	    "%u.%u.%u.%u", 
	    alert_ip_address_bytes[0], 
	    alert_ip_address_bytes[1], 
	    alert_ip_address_bytes[2], 
	    alert_ip_address_bytes[3]);
  
  FIID_OBJ_GET_DATA_CLEANUP (obj_cmd_rs, 
                             "alerting_mac_address", 
                             alert_mac_address_bytes, 
                             6);
  snprintf (lad->alert_mac_address, 
	    18, 
	    "%02X:%02X:%02X:%02X:%02X:%02X", 
	    alert_mac_address_bytes[0],
	    alert_mac_address_bytes[1],
	    alert_mac_address_bytes[2],
	    alert_mac_address_bytes[3],
	    alert_mac_address_bytes[4],
	    alert_mac_address_bytes[5]);
  
  rv = 0;
  
 cleanup:
  FIID_OBJ_DESTROY_NO_RETURN (obj_cmd_rs);
  return (rv);
}

static int 
_record_string_to_lan_alert_destination (const char *record_string, 
                                         lan_alert_destination_t *lad)
{
  char *value_string = NULL;
  int n;
    
  assert(record_string);
  assert(lad);
  
  GET_VALUE_STRING_BY_KEY_RETURN (record_string, 
				  LAD_ALERT_DESTINATION_SELECTOR_KEY_STRING, 
				  value_string);
  if ((n = string_to_destination_selector (value_string)) < 0)
    {
      fprintf (stderr, 
	       "Invalid value %s for %s\n", 
	       value_string, 
	       LAD_ALERT_DESTINATION_SELECTOR_KEY_STRING);
      return -1;
    }
  lad->destination_selector = n;
  
  GET_VALUE_STRING_BY_KEY_RETURN (record_string, 
				  LAD_ALERT_DESTINATION_TYPE_KEY_STRING, 
				  value_string);
  if ((n = string_to_destination_type (value_string)) < 0)
    {
      fprintf (stderr, 
	       "Invalid value %s for %s\n", 
	       value_string, 
	       LAD_ALERT_DESTINATION_TYPE_KEY_STRING);
      return -1;
    }
  lad->destination_type = n;
  
  GET_VALUE_STRING_BY_KEY_RETURN (record_string, 
				  LAD_ALERT_ACKNOWLEDGE_KEY_STRING, 
				  value_string);
  if ((n = string_to_alert_acknowledge (value_string)) < 0)
    {
      fprintf (stderr, 
	       "Invalid value %s for %s\n", 
	       value_string, 
	       LAD_ALERT_ACKNOWLEDGE_KEY_STRING);
      return -1;
    }
  lad->alert_acknowledge = n;
  
  GET_VALUE_STRING_BY_KEY_RETURN (record_string, 
				  LAD_ALERT_ACKNOWLEDGE_TIMEOUT_KEY_STRING, 
				  value_string);
  if ((n = string_to_alert_acknowledge_timeout (value_string)) < 0)
    {
      fprintf (stderr, 
	       "Invalid value %s for %s\n", 
	       value_string, 
	       LAD_ALERT_ACKNOWLEDGE_TIMEOUT_KEY_STRING);
      return -1;
    }
  lad->alert_acknowledge_timeout = n;
  
  GET_VALUE_STRING_BY_KEY_RETURN (record_string, 
				  LAD_ALERT_RETRIES_KEY_STRING, 
				  value_string);
  if ((n = string_to_alert_retries (value_string)) < 0)
    {
      fprintf (stderr, 
	       "Invalid value %s for %s\n", 
	       value_string, 
	       LAD_ALERT_RETRIES_KEY_STRING);
      return -1;
    }
  lad->alert_retries = n;
  
  GET_VALUE_STRING_BY_KEY_RETURN (record_string, 
				  LAD_ALERT_GATEWAY_KEY_STRING, 
				  value_string);
  if ((n = string_to_gateway_selector (value_string)) < 0)
    {
      fprintf (stderr, 
	       "Invalid value %s for %s\n", 
	       value_string, 
	       LAD_ALERT_GATEWAY_KEY_STRING);
      return -1;
    }
  lad->gateway_selector = n;
  
  {
    char *str = NULL;
    GET_VALUE_STRING_BY_KEY_RETURN (record_string, 
				    LAD_ALERT_IP_ADDRESS_KEY_STRING, 
				    value_string);
    if (string_to_alert_ip_address (value_string, &str) < 0)
      {
	fprintf (stderr, 
		 "Invalid value %s for %s\n", 
		 value_string, 
		 LAD_ALERT_IP_ADDRESS_KEY_STRING);
	return -1;
      }
    strncpy (lad->alert_ip_address, str, 15);
    free (str);
    
    GET_VALUE_STRING_BY_KEY_RETURN (record_string, 
				    LAD_ALERT_MAC_ADDRESS_KEY_STRING, 
				    value_string);
    if (string_to_alert_mac_address (value_string, &str) < 0)
      {
        fprintf (stderr, 
                 "Invalid value %s for %s\n", 
                 value_string, 
                 LAD_ALERT_MAC_ADDRESS_KEY_STRING);
        return -1;
      }
    strncpy (lad->alert_mac_address, str, 17);
    free (str);
  }
  
  return 0;
}

int 
get_lan_alert_destination_list (FILE *fp, lan_alert_destination_t **lad_list, int *count)
{
  lan_alert_destination_t *l_lad_list = NULL;
  int l_count;
  char *record = NULL;
  int i;
  int rv;
  
  assert(fp);
  assert(lad_list);
  assert(count);

  if (_get_record_count (fp, &l_count) != 0)
    return (-1);
  
  if (!(l_lad_list = (lan_alert_destination_t *) calloc (l_count, 
                                                         sizeof (lan_alert_destination_t))))
    {
      perror("calloc");
      return -1;
    }
  
  fseek (fp, 0, SEEK_SET);
  
  for (i = 0; i < l_count; i++)
    {
      _fread_record (fp, &record);
      rv = _record_string_to_lan_alert_destination (record, &l_lad_list[i]);
      free (record);
      if (rv != 0)
	{
	  free (l_lad_list);
	  return (-1);
	}
    }
  
  *lad_list = l_lad_list;
  *count = l_count;
  
  return 0;
}

int 
set_lan_alert_destination (struct ipmi_pef_state_data *state_data, lan_alert_destination_t *lad)
{
  fiid_obj_t obj_cmd_rs = NULL;
  int rv = -1;
  int8_t channel_number;
  uint32_t alert_ip_address_bytes = 0;
  uint64_t alert_mac_address_bytes = 0;
  
  assert(state_data);
  assert(lad);
  
  if (get_lan_channel_number (state_data, &channel_number) != PEF_ERR_SUCCESS)
    goto cleanup; 
  
  if (ipmi_ipv4_address_string2int (lad->alert_ip_address, &alert_ip_address_bytes) < 0)
    goto cleanup;
  
  if (ipmi_mac_address_string2int (lad->alert_mac_address, &alert_mac_address_bytes) < 0)
    goto cleanup;
  
  FIID_OBJ_CREATE (obj_cmd_rs, tmpl_cmd_set_lan_configuration_parameters_rs);
  if (ipmi_cmd_set_lan_configuration_parameters_destination_type (state_data->dev,
                                                                  channel_number,
                                                                  lad->destination_selector,
                                                                  lad->destination_type,
                                                                  lad->alert_acknowledge,
                                                                  lad->alert_acknowledge_timeout,
                                                                  lad->alert_retries,
                                                                  obj_cmd_rs) != 0)
    goto cleanup;
  
  if (ipmi_cmd_set_lan_configuration_parameters_destination_addresses (state_data->dev, 
                                                                       channel_number,
                                                                       lad->destination_selector,
                                                                       lad->gateway_selector,
                                                                       alert_ip_address_bytes,
                                                                       alert_mac_address_bytes,
                                                                       obj_cmd_rs) != 0)
    goto cleanup;
  
  rv = 0;
  
 cleanup:
  FIID_OBJ_DESTROY_NO_RETURN (obj_cmd_rs);
  return (rv);
}

int 
get_alert_policy_table (struct ipmi_pef_state_data *state_data, 
			int entry_number, 
			pef_alert_policy_table_t *apt)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  int rv = -1;
  
  assert(state_data);
  assert(apt);
  
  FIID_OBJ_CREATE (obj_cmd_rs, 
		   tmpl_cmd_get_pef_configuration_parameters_alert_policy_table_rs);
  
  if (ipmi_cmd_get_pef_configuration_parameters_alert_policy_table (state_data->dev, 
								    IPMI_GET_PEF_PARAMETER,
								    entry_number, 
								    BLOCK_SELECTOR, 
								    obj_cmd_rs) != 0)
    goto cleanup;
  
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "alert_policy_entry_number", &val);
  apt->alert_policy_entry_number = val;
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "policy_number.policy_type", &val);
  apt->policy_type = val;
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "policy_number.enabled", &val);
  apt->policy_enabled = val;
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "policy_number.policy_number", &val);
  apt->policy_number = val;
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "channel_destination.destination_selector", &val);
  apt->destination_selector = val;
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "channel_destination.channel_number", &val);
  apt->channel_number = val;
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "alert_string_key.alert_string_set_selector", &val);
  apt->alert_string_set_selector = val;
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "alert_string_key.event_specific_alert_string", &val);
  apt->event_specific_alert_string = val;
  
  rv = 0;
  
 cleanup:
  FIID_OBJ_DESTROY_NO_RETURN (obj_cmd_rs);
  return (rv);
}

static int 
_record_string_to_alert_policy_table (const char *record_string, 
                                      pef_alert_policy_table_t *apt)
{
  char *value_string = NULL;
  int n;
  
  assert(record_string);
  assert(apt);
  
  GET_VALUE_STRING_BY_KEY_RETURN (record_string, 
				  APT_ALERT_POLICY_ENTRY_NUMBER_KEY_STRING, 
				  value_string);
  if ((n = string_to_alert_policy_entry_number (value_string)) < 0)
    {
      fprintf (stderr, 
	       "Invalid value %s for %s\n", 
	       value_string, 
	       APT_ALERT_POLICY_ENTRY_NUMBER_KEY_STRING);
      return -1;
    }
  apt->alert_policy_entry_number = n;
  
  GET_VALUE_STRING_BY_KEY_RETURN (record_string, 
				  APT_POLICY_TYPE_KEY_STRING, 
				  value_string);
  if ((n = string_to_policy_type (value_string)) < 0)
    {
      fprintf (stderr, 
	       "Invalid value %s for %s\n", 
	       value_string, 
	       APT_POLICY_TYPE_KEY_STRING);
      return -1;
    }
  apt->policy_type = n;
  
  GET_VALUE_STRING_BY_KEY_RETURN (record_string, 
				  APT_POLICY_ENABLED_KEY_STRING, 
				  value_string);
  if ((n = string_to_policy_enabled (value_string)) < 0)
    {
      fprintf (stderr, 
	       "Invalid value %s for %s\n", 
	       value_string, 
	       APT_POLICY_ENABLED_KEY_STRING);
      return -1;
    }
  apt->policy_enabled = n;
  
  GET_VALUE_STRING_BY_KEY_RETURN (record_string, 
				  APT_POLICY_NUMBER_KEY_STRING, 
				  value_string);
  if ((n = string_to_policy_number (value_string)) < 0)
    {
      fprintf (stderr, 
	       "Invalid value %s for %s\n", 
	       value_string, 
	       APT_POLICY_NUMBER_KEY_STRING);
      return -1;
    }
  apt->policy_number = n;
  
  GET_VALUE_STRING_BY_KEY_RETURN (record_string, 
				  APT_DESTINATION_SELECTOR_KEY_STRING, 
				  value_string);
  if ((n = string_to_destination_selector (value_string)) < 0)
    {
      fprintf (stderr, 
	       "Invalid value %s for %s\n", 
	       value_string, 
	       APT_DESTINATION_SELECTOR_KEY_STRING);
      return -1;
    }
  apt->destination_selector = n;
  
  GET_VALUE_STRING_BY_KEY_RETURN (record_string, 
				  APT_CHANNEL_NUMBER_KEY_STRING, 
				  value_string);
  if ((n = string_to_channel_number (value_string)) < 0)
    {
      fprintf (stderr, 
	       "Invalid value %s for %s\n", 
	       value_string, 
	       APT_CHANNEL_NUMBER_KEY_STRING);
      return -1;
    }
  apt->channel_number = n;
  
  GET_VALUE_STRING_BY_KEY_RETURN (record_string, 
				  APT_ALERT_STRING_SET_SELECTOR_KEY_STRING, 
				  value_string);
  if ((n = string_to_alert_string_set_selector (value_string)) < 0)
    {
      fprintf (stderr, 
	       "Invalid value %s for %s\n", 
	       value_string, 
	       APT_ALERT_STRING_SET_SELECTOR_KEY_STRING);
      return -1;
    }
  apt->alert_string_set_selector = n;
  
  GET_VALUE_STRING_BY_KEY_RETURN (record_string, 
				  APT_EVENT_SPECIFIC_ALERT_STRING_KEY_STRING, 
				  value_string);
  if ((n = string_to_event_specific_alert_string (value_string)) < 0)
    {
      fprintf (stderr, 
	       "Invalid value %s for %s\n", 
	       value_string, 
	       APT_EVENT_SPECIFIC_ALERT_STRING_KEY_STRING);
      return -1;
    }
  apt->event_specific_alert_string = n;
  
  return 0;
}

int 
get_alert_policy_table_list (FILE *fp, pef_alert_policy_table_t **apt_list, int *count)
{
  pef_alert_policy_table_t *l_apt_list = NULL;
  int l_count;
  char *record = NULL;
  int i;
  int rv;
  
  assert(fp);
  assert(apt_list);
  assert(count);
  
  if (_get_record_count (fp, &l_count) != 0)
    return (-1);
  
  if (!(l_apt_list = (pef_alert_policy_table_t *) calloc (l_count, 
                                                          sizeof (pef_alert_policy_table_t))))
    {
      perror("calloc");
      return -1;
    }
  
  fseek (fp, 0, SEEK_SET);
  
  for (i = 0; i < l_count; i++)
    {
      _fread_record (fp, &record);
      rv = _record_string_to_alert_policy_table (record, &l_apt_list[i]);
      free (record);
      if (rv != 0)
	{
	  free (l_apt_list);
	  return (-1);
	}
    }
  
  *apt_list = l_apt_list;
  *count = l_count;
  
  return 0;
}

int 
set_alert_policy_table (struct ipmi_pef_state_data *state_data, pef_alert_policy_table_t *apt)
{
  fiid_obj_t obj_cmd_rs = NULL;
  int rv = -1;
  
  assert(state_data);
  assert(apt);
  
  FIID_OBJ_CREATE (obj_cmd_rs, tmpl_cmd_set_pef_configuration_parameters_rs);
  if (ipmi_cmd_set_pef_configuration_parameters_alert_policy_table (state_data->dev, 
								    apt->alert_policy_entry_number, 
								    apt->policy_type, 
								    apt->policy_enabled, 
								    apt->policy_number, 
								    apt->destination_selector, 
								    apt->channel_number, 
								    apt->alert_string_set_selector, 
								    apt->event_specific_alert_string, 
								    obj_cmd_rs) != 0)
    goto cleanup;
  
  rv = 0;
  
 cleanup:
  FIID_OBJ_DESTROY_NO_RETURN (obj_cmd_rs);
  return (rv);
}

int 
get_event_filter_table (struct ipmi_pef_state_data *state_data, int filter, pef_event_filter_table_t *eft)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  int rv = -1;
  
  assert(state_data);
  assert(eft);
  
  FIID_OBJ_CREATE (obj_cmd_rs, tmpl_cmd_get_pef_configuration_parameters_event_filter_table_rs);
  
  if (ipmi_cmd_get_pef_configuration_parameters_event_filter_table (state_data->dev,
								    IPMI_GET_PEF_PARAMETER,
								    filter,
								    BLOCK_SELECTOR,
								    obj_cmd_rs) != 0)
    goto cleanup;
  
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "filter_number", &val);
  eft->filter_number = val;
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "filter_configuration.type", &val);
  eft->filter_type = val;
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "filter_configuration.filter", &val);
  eft->enable_filter = val;
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "event_filter_action.alert", &val);
  eft->event_filter_action_alert = val;
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "event_filter_action.power_off", &val);
  eft->event_filter_action_power_off = val;
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "event_filter_action.reset", &val);
  eft->event_filter_action_reset = val;
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "event_filter_action.power_cycle", &val);
  eft->event_filter_action_power_cycle = val;
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "event_filter_action.oem", &val);
  eft->event_filter_action_oem = val;
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "event_filter_action.diagnostic_interrupt", &val);
  eft->event_filter_action_diagnostic_interrupt = val;
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "event_filter_action.group_control_operation", &val);
  eft->event_filter_action_group_control_operation = val;
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "alert_policy_number.policy_number", &val);
  eft->alert_policy_number = val;
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "alert_policy_number.group_control_selector", &val);
  eft->group_control_selector = val;
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "event_severity", &val);
  eft->event_severity = val;
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "generator_id_byte1", &val);
  eft->generator_id_byte1 = val;
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "generator_id_byte2", &val);
  eft->generator_id_byte2 = val;
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "sensor_type", &val);
  eft->sensor_type = val;
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "sensor_number", &val);
  eft->sensor_number = val;
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "event_trigger", &val);
  eft->event_trigger = val;
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "event_data1_offset_mask", &val);
  eft->event_data1_offset_mask = val;
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "event_data1_AND_mask", &val);
  eft->event_data1_AND_mask = val;
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "event_data1_compare1", &val);
  eft->event_data1_compare1 = val;
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "event_data1_compare2", &val);
  eft->event_data1_compare2 = val;
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "event_data2_AND_mask", &val);
  eft->event_data2_AND_mask = val;
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "event_data2_compare1", &val);
  eft->event_data2_compare1 = val;
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "event_data2_compare2", &val);
  eft->event_data2_compare2 = val;
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "event_data3_AND_mask", &val);
  eft->event_data3_AND_mask = val;
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "event_data3_compare1", &val);
  eft->event_data3_compare1 = val;
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, "event_data3_compare2", &val);
  eft->event_data3_compare2 = val;
  
  rv = 0;
  
 cleanup:
  FIID_OBJ_DESTROY_NO_RETURN (obj_cmd_rs);
  return (rv);
}

static int 
_record_string_to_event_filter_table (const char *record_string, 
                                      pef_event_filter_table_t *eft)
{
  char *value_string = NULL;
  int n;
  
  assert(record_string);
  assert(eft);
  
  GET_VALUE_STRING_BY_KEY_RETURN (record_string, 
				  FILTER_NUMBER_KEY_STRING, 
				  value_string);
  if ((n = string_to_filter_number (value_string)) < 0)
    {
      fprintf (stderr, 
	       "Invalid value %s for %s\n", 
	       value_string, 
	       FILTER_NUMBER_KEY_STRING);
      return -1;
    }
  eft->filter_number = n;
  
  GET_VALUE_STRING_BY_KEY_RETURN (record_string, 
				  FILTER_TYPE_KEY_STRING, 
				  value_string);
  if ((n = string_to_filter_type (value_string)) < 0)
    {
      fprintf (stderr, 
	       "Invalid value %s for %s\n", 
	       value_string, 
	       FILTER_TYPE_KEY_STRING);
      return -1;
    }
  eft->filter_type = n;
  
  GET_VALUE_STRING_BY_KEY_RETURN (record_string, 
				  ENABLE_FILTER_KEY_STRING, 
				  value_string);
  if ((n = string_to_enable_filter (value_string)) < 0)
    {
      fprintf (stderr, 
	       "Invalid value %s for %s\n", 
	       value_string, 
	       ENABLE_FILTER_KEY_STRING);
      return -1;
    }
  eft->enable_filter = n;
  
  GET_VALUE_STRING_BY_KEY_RETURN (record_string, 
				  EVENT_FILTER_ACTION_ALERT_KEY_STRING, 
				  value_string);
  if ((n = string_to_event_filter_action_alert (value_string)) < 0)
    {
      fprintf (stderr, 
	       "Invalid value %s for %s\n", 
	       value_string, 
	       EVENT_FILTER_ACTION_ALERT_KEY_STRING);
      return -1;
    }
  eft->event_filter_action_alert = n;
  
  GET_VALUE_STRING_BY_KEY_RETURN (record_string, 
				  EVENT_FILTER_ACTION_POWER_OFF_KEY_STRING, 
				  value_string);
  if ((n = string_to_event_filter_action_power_off (value_string)) < 0)
    {
      fprintf (stderr, 
	       "Invalid value %s for %s\n", 
	       value_string, 
	       EVENT_FILTER_ACTION_POWER_OFF_KEY_STRING);
      return -1;
    }
  eft->event_filter_action_power_off = n;
  
  GET_VALUE_STRING_BY_KEY_RETURN (record_string, 
				  EVENT_FILTER_ACTION_RESET_KEY_STRING, 
				  value_string);
  if ((n = string_to_event_filter_action_reset (value_string)) < 0)
    {
      fprintf (stderr, 
	       "Invalid value %s for %s\n", 
	       value_string, 
	       EVENT_FILTER_ACTION_RESET_KEY_STRING);
      return -1;
    }
  eft->event_filter_action_reset = n;
  
  GET_VALUE_STRING_BY_KEY_RETURN (record_string, 
				  EVENT_FILTER_ACTION_POWER_CYCLE_KEY_STRING, 
				  value_string);
  if ((n = string_to_event_filter_action_power_cycle (value_string)) < 0)
    {
      fprintf (stderr, 
	       "Invalid value %s for %s\n", 
	       value_string, 
	       EVENT_FILTER_ACTION_POWER_CYCLE_KEY_STRING);
      return -1;
    }
  eft->event_filter_action_power_cycle = n;
  
  GET_VALUE_STRING_BY_KEY_RETURN (record_string, 
				  EVENT_FILTER_ACTION_OEM_KEY_STRING, 
				  value_string);
  if ((n = string_to_event_filter_action_oem (value_string)) < 0)
    {
      fprintf (stderr, 
	       "Invalid value %s for %s\n", 
	       value_string, 
	       EVENT_FILTER_ACTION_OEM_KEY_STRING);
      return -1;
    }
  eft->event_filter_action_oem = n;
  
  GET_VALUE_STRING_BY_KEY_RETURN (record_string, 
				  EVENT_FILTER_ACTION_DIAGNOSTIC_INTERRUPT_KEY_STRING, 
				  value_string);
  if ((n = string_to_event_filter_action_diagnostic_interrupt (value_string)) < 0)
    {
      fprintf (stderr, 
	       "Invalid value %s for %s\n", 
	       value_string, 
	       EVENT_FILTER_ACTION_DIAGNOSTIC_INTERRUPT_KEY_STRING);
      return -1;
    }
  eft->event_filter_action_diagnostic_interrupt = n;
  
  GET_VALUE_STRING_BY_KEY_RETURN (record_string, 
				  EVENT_FILTER_ACTION_GROUP_CONTROL_OPERATION_KEY_STRING, 
				  value_string);
  if ((n = string_to_event_filter_action_group_control_operation (value_string)) < 0)
    {
      fprintf (stderr, 
	       "Invalid value %s for %s\n", 
	       value_string, 
	       EVENT_FILTER_ACTION_GROUP_CONTROL_OPERATION_KEY_STRING);
      return -1;
    }
  eft->event_filter_action_group_control_operation = n;
  
  GET_VALUE_STRING_BY_KEY_RETURN (record_string, 
				  ALERT_POLICY_NUMBER_KEY_STRING, 
				  value_string);
  if ((n = string_to_alert_policy_number (value_string)) < 0)
    {
      fprintf (stderr, 
	       "Invalid value %s for %s\n", 
	       value_string, 
	       ALERT_POLICY_NUMBER_KEY_STRING);
      return -1;
    }
  eft->alert_policy_number = n;
  
  GET_VALUE_STRING_BY_KEY_RETURN (record_string, 
				  GROUP_CONTROL_SELECTOR_KEY_STRING, 
				  value_string);
  if ((n = string_to_group_control_selector (value_string)) < 0)
    {
      fprintf (stderr, 
	       "Invalid value %s for %s\n", 
	       value_string, 
	       GROUP_CONTROL_SELECTOR_KEY_STRING);
      return -1;
    }
  eft->group_control_selector = n;
  
  GET_VALUE_STRING_BY_KEY_RETURN (record_string, 
				  EVENT_SEVERITY_KEY_STRING, 
				  value_string);
  if ((n = string_to_event_severity (value_string)) < 0)
    {
      fprintf (stderr, 
	       "Invalid value %s for %s\n", 
	       value_string, 
	       EVENT_SEVERITY_KEY_STRING);
      return -1;
    }
  eft->event_severity = n;
  
  GET_VALUE_STRING_BY_KEY_RETURN (record_string, 
				  GENERATOR_ID_BYTE1_KEY_STRING, 
				  value_string);
  if ((n = string_to_generator_id_byte1 (value_string)) < 0)
    {
      fprintf (stderr, 
	       "Invalid value %s for %s\n", 
	       value_string, 
	       GENERATOR_ID_BYTE1_KEY_STRING);
      return -1;
    }
  eft->generator_id_byte1 = n;
  
  GET_VALUE_STRING_BY_KEY_RETURN (record_string, 
				  GENERATOR_ID_BYTE2_KEY_STRING, 
				  value_string);
  if ((n = string_to_generator_id_byte2 (value_string)) < 0)
    {
      fprintf (stderr, 
	       "Invalid value %s for %s\n", 
	       value_string, 
	       GENERATOR_ID_BYTE2_KEY_STRING);
      return -1;
    }
  eft->generator_id_byte2 = n;
  
  GET_VALUE_STRING_BY_KEY_RETURN (record_string, 
				  SENSOR_TYPE_KEY_STRING, 
				  value_string);
  if ((n = string_to_sensor_type (value_string)) < 0)
    {
      fprintf (stderr, 
	       "Invalid value %s for %s\n", 
	       value_string, 
	       SENSOR_TYPE_KEY_STRING);
      return -1;
    }
  eft->sensor_type = n;
  
  GET_VALUE_STRING_BY_KEY_RETURN (record_string, 
				  SENSOR_NUMBER_KEY_STRING, 
				  value_string);
  if ((n = string_to_sensor_number (value_string)) < 0)
    {
      fprintf (stderr, 
	       "Invalid value %s for %s\n", 
	       value_string, 
	       SENSOR_NUMBER_KEY_STRING);
      return -1;
    }
  eft->sensor_number = n;
  
  GET_VALUE_STRING_BY_KEY_RETURN (record_string, 
				  EVENT_TRIGGER_KEY_STRING, 
				  value_string);
  if ((n = string_to_event_trigger (value_string)) < 0)
    {
      fprintf (stderr, 
	       "Invalid value %s for %s\n", 
	       value_string, 
	       EVENT_TRIGGER_KEY_STRING);
      return -1;
    }
  eft->event_trigger = n;
  
  GET_VALUE_STRING_BY_KEY_RETURN (record_string, 
				  EVENT_DATA1_OFFSET_MASK_KEY_STRING, 
				  value_string);
  if ((n = string_to_event_data1_offset_mask (value_string)) < 0)
    {
      fprintf (stderr, 
	       "Invalid value %s for %s\n", 
	       value_string, 
	       EVENT_DATA1_OFFSET_MASK_KEY_STRING);
      return -1;
    }
  eft->event_data1_offset_mask = n;
  
  GET_VALUE_STRING_BY_KEY_RETURN (record_string, 
				  EVENT_DATA1_AND_MASK_KEY_STRING, 
				  value_string);
  if ((n = string_to_event_data1_AND_mask (value_string)) < 0)
    {
      fprintf (stderr, 
	       "Invalid value %s for %s\n", 
	       value_string, 
	       EVENT_DATA1_AND_MASK_KEY_STRING);
      return -1;
    }
  eft->event_data1_AND_mask = n;
  
  GET_VALUE_STRING_BY_KEY_RETURN (record_string, 
				  EVENT_DATA1_COMPARE1_KEY_STRING, 
				  value_string);
  if ((n = string_to_event_data1_compare1 (value_string)) < 0)
    {
      fprintf (stderr, 
	       "Invalid value %s for %s\n", 
	       value_string, 
	       EVENT_DATA1_COMPARE1_KEY_STRING);
      return -1;
    }
  eft->event_data1_compare1 = n;
  
  GET_VALUE_STRING_BY_KEY_RETURN (record_string, 
				  EVENT_DATA1_COMPARE2_KEY_STRING, 
				  value_string);
  if ((n = string_to_event_data1_compare2 (value_string)) < 0)
    {
      fprintf (stderr, 
	       "Invalid value %s for %s\n", 
	       value_string, 
	       EVENT_DATA1_COMPARE2_KEY_STRING);
      return -1;
    }
  eft->event_data1_compare2 = n;
  
  GET_VALUE_STRING_BY_KEY_RETURN (record_string, 
				  EVENT_DATA2_AND_MASK_KEY_STRING, 
				  value_string);
  if ((n = string_to_event_data2_AND_mask (value_string)) < 0)
    {
      fprintf (stderr, 
	       "Invalid value %s for %s\n", 
	       value_string, 
	       EVENT_DATA2_AND_MASK_KEY_STRING);
      return -1;
    }
  eft->event_data2_AND_mask = n;
  
  GET_VALUE_STRING_BY_KEY_RETURN (record_string, 
				  EVENT_DATA2_COMPARE1_KEY_STRING, 
				  value_string);
  if ((n = string_to_event_data2_compare1 (value_string)) < 0)
    {
      fprintf (stderr, 
	       "Invalid value %s for %s\n", 
	       value_string, 
	       EVENT_DATA2_COMPARE1_KEY_STRING);
      return -1;
    }
  eft->event_data2_compare1 = n;
  
  GET_VALUE_STRING_BY_KEY_RETURN (record_string, 
				  EVENT_DATA2_COMPARE2_KEY_STRING, 
				  value_string);
  if ((n = string_to_event_data2_compare2 (value_string)) < 0)
    {
      fprintf (stderr, 
	       "Invalid value %s for %s\n", 
	       value_string, 
	       EVENT_DATA2_COMPARE2_KEY_STRING);
      return -1;
    }
  eft->event_data2_compare2 = n;
  
  GET_VALUE_STRING_BY_KEY_RETURN (record_string, 
				  EVENT_DATA3_AND_MASK_KEY_STRING, 
				  value_string);
  if ((n = string_to_event_data3_AND_mask (value_string)) < 0)
    {
      fprintf (stderr, 
	       "Invalid value %s for %s\n", 
	       value_string, 
	       EVENT_DATA3_AND_MASK_KEY_STRING);
      return -1;
    }
  eft->event_data3_AND_mask = n;
  
  GET_VALUE_STRING_BY_KEY_RETURN (record_string, 
				  EVENT_DATA3_COMPARE1_KEY_STRING, 
				  value_string);
  if ((n = string_to_event_data3_compare1 (value_string)) < 0)
    {
      fprintf (stderr, 
	       "Invalid value %s for %s\n", 
	       value_string, 
	       EVENT_DATA3_COMPARE1_KEY_STRING);
      return -1;
    }
  eft->event_data3_compare1 = n;
  
  GET_VALUE_STRING_BY_KEY_RETURN (record_string, 
				  EVENT_DATA3_COMPARE2_KEY_STRING, 
				  value_string);
  if ((n = string_to_event_data3_compare2 (value_string)) < 0)
    {
      fprintf (stderr, 
	       "Invalid value %s for %s\n", 
	       value_string, 
	       EVENT_DATA3_COMPARE2_KEY_STRING);
      return -1;
    }
  eft->event_data3_compare2 = n;
  
  return 0;
}

int 
get_event_filter_table_list (FILE *fp, pef_event_filter_table_t **eft_list, int *count)
{
  pef_event_filter_table_t *l_eft_list = NULL;
  int l_count;
  char *record = NULL;
  int i;
  int rv;
  
  assert(fp);
  assert(eft_list);
  assert(count);

  if (_get_record_count (fp, &l_count) != 0)
    return (-1);
  
  if (!(l_eft_list = (pef_event_filter_table_t *) calloc (l_count, 
                                                          sizeof (pef_event_filter_table_t))))
    {
      perror("calloc");
      return -1;
    }
  
  fseek (fp, 0, SEEK_SET);
  
  for (i = 0; i < l_count; i++)
    {
      _fread_record (fp, &record);
      rv = _record_string_to_event_filter_table (record, &l_eft_list[i]);
      free (record);
      if (rv != 0)
	{
	  free (l_eft_list);
	  return (-1);
	}
    }
  
  *eft_list = l_eft_list;
  *count = l_count;
  
  return 0;
}

int 
set_event_filter_table (struct ipmi_pef_state_data *state_data, pef_event_filter_table_t *eft)
{
  fiid_obj_t obj_cmd_rs = NULL;
  int rv = -1;
  
  assert(state_data);
  assert(eft);
  
  FIID_OBJ_CREATE (obj_cmd_rs, tmpl_cmd_set_pef_configuration_parameters_rs);
  
  if (ipmi_cmd_set_pef_configuration_parameters_event_filter_table (state_data->dev, 
								    eft->filter_number, 
								    eft->filter_type, 
								    eft->enable_filter, 
								    eft->event_filter_action_alert, 
								    eft->event_filter_action_power_off, 
								    eft->event_filter_action_reset, 
								    eft->event_filter_action_power_cycle, 
								    eft->event_filter_action_oem, 
								    eft->event_filter_action_diagnostic_interrupt, 
								    eft->event_filter_action_group_control_operation, 
								    eft->alert_policy_number, 
								    eft->group_control_selector, 
								    eft->event_severity, 
								    eft->generator_id_byte1, 
								    eft->generator_id_byte2, 
								    eft->sensor_type, 
								    eft->sensor_number, 
								    eft->event_trigger, 
								    eft->event_data1_offset_mask, 
								    eft->event_data1_AND_mask, 
								    eft->event_data1_compare1, 
								    eft->event_data1_compare2, 
								    eft->event_data2_AND_mask, 
								    eft->event_data2_compare1, 
								    eft->event_data2_compare2, 
								    eft->event_data3_AND_mask, 
								    eft->event_data3_compare1, 
								    eft->event_data3_compare2, 
								    obj_cmd_rs) != 0)
    goto cleanup;
  
  rv = 0;
  
 cleanup:
  FIID_OBJ_DESTROY_NO_RETURN (obj_cmd_rs);
  return (rv);
}



