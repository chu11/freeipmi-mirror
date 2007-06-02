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
#include "ipmi-common.h"
#include "freeipmi-portability.h"

#include "pef-config.h"
#include "pef-config-common.h"
#include "pef-config-map.h"
#include "pef-config-utils.h"
#include "pef-config-wrapper.h"

pef_err_t
get_pef_control (pef_config_state_data_t *state_data,
                 uint8_t *pef,
                 uint8_t *pef_event_messages,
                 uint8_t *pef_startup_delay,
                 uint8_t *pef_alert_startup_delay)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val = 0;
  pef_err_t rv = PEF_ERR_FATAL_ERROR;

  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_get_pef_configuration_parameters_pef_control_rs)))
    goto cleanup;

  if (ipmi_cmd_get_pef_configuration_parameters_pef_control (state_data->dev,
                                                             IPMI_GET_PEF_PARAMETER,
                                                             SET_SELECTOR,
                                                             BLOCK_SELECTOR,
                                                             obj_cmd_rs) < 0)
    {
      rv = PEF_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  if (fiid_obj_get (obj_cmd_rs, "pef", &val) < 0)
    goto cleanup;
  *pef = val;

  if (fiid_obj_get (obj_cmd_rs, "pef_event_messages", &val) < 0)
    goto cleanup;
  *pef_event_messages = val;

  if (fiid_obj_get (obj_cmd_rs, "pef_startup_delay", &val) < 0)
    goto cleanup;
  *pef_startup_delay = val;

  if (fiid_obj_get (obj_cmd_rs, "pef_alert_startup_delay", &val) < 0)
    goto cleanup;
  *pef_alert_startup_delay = val;

  rv = PEF_ERR_SUCCESS;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

pef_err_t
set_pef_control (pef_config_state_data_t *state_data,
                 uint8_t pef,
                 uint8_t pef_event_messages,
                 uint8_t pef_startup_delay,
                 uint8_t pef_alert_startup_delay)
{
  fiid_obj_t obj_cmd_rs = NULL;
  pef_err_t rv = PEF_ERR_FATAL_ERROR;

  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_set_pef_configuration_parameters_rs)))
    goto cleanup;

  if (ipmi_cmd_set_pef_configuration_parameters_pef_control (state_data->dev,
                                                             pef,
                                                             pef_event_messages,
                                                             pef_startup_delay,
                                                             pef_alert_startup_delay,
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
get_pef_action_global_control (pef_config_state_data_t *state_data,
                               uint8_t *alert_action,
                               uint8_t *power_down_action,
                               uint8_t *reset_action,
                               uint8_t *power_cycle_action,
                               uint8_t *oem_action,
                               uint8_t *diagnostic_interrupt)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val = 0;
  pef_err_t rv = PEF_ERR_FATAL_ERROR;

  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_get_pef_configuration_parameters_pef_action_global_control_rs)))
    goto cleanup;

  if (ipmi_cmd_get_pef_configuration_parameters_pef_action_global_control (state_data->dev,
                                                                           IPMI_GET_PEF_PARAMETER,
                                                                           SET_SELECTOR,
                                                                           BLOCK_SELECTOR,
                                                                           obj_cmd_rs) < 0)
    {
      rv = PEF_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  if (fiid_obj_get (obj_cmd_rs, "alert_action", &val) < 0)
    goto cleanup;
  *alert_action = val;

  if (fiid_obj_get (obj_cmd_rs, "power_down_action", &val) < 0)
    goto cleanup;
  *power_down_action = val;

  if (fiid_obj_get (obj_cmd_rs, "reset_action", &val) < 0)
    goto cleanup;
  *reset_action = val;

  if (fiid_obj_get (obj_cmd_rs, "power_cycle_action", &val) < 0)
    goto cleanup;
  *power_cycle_action = val;

  if (fiid_obj_get (obj_cmd_rs, "oem_action", &val) < 0)
    goto cleanup;
  *oem_action = val;

  if (fiid_obj_get (obj_cmd_rs, "diagnostic_interrupt", &val) < 0)
    goto cleanup;
  *diagnostic_interrupt = val;

  rv = PEF_ERR_SUCCESS;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

pef_err_t
set_pef_action_global_control (pef_config_state_data_t *state_data,
                               uint8_t alert_action,
                               uint8_t power_down_action,
                               uint8_t reset_action,
                               uint8_t power_cycle_action,
                               uint8_t oem_action,
                               uint8_t diagnostic_interrupt)
{
  fiid_obj_t obj_cmd_rs = NULL;
  pef_err_t rv = PEF_ERR_FATAL_ERROR;

  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_set_pef_configuration_parameters_rs)))
    goto cleanup;

  if (ipmi_cmd_set_pef_configuration_parameters_pef_action_global_control (state_data->dev,
                                                                           alert_action,
                                                                           power_down_action,
                                                                           reset_action,
                                                                           power_cycle_action,
                                                                           oem_action,
                                                                           diagnostic_interrupt,
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
get_pef_startup_delay (pef_config_state_data_t *state_data,
                       uint8_t *pef_startup_delay)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val = 0;
  pef_err_t rv = PEF_ERR_FATAL_ERROR;

  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_get_pef_configuration_parameters_pef_startup_delay_rs)))
    goto cleanup;

  if (ipmi_cmd_get_pef_configuration_parameters_pef_startup_delay (state_data->dev,
                                                                   IPMI_GET_PEF_PARAMETER,
                                                                   SET_SELECTOR,
                                                                   BLOCK_SELECTOR,
                                                                   obj_cmd_rs) < 0)
    {
      rv = PEF_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  if (fiid_obj_get (obj_cmd_rs, "pef_startup_delay", &val) < 0)
    goto cleanup;
  *pef_startup_delay = val;

  rv = PEF_ERR_SUCCESS;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

pef_err_t
set_pef_startup_delay (pef_config_state_data_t *state_data,
                       uint8_t pef_startup_delay)
{
  fiid_obj_t obj_cmd_rs = NULL;
  pef_err_t rv = PEF_ERR_FATAL_ERROR;

  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_set_pef_configuration_parameters_rs)))
    goto cleanup;

  if (ipmi_cmd_set_pef_configuration_parameters_pef_startup_delay (state_data->dev,
                                                                   pef_startup_delay,
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
get_pef_alert_startup_delay (pef_config_state_data_t *state_data,
                             uint8_t *pef_alert_startup_delay)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val = 0;
  pef_err_t rv = PEF_ERR_FATAL_ERROR;

  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_get_pef_configuration_parameters_pef_alert_startup_delay_rs)))
    goto cleanup;

  if (ipmi_cmd_get_pef_configuration_parameters_pef_alert_startup_delay (state_data->dev,
                                                                         IPMI_GET_PEF_PARAMETER,
                                                                         SET_SELECTOR,
                                                                         BLOCK_SELECTOR,
                                                                         obj_cmd_rs) < 0)
    {
      rv = PEF_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  if (fiid_obj_get (obj_cmd_rs, "pef_alert_startup_delay", &val) < 0)
    goto cleanup;
  *pef_alert_startup_delay = val;

  rv = PEF_ERR_SUCCESS;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

pef_err_t
set_pef_alert_startup_delay (pef_config_state_data_t *state_data,
                             uint8_t pef_alert_startup_delay)
{
  fiid_obj_t obj_cmd_rs = NULL;
  pef_err_t rv = PEF_ERR_FATAL_ERROR;

  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_set_pef_configuration_parameters_rs)))
    goto cleanup;

  if (ipmi_cmd_set_pef_configuration_parameters_pef_alert_startup_delay (state_data->dev,
                                                                         pef_alert_startup_delay,
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
get_pef_alert_string_keys (pef_config_state_data_t *state_data,
                           uint8_t string_selector,
                           uint8_t *event_filter_number,
                           uint8_t *alert_string_set)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val = 0;
  pef_err_t rv = PEF_ERR_FATAL_ERROR;

  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_get_pef_configuration_parameters_alert_string_keys_rs)))
    goto cleanup;

  if (ipmi_cmd_get_pef_configuration_parameters_alert_string_keys (state_data->dev,
                                                                   IPMI_GET_PEF_PARAMETER,
                                                                   string_selector,
                                                                   BLOCK_SELECTOR,
                                                                   obj_cmd_rs) < 0)
    {
      rv = PEF_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  if (fiid_obj_get (obj_cmd_rs, "filter_number", &val) < 0)
    goto cleanup;
  *event_filter_number = val;

  if (fiid_obj_get (obj_cmd_rs, "set_number_for_string", &val) < 0)
    goto cleanup;
  *alert_string_set = val;

  rv = PEF_ERR_SUCCESS;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

pef_err_t
set_pef_alert_string_keys (pef_config_state_data_t *state_data,
                           uint8_t string_selector,
                           uint8_t event_filter_number,
                           uint8_t alert_string_set)
{
  fiid_obj_t obj_cmd_rs = NULL;
  pef_err_t rv = PEF_ERR_FATAL_ERROR;

  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_set_pef_configuration_parameters_rs)))
    goto cleanup;

  if (ipmi_cmd_set_pef_configuration_parameters_alert_string_keys (state_data->dev,
                                                                   string_selector,
                                                                   event_filter_number,
                                                                   alert_string_set,
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
get_pef_alert_string (pef_config_state_data_t *state_data,
                      uint8_t string_selector,
                      uint8_t *alert_string,
                      uint32_t alert_string_len)
{
  fiid_obj_t obj_cmd_rs = NULL;
  pef_err_t rv = PEF_ERR_FATAL_ERROR;
  int blocks;
  int i;

  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_get_pef_configuration_parameters_alert_strings_rs)))
    goto cleanup;

  memset(alert_string, '\0', alert_string_len);

  if (!((alert_string_len - 1) % 16))
    blocks = (alert_string_len - 1)/16;
  else
    blocks = (alert_string_len - 1)/16 + 1;

  for (i = 0; i < blocks; i++)
    {
      fiid_obj_clear(obj_cmd_rs);
      int j;

      if (ipmi_cmd_get_pef_configuration_parameters_alert_string (state_data->dev,
                                                                  IPMI_GET_PEF_PARAMETER,
                                                                  string_selector,
                                                                  i + 1,
                                                                  obj_cmd_rs) < 0)
        {
          rv = PEF_ERR_NON_FATAL_ERROR;
          goto cleanup;
        }
      
      /* XXX: Be lazy for now, assume no strings will overflow
       * whatever is passed in, so don't check for overflow errors
       * from fiid_obj_get_data.
       */
      if (fiid_obj_get_data (obj_cmd_rs,
                             "string_data",
                             alert_string + (i * 16),
                             (alert_string_len - 1) - (i * 16)) < 0)
        goto cleanup;

      /* Check if we've found a nul character */
      for (j = 0; j < 16; j++)
        {
          if (!((alert_string + (i * 16))[j]))
            goto done;
        }
    }

 done:
  rv = PEF_ERR_SUCCESS;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

pef_err_t
set_pef_alert_string (pef_config_state_data_t *state_data,
                      uint8_t string_selector,
                      uint8_t *alert_string)
{
  fiid_obj_t obj_cmd_rs = NULL;
  pef_err_t rv = PEF_ERR_FATAL_ERROR;
  uint8_t *alert_string_buf = NULL;
  int alert_string_len = 0;
  int alert_string_buf_len = 0;
  int blocks;
  int i;

  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_set_pef_configuration_parameters_rs)))
    goto cleanup;
  
  if (alert_string)
    alert_string_len = strlen(alert_string);

  /* We need to write a nul char, so count it as part of the buflen */
  alert_string_buf_len = alert_string_len + 1;
  
  if (!(alert_string_buf = (uint8_t *)malloc(alert_string_buf_len)))
    {
      perror("strdup");
      goto cleanup;
    }
  memset(alert_string_buf, '\0', alert_string_buf_len);

  if (alert_string && alert_string_len)
    memcpy(alert_string_buf, alert_string, alert_string_len);

  if (!((alert_string_buf_len) % 16))
    blocks = (alert_string_buf_len)/16;
  else
    blocks = (alert_string_buf_len)/16 + 1;

  for (i = 0; i < blocks; i++)
    {
      uint8_t len_to_write;

      if ((alert_string_buf_len - (i * 16)) < 16)
        len_to_write = alert_string_buf_len - (i * 16);
      else
        len_to_write = 16;

      if (ipmi_cmd_set_pef_configuration_parameters_alert_strings (state_data->dev,
                                                                   string_selector,
                                                                   i+1,
                                                                   alert_string_buf + (i * 16),
                                                                   len_to_write,
                                                                   obj_cmd_rs) < 0)
        {
          rv = PEF_ERR_NON_FATAL_ERROR;
          goto cleanup;
        }
    }

  rv = PEF_ERR_SUCCESS;
 cleanup:
  if (alert_string_buf)
    free(alert_string_buf);
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

pef_err_t
get_bmc_community_string (pef_config_state_data_t *state_data,
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
set_bmc_community_string (pef_config_state_data_t *state_data,
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
get_bmc_destination_type(pef_config_state_data_t *state_data,
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
set_bmc_destination_type(pef_config_state_data_t *state_data,
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
set_bmc_destination_addresses(pef_config_state_data_t *state_data,
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
get_bmc_pef_conf_event_filter_table (struct pef_config_state_data *state_data, 
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
                                     uint16_t *event_data1_offset_mask,
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
set_bmc_pef_conf_event_filter_table (struct pef_config_state_data *state_data, 
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
                                     uint16_t event_data1_offset_mask,
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

