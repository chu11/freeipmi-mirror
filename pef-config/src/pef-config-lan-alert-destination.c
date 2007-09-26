#if HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */

#include "pef-config.h"
#include "pef-config-map.h"
#include "pef-config-utils.h"
#include "pef-config-validate.h"
#include "pef-config-wrapper.h"

#include "freeipmi-portability.h"

#define PEF_CONFIG_MAXIPADDRLEN 16
#define PEF_CONFIG_MAXMACADDRLEN 24

static config_err_t
destination_type_get (pef_config_state_data_t *state_data,
                      uint8_t destination_selector,
                      uint8_t *alert_destination_type,
                      uint8_t *alert_acknowledge,
                      uint8_t *alert_acknowledge_timeout,
                      uint8_t *alert_retries)
{
  uint8_t tmp_alert_destination_type;
  uint8_t tmp_alert_acknowledge;
  uint8_t tmp_alert_acknowledge_timeout;
  uint8_t tmp_alert_retries;
  config_err_t ret;

  if ((ret = get_bmc_destination_type (state_data,
                                       destination_selector,
                                       &tmp_alert_destination_type,
                                       &tmp_alert_acknowledge,
                                       &tmp_alert_acknowledge_timeout,
                                       &tmp_alert_retries)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (alert_destination_type)
    *alert_destination_type = tmp_alert_destination_type;
  if (alert_acknowledge)
    *alert_acknowledge = tmp_alert_acknowledge;
  if (alert_acknowledge_timeout)
    *alert_acknowledge_timeout = tmp_alert_acknowledge_timeout;
  if (alert_retries)
    *alert_retries = tmp_alert_retries;

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
destination_type_set (pef_config_state_data_t *state_data,
                      uint8_t destination_selector,
                      uint8_t alert_destination_type,
                      uint8_t alert_destination_type_is_set,
                      uint8_t alert_acknowledge,
                      uint8_t alert_acknowledge_is_set,
                      uint8_t alert_acknowledge_timeout,
                      uint8_t alert_acknowledge_timeout_is_set,
                      uint8_t alert_retries,
                      uint8_t alert_retries_is_set)
{
  uint8_t tmp_alert_destination_type;
  uint8_t tmp_alert_acknowledge;
  uint8_t tmp_alert_acknowledge_timeout;
  uint8_t tmp_alert_retries;
  config_err_t ret;

  if ((ret = get_bmc_destination_type(state_data,
                                      destination_selector,
                                      &tmp_alert_destination_type,
                                      &tmp_alert_acknowledge,
                                      &tmp_alert_acknowledge_timeout,
                                      &tmp_alert_retries)) != CONFIG_ERR_SUCCESS)
    return ret;
  
  if (alert_destination_type_is_set)
    tmp_alert_destination_type = alert_destination_type;
  if (alert_acknowledge_is_set)
    tmp_alert_acknowledge = alert_acknowledge;
  if (alert_acknowledge_timeout_is_set)
    tmp_alert_acknowledge_timeout = alert_acknowledge_timeout;
  if (alert_retries_is_set)
    tmp_alert_retries = alert_retries;

  if ((ret = set_bmc_destination_type(state_data,
                                      destination_selector,
                                      tmp_alert_destination_type,
                                      tmp_alert_acknowledge,
                                      tmp_alert_acknowledge_timeout,
                                      tmp_alert_retries)) != CONFIG_ERR_SUCCESS)
    return ret;

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
alert_destination_type_checkout (const char *section_name,
                                 struct config_keyvalue *kv,
                                 void *arg)
{
  pef_config_state_data_t *state_data = (pef_config_state_data_t *)arg;
  uint8_t destination_type;
  config_err_t ret;
  uint8_t destination_selector;

  destination_selector = atoi (section_name + strlen ("Lan_Alert_Destination_"));

  if ((ret = destination_type_get (state_data,
                                   destination_selector,
                                   &destination_type,
                                   NULL,
                                   NULL,
                                   NULL)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (config_section_update_keyvalue_output(kv, alert_destination_type_string (destination_type)) < 0)
    return CONFIG_ERR_FATAL_ERROR;
  
  return CONFIG_ERR_SUCCESS;
}

static config_err_t
alert_destination_type_commit (const char *section_name,
                               const struct config_keyvalue *kv,
                               void *arg)
{
  pef_config_state_data_t *state_data = (pef_config_state_data_t *)arg;
  uint8_t destination_selector;

  destination_selector = atoi (section_name + strlen ("Lan_Alert_Destination_"));

  return destination_type_set (state_data,
                               destination_selector,
                               alert_destination_type_number (kv->value_input), 1,
                               0, 0,
                               0, 0,
                               0, 0);
}

static config_err_t
alert_acknowledge_checkout (const char *section_name,
                            struct config_keyvalue *kv,
                            void *arg)
{
  pef_config_state_data_t *state_data = (pef_config_state_data_t *)arg;
  uint8_t alert_acknowledge;
  config_err_t ret;
  uint8_t destination_selector;

  destination_selector = atoi (section_name + strlen ("Lan_Alert_Destination_"));

  if ((ret = destination_type_get (state_data,
                                   destination_selector,
                                   NULL,
                                   &alert_acknowledge,
                                   NULL,
                                   NULL)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (config_section_update_keyvalue_output(kv, alert_acknowledge ? "Yes" : "No") < 0)
    return CONFIG_ERR_FATAL_ERROR;
  
  return CONFIG_ERR_SUCCESS;
}

static config_err_t
alert_acknowledge_commit (const char *section_name,
                          const struct config_keyvalue *kv,
                          void *arg)
{
  pef_config_state_data_t *state_data = (pef_config_state_data_t *)arg;
  uint8_t destination_selector;

  destination_selector = atoi (section_name + strlen ("Lan_Alert_Destination_"));

  return destination_type_set (state_data,
                               destination_selector,
                               0, 0,
                               same (kv->value_input, "yes"), 1,
                               0, 0,
                               0, 0);
}

static config_err_t
alert_acknowledge_timeout_checkout (const char *section_name,
                                    struct config_keyvalue *kv,
                                    void *arg)
{
  pef_config_state_data_t *state_data = (pef_config_state_data_t *)arg;
  uint8_t alert_acknowledge_timeout;
  config_err_t ret;
  uint8_t destination_selector;
  
  destination_selector = atoi (section_name + strlen ("Lan_Alert_Destination_"));

  if ((ret = destination_type_get (state_data,
                                   destination_selector,
                                   NULL,
                                   NULL,
                                   &alert_acknowledge_timeout,
                                   NULL)) != CONFIG_ERR_SUCCESS)
    return ret;

  if (config_section_update_keyvalue_output_int(kv, alert_acknowledge_timeout) < 0)
    return CONFIG_ERR_FATAL_ERROR;

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
alert_acknowledge_timeout_commit (const char *section_name,
                                  const struct config_keyvalue *kv,
                                  void *arg)
{
  pef_config_state_data_t *state_data = (pef_config_state_data_t *)arg;
  uint8_t destination_selector;
  uint8_t alert_acknowledge_timeout;

  destination_selector = atoi (section_name + strlen ("Lan_Alert_Destination_"));

  alert_acknowledge_timeout = atoi (kv->value_input);

  return destination_type_set (state_data,
                               destination_selector,
                               0, 0,
                               0, 0,
                               alert_acknowledge_timeout, 1,
                               0, 0);
}

static config_err_t
alert_retries_checkout (const char *section_name,
                        struct config_keyvalue *kv,
                        void *arg)
{
  pef_config_state_data_t *state_data = (pef_config_state_data_t *)arg;
  uint8_t alert_retries;
  config_err_t ret;
  uint8_t destination_selector;
  
  destination_selector = atoi (section_name + strlen ("Lan_Alert_Destination_"));

  if ((ret = destination_type_get (state_data,
                                   destination_selector,
                                   NULL,
                                   NULL,
                                   NULL,
                                   &alert_retries)) != CONFIG_ERR_SUCCESS)
    return ret;
  
  if (config_section_update_keyvalue_output_int(kv, alert_retries) < 0)
    return CONFIG_ERR_FATAL_ERROR;

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
alert_retries_commit (const char *section_name,
                      const struct config_keyvalue *kv,
                      void *arg)
{
  pef_config_state_data_t *state_data = (pef_config_state_data_t *)arg;
  uint8_t destination_selector;
  uint8_t alert_retries;

  destination_selector = atoi (section_name + strlen ("Lan_Alert_Destination_"));

  alert_retries = atoi (kv->value_input);

  return destination_type_set (state_data,
                               destination_selector,
                               0, 0,
                               0, 0,
                               0, 0,
                               alert_retries, 1);
}

config_validate_t
alert_retries_validate (const char *section_name,
                        const char *key_name,
                        const char *value)
{
  return config_check_number_range(value, 0, IPMI_ALERT_RETRIES_MAX);
}

static config_err_t
destination_addresses_get (pef_config_state_data_t *state_data,
                           uint8_t destination_selector,
                           uint8_t *alert_gateway,
                           char *alert_ip_address,
                           unsigned int alert_ip_address_len,
                           char *alert_mac_address,
                           unsigned int alert_mac_address_len)
{
  uint8_t tmp_alert_gateway;
  char tmp_ip[PEF_CONFIG_MAXIPADDRLEN + 1];
  char tmp_mac[PEF_CONFIG_MAXMACADDRLEN+1];
  config_err_t ret;

  memset(tmp_ip, '\0', PEF_CONFIG_MAXIPADDRLEN + 1);
  memset(tmp_mac, '\0', PEF_CONFIG_MAXMACADDRLEN+1);

  if ((ret = get_bmc_destination_addresses (state_data,
                                            destination_selector,
                                            &tmp_alert_gateway,
                                            tmp_ip,
                                            PEF_CONFIG_MAXIPADDRLEN + 1,
                                            tmp_mac,
                                            PEF_CONFIG_MAXMACADDRLEN+1)) != CONFIG_ERR_SUCCESS)
    return ret;
  
  if (alert_gateway)
    *alert_gateway = tmp_alert_gateway;
  if (alert_ip_address)
    { 
      if (alert_ip_address_len >= PEF_CONFIG_MAXIPADDRLEN + 1)
        memcpy(alert_ip_address, tmp_ip, PEF_CONFIG_MAXIPADDRLEN + 1);
      else
        return CONFIG_ERR_FATAL_ERROR;
    }
  if (alert_mac_address)
    { 
      if (alert_mac_address_len >= PEF_CONFIG_MAXMACADDRLEN + 1)
        memcpy(alert_mac_address, tmp_mac, PEF_CONFIG_MAXMACADDRLEN + 1);
      else
        return CONFIG_ERR_FATAL_ERROR;
    }

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
destination_addresses_set (pef_config_state_data_t *state_data,
                           uint8_t destination_selector,
                           uint8_t alert_gateway,
                           int alert_gateway_is_set,
                           char *alert_ip_address,
                           int alert_ip_address_is_set,
                           char *alert_mac_address,
                           int alert_mac_address_is_set)
{
  uint8_t tmp_alert_gateway;
  char tmp_ip[PEF_CONFIG_MAXIPADDRLEN + 1];
  char tmp_mac[PEF_CONFIG_MAXMACADDRLEN+1];
  char *tmp_ip_ptr;
  char *tmp_mac_ptr;
  config_err_t ret;

  memset(tmp_ip, '\0', PEF_CONFIG_MAXIPADDRLEN + 1);
  memset(tmp_mac, '\0', PEF_CONFIG_MAXMACADDRLEN+1);

  tmp_ip_ptr = tmp_ip;
  tmp_mac_ptr = tmp_mac;

  if ((ret = get_bmc_destination_addresses (state_data,
                                            destination_selector,
                                            &tmp_alert_gateway,
                                            tmp_ip_ptr,
                                            PEF_CONFIG_MAXIPADDRLEN+1,
                                            tmp_mac_ptr,
                                            PEF_CONFIG_MAXMACADDRLEN+1)) != CONFIG_ERR_SUCCESS)
    return ret;
  
  if (alert_gateway_is_set)
    tmp_alert_gateway = alert_gateway;
  if (alert_ip_address && alert_ip_address_is_set)
    tmp_ip_ptr = alert_ip_address;
  if (alert_mac_address && alert_mac_address_is_set)
    tmp_mac_ptr = alert_mac_address;

  if ((ret = set_bmc_destination_addresses(state_data,
                                           destination_selector,
                                           tmp_alert_gateway,
                                           tmp_ip_ptr,
                                           tmp_mac_ptr)) != CONFIG_ERR_SUCCESS)
    return ret;

  return CONFIG_ERR_SUCCESS;
}

static config_err_t
alert_gateway_checkout (const char *section_name,
                        struct config_keyvalue *kv,
                        void *arg)
{
  pef_config_state_data_t *state_data = (pef_config_state_data_t *)arg;
  uint8_t gateway;
  config_err_t ret;
  uint8_t destination_selector;
  
  destination_selector = atoi (section_name + strlen ("Lan_Alert_Destination_"));

  if ((ret = destination_addresses_get (state_data,
                                        destination_selector,
                                        &gateway,
                                        NULL,
                                        0,
                                        NULL,
                                        0)) != CONFIG_ERR_SUCCESS)
    return ret;
  
  if (config_section_update_keyvalue_output(kv, alert_gateway_string (gateway)) < 0)
    return CONFIG_ERR_FATAL_ERROR;
  
  return CONFIG_ERR_SUCCESS;
}

static config_err_t
alert_gateway_commit (const char *section_name,
                      const struct config_keyvalue *kv,
                      void *arg)
{
  pef_config_state_data_t *state_data = (pef_config_state_data_t *)arg;
  uint8_t destination_selector;

  destination_selector = atoi (section_name + strlen ("Lan_Alert_Destination_"));

  return destination_addresses_set (state_data,
                                    destination_selector,
                                    alert_gateway_number (kv->value_input), 1,
                                    NULL, 0,
                                    NULL, 0);
}

static config_err_t
alert_ip_address_checkout (const char *section_name,
                           struct config_keyvalue *kv,
                           void *arg)
{
  pef_config_state_data_t *state_data = (pef_config_state_data_t *)arg;
  config_err_t ret;
  char alert_ip[PEF_CONFIG_MAXIPADDRLEN + 1];
  uint8_t destination_selector;
  
  destination_selector = atoi (section_name + strlen ("Lan_Alert_Destination_"));
  
  if ((ret = destination_addresses_get (state_data,
                                        destination_selector,
                                        NULL,
                                        alert_ip,
                                        PEF_CONFIG_MAXIPADDRLEN + 1,
                                        NULL,
                                        0)) != CONFIG_ERR_SUCCESS)
    return ret;
  
  if (config_section_update_keyvalue_output(kv, alert_ip) < 0)
    return CONFIG_ERR_FATAL_ERROR;
  
  return CONFIG_ERR_SUCCESS;
}

static config_err_t
alert_ip_address_commit (const char *section_name,
                         const struct config_keyvalue *kv,
                         void *arg)
{
  pef_config_state_data_t *state_data = (pef_config_state_data_t *)arg;
  uint8_t destination_selector;

  destination_selector = atoi (section_name + strlen ("Lan_Alert_Destination_"));

  return destination_addresses_set (state_data,
                                    destination_selector,
                                    0, 0,
                                    kv->value_input, 1,
                                    NULL, 0);
}

static config_err_t
alert_mac_address_checkout (const char *section_name,
                            struct config_keyvalue *kv,
                            void *arg)
{
  pef_config_state_data_t *state_data = (pef_config_state_data_t *)arg;
  config_err_t ret;
  char alert_mac[PEF_CONFIG_MAXMACADDRLEN + 1];
  uint8_t destination_selector;
  
  destination_selector = atoi (section_name + strlen ("Lan_Alert_Destination_"));
  
  if ((ret = destination_addresses_get (state_data,
                                        destination_selector,
                                        NULL,
                                        NULL,
                                        0,
                                        alert_mac,
                                        PEF_CONFIG_MAXMACADDRLEN + 1)) != CONFIG_ERR_SUCCESS)
    return ret;
  
  if (config_section_update_keyvalue_output(kv, alert_mac) < 0)
    return CONFIG_ERR_FATAL_ERROR;
  
  return CONFIG_ERR_SUCCESS;
}

static config_err_t
alert_mac_address_commit (const char *section_name,
                          const struct config_keyvalue *kv,
                          void *arg)
{
  pef_config_state_data_t *state_data = (pef_config_state_data_t *)arg;
  uint8_t destination_selector;

  destination_selector = atoi (section_name + strlen ("Lan_Alert_Destination_"));

  return destination_addresses_set (state_data,
                                    destination_selector,
                                    0, 0,
                                    NULL, 0,
                                    kv->value_input, 1);
}

struct config_section *
pef_config_lan_alert_destination_section_get (pef_config_state_data_t *state_data, int num)
{
  struct config_section *section = NULL;
  char buf[64];

  if (num <= 0)
    {
      fprintf(stderr, "Invalid Num = %d\n", num);
      return NULL;
    }

  snprintf(buf, 64, "Lan_Alert_Destination_%d", num);

  if (!(section = config_section_create (buf, 
                                         NULL, 
                                         NULL, 
                                         0)))
    goto cleanup;

  if (config_section_add_key (section,
                              "Alert_Destination_Type",
                              "Possible values: PET_Trap/OEM1/OEM2",
                              0,
                              alert_destination_type_checkout,
                              alert_destination_type_commit,
                              alert_destination_type_validate) < 0) 
    goto cleanup;

  if (config_section_add_key (section,
                              "Alert_Acknowledge",
                              "Possible values: Yes/No",
                              0,
                              alert_acknowledge_checkout,
                              alert_acknowledge_commit,
                              config_yes_no_validate) < 0) 
    goto cleanup;

  if (config_section_add_key (section,
                              "Alert_Acknowledge_Timeout",
                              "Give valid unsigned number in seconds",
                              0,
                              alert_acknowledge_timeout_checkout,
                              alert_acknowledge_timeout_commit,
                              config_number_range_one_byte) < 0) 
    goto cleanup;

  if (config_section_add_key (section,
                              "Alert_Retries",
                              "Give valid unsigned number",
                              0,
                              alert_retries_checkout,
                              alert_retries_commit,
                              alert_retries_validate) < 0) 
    goto cleanup;

  if (config_section_add_key (section,
                              "Alert_Gateway",
                              "Possible values: Default/Backup",
                              0,
                              alert_gateway_checkout,
                              alert_gateway_commit,
                              alert_gateway_validate) < 0) 
    goto cleanup;

  if (config_section_add_key (section,
                              "Alert_IP_Address",
                              "Give valid IP address",
                              0,
                              alert_ip_address_checkout,
                              alert_ip_address_commit,
                              config_ip_address_validate) < 0) 
    goto cleanup;

  if (config_section_add_key (section,
                              "Alert_MAC_Address",
                              "Give valid MAC address",
                              0,
                              alert_mac_address_checkout,
                              alert_mac_address_commit,
                              config_mac_address_validate) < 0) 
    goto cleanup;

  return section;

 cleanup:
  if (section)
    config_section_destroy(section);
  return NULL;
}

