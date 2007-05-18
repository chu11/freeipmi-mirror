#include "bmc-config.h"
#include "bmc-config-common.h"
#include "bmc-config-wrapper.h"
#include "bmc-config-diff.h"
#include "bmc-config-utils.h"
#include "bmc-config-map.h"
#include "bmc-config-sections.h"
#include "bmc-config-validate.h"

#define BMC_MAXIPADDRLEN 16
#define BMC_MAXMACADDRLEN 24

static bmc_err_t
community_string_checkout (bmc_config_state_data_t *state_data,
                           const struct section *sect,
                           struct keyvalue *kv)
{
  uint8_t community_string[IPMI_MAX_COMMUNITY_STRING_LENGTH+1] = { 0, };
  bmc_err_t ret;

  if ((ret = get_bmc_community_string (state_data,
                                       community_string,
                                       IPMI_MAX_COMMUNITY_STRING_LENGTH+1)) != BMC_ERR_SUCCESS) 
    return ret;
		    
  if (kv->value)
    free (kv->value);

  if (!(kv->value = strdup ((char *)community_string)))
    {
      perror("strdup");
      return BMC_ERR_FATAL_ERROR;
    }

  return BMC_ERR_SUCCESS;
}

static bmc_err_t
community_string_commit (bmc_config_state_data_t *state_data,
                         const struct section *sect,
                         const struct keyvalue *kv)
{
  if (!kv->value)
    return BMC_ERR_FATAL_ERROR;

  return set_bmc_community_string (state_data,
                                   (uint8_t *)kv->value);
}

static bmc_diff_t
community_string_diff (bmc_config_state_data_t *state_data,
                       const struct section *sect,
                       const struct keyvalue *kv)
{
  uint8_t community_string[IPMI_MAX_COMMUNITY_STRING_LENGTH+1] = { 0, };
  bmc_err_t rc;
  bmc_diff_t ret;

  if ((rc = get_bmc_community_string (state_data,
                                      community_string,
                                      IPMI_MAX_COMMUNITY_STRING_LENGTH+1)) != BMC_ERR_SUCCESS)
    {
      if (rc == BMC_ERR_NON_FATAL_ERROR)
        return BMC_DIFF_NON_FATAL_ERROR;
      return BMC_DIFF_FATAL_ERROR;
    }

  if (!kv->value || !same (kv->value, (char *)community_string))
    ret = BMC_DIFF_DIFFERENT;
  else
    ret = BMC_DIFF_SAME;

  if (ret == BMC_DIFF_DIFFERENT)
    report_diff (sect->section_name,
		 kv->key,
		 kv->value,
		 (char *)community_string);
  return ret;
}

static bmc_validate_t
community_string_validate (bmc_config_state_data_t *state_data,
                           const struct section *sect,
                           const char *value)
{
  if (!value || strlen (value) > IPMI_MAX_COMMUNITY_STRING_LENGTH)
    return BMC_VALIDATE_INVALID_VALUE;
  return BMC_VALIDATE_VALID_VALUE;
}

static bmc_err_t
destination_type_get (bmc_config_state_data_t *state_data,
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
  bmc_err_t ret;

  if ((ret = get_bmc_lan_conf_destination_type (state_data,
                                                destination_selector,
                                                &tmp_alert_destination_type,
                                                &tmp_alert_acknowledge,
                                                &tmp_alert_acknowledge_timeout,
                                                &tmp_alert_retries)) != BMC_ERR_SUCCESS)
    return ret;

  if (alert_destination_type)
    *alert_destination_type = tmp_alert_destination_type;
  if (alert_acknowledge)
    *alert_acknowledge = tmp_alert_acknowledge;
  if (alert_acknowledge_timeout)
    *alert_acknowledge_timeout = tmp_alert_acknowledge_timeout;
  if (alert_retries)
    *alert_retries = tmp_alert_retries;

  return BMC_ERR_SUCCESS;
}

static bmc_err_t
destination_type_set (bmc_config_state_data_t *state_data,
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
  bmc_err_t ret;

  if ((ret = get_bmc_lan_conf_destination_type(state_data,
                                               destination_selector,
                                               &tmp_alert_destination_type,
                                               &tmp_alert_acknowledge,
                                               &tmp_alert_acknowledge_timeout,
                                               &tmp_alert_retries)) != BMC_ERR_SUCCESS)
    return ret;
  
  if (alert_destination_type_is_set)
    tmp_alert_destination_type = alert_destination_type;
  if (alert_acknowledge_is_set)
    tmp_alert_acknowledge = alert_acknowledge;
  if (alert_acknowledge_timeout_is_set)
    tmp_alert_acknowledge_timeout = alert_acknowledge_timeout;
  if (alert_retries_is_set)
    tmp_alert_retries = alert_retries;

  if ((ret = set_bmc_lan_conf_destination_type(state_data,
                                               destination_selector,
                                               tmp_alert_destination_type,
                                               tmp_alert_acknowledge,
                                               tmp_alert_acknowledge_timeout,
                                               tmp_alert_retries)) != BMC_ERR_SUCCESS)
    return ret;

  return BMC_ERR_SUCCESS;
}

static bmc_err_t
alert_destination_type_checkout (bmc_config_state_data_t *state_data,
                                 const struct section *sect,
                                 struct keyvalue *kv)
{
  uint8_t destination_type;
  bmc_err_t ret;
  uint8_t destination_selector;
  uint8_t number_of_lan_destinations;

  destination_selector = atoi (kv->key + strlen ("Alert_Destination_Type_"));

  if ((ret = get_number_of_lan_destinations (state_data, 
                                             &number_of_lan_destinations)) != BMC_ERR_SUCCESS)
    return ret;

  if (destination_selector > number_of_lan_destinations)
    return BMC_ERR_NON_FATAL_ERROR;

  if ((ret = destination_type_get (state_data,
                                   destination_selector,
                                   &destination_type,
                                   NULL,
                                   NULL,
                                   NULL)) != BMC_ERR_SUCCESS)
    return ret;

  if (kv->value)
    free (kv->value);
  
  if (!(kv->value = strdup (alert_destination_type_string (destination_type))))
    {
      perror("strdup");
      return BMC_ERR_FATAL_ERROR;
    }
  
  return BMC_ERR_SUCCESS;
}

static bmc_err_t
alert_destination_type_commit (bmc_config_state_data_t *state_data,
                               const struct section *sect,
                               const struct keyvalue *kv)
{
  uint8_t destination_selector;
  uint8_t number_of_lan_destinations;
  bmc_err_t ret;

  destination_selector = atoi (kv->key + strlen ("Alert_Destination_Type_"));

  if ((ret = get_number_of_lan_destinations (state_data, 
                                             &number_of_lan_destinations)) != BMC_ERR_SUCCESS)
    return ret;

  if (destination_selector > number_of_lan_destinations)
    return BMC_ERR_NON_FATAL_ERROR;

  return destination_type_set (state_data,
                               destination_selector,
                               alert_destination_type_number (kv->value), 1,
                               0, 0,
                               0, 0,
                               0, 0);
}

static bmc_diff_t
alert_destination_type_diff (bmc_config_state_data_t *state_data,
                             const struct section *sect,
                             const struct keyvalue *kv)
{
  uint8_t get_val;
  uint8_t passed_val;
  bmc_err_t rc;
  bmc_diff_t ret;
  uint8_t destination_selector;
  uint8_t number_of_lan_destinations;

  destination_selector = atoi (kv->key + strlen ("Alert_Destination_Type_"));

  if ((rc = get_number_of_lan_destinations (state_data, 
                                            &number_of_lan_destinations)) != BMC_ERR_SUCCESS)
    return rc;

  if (destination_selector > number_of_lan_destinations)
    return BMC_ERR_NON_FATAL_ERROR;

  if ((rc = destination_type_get (state_data,
                                  destination_selector,
                                  &get_val,
                                  0,
                                  0,
                                  0)) != BMC_ERR_SUCCESS)
    {
      if (rc == BMC_ERR_NON_FATAL_ERROR)
        return BMC_DIFF_NON_FATAL_ERROR;
      return BMC_DIFF_FATAL_ERROR;
    }
  
  passed_val = alert_destination_type_number (kv->value);
  if (passed_val == get_val)
    ret = BMC_DIFF_SAME;
  else 
    {
      ret = BMC_DIFF_DIFFERENT;
      report_diff (sect->section_name,
                   kv->key,
                   kv->value,
                   alert_destination_type_string (get_val));
    }
  return ret;
}

static bmc_err_t
alert_acknowledge_checkout (bmc_config_state_data_t *state_data,
                            const struct section *sect,
                            struct keyvalue *kv)
{
  uint8_t alert_acknowledge;
  bmc_err_t ret;
  uint8_t destination_selector;
  uint8_t number_of_lan_destinations;

  destination_selector = atoi (kv->key + strlen ("Alert_Acknowledge_"));

  if ((ret = get_number_of_lan_destinations (state_data, 
                                             &number_of_lan_destinations)) != BMC_ERR_SUCCESS)
    return ret;

  if (destination_selector > number_of_lan_destinations)
    return BMC_ERR_NON_FATAL_ERROR;

  if ((ret = destination_type_get (state_data,
                                   destination_selector,
                                   NULL,
                                   &alert_acknowledge,
                                   NULL,
                                   NULL)) != BMC_ERR_SUCCESS)
    return ret;

  if (kv->value)
    free (kv->value);
  
  if (alert_acknowledge)
    {
      if (!(kv->value = strdup ("Yes")))
        {
          perror("strdup");
          return BMC_ERR_FATAL_ERROR;
        }
    }
  else
    {
      if (!(kv->value = strdup ("No")))
        {
          perror("strdup");
          return BMC_ERR_FATAL_ERROR;
        }
    }
  
  return BMC_ERR_SUCCESS;
}

static bmc_err_t
alert_acknowledge_commit (bmc_config_state_data_t *state_data,
                          const struct section *sect,
                          const struct keyvalue *kv)
{
  uint8_t destination_selector;
  uint8_t number_of_lan_destinations;
  bmc_err_t ret;

  destination_selector = atoi (kv->key + strlen ("Alert_Acknowledge_"));

  if ((ret = get_number_of_lan_destinations (state_data, 
                                             &number_of_lan_destinations)) != BMC_ERR_SUCCESS)
    return ret;

  if (destination_selector > number_of_lan_destinations)
    return BMC_ERR_NON_FATAL_ERROR;

  return destination_type_set (state_data,
                               destination_selector,
                               0, 0,
                               same (kv->value, "yes"), 1,
                               0, 0,
                               0, 0);
}

static bmc_diff_t
alert_acknowledge_diff (bmc_config_state_data_t *state_data,
                        const struct section *sect,
                        const struct keyvalue *kv)
{
  uint8_t get_val;
  uint8_t passed_val;
  bmc_err_t rc;
  bmc_diff_t ret;
  uint8_t destination_selector;
  uint8_t number_of_lan_destinations;

  destination_selector = atoi (kv->key + strlen ("Alert_Acknowledge_"));
  
  if ((rc = get_number_of_lan_destinations (state_data, 
                                            &number_of_lan_destinations)) != BMC_ERR_SUCCESS)
    return rc;
  
  if (destination_selector > number_of_lan_destinations)
    return BMC_ERR_NON_FATAL_ERROR;
  
  if ((rc = destination_type_get (state_data,
                                  destination_selector,
                                  0,
                                  &get_val,
                                  0,
                                  0)) != BMC_ERR_SUCCESS)
    {
      if (rc == BMC_ERR_NON_FATAL_ERROR)
        return BMC_DIFF_NON_FATAL_ERROR;
      return BMC_DIFF_FATAL_ERROR;
    }
  
  passed_val = same (kv->value, "Yes");

  if (passed_val == get_val)
    ret = BMC_DIFF_SAME;
  else 
    {
      ret = BMC_DIFF_DIFFERENT;
      report_diff (sect->section_name,
                   kv->key,
                   kv->value,
                   get_val ? "Yes" : "No");
    }
  return ret;
}

static bmc_err_t
alert_acknowledge_timeout_checkout (bmc_config_state_data_t *state_data,
                                    const struct section *sect,
                                    struct keyvalue *kv)
{
  uint8_t alert_acknowledge_timeout;
  bmc_err_t ret;
  uint8_t destination_selector;
  uint8_t number_of_lan_destinations;
  
  destination_selector = atoi (kv->key + strlen ("Alert_Acknowledge_Timeout_"));

  if ((ret = get_number_of_lan_destinations (state_data, 
                                             &number_of_lan_destinations)) != BMC_ERR_SUCCESS)
    return ret;
  
  if (destination_selector > number_of_lan_destinations)
    return BMC_ERR_NON_FATAL_ERROR;
  
  if ((ret = destination_type_get (state_data,
                                   destination_selector,
                                   NULL,
                                   NULL,
                                   &alert_acknowledge_timeout,
                                   NULL)) != BMC_ERR_SUCCESS)
    return ret;

  if (kv->value)
    free (kv->value);

  if (asprintf (&kv->value, "%u", alert_acknowledge_timeout) < 0)
    {
      perror("asprintf");
      return BMC_ERR_FATAL_ERROR;
    }

  return BMC_ERR_SUCCESS;
}

static bmc_err_t
alert_acknowledge_timeout_commit (bmc_config_state_data_t *state_data,
                          const struct section *sect,
                          const struct keyvalue *kv)
{
  uint8_t destination_selector;
  uint8_t number_of_lan_destinations;
  bmc_err_t ret;
  uint8_t alert_acknowledge_timeout;

  destination_selector = atoi (kv->key + strlen ("Alert_Acknowledge_Timeout_"));

  if ((ret = get_number_of_lan_destinations (state_data, 
                                             &number_of_lan_destinations)) != BMC_ERR_SUCCESS)
    return ret;

  if (destination_selector > number_of_lan_destinations)
    return BMC_ERR_NON_FATAL_ERROR;

  alert_acknowledge_timeout = atoi (kv->value);

  return destination_type_set (state_data,
                               destination_selector,
                               0, 0,
                               0, 0,
                               alert_acknowledge_timeout, 1,
                               0, 0);
}

static bmc_diff_t
alert_acknowledge_timeout_diff (bmc_config_state_data_t *state_data,
                                const struct section *sect,
                                const struct keyvalue *kv)
{
  uint8_t get_val;
  uint8_t passed_val;
  bmc_err_t rc;
  bmc_diff_t ret;
  uint8_t destination_selector;
  uint8_t number_of_lan_destinations;

  destination_selector = atoi (kv->key + strlen ("Alert_Acknowledge_Timeout_"));
  
  if ((rc = get_number_of_lan_destinations (state_data, 
                                            &number_of_lan_destinations)) != BMC_ERR_SUCCESS)
    return rc;
  
  if (destination_selector > number_of_lan_destinations)
    return BMC_ERR_NON_FATAL_ERROR;
  
  if ((rc = destination_type_get (state_data,
                                  destination_selector,
                                  0,
                                  0,
                                  &get_val,
                                  0)) != BMC_ERR_SUCCESS)
    {
      if (rc == BMC_ERR_NON_FATAL_ERROR)
        return BMC_DIFF_NON_FATAL_ERROR;
      return BMC_DIFF_FATAL_ERROR;
    }
  
  passed_val = atoi (kv->value);

  if (passed_val == get_val)
    ret = BMC_DIFF_SAME;
  else 
    {
      char num[32];
      ret = BMC_DIFF_DIFFERENT;
      sprintf (num, "%u", get_val);
      report_diff (sect->section_name,
                   kv->key,
                   kv->value,
                   num);
    }
  return ret;
}

static bmc_err_t
alert_retries_checkout (bmc_config_state_data_t *state_data,
                        const struct section *sect,
                        struct keyvalue *kv)
{
  uint8_t alert_retries;
  bmc_err_t ret;
  uint8_t destination_selector;
  uint8_t number_of_lan_destinations;
  
  destination_selector = atoi (kv->key + strlen ("Alert_Retries_"));

  if ((ret = get_number_of_lan_destinations (state_data, 
                                             &number_of_lan_destinations)) != BMC_ERR_SUCCESS)
    return ret;
  
  if (destination_selector > number_of_lan_destinations)
    return BMC_ERR_NON_FATAL_ERROR;
  
  if ((ret = destination_type_get (state_data,
                                   destination_selector,
                                   NULL,
                                   NULL,
                                   NULL,
                                   &alert_retries)) != BMC_ERR_SUCCESS)
    return ret;
  
  if (kv->value)
    free (kv->value);
  
  if (asprintf (&kv->value, "%u", alert_retries) < 0)
    {
      perror("asprintf");
      return BMC_ERR_FATAL_ERROR;
    }

  return BMC_ERR_SUCCESS;
}

static bmc_err_t
alert_retries_commit (bmc_config_state_data_t *state_data,
                      const struct section *sect,
                      const struct keyvalue *kv)
{
  uint8_t destination_selector;
  uint8_t number_of_lan_destinations;
  bmc_err_t ret;
  uint8_t alert_retries;

  destination_selector = atoi (kv->key + strlen ("Alert_Retries_"));

  if ((ret = get_number_of_lan_destinations (state_data, 
                                             &number_of_lan_destinations)) != BMC_ERR_SUCCESS)
    return ret;

  if (destination_selector > number_of_lan_destinations)
    return BMC_ERR_NON_FATAL_ERROR;

  alert_retries = atoi (kv->value);

  return destination_type_set (state_data,
                               destination_selector,
                               0, 0,
                               0, 0,
                               0, 0,
                               alert_retries, 1);
}

static bmc_diff_t
alert_retries_diff (bmc_config_state_data_t *state_data,
                    const struct section *sect,
                    const struct keyvalue *kv)
{
  uint8_t get_val;
  uint8_t passed_val;
  bmc_err_t rc;
  bmc_diff_t ret;
  uint8_t destination_selector;
  uint8_t number_of_lan_destinations;

  destination_selector = atoi (kv->key + strlen ("Alert_Retries_"));
  
  if ((rc = get_number_of_lan_destinations (state_data, 
                                            &number_of_lan_destinations)) != BMC_ERR_SUCCESS)
    return rc;
  
  if (destination_selector > number_of_lan_destinations)
    return BMC_ERR_NON_FATAL_ERROR;
  
  if ((rc = destination_type_get (state_data,
                                  destination_selector,
                                  0,
                                  0,
                                  0,
                                  &get_val)) != BMC_ERR_SUCCESS)
    {
      if (rc == BMC_ERR_NON_FATAL_ERROR)
        return BMC_DIFF_NON_FATAL_ERROR;
      return BMC_DIFF_FATAL_ERROR;
    }
  
  passed_val = atoi (kv->value);

  if (passed_val == get_val)
    ret = BMC_DIFF_SAME;
  else 
    {
      char num[32];
      ret = BMC_DIFF_DIFFERENT;
      sprintf (num, "%u", get_val);
      report_diff (sect->section_name,
                   kv->key,
                   kv->value,
                   num);
    }
  return ret;
}

bmc_validate_t
alert_retries_validate (bmc_config_state_data_t *state_data,
                        const struct section *sect,
                        const char *value)
{
  long int conv;
  char *endptr;

  conv = strtol (value, &endptr, 0);

  if (*endptr)
    return BMC_VALIDATE_INVALID_VALUE;

  if (conv < 0 || conv > IPMI_ALERT_RETRIES_MAX)
    return BMC_VALIDATE_INVALID_VALUE;

  return BMC_VALIDATE_VALID_VALUE;
}

static bmc_err_t
destination_addresses_get (bmc_config_state_data_t *state_data,
                           uint8_t destination_selector,
                           uint8_t *alert_gateway,
                           char *alert_ip_address,
                           unsigned int alert_ip_address_len,
                           char *alert_mac_address,
                           unsigned int alert_mac_address_len)
{
  uint8_t tmp_alert_gateway;
  char tmp_ip[BMC_MAXIPADDRLEN + 1];
  char tmp_mac[BMC_MAXMACADDRLEN+1];
  bmc_err_t ret;

  memset(tmp_ip, '\0', BMC_MAXIPADDRLEN + 1);
  memset(tmp_mac, '\0', BMC_MAXMACADDRLEN+1);

  if ((ret = get_bmc_lan_conf_destination_addresses (state_data,
                                                     destination_selector,
                                                     &tmp_alert_gateway,
                                                     tmp_ip,
                                                     BMC_MAXIPADDRLEN + 1,
                                                     tmp_mac,
                                                     BMC_MAXMACADDRLEN+1)) != BMC_ERR_SUCCESS)
    return ret;
  
  if (alert_gateway)
    *alert_gateway = tmp_alert_gateway;
  if (alert_ip_address)
    { 
      if (alert_ip_address_len >= BMC_MAXIPADDRLEN + 1)
        memcpy(alert_ip_address, tmp_ip, BMC_MAXIPADDRLEN + 1);
      else
        return BMC_ERR_FATAL_ERROR;
    }
  if (alert_mac_address)
    { 
      if (alert_mac_address_len >= BMC_MAXMACADDRLEN + 1)
        memcpy(alert_mac_address, tmp_mac, BMC_MAXMACADDRLEN + 1);
      else
        return BMC_ERR_FATAL_ERROR;
    }

  return BMC_ERR_SUCCESS;
}

static bmc_err_t
destination_addresses_set (bmc_config_state_data_t *state_data,
                           uint8_t destination_selector,
                           uint8_t alert_gateway,
                           int alert_gateway_is_set,
                           char *alert_ip_address,
                           int alert_ip_address_is_set,
                           char *alert_mac_address,
                           int alert_mac_address_is_set)
{
  uint8_t tmp_alert_gateway;
  char tmp_ip[BMC_MAXIPADDRLEN + 1];
  char tmp_mac[BMC_MAXMACADDRLEN+1];
  char *tmp_ip_ptr;
  char *tmp_mac_ptr;
  bmc_err_t ret;

  memset(tmp_ip, '\0', BMC_MAXIPADDRLEN + 1);
  memset(tmp_mac, '\0', BMC_MAXMACADDRLEN+1);

  tmp_ip_ptr = tmp_ip;
  tmp_mac_ptr = tmp_mac;

  if ((ret = get_bmc_lan_conf_destination_addresses (state_data,
                                                     destination_selector,
                                                     &tmp_alert_gateway,
                                                     tmp_ip_ptr,
                                                     BMC_MAXIPADDRLEN+1,
                                                     tmp_mac_ptr,
                                                     BMC_MAXMACADDRLEN+1)) != BMC_ERR_SUCCESS)
    return ret;
  
  if (alert_gateway_is_set)
    tmp_alert_gateway = alert_gateway;
  if (alert_ip_address && alert_ip_address_is_set)
    tmp_ip_ptr = alert_ip_address;
  if (alert_mac_address && alert_mac_address_is_set)
    tmp_mac_ptr = alert_mac_address;

  if ((ret = set_bmc_lan_conf_destination_addresses(state_data,
                                                    destination_selector,
                                                    tmp_alert_gateway,
                                                    tmp_ip_ptr,
                                                    tmp_mac_ptr)) != BMC_ERR_SUCCESS)
    return ret;

  return BMC_ERR_SUCCESS;
}

static bmc_err_t
alert_gateway_checkout (bmc_config_state_data_t *state_data,
                        const struct section *sect,
                        struct keyvalue *kv)
{
  uint8_t gateway;
  bmc_err_t ret;
  uint8_t destination_selector;
  uint8_t number_of_lan_destinations;
  
  destination_selector = atoi (kv->key + strlen ("Alert_Gateway_"));

  if ((ret = get_number_of_lan_destinations (state_data, 
                                             &number_of_lan_destinations)) != BMC_ERR_SUCCESS)
    return ret;
  
  if (destination_selector > number_of_lan_destinations)
    return BMC_ERR_NON_FATAL_ERROR;
  
  if ((ret = destination_addresses_get (state_data,
                                        destination_selector,
                                        &gateway,
                                        NULL,
                                        0,
                                        NULL,
                                        0)) != BMC_ERR_SUCCESS)
    return ret;
  
  if (kv->value)
    free (kv->value);
  
  if (!(kv->value = strdup (alert_gateway_string (gateway))))
    {
      perror("strdup");
      return BMC_ERR_FATAL_ERROR;
    }
  
  return BMC_ERR_SUCCESS;
}

static bmc_err_t
alert_gateway_commit (bmc_config_state_data_t *state_data,
                      const struct section *sect,
                      const struct keyvalue *kv)
{
  uint8_t destination_selector;
  uint8_t number_of_lan_destinations;
  bmc_err_t ret;

  destination_selector = atoi (kv->key + strlen ("Alert_Gateway_"));

  if ((ret = get_number_of_lan_destinations (state_data, 
                                             &number_of_lan_destinations)) != BMC_ERR_SUCCESS)
    return ret;

  if (destination_selector > number_of_lan_destinations)
    return BMC_ERR_NON_FATAL_ERROR;

  return destination_addresses_set (state_data,
                                    destination_selector,
                                    alert_gateway_number (kv->value), 1,
                                    NULL, 0,
                                    NULL, 0);
}

static bmc_diff_t
alert_gateway_diff (bmc_config_state_data_t *state_data,
                    const struct section *sect,
                    const struct keyvalue *kv)
{
  uint8_t get_val;
  uint8_t passed_val;
  bmc_err_t rc;
  bmc_diff_t ret;
  uint8_t destination_selector;
  uint8_t number_of_lan_destinations;

  destination_selector = atoi (kv->key + strlen ("Alert_Gateway_"));

  if ((rc = get_number_of_lan_destinations (state_data, 
                                            &number_of_lan_destinations)) != BMC_ERR_SUCCESS)
    return rc;

  if (destination_selector > number_of_lan_destinations)
    return BMC_ERR_NON_FATAL_ERROR;

  if ((rc = destination_addresses_get (state_data,
                                       destination_selector,
                                       &get_val,
                                       NULL,
                                       0,
                                       NULL,
                                       0)) != BMC_ERR_SUCCESS)
    {
      if (rc == BMC_ERR_NON_FATAL_ERROR)
        return BMC_DIFF_NON_FATAL_ERROR;
      return BMC_DIFF_FATAL_ERROR;
    }
  
  passed_val = alert_gateway_number (kv->value);
  if (passed_val == get_val)
    ret = BMC_DIFF_SAME;
  else 
    {
      ret = BMC_DIFF_DIFFERENT;
      report_diff (sect->section_name,
                   kv->key,
                   kv->value,
                   alert_gateway_string (get_val));
    }
  return ret;
}

static bmc_err_t
alert_ip_address_checkout (bmc_config_state_data_t *state_data,
                           const struct section *sect,
                           struct keyvalue *kv)
{
  bmc_err_t ret;
  char alert_ip[BMC_MAXIPADDRLEN + 1];
  uint8_t destination_selector;
  uint8_t number_of_lan_destinations;
  
  destination_selector = atoi (kv->key + strlen ("Alert_Ip_Address_"));
  
  if ((ret = get_number_of_lan_destinations (state_data, 
                                             &number_of_lan_destinations)) != BMC_ERR_SUCCESS)
    return ret;
  
  if (destination_selector > number_of_lan_destinations)
    return BMC_ERR_NON_FATAL_ERROR;
  
  if ((ret = destination_addresses_get (state_data,
                                        destination_selector,
                                        NULL,
                                        alert_ip,
                                        BMC_MAXIPADDRLEN + 1,
                                        NULL,
                                        0)) != BMC_ERR_SUCCESS)
    return ret;
  
  if (kv->value)
    free (kv->value);
  
  if (!(kv->value = strdup (alert_ip)))
    {
      perror("strdup");
      return BMC_ERR_FATAL_ERROR;
    }
  
  return BMC_ERR_SUCCESS;
}

static bmc_err_t
alert_ip_address_commit (bmc_config_state_data_t *state_data,
                         const struct section *sect,
                         const struct keyvalue *kv)
{
  uint8_t destination_selector;
  uint8_t number_of_lan_destinations;
  bmc_err_t ret;

  destination_selector = atoi (kv->key + strlen ("Alert_Ip_Address_"));

  if ((ret = get_number_of_lan_destinations (state_data, 
                                             &number_of_lan_destinations)) != BMC_ERR_SUCCESS)
    return ret;

  if (destination_selector > number_of_lan_destinations)
    return BMC_ERR_NON_FATAL_ERROR;

  return destination_addresses_set (state_data,
                                    destination_selector,
                                    0, 0,
                                    kv->value, 1,
                                    NULL, 0);
}

static bmc_diff_t
alert_ip_address_diff (bmc_config_state_data_t *state_data,
                       const struct section *sect,
                       const struct keyvalue *kv)
{
  char alert_ip[BMC_MAXIPADDRLEN + 1];
  bmc_err_t rc;
  bmc_diff_t ret;
  uint8_t destination_selector;
  uint8_t number_of_lan_destinations;

  destination_selector = atoi (kv->key + strlen ("Alert_Ip_Address_"));

  if ((rc = get_number_of_lan_destinations (state_data, 
                                            &number_of_lan_destinations)) != BMC_ERR_SUCCESS)
    return rc;

  if (destination_selector > number_of_lan_destinations)
    return BMC_ERR_NON_FATAL_ERROR;

  if ((rc = destination_addresses_get (state_data,
                                       destination_selector,
                                       NULL,
                                       alert_ip,
                                       BMC_MAXIPADDRLEN + 1,
                                       NULL,
                                       0)) != BMC_ERR_SUCCESS)
    {
      if (rc == BMC_ERR_NON_FATAL_ERROR)
        return BMC_DIFF_NON_FATAL_ERROR;
      return BMC_DIFF_FATAL_ERROR;
    }
  
  if (same (alert_ip, kv->value))
    ret = BMC_DIFF_SAME;
  else 
    {
      ret = BMC_DIFF_DIFFERENT;
      report_diff (sect->section_name,
                   kv->key,
                   kv->value,
                   alert_ip);
    }
  return ret;
}

static bmc_err_t
alert_mac_address_checkout (bmc_config_state_data_t *state_data,
                           const struct section *sect,
                           struct keyvalue *kv)
{
  bmc_err_t ret;
  char alert_mac[BMC_MAXMACADDRLEN + 1];
  uint8_t destination_selector;
  uint8_t number_of_lan_destinations;
  
  destination_selector = atoi (kv->key + strlen ("Alert_Mac_Address_"));
  
  if ((ret = get_number_of_lan_destinations (state_data, 
                                             &number_of_lan_destinations)) != BMC_ERR_SUCCESS)
    return ret;
  
  if (destination_selector > number_of_lan_destinations)
    return BMC_ERR_NON_FATAL_ERROR;
  
  if ((ret = destination_addresses_get (state_data,
                                        destination_selector,
                                        NULL,
                                        NULL,
                                        0,
                                        alert_mac,
                                        BMC_MAXMACADDRLEN + 1)) != BMC_ERR_SUCCESS)
    return ret;
  
  if (kv->value)
    free (kv->value);
  
  if (!(kv->value = strdup (alert_mac)))
    {
      perror("strdup");
      return BMC_ERR_FATAL_ERROR;
    }
  
  return BMC_ERR_SUCCESS;
}

static bmc_err_t
alert_mac_address_commit (bmc_config_state_data_t *state_data,
                         const struct section *sect,
                         const struct keyvalue *kv)
{
  uint8_t destination_selector;
  uint8_t number_of_lan_destinations;
  bmc_err_t ret;

  destination_selector = atoi (kv->key + strlen ("Alert_Mac_Address_"));

  if ((ret = get_number_of_lan_destinations (state_data, 
                                             &number_of_lan_destinations)) != BMC_ERR_SUCCESS)
    return ret;

  if (destination_selector > number_of_lan_destinations)
    return BMC_ERR_NON_FATAL_ERROR;

  return destination_addresses_set (state_data,
                                    destination_selector,
                                    0, 0,
                                    NULL, 0,
                                    kv->value, 1);
}

static bmc_diff_t
alert_mac_address_diff (bmc_config_state_data_t *state_data,
                       const struct section *sect,
                       const struct keyvalue *kv)
{
  char alert_mac[BMC_MAXMACADDRLEN + 1];
  bmc_err_t rc;
  bmc_diff_t ret;
  uint8_t destination_selector;
  uint8_t number_of_lan_destinations;

  destination_selector = atoi (kv->key + strlen ("Alert_Mac_Address_"));

  if ((rc = get_number_of_lan_destinations (state_data, 
                                            &number_of_lan_destinations)) != BMC_ERR_SUCCESS)
    return rc;

  if (destination_selector > number_of_lan_destinations)
    return BMC_ERR_NON_FATAL_ERROR;

  if ((rc = destination_addresses_get (state_data,
                                       destination_selector,
                                       NULL,
                                       NULL,
                                       0,
                                       alert_mac,
                                       BMC_MAXMACADDRLEN + 1)) != BMC_ERR_SUCCESS)
    {
      if (rc == BMC_ERR_NON_FATAL_ERROR)
        return BMC_DIFF_NON_FATAL_ERROR;
      return BMC_DIFF_FATAL_ERROR;
    }
  
  if (same (alert_mac, kv->value))
    ret = BMC_DIFF_SAME;
  else 
    {
      ret = BMC_DIFF_DIFFERENT;
      report_diff (sect->section_name,
                   kv->key,
                   kv->value,
                   alert_mac);
    }
  return ret;
}

struct section *
bmc_lan_conf_alert_section_get (bmc_config_state_data_t *state_data)
{
  struct section *lan_conf_alert_section = NULL;

  if (!(lan_conf_alert_section = bmc_config_section_create (state_data, "Lan_Conf_Alert")))
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       lan_conf_alert_section,
                                       "Community_String",
                                       "Give valid string",
                                       0,
                                       community_string_checkout,
                                       community_string_commit,
                                       community_string_diff,
                                       community_string_validate) < 0) 
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       lan_conf_alert_section,
                                       "Alert_Destination_Type_1",
                                       "Possible values: PET_Trap/OEM1/OEM2",
                                       0,
                                       alert_destination_type_checkout,
                                       alert_destination_type_commit,
                                       alert_destination_type_diff,
                                       alert_destination_type_number_validate) < 0) 
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       lan_conf_alert_section,
                                       "Alert_Acknowledge_1",
                                       "Possible values: Yes/No",
                                       0,
                                       alert_acknowledge_checkout,
                                       alert_acknowledge_commit,
                                       alert_acknowledge_diff,
                                       yes_no_validate) < 0) 
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       lan_conf_alert_section,
                                       "Alert_Acknowledge_Timeout_1",
                                       "Give valid unsigned number in seconds",
                                       0,
                                       alert_acknowledge_timeout_checkout,
                                       alert_acknowledge_timeout_commit,
                                       alert_acknowledge_timeout_diff,
                                       number_range_one_byte) < 0) 
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       lan_conf_alert_section,
                                       "Alert_Retries_1",
                                       "Give valid unsigned number",
                                       0,
                                       alert_retries_checkout,
                                       alert_retries_commit,
                                       alert_retries_diff,
                                       alert_retries_validate) < 0) 
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       lan_conf_alert_section,
                                       "Alert_Gateway_1",
                                       "Possible values: Default/Backup",
                                       0,
                                       alert_gateway_checkout,
                                       alert_gateway_commit,
                                       alert_gateway_diff,
                                       alert_gateway_number_validate) < 0) 
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       lan_conf_alert_section,
                                       "Alert_IP_Address_1",
                                       "Give valid IP address",
                                       0,
                                       alert_ip_address_checkout,
                                       alert_ip_address_commit,
                                       alert_ip_address_diff,
                                       ip_address_validate) < 0) 
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       lan_conf_alert_section,
                                       "Alert_MAC_Address_1",
                                       "Give valid MAC address",
                                       0,
                                       alert_mac_address_checkout,
                                       alert_mac_address_commit,
                                       alert_mac_address_diff,
                                       mac_address_validate) < 0) 
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       lan_conf_alert_section,
                                       "Alert_Destination_Type_2",
                                       "Possible values: PET_Trap/OEM1/OEM2",
                                       0,
                                       alert_destination_type_checkout,
                                       alert_destination_type_commit,
                                       alert_destination_type_diff,
                                       alert_destination_type_number_validate) < 0) 
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       lan_conf_alert_section,
                                       "Alert_Acknowledge_2",
                                       "Possible values: Yes/No",
                                       0,
                                       alert_acknowledge_checkout,
                                       alert_acknowledge_commit,
                                       alert_acknowledge_diff,
                                       yes_no_validate) < 0) 
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       lan_conf_alert_section,
                                       "Alert_Acknowledge_Timeout_2",
                                       "Give valid unsigned number in seconds",
                                       0,
                                       alert_acknowledge_timeout_checkout,
                                       alert_acknowledge_timeout_commit,
                                       alert_acknowledge_timeout_diff,
                                       number_range_one_byte) < 0) 
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       lan_conf_alert_section,
                                       "Alert_Retries_2",
                                       "Give valid unsigned number",
                                       0,
                                       alert_retries_checkout,
                                       alert_retries_commit,
                                       alert_retries_diff,
                                       alert_retries_validate) < 0) 
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       lan_conf_alert_section,
                                       "Alert_Gateway_2",
                                       "Possible values: Default/Backup",
                                       0,
                                       alert_gateway_checkout,
                                       alert_gateway_commit,
                                       alert_gateway_diff,
                                       alert_gateway_number_validate) < 0) 
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       lan_conf_alert_section,
                                       "Alert_IP_Address_2",
                                       "Give valid IP address",
                                       0,
                                       alert_ip_address_checkout,
                                       alert_ip_address_commit,
                                       alert_ip_address_diff,
                                       ip_address_validate) < 0) 
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       lan_conf_alert_section,
                                       "Alert_MAC_Address_2",
                                       "Give valid MAC address",
                                       0,
                                       alert_mac_address_checkout,
                                       alert_mac_address_commit,
                                       alert_mac_address_diff,
                                       mac_address_validate) < 0) 
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       lan_conf_alert_section,
                                       "Alert_Destination_Type_3",
                                       "Possible values: PET_Trap/OEM1/OEM2",
                                       0,
                                       alert_destination_type_checkout,
                                       alert_destination_type_commit,
                                       alert_destination_type_diff,
                                       alert_destination_type_number_validate) < 0) 
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       lan_conf_alert_section,
                                       "Alert_Acknowledge_3",
                                       "Possible values: Yes/No",
                                       0,
                                       alert_acknowledge_checkout,
                                       alert_acknowledge_commit,
                                       alert_acknowledge_diff,
                                       yes_no_validate) < 0) 
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       lan_conf_alert_section,
                                       "Alert_Acknowledge_Timeout_3",
                                       "Give valid unsigned number in seconds",
                                       0,
                                       alert_acknowledge_timeout_checkout,
                                       alert_acknowledge_timeout_commit,
                                       alert_acknowledge_timeout_diff,
                                       number_range_one_byte) < 0) 
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       lan_conf_alert_section,
                                       "Alert_Retries_3",
                                       "Give valid unsigned number",
                                       0,
                                       alert_retries_checkout,
                                       alert_retries_commit,
                                       alert_retries_diff,
                                       alert_retries_validate) < 0) 
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       lan_conf_alert_section,
                                       "Alert_Gateway_3",
                                       "Possible values: Default/Backup",
                                       0,
                                       alert_gateway_checkout,
                                       alert_gateway_commit,
                                       alert_gateway_diff,
                                       alert_gateway_number_validate) < 0) 
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       lan_conf_alert_section,
                                       "Alert_IP_Address_3",
                                       "Give valid IP address",
                                       0,
                                       alert_ip_address_checkout,
                                       alert_ip_address_commit,
                                       alert_ip_address_diff,
                                       ip_address_validate) < 0) 
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       lan_conf_alert_section,
                                       "Alert_MAC_Address_3",
                                       "Give valid MAC address",
                                       0,
                                       alert_mac_address_checkout,
                                       alert_mac_address_commit,
                                       alert_mac_address_diff,
                                       mac_address_validate) < 0) 
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       lan_conf_alert_section,
                                       "Alert_Destination_Type_4",
                                       "Possible values: PET_Trap/OEM1/OEM2",
                                       0,
                                       alert_destination_type_checkout,
                                       alert_destination_type_commit,
                                       alert_destination_type_diff,
                                       alert_destination_type_number_validate) < 0) 
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       lan_conf_alert_section,
                                       "Alert_Acknowledge_4",
                                       "Possible values: Yes/No",
                                       0,
                                       alert_acknowledge_checkout,
                                       alert_acknowledge_commit,
                                       alert_acknowledge_diff,
                                       yes_no_validate) < 0) 
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       lan_conf_alert_section,
                                       "Alert_Acknowledge_Timeout_4",
                                       "Give valid unsigned number in seconds",
                                       0,
                                       alert_acknowledge_timeout_checkout,
                                       alert_acknowledge_timeout_commit,
                                       alert_acknowledge_timeout_diff,
                                       number_range_one_byte) < 0) 
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       lan_conf_alert_section,
                                       "Alert_Retries_4",
                                       "Give valid unsigned number",
                                       0,
                                       alert_retries_checkout,
                                       alert_retries_commit,
                                       alert_retries_diff,
                                       alert_retries_validate) < 0) 
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       lan_conf_alert_section,
                                       "Alert_Gateway_4",
                                       "Possible values: Default/Backup",
                                       0,
                                       alert_gateway_checkout,
                                       alert_gateway_commit,
                                       alert_gateway_diff,
                                       alert_gateway_number_validate) < 0) 
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       lan_conf_alert_section,
                                       "Alert_IP_Address_4",
                                       "Give valid IP address",
                                       0,
                                       alert_ip_address_checkout,
                                       alert_ip_address_commit,
                                       alert_ip_address_diff,
                                       ip_address_validate) < 0) 
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       lan_conf_alert_section,
                                       "Alert_MAC_Address_4",
                                       "Give valid MAC address",
                                       0,
                                       alert_mac_address_checkout,
                                       alert_mac_address_commit,
                                       alert_mac_address_diff,
                                       mac_address_validate) < 0) 
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       lan_conf_alert_section,
                                       "Alert_Destination_Type_5",
                                       "Possible values: PET_Trap/OEM1/OEM2",
                                       0,
                                       alert_destination_type_checkout,
                                       alert_destination_type_commit,
                                       alert_destination_type_diff,
                                       alert_destination_type_number_validate) < 0) 
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       lan_conf_alert_section,
                                       "Alert_Acknowledge_5",
                                       "Possible values: Yes/No",
                                       0,
                                       alert_acknowledge_checkout,
                                       alert_acknowledge_commit,
                                       alert_acknowledge_diff,
                                       yes_no_validate) < 0) 
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       lan_conf_alert_section,
                                       "Alert_Acknowledge_Timeout_5",
                                       "Give valid unsigned number in seconds",
                                       0,
                                       alert_acknowledge_timeout_checkout,
                                       alert_acknowledge_timeout_commit,
                                       alert_acknowledge_timeout_diff,
                                       number_range_one_byte) < 0) 
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       lan_conf_alert_section,
                                       "Alert_Retries_5",
                                       "Give valid unsigned number",
                                       0,
                                       alert_retries_checkout,
                                       alert_retries_commit,
                                       alert_retries_diff,
                                       alert_retries_validate) < 0) 
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       lan_conf_alert_section,
                                       "Alert_Gateway_5",
                                       "Possible values: Default/Backup",
                                       0,
                                       alert_gateway_checkout,
                                       alert_gateway_commit,
                                       alert_gateway_diff,
                                       alert_gateway_number_validate) < 0) 
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       lan_conf_alert_section,
                                       "Alert_IP_Address_5",
                                       "Give valid IP address",
                                       0,
                                       alert_ip_address_checkout,
                                       alert_ip_address_commit,
                                       alert_ip_address_diff,
                                       ip_address_validate) < 0) 
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       lan_conf_alert_section,
                                       "Alert_MAC_Address_5",
                                       "Give valid MAC address",
                                       0,
                                       alert_mac_address_checkout,
                                       alert_mac_address_commit,
                                       alert_mac_address_diff,
                                       mac_address_validate) < 0) 
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       lan_conf_alert_section,
                                       "Alert_Destination_Type_6",
                                       "Possible values: PET_Trap/OEM1/OEM2",
                                       0,
                                       alert_destination_type_checkout,
                                       alert_destination_type_commit,
                                       alert_destination_type_diff,
                                       alert_destination_type_number_validate) < 0) 
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       lan_conf_alert_section,
                                       "Alert_Acknowledge_6",
                                       "Possible values: Yes/No",
                                       0,
                                       alert_acknowledge_checkout,
                                       alert_acknowledge_commit,
                                       alert_acknowledge_diff,
                                       yes_no_validate) < 0) 
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       lan_conf_alert_section,
                                       "Alert_Acknowledge_Timeout_6",
                                       "Give valid unsigned number in seconds",
                                       0,
                                       alert_acknowledge_timeout_checkout,
                                       alert_acknowledge_timeout_commit,
                                       alert_acknowledge_timeout_diff,
                                       number_range_one_byte) < 0) 
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       lan_conf_alert_section,
                                       "Alert_Retries_6",
                                       "Give valid unsigned number",
                                       0,
                                       alert_retries_checkout,
                                       alert_retries_commit,
                                       alert_retries_diff,
                                       alert_retries_validate) < 0) 
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       lan_conf_alert_section,
                                       "Alert_Gateway_6",
                                       "Possible values: Default/Backup",
                                       0,
                                       alert_gateway_checkout,
                                       alert_gateway_commit,
                                       alert_gateway_diff,
                                       alert_gateway_number_validate) < 0) 
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       lan_conf_alert_section,
                                       "Alert_IP_Address_6",
                                       "Give valid IP address",
                                       0,
                                       alert_ip_address_checkout,
                                       alert_ip_address_commit,
                                       alert_ip_address_diff,
                                       ip_address_validate) < 0) 
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       lan_conf_alert_section,
                                       "Alert_MAC_Address_6",
                                       "Give valid MAC address",
                                       0,
                                       alert_mac_address_checkout,
                                       alert_mac_address_commit,
                                       alert_mac_address_diff,
                                       mac_address_validate) < 0) 
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       lan_conf_alert_section,
                                       "Alert_Destination_Type_7",
                                       "Possible values: PET_Trap/OEM1/OEM2",
                                       0,
                                       alert_destination_type_checkout,
                                       alert_destination_type_commit,
                                       alert_destination_type_diff,
                                       alert_destination_type_number_validate) < 0) 
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       lan_conf_alert_section,
                                       "Alert_Acknowledge_7",
                                       "Possible values: Yes/No",
                                       0,
                                       alert_acknowledge_checkout,
                                       alert_acknowledge_commit,
                                       alert_acknowledge_diff,
                                       yes_no_validate) < 0) 
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       lan_conf_alert_section,
                                       "Alert_Acknowledge_Timeout_7",
                                       "Give valid unsigned number in seconds",
                                       0,
                                       alert_acknowledge_timeout_checkout,
                                       alert_acknowledge_timeout_commit,
                                       alert_acknowledge_timeout_diff,
                                       number_range_one_byte) < 0) 
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       lan_conf_alert_section,
                                       "Alert_Retries_7",
                                       "Give valid unsigned number",
                                       0,
                                       alert_retries_checkout,
                                       alert_retries_commit,
                                       alert_retries_diff,
                                       alert_retries_validate) < 0) 
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       lan_conf_alert_section,
                                       "Alert_Gateway_7",
                                       "Possible values: Default/Backup",
                                       0,
                                       alert_gateway_checkout,
                                       alert_gateway_commit,
                                       alert_gateway_diff,
                                       alert_gateway_number_validate) < 0) 
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       lan_conf_alert_section,
                                       "Alert_IP_Address_7",
                                       "Give valid IP address",
                                       0,
                                       alert_ip_address_checkout,
                                       alert_ip_address_commit,
                                       alert_ip_address_diff,
                                       ip_address_validate) < 0) 
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       lan_conf_alert_section,
                                       "Alert_MAC_Address_7",
                                       "Give valid MAC address",
                                       0,
                                       alert_mac_address_checkout,
                                       alert_mac_address_commit,
                                       alert_mac_address_diff,
                                       mac_address_validate) < 0) 
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       lan_conf_alert_section,
                                       "Alert_Destination_Type_8",
                                       "Possible values: PET_Trap/OEM1/OEM2",
                                       0,
                                       alert_destination_type_checkout,
                                       alert_destination_type_commit,
                                       alert_destination_type_diff,
                                       alert_destination_type_number_validate) < 0) 
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       lan_conf_alert_section,
                                       "Alert_Acknowledge_8",
                                       "Possible values: Yes/No",
                                       0,
                                       alert_acknowledge_checkout,
                                       alert_acknowledge_commit,
                                       alert_acknowledge_diff,
                                       yes_no_validate) < 0) 
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       lan_conf_alert_section,
                                       "Alert_Acknowledge_Timeout_8",
                                       "Give valid unsigned number in seconds",
                                       0,
                                       alert_acknowledge_timeout_checkout,
                                       alert_acknowledge_timeout_commit,
                                       alert_acknowledge_timeout_diff,
                                       number_range_one_byte) < 0) 
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       lan_conf_alert_section,
                                       "Alert_Retries_8",
                                       "Give valid unsigned number",
                                       0,
                                       alert_retries_checkout,
                                       alert_retries_commit,
                                       alert_retries_diff,
                                       alert_retries_validate) < 0) 
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       lan_conf_alert_section,
                                       "Alert_Gateway_8",
                                       "Possible values: Default/Backup",
                                       0,
                                       alert_gateway_checkout,
                                       alert_gateway_commit,
                                       alert_gateway_diff,
                                       alert_gateway_number_validate) < 0) 
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       lan_conf_alert_section,
                                       "Alert_IP_Address_8",
                                       "Give valid IP address",
                                       0,
                                       alert_ip_address_checkout,
                                       alert_ip_address_commit,
                                       alert_ip_address_diff,
                                       ip_address_validate) < 0) 
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       lan_conf_alert_section,
                                       "Alert_MAC_Address_8",
                                       "Give valid MAC address",
                                       0,
                                       alert_mac_address_checkout,
                                       alert_mac_address_commit,
                                       alert_mac_address_diff,
                                       mac_address_validate) < 0) 
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       lan_conf_alert_section,
                                       "Alert_Destination_Type_9",
                                       "Possible values: PET_Trap/OEM1/OEM2",
                                       0,
                                       alert_destination_type_checkout,
                                       alert_destination_type_commit,
                                       alert_destination_type_diff,
                                       alert_destination_type_number_validate) < 0) 
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       lan_conf_alert_section,
                                       "Alert_Acknowledge_9",
                                       "Possible values: Yes/No",
                                       0,
                                       alert_acknowledge_checkout,
                                       alert_acknowledge_commit,
                                       alert_acknowledge_diff,
                                       yes_no_validate) < 0) 
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       lan_conf_alert_section,
                                       "Alert_Acknowledge_Timeout_9",
                                       "Give valid unsigned number in seconds",
                                       0,
                                       alert_acknowledge_timeout_checkout,
                                       alert_acknowledge_timeout_commit,
                                       alert_acknowledge_timeout_diff,
                                       number_range_one_byte) < 0) 
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       lan_conf_alert_section,
                                       "Alert_Retries_9",
                                       "Give valid unsigned number",
                                       0,
                                       alert_retries_checkout,
                                       alert_retries_commit,
                                       alert_retries_diff,
                                       alert_retries_validate) < 0) 
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       lan_conf_alert_section,
                                       "Alert_Gateway_9",
                                       "Possible values: Default/Backup",
                                       0,
                                       alert_gateway_checkout,
                                       alert_gateway_commit,
                                       alert_gateway_diff,
                                       alert_gateway_number_validate) < 0) 
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       lan_conf_alert_section,
                                       "Alert_IP_Address_9",
                                       "Give valid IP address",
                                       0,
                                       alert_ip_address_checkout,
                                       alert_ip_address_commit,
                                       alert_ip_address_diff,
                                       ip_address_validate) < 0) 
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       lan_conf_alert_section,
                                       "Alert_MAC_Address_9",
                                       "Give valid MAC address",
                                       0,
                                       alert_mac_address_checkout,
                                       alert_mac_address_commit,
                                       alert_mac_address_diff,
                                       mac_address_validate) < 0) 
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       lan_conf_alert_section,
                                       "Alert_Destination_Type_10",
                                       "Possible values: PET_Trap/OEM1/OEM2",
                                       0,
                                       alert_destination_type_checkout,
                                       alert_destination_type_commit,
                                       alert_destination_type_diff,
                                       alert_destination_type_number_validate) < 0) 
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       lan_conf_alert_section,
                                       "Alert_Acknowledge_10",
                                       "Possible values: Yes/No",
                                       0,
                                       alert_acknowledge_checkout,
                                       alert_acknowledge_commit,
                                       alert_acknowledge_diff,
                                       yes_no_validate) < 0) 
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       lan_conf_alert_section,
                                       "Alert_Acknowledge_Timeout_10",
                                       "Give valid unsigned number in seconds",
                                       0,
                                       alert_acknowledge_timeout_checkout,
                                       alert_acknowledge_timeout_commit,
                                       alert_acknowledge_timeout_diff,
                                       number_range_one_byte) < 0) 
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       lan_conf_alert_section,
                                       "Alert_Retries_10",
                                       "Give valid unsigned number",
                                       0,
                                       alert_retries_checkout,
                                       alert_retries_commit,
                                       alert_retries_diff,
                                       alert_retries_validate) < 0) 
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       lan_conf_alert_section,
                                       "Alert_Gateway_10",
                                       "Possible values: Default/Backup",
                                       0,
                                       alert_gateway_checkout,
                                       alert_gateway_commit,
                                       alert_gateway_diff,
                                       alert_gateway_number_validate) < 0) 
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       lan_conf_alert_section,
                                       "Alert_IP_Address_10",
                                       "Give valid IP address",
                                       0,
                                       alert_ip_address_checkout,
                                       alert_ip_address_commit,
                                       alert_ip_address_diff,
                                       ip_address_validate) < 0) 
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       lan_conf_alert_section,
                                       "Alert_MAC_Address_10",
                                       "Give valid MAC address",
                                       0,
                                       alert_mac_address_checkout,
                                       alert_mac_address_commit,
                                       alert_mac_address_diff,
                                       mac_address_validate) < 0) 
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       lan_conf_alert_section,
                                       "Alert_Destination_Type_11",
                                       "Possible values: PET_Trap/OEM1/OEM2",
                                       0,
                                       alert_destination_type_checkout,
                                       alert_destination_type_commit,
                                       alert_destination_type_diff,
                                       alert_destination_type_number_validate) < 0) 
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       lan_conf_alert_section,
                                       "Alert_Acknowledge_11",
                                       "Possible values: Yes/No",
                                       0,
                                       alert_acknowledge_checkout,
                                       alert_acknowledge_commit,
                                       alert_acknowledge_diff,
                                       yes_no_validate) < 0) 
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       lan_conf_alert_section,
                                       "Alert_Acknowledge_Timeout_11",
                                       "Give valid unsigned number in seconds",
                                       0,
                                       alert_acknowledge_timeout_checkout,
                                       alert_acknowledge_timeout_commit,
                                       alert_acknowledge_timeout_diff,
                                       number_range_one_byte) < 0) 
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       lan_conf_alert_section,
                                       "Alert_Retries_11",
                                       "Give valid unsigned number",
                                       0,
                                       alert_retries_checkout,
                                       alert_retries_commit,
                                       alert_retries_diff,
                                       alert_retries_validate) < 0) 
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       lan_conf_alert_section,
                                       "Alert_Gateway_11",
                                       "Possible values: Default/Backup",
                                       0,
                                       alert_gateway_checkout,
                                       alert_gateway_commit,
                                       alert_gateway_diff,
                                       alert_gateway_number_validate) < 0) 
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       lan_conf_alert_section,
                                       "Alert_IP_Address_11",
                                       "Give valid IP address",
                                       0,
                                       alert_ip_address_checkout,
                                       alert_ip_address_commit,
                                       alert_ip_address_diff,
                                       ip_address_validate) < 0) 
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       lan_conf_alert_section,
                                       "Alert_MAC_Address_11",
                                       "Give valid MAC address",
                                       0,
                                       alert_mac_address_checkout,
                                       alert_mac_address_commit,
                                       alert_mac_address_diff,
                                       mac_address_validate) < 0) 
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       lan_conf_alert_section,  
                                       "Alert_Destination_Type_12",
                                       "Possible values: PET_Trap/OEM1/OEM2",
                                       0,
                                       alert_destination_type_checkout,
                                       alert_destination_type_commit,
                                       alert_destination_type_diff,
                                       alert_destination_type_number_validate) < 0) 
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       lan_conf_alert_section,
                                       "Alert_Acknowledge_12",
                                       "Possible values: Yes/No",
                                       0,
                                       alert_acknowledge_checkout,
                                       alert_acknowledge_commit,
                                       alert_acknowledge_diff,
                                       yes_no_validate) < 0) 
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       lan_conf_alert_section,
                                       "Alert_Acknowledge_Timeout_12",
                                       "Give valid unsigned number in seconds",
                                       0,
                                       alert_acknowledge_timeout_checkout,
                                       alert_acknowledge_timeout_commit,
                                       alert_acknowledge_timeout_diff,
                                       number_range_one_byte) < 0) 
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       lan_conf_alert_section,
                                       "Alert_Retries_12",
                                       "Give valid unsigned number",
                                       0,
                                       alert_retries_checkout,
                                       alert_retries_commit,
                                       alert_retries_diff,
                                       alert_retries_validate) < 0) 
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       lan_conf_alert_section,
                                       "Alert_Gateway_12",
                                       "Possible values: Default/Backup",
                                       0,
                                       alert_gateway_checkout,
                                       alert_gateway_commit,
                                       alert_gateway_diff,
                                       alert_gateway_number_validate) < 0) 
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       lan_conf_alert_section,
                                       "Alert_IP_Address_12",
                                       "Give valid IP address",
                                       0,
                                       alert_ip_address_checkout,
                                       alert_ip_address_commit,
                                       alert_ip_address_diff,
                                       ip_address_validate) < 0) 
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       lan_conf_alert_section,
                                       "Alert_MAC_Address_12",
                                       "Give valid MAC address",
                                       0,
                                       alert_mac_address_checkout,
                                       alert_mac_address_commit,
                                       alert_mac_address_diff,
                                       mac_address_validate) < 0) 
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       lan_conf_alert_section,
                                       "Alert_Destination_Type_13",
                                       "Possible values: PET_Trap/OEM1/OEM2",
                                       0,
                                       alert_destination_type_checkout,
                                       alert_destination_type_commit,
                                       alert_destination_type_diff,
                                       alert_destination_type_number_validate) < 0) 
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       lan_conf_alert_section,
                                       "Alert_Acknowledge_13",
                                       "Possible values: Yes/No",
                                       0,
                                       alert_acknowledge_checkout,
                                       alert_acknowledge_commit,
                                       alert_acknowledge_diff,
                                       yes_no_validate) < 0) 
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       lan_conf_alert_section,
                                       "Alert_Acknowledge_Timeout_13",
                                       "Give valid unsigned number in seconds",
                                       0,
                                       alert_acknowledge_timeout_checkout,
                                       alert_acknowledge_timeout_commit,
                                       alert_acknowledge_timeout_diff,
                                       number_range_one_byte) < 0) 
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       lan_conf_alert_section,
                                       "Alert_Retries_13",
                                       "Give valid unsigned number",
                                       0,
                                       alert_retries_checkout,
                                       alert_retries_commit,
                                       alert_retries_diff,
                                       alert_retries_validate) < 0) 
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       lan_conf_alert_section,
                                       "Alert_Gateway_13",
                                       "Possible values: Default/Backup",
                                       0,
                                       alert_gateway_checkout,
                                       alert_gateway_commit,
                                       alert_gateway_diff,
                                       alert_gateway_number_validate) < 0) 
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       lan_conf_alert_section,
                                       "Alert_IP_Address_13",
                                       "Give valid IP address",
                                       0,
                                       alert_ip_address_checkout,
                                       alert_ip_address_commit,
                                       alert_ip_address_diff,
                                       ip_address_validate) < 0) 
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       lan_conf_alert_section,
                                       "Alert_MAC_Address_13",
                                       "Give valid MAC address",
                                       0,
                                       alert_mac_address_checkout,
                                       alert_mac_address_commit,
                                       alert_mac_address_diff,
                                       mac_address_validate) < 0) 
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       lan_conf_alert_section,
                                       "Alert_Destination_Type_14",
                                       "Possible values: PET_Trap/OEM1/OEM2",
                                       0,
                                       alert_destination_type_checkout,
                                       alert_destination_type_commit,
                                       alert_destination_type_diff,
                                       alert_destination_type_number_validate) < 0) 
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       lan_conf_alert_section,
                                       "Alert_Acknowledge_14",
                                       "Possible values: Yes/No",
                                       0,
                                       alert_acknowledge_checkout,
                                       alert_acknowledge_commit,
                                       alert_acknowledge_diff,
                                       yes_no_validate) < 0) 
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       lan_conf_alert_section,
                                       "Alert_Acknowledge_Timeout_14",
                                       "Give valid unsigned number in seconds",
                                       0,
                                       alert_acknowledge_timeout_checkout,
                                       alert_acknowledge_timeout_commit,
                                       alert_acknowledge_timeout_diff,
                                       number_range_one_byte) < 0) 
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       lan_conf_alert_section,
                                       "Alert_Retries_14",
                                       "Give valid unsigned number",
                                       0,
                                       alert_retries_checkout,
                                       alert_retries_commit,
                                       alert_retries_diff,
                                       alert_retries_validate) < 0) 
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       lan_conf_alert_section,
                                       "Alert_Gateway_14",
                                       "Possible values: Default/Backup",
                                       0,
                                       alert_gateway_checkout,
                                       alert_gateway_commit,
                                       alert_gateway_diff,
                                       alert_gateway_number_validate) < 0) 
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       lan_conf_alert_section,
                                       "Alert_IP_Address_14",
                                       "Give valid IP address",
                                       0,
                                       alert_ip_address_checkout,
                                       alert_ip_address_commit,
                                       alert_ip_address_diff,
                                       ip_address_validate) < 0) 
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       lan_conf_alert_section,
                                       "Alert_MAC_Address_14",
                                       "Give valid MAC address",
                                       0,
                                       alert_mac_address_checkout,
                                       alert_mac_address_commit,
                                       alert_mac_address_diff,
                                       mac_address_validate) < 0) 
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       lan_conf_alert_section,
                                       "Alert_Destination_Type_15",
                                       "Possible values: PET_Trap/OEM1/OEM2",
                                       0,
                                       alert_destination_type_checkout,
                                       alert_destination_type_commit,
                                       alert_destination_type_diff,
                                       alert_destination_type_number_validate) < 0) 
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       lan_conf_alert_section,
                                       "Alert_Acknowledge_15",
                                       "Possible values: Yes/No",
                                       0,
                                       alert_acknowledge_checkout,
                                       alert_acknowledge_commit,
                                       alert_acknowledge_diff,
                                       yes_no_validate) < 0) 
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       lan_conf_alert_section,
                                       "Alert_Acknowledge_Timeout_15",
                                       "Give valid unsigned number in seconds",
                                       0,
                                       alert_acknowledge_timeout_checkout,
                                       alert_acknowledge_timeout_commit,
                                       alert_acknowledge_timeout_diff,
                                       number_range_one_byte) < 0) 
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       lan_conf_alert_section,
                                       "Alert_Retries_15",
                                       "Give valid unsigned number",
                                       0,
                                       alert_retries_checkout,
                                       alert_retries_commit,
                                       alert_retries_diff,
                                       alert_retries_validate) < 0) 
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       lan_conf_alert_section,
                                       "Alert_Gateway_15",
                                       "Possible values: Default/Backup",
                                       0,
                                       alert_gateway_checkout,
                                       alert_gateway_commit,
                                       alert_gateway_diff,
                                       alert_gateway_number_validate) < 0) 
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       lan_conf_alert_section,
                                       "Alert_IP_Address_15",
                                       "Give valid IP address",
                                       0,
                                       alert_ip_address_checkout,
                                       alert_ip_address_commit,
                                       alert_ip_address_diff,
                                       ip_address_validate) < 0) 
    goto cleanup;

  if (bmc_config_section_add_keyvalue (state_data,
                                       lan_conf_alert_section,
                                       "Alert_MAC_Address_15",
                                       "Give valid MAC address",
                                       0,
                                       alert_mac_address_checkout,
                                       alert_mac_address_commit,
                                       alert_mac_address_diff,
                                       mac_address_validate) < 0) 
    goto cleanup;

  return lan_conf_alert_section;

 cleanup:
  if (lan_conf_alert_section)
    bmc_config_section_destroy(state_data, lan_conf_alert_section);
  return NULL;
}

