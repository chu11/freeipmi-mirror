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
#include "bmc-config-wrapper.h"
#include "bmc-config-utils.h"
#include "bmc-config-map.h"
#include "bmc-config-validate.h"

#include "config-common.h"
#include "config-section.h"
#include "config-validate.h"

#define BMC_MAXIPADDRLEN 16
#define BMC_MAXMACADDRLEN 24

#define KEY_NAME_IP_ADDRESS_SOURCE           "IP_Address_Source"
#define KEY_NAME_IP_ADDRESS                  "IP_Address"
#define KEY_NAME_MAC_ADDRESS                 "MAC_Address"
#define KEY_NAME_SUBNET_MASK                 "Subnet_Mask"
#define KEY_NAME_DEFAULT_GATEWAY_IP_ADDRESS  "Default_Gateway_IP_Address"
#define KEY_NAME_DEFAULT_GATEWAY_MAC_ADDRESS "Default_Gateway_MAC_Address"
#define KEY_NAME_BACKUP_GATEWAY_IP_ADDRESS   "Backup_Gateway_IP_Address"
#define KEY_NAME_BACKUP_GATEWAY_MAC_ADDRESS  "Backup_Gateway_MAC_Address"
#define KEY_NAME_VLAN_ID                     "Vlan_id"               
#define KEY_NAME_VLAN_ID_ENABLE              "Vlan_Id_Enable"
#define KEY_NAME_VLAN_PRIORITY               "Vlan_Priority"

static config_err_t 
_get_lan_conf_ip_address_source (bmc_config_state_data_t *state_data, 
                                 int debug,
                                 uint8_t *ip_address_source)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_get_lan_configuration_parameters_ip_address_source_rs)))
    goto cleanup;

  if ((ret = get_lan_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_get_lan_configuration_parameters_ip_address_source (state_data->dev, 
								   channel_number, 
								   IPMI_GET_LAN_PARAMETER, 
								   SET_SELECTOR, 
								   BLOCK_SELECTOR, 
								   obj_cmd_rs) < 0)
    {
      if (debug)
        fprintf(stderr,
                "ipmi_cmd_get_lan_configuration_parameters_ip_address_source: %s\n",
                ipmi_device_strerror(ipmi_device_errnum(state_data->dev)));
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }
  
  if (fiid_obj_get (obj_cmd_rs, "ip_address_source", &val) < 0)
    goto cleanup;
  *ip_address_source = val;
  
  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

static config_err_t 
_get_lan_conf_ip_address (bmc_config_state_data_t *state_data, 
                          int debug,
                          char *ip_address,
                          unsigned int ip_address_len)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint8_t ip_address_bytes[4];
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_get_lan_configuration_parameters_ip_address_rs)))
    goto cleanup;

  if ((ret = get_lan_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_get_lan_configuration_parameters_ip_address (state_data->dev, 
							    channel_number, 
							    IPMI_GET_LAN_PARAMETER, 
							    SET_SELECTOR, 
							    BLOCK_SELECTOR, 
							    obj_cmd_rs) < 0)
    {
      if (debug)
        fprintf(stderr,
                "ipmi_cmd_get_lan_configuration_parameters_ip_address: %s\n",
                ipmi_device_strerror(ipmi_device_errnum(state_data->dev)));
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }
  
  if (ip_address && ip_address_len)
    {
      if (fiid_obj_get_data (obj_cmd_rs, 
                             "ip_address", 
                             ip_address_bytes,
                             4) < 0)
        goto cleanup;

      memset(ip_address, '\0', ip_address_len);
      snprintf (ip_address, 
                ip_address_len - 1,
                "%u.%u.%u.%u", 
                ip_address_bytes[0], 
                ip_address_bytes[1], 
                ip_address_bytes[2], 
                ip_address_bytes[3]);
    }
  else
    goto cleanup;
  
  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

static config_err_t 
_get_lan_conf_mac_address (bmc_config_state_data_t *state_data, 
                           int debug,
                           char *mac_address,
                           unsigned int mac_address_len)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint8_t mac_address_bytes[6];
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_get_lan_configuration_parameters_mac_address_rs)))
    goto cleanup;

  if ((ret = get_lan_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_get_lan_configuration_parameters_mac_address (state_data->dev, 
							     channel_number, 
							     IPMI_GET_LAN_PARAMETER, 
							     SET_SELECTOR, 
							     BLOCK_SELECTOR, 
							     obj_cmd_rs) < 0)
    {
      if (debug)
        fprintf(stderr,
                "ipmi_cmd_get_lan_configuration_parameters_mac_address: %s\n",
                ipmi_device_strerror(ipmi_device_errnum(state_data->dev)));
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }
  
  if (mac_address && mac_address_len)
    {
      if (fiid_obj_get_data (obj_cmd_rs, 
                             "mac_address", 
                             mac_address_bytes,
                             6) < 0)
        goto cleanup;

      memset(mac_address, '\0', mac_address_len);
      snprintf (mac_address, 
                mac_address_len - 1,
                "%02X:%02X:%02X:%02X:%02X:%02X", 
                mac_address_bytes[0], 
                mac_address_bytes[1], 
                mac_address_bytes[2], 
                mac_address_bytes[3], 
                mac_address_bytes[4], 
                mac_address_bytes[5]);
    }
  else
    goto cleanup;

  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

static config_err_t 
_get_lan_conf_subnet_mask (bmc_config_state_data_t *state_data, 
                           int debug,
                           char *subnet_mask,
                           unsigned int subnet_mask_len)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint8_t subnet_mask_bytes[4];
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_get_lan_configuration_parameters_subnet_mask_rs)))
    goto cleanup;

  if ((ret = get_lan_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_get_lan_configuration_parameters_subnet_mask (state_data->dev, 
							     channel_number, 
							     IPMI_GET_LAN_PARAMETER, 
							     SET_SELECTOR, 
							     BLOCK_SELECTOR, 
							     obj_cmd_rs) < 0)
    {
      if (debug)
        fprintf(stderr,
                "ipmi_cmd_get_lan_configuration_parameters_subnet_mask: %s\n",
                ipmi_device_strerror(ipmi_device_errnum(state_data->dev)));
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }
  
  if (subnet_mask && subnet_mask_len)
    {
      if (fiid_obj_get_data (obj_cmd_rs, 
                             "subnet_mask", 
                             subnet_mask_bytes,
                             4) < 0)
        goto cleanup;

      memset(subnet_mask, '\0', subnet_mask_len);
      snprintf (subnet_mask, 
                subnet_mask_len - 1,
                "%u.%u.%u.%u", 
                subnet_mask_bytes[0], 
                subnet_mask_bytes[1], 
                subnet_mask_bytes[2], 
                subnet_mask_bytes[3]);
    }
  else
    goto cleanup;
  
  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

static config_err_t 
_get_lan_conf_default_gateway_address (bmc_config_state_data_t *state_data, 
                                       int debug,
                                       char *default_gateway_address,
                                       unsigned int default_gateway_address_len)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint8_t ip_address_bytes[4];
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_get_lan_configuration_parameters_default_gateway_address_rs)))
    goto cleanup;

  if ((ret = get_lan_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_get_lan_configuration_parameters_default_gateway_address (state_data->dev, 
									 channel_number, 
									 IPMI_GET_LAN_PARAMETER, 
									 SET_SELECTOR, 
									 BLOCK_SELECTOR, 
									 obj_cmd_rs) < 0)
    {
      if (debug)
        fprintf(stderr,
                "ipmi_cmd_get_lan_configuration_parameters_default_gateway_address: %s\n",
                ipmi_device_strerror(ipmi_device_errnum(state_data->dev)));
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }
  
  if (default_gateway_address && default_gateway_address_len)
    {
      if (fiid_obj_get_data (obj_cmd_rs, 
                             "ip_address", 
                             ip_address_bytes,
                             4) < 0)
        goto cleanup;

      memset(default_gateway_address, '\0', default_gateway_address_len);
      snprintf (default_gateway_address, 
                default_gateway_address_len - 1,
                "%u.%u.%u.%u", 
                ip_address_bytes[0], 
                ip_address_bytes[1], 
                ip_address_bytes[2], 
                ip_address_bytes[3]);
    }
  else
    goto cleanup;
  
  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

static config_err_t 
_get_lan_conf_default_gateway_mac_address (bmc_config_state_data_t *state_data, 
                                           int debug,
                                           char *default_gateway_mac_address,
                                           unsigned int default_gateway_mac_address_len)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint8_t mac_address_bytes[6];
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;

  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_get_lan_configuration_parameters_default_gateway_mac_address_rs)))
    goto cleanup;

  if ((ret = get_lan_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_get_lan_configuration_parameters_default_gateway_mac_address (state_data->dev, 
									     channel_number, 
									     IPMI_GET_LAN_PARAMETER, 
									     SET_SELECTOR, 
									     BLOCK_SELECTOR, 
									     obj_cmd_rs) < 0)
    {
      if (debug)
        fprintf(stderr,
                "ipmi_cmd_get_lan_configuration_parameters_default_gateway_mac_address: %s\n",
                ipmi_device_strerror(ipmi_device_errnum(state_data->dev)));
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }
  
  if (default_gateway_mac_address && default_gateway_mac_address_len)
    {
      if (fiid_obj_get_data (obj_cmd_rs, 
                             "mac_address", 
                             mac_address_bytes,
                             6) < 0)
        goto cleanup;

      memset(default_gateway_mac_address, '\0', default_gateway_mac_address_len);
      snprintf (default_gateway_mac_address, 
                default_gateway_mac_address_len - 1,
                "%02X:%02X:%02X:%02X:%02X:%02X", 
                mac_address_bytes[0], 
                mac_address_bytes[1], 
                mac_address_bytes[2], 
                mac_address_bytes[3], 
                mac_address_bytes[4], 
                mac_address_bytes[5]);
    }
  else
    goto cleanup;
  
  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

static config_err_t 
_get_lan_conf_backup_gateway_address (bmc_config_state_data_t *state_data, 
                                      int debug,
                                      char *backup_gateway_address,
                                      unsigned int backup_gateway_address_len)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint8_t ip_address_bytes[4];
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_get_lan_configuration_parameters_backup_gateway_address_rs)))
    goto cleanup;

  if ((ret = get_lan_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_get_lan_configuration_parameters_backup_gateway_address (state_data->dev, 
									channel_number, 
									IPMI_GET_LAN_PARAMETER, 
									SET_SELECTOR, 
									BLOCK_SELECTOR, 
									obj_cmd_rs) < 0)
    {
      if (debug)
        fprintf(stderr,
                "ipmi_cmd_get_lan_configuration_parameters_backup_gateway_address: %s\n",
                ipmi_device_strerror(ipmi_device_errnum(state_data->dev)));
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }
  
  if (backup_gateway_address && backup_gateway_address_len)
    {
      if (fiid_obj_get_data (obj_cmd_rs, 
                             "ip_address", 
                             ip_address_bytes,
                             4) < 0)
        goto cleanup;

      memset(backup_gateway_address, '\0', backup_gateway_address_len);
      snprintf (backup_gateway_address, 
                backup_gateway_address_len - 1,
                "%u.%u.%u.%u", 
                ip_address_bytes[0], 
                ip_address_bytes[1], 
                ip_address_bytes[2], 
                ip_address_bytes[3]);
    }
  else
    goto cleanup;
  
  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

static config_err_t 
_get_lan_conf_backup_gateway_mac_address (bmc_config_state_data_t *state_data, 
                                          int debug,
                                          char *backup_gateway_mac_address,
                                          unsigned int backup_gateway_mac_address_len)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint8_t mac_address_bytes[6];
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_get_lan_configuration_parameters_backup_gateway_mac_address_rs)))
    goto cleanup;

  if ((ret = get_lan_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_get_lan_configuration_parameters_backup_gateway_mac_address (state_data->dev, 
									    channel_number, 
									    IPMI_GET_LAN_PARAMETER, 
									    SET_SELECTOR, 
									    BLOCK_SELECTOR, 
									    obj_cmd_rs) < 0)
    {
      if (debug)
        fprintf(stderr,
                "ipmi_cmd_get_lan_configuration_parameters_backup_gateway_mac_address: %s\n",
                ipmi_device_strerror(ipmi_device_errnum(state_data->dev)));
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }
  
  if (backup_gateway_mac_address && backup_gateway_mac_address_len)
    {
      if (fiid_obj_get_data (obj_cmd_rs, 
                             "mac_address", 
                             mac_address_bytes,
                             6) < 0)
        goto cleanup;

      memset(backup_gateway_mac_address, '\0', backup_gateway_mac_address_len);
      snprintf (backup_gateway_mac_address, 
                backup_gateway_mac_address_len - 1,
                "%02X:%02X:%02X:%02X:%02X:%02X", 
                mac_address_bytes[0], 
                mac_address_bytes[1], 
                mac_address_bytes[2], 
                mac_address_bytes[3], 
                mac_address_bytes[4], 
                mac_address_bytes[5]);
    }
  else
    goto cleanup;
  
  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

static config_err_t 
_get_lan_conf_vlan_id (bmc_config_state_data_t *state_data, 
                       int debug,
                       uint32_t *vlan_id,
                       uint8_t *vlan_id_enable)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_get_lan_configuration_parameters_vlan_id_rs)))
    goto cleanup;

  if ((ret = get_lan_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_get_lan_configuration_parameters_vlan_id (state_data->dev, 
							 channel_number, 
							 IPMI_GET_LAN_PARAMETER, 
							 SET_SELECTOR, 
							 BLOCK_SELECTOR, 
							 obj_cmd_rs) < 0) 
    {
      if (debug)
        fprintf(stderr,
                "ipmi_cmd_get_lan_configuration_parameters_vlan_id: %s\n",
                ipmi_device_strerror(ipmi_device_errnum(state_data->dev)));
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }
  
  if (fiid_obj_get (obj_cmd_rs, "vlan_id", &val) < 0)
    goto cleanup;
  *vlan_id = val;

  if (fiid_obj_get (obj_cmd_rs, "vlan_id_enable", &val) < 0)
    goto cleanup;
  *vlan_id_enable = val;
  
  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

static config_err_t 
_get_lan_conf_vlan_priority (bmc_config_state_data_t *state_data, 
                             int debug,
                             uint8_t *vlan_priority)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_get_lan_configuration_parameters_vlan_priority_rs)))
    goto cleanup;

  if ((ret = get_lan_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_get_lan_configuration_parameters_vlan_priority (state_data->dev, 
							       channel_number, 
							       IPMI_GET_LAN_PARAMETER, 
							       SET_SELECTOR, 
							       BLOCK_SELECTOR, 
							       obj_cmd_rs) < 0)
    {
      if (debug)
        fprintf(stderr,
                "ipmi_cmd_get_lan_configuration_parameters_vlan_priority: %s\n",
                ipmi_device_strerror(ipmi_device_errnum(state_data->dev)));
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }
  
  if (fiid_obj_get (obj_cmd_rs, "vlan_priority", &val) < 0)
    goto cleanup;
  *vlan_priority = val;
  
  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return (rv);
}

static config_err_t 
_set_lan_conf_ip_address_source (bmc_config_state_data_t *state_data, 
                                 int debug,
                                 uint8_t ip_address_source)
{
  fiid_obj_t obj_cmd_rs = NULL;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;

  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_set_lan_configuration_parameters_rs)))
    goto cleanup;

  if ((ret = get_lan_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_set_lan_configuration_parameters_ip_address_source (state_data->dev, 
								   channel_number, 
								   ip_address_source, 
								   obj_cmd_rs) < 0)
    {
      if (debug)
        fprintf(stderr,
                "ipmi_cmd_set_lan_configuration_parameters_ip_address_source: %s\n",
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
_set_lan_conf_ip_address (bmc_config_state_data_t *state_data, 
                          int debug,
                          char *ip_address)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint32_t ip_address_val = 0;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;
  
  if (ipmi_ipv4_address_string2int(ip_address, &ip_address_val) < 0)
    goto cleanup;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_set_lan_configuration_parameters_rs)))
    goto cleanup;

  if ((ret = get_lan_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_set_lan_configuration_parameters_ip_address (state_data->dev, 
							    channel_number, 
							    ip_address_val, 
							    obj_cmd_rs) < 0)
    {
      if (debug)
        fprintf(stderr,
                "ipmi_cmd_set_lan_configuration_parameters_ip_address: %s\n",
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
_set_lan_conf_mac_address (bmc_config_state_data_t *state_data, 
                           int debug,
                           char *mac_address)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t mac_address_val = 0;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;

  if (ipmi_mac_address_string2int(mac_address, &mac_address_val) < 0)
    goto cleanup;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_set_lan_configuration_parameters_rs)))
    goto cleanup;

  if ((ret = get_lan_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_set_lan_configuration_parameters_mac_address (state_data->dev, 
							     channel_number, 
							     mac_address_val, 
							     obj_cmd_rs) < 0)
    {
      if (debug)
        fprintf(stderr,
                "ipmi_cmd_set_lan_configuration_parameters_mac_address: %s\n",
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
_set_lan_conf_subnet_mask (bmc_config_state_data_t *state_data, 
                           int debug,
                           char *subnet_mask)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint32_t subnet_mask_val = 0;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;
  
  if (ipmi_ipv4_address_string2int(subnet_mask, &subnet_mask_val) < 0)
    goto cleanup;

  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_set_lan_configuration_parameters_rs)))
    goto cleanup;

  if ((ret = get_lan_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_set_lan_configuration_parameters_subnet_mask (state_data->dev, 
							     channel_number, 
							     subnet_mask_val, 
							     obj_cmd_rs) < 0)
    {
      if (debug)
        fprintf(stderr,
                "ipmi_cmd_set_lan_configuration_parameters_subnet_mask: %s\n",
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
_set_lan_conf_default_gateway_address (bmc_config_state_data_t *state_data, 
                                       int debug,
                                       char *default_gateway_address)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint32_t ip_address_val = 0;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;
  
  if (ipmi_ipv4_address_string2int(default_gateway_address, &ip_address_val) < 0)
    goto cleanup;

  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_set_lan_configuration_parameters_rs)))
    goto cleanup;

  if ((ret = get_lan_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_set_lan_configuration_parameters_default_gateway_address (state_data->dev, 
									 channel_number, 
									 ip_address_val, 
									 obj_cmd_rs) < 0)
    {
      if (debug)
        fprintf(stderr,
                "ipmi_cmd_set_lan_configuration_parameters_default_gateway_address: %s\n",
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
_set_lan_conf_default_gateway_mac_address (bmc_config_state_data_t *state_data, 
                                           int debug,
                                           char *default_gateway_mac_address)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t mac_address_val = 0;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;
  
  if (ipmi_mac_address_string2int(default_gateway_mac_address, &mac_address_val) < 0)
    goto cleanup;
 
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_set_lan_configuration_parameters_rs)))
    goto cleanup;

  if ((ret = get_lan_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_set_lan_configuration_parameters_default_gateway_mac_address (state_data->dev, 
									     channel_number, 
									     mac_address_val, 
									     obj_cmd_rs) < 0)
    {
      if (debug)
        fprintf(stderr,
                "ipmi_cmd_set_lan_configuration_parameters_default_gateway_mac_address: %s\n",
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
_set_lan_conf_backup_gateway_address (bmc_config_state_data_t *state_data, 
                                      int debug,
                                      char *backup_gateway_address)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint32_t ip_address_val = 0;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;
  
  if (ipmi_ipv4_address_string2int(backup_gateway_address, &ip_address_val) < 0)
    goto cleanup;

  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_set_lan_configuration_parameters_rs)))
    goto cleanup;

  if ((ret = get_lan_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_set_lan_configuration_parameters_backup_gateway_address (state_data->dev, 
									channel_number, 
									ip_address_val, 
									obj_cmd_rs) < 0)
    {
      if (debug)
        fprintf(stderr,
                "ipmi_cmd_set_lan_configuration_parameters_backup_gateway_address: %s\n",
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
_set_lan_conf_backup_gateway_mac_address (bmc_config_state_data_t *state_data, 
                                          int debug,
                                          char *backup_gateway_mac_address)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t mac_address_val = 0;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;
  
  if (ipmi_mac_address_string2int(backup_gateway_mac_address, &mac_address_val) < 0)
    goto cleanup;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_set_lan_configuration_parameters_rs)))
    goto cleanup;

  if ((ret = get_lan_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_set_lan_configuration_parameters_backup_gateway_mac_address (state_data->dev, 
									    channel_number, 
									    mac_address_val, 
									    obj_cmd_rs) < 0)
    {
      if (debug)
        fprintf(stderr,
                "ipmi_cmd_set_lan_configuration_parameters_backup_gateway_mac_address: %s\n",
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
_set_lan_conf_vlan_id (bmc_config_state_data_t *state_data, 
                       int debug,
                       uint32_t vlan_id,
                       uint8_t vlan_id_enable)
{
  fiid_obj_t obj_cmd_rs = NULL;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_set_lan_configuration_parameters_rs)))
    goto cleanup;

  if ((ret = get_lan_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_set_lan_configuration_parameters_vlan_id (state_data->dev, 
							 channel_number, 
                                                         vlan_id,
							 vlan_id_enable, 
							 obj_cmd_rs) < 0)
    {
      if (debug)
        fprintf(stderr,
                "ipmi_cmd_set_lan_configuration_parameters_vlan_id: %s\n",
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
_set_lan_conf_vlan_priority (bmc_config_state_data_t *state_data, 
                             int debug,
                             uint8_t vlan_priority)
{
  fiid_obj_t obj_cmd_rs = NULL;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;
  uint8_t channel_number;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_set_lan_configuration_parameters_rs)))
    goto cleanup;

  if ((ret = get_lan_channel_number (state_data, &channel_number)) != CONFIG_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if (ipmi_cmd_set_lan_configuration_parameters_vlan_priority (state_data->dev, 
							       channel_number, 
							       vlan_priority, 
							       obj_cmd_rs) < 0)
    {
      if (debug)
        fprintf(stderr,
                "ipmi_cmd_set_lan_configuration_parameters_vlan_priority: %s\n",
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
_lan_conf_checkout(const char *section_name,
                   struct config_keyvalue *keyvalues,
                   int debug,
                   void *arg)
{
  bmc_config_state_data_t *state_data;
  struct config_keyvalue *kv;
  config_err_t rv = CONFIG_ERR_SUCCESS;
  config_err_t ret;
  uint8_t ip_address_source_checkout = 0;
  uint8_t ip_address_source_checkout_success = 0;
  uint8_t ip_address_checkout = 0;
  uint8_t ip_address_checkout_success = 0;
  uint8_t mac_address_checkout = 0;
  uint8_t mac_address_checkout_success = 0;
  uint8_t subnet_mask_checkout = 0;
  uint8_t subnet_mask_checkout_success = 0;
  uint8_t default_gateway_ip_address_checkout = 0;
  uint8_t default_gateway_ip_address_checkout_success = 0;
  uint8_t default_gateway_mac_address_checkout = 0;
  uint8_t default_gateway_mac_address_checkout_success = 0;
  uint8_t backup_gateway_ip_address_checkout = 0;
  uint8_t backup_gateway_ip_address_checkout_success = 0;
  uint8_t backup_gateway_mac_address_checkout = 0;
  uint8_t backup_gateway_mac_address_checkout_success = 0;
  uint8_t vlan_id_checkout = 0;
  uint8_t vlan_id_checkout_success = 0;
  uint8_t vlan_priority_checkout = 0;
  uint8_t vlan_priority_checkout_success = 0;
  uint8_t ip_address_source;
  char ip_address[BMC_MAXIPADDRLEN + 1];
  char mac_address[BMC_MAXMACADDRLEN + 1];
  char subnet_mask[BMC_MAXIPADDRLEN + 1];
  char default_gateway_ip_address[BMC_MAXIPADDRLEN + 1];
  char default_gateway_mac_address[BMC_MAXMACADDRLEN + 1];
  char backup_gateway_ip_address[BMC_MAXIPADDRLEN + 1];
  char backup_gateway_mac_address[BMC_MAXMACADDRLEN + 1];
  uint32_t vlan_id;
  uint8_t vlan_id_enable;
  uint8_t vlan_priority;
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

      if (!strcasecmp(kv->key->key_name, KEY_NAME_IP_ADDRESS_SOURCE))
        ip_address_source_checkout++;
      else if (!strcasecmp(kv->key->key_name, KEY_NAME_IP_ADDRESS))
        ip_address_checkout++;
      else if (!strcasecmp(kv->key->key_name, KEY_NAME_MAC_ADDRESS))
        mac_address_checkout++;
      else if (!strcasecmp(kv->key->key_name, KEY_NAME_SUBNET_MASK))
        subnet_mask_checkout++;
      else if (!strcasecmp(kv->key->key_name, KEY_NAME_DEFAULT_GATEWAY_IP_ADDRESS))
        default_gateway_ip_address_checkout++;
      else if (!strcasecmp(kv->key->key_name, KEY_NAME_DEFAULT_GATEWAY_MAC_ADDRESS))
        default_gateway_mac_address_checkout++;
      else if (!strcasecmp(kv->key->key_name, KEY_NAME_BACKUP_GATEWAY_IP_ADDRESS))
        backup_gateway_ip_address_checkout++;
      else if (!strcasecmp(kv->key->key_name, KEY_NAME_BACKUP_GATEWAY_MAC_ADDRESS))
        backup_gateway_mac_address_checkout++;
      else if (!strcasecmp(kv->key->key_name, KEY_NAME_VLAN_ID)
               || !strcasecmp(kv->key->key_name, KEY_NAME_VLAN_ID_ENABLE))
        vlan_id_checkout++;
      else if (!strcasecmp(kv->key->key_name, KEY_NAME_VLAN_PRIORITY))
        vlan_priority_checkout++;
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

  if (ip_address_source_checkout)
    {
      if ((ret = _get_lan_conf_ip_address_source (state_data,
                                                  debug,
                                                  &ip_address_source)) == CONFIG_ERR_FATAL_ERROR)
        return CONFIG_ERR_FATAL_ERROR;
      if (ret == CONFIG_ERR_SUCCESS)
        ip_address_source_checkout_success++;
      else
        rv = ret;
    }
  if (ip_address_checkout)
    {
      if ((ret = _get_lan_conf_ip_address (state_data,
                                           debug,
                                           ip_address,
                                           BMC_MAXIPADDRLEN + 1)) == CONFIG_ERR_FATAL_ERROR)
        return CONFIG_ERR_FATAL_ERROR;
      if (ret == CONFIG_ERR_SUCCESS)
        ip_address_checkout_success++;
      else
        rv = ret;
    }
  if (mac_address_checkout)
    {
      if ((ret = _get_lan_conf_mac_address (state_data,
                                            debug,
                                            mac_address,
                                            BMC_MAXMACADDRLEN+1)) == CONFIG_ERR_FATAL_ERROR)
        return CONFIG_ERR_FATAL_ERROR;
      if (ret == CONFIG_ERR_SUCCESS)
        mac_address_checkout_success++;
      else
        rv = ret;
    }
  if (subnet_mask_checkout)
    {
      if ((ret = _get_lan_conf_subnet_mask (state_data,
                                            debug,
                                            subnet_mask,
                                            BMC_MAXIPADDRLEN + 1)) == CONFIG_ERR_FATAL_ERROR)
        return CONFIG_ERR_FATAL_ERROR;
      if (ret == CONFIG_ERR_SUCCESS)
        subnet_mask_checkout_success++;
      else
        rv = ret;
    }
  if (default_gateway_ip_address_checkout)
    {
      if ((ret = _get_lan_conf_default_gateway_address (state_data,
                                                        debug,
                                                        default_gateway_ip_address,
                                                        BMC_MAXIPADDRLEN + 1)) == CONFIG_ERR_FATAL_ERROR)
        return CONFIG_ERR_FATAL_ERROR;
      if (ret == CONFIG_ERR_SUCCESS)
        default_gateway_ip_address_checkout_success++;
      else
        rv = ret;
    }
  if (default_gateway_mac_address_checkout)
    {
      if ((ret = _get_lan_conf_default_gateway_mac_address (state_data,
                                                            debug,
                                                            default_gateway_mac_address,
                                                            BMC_MAXMACADDRLEN+1)) == CONFIG_ERR_FATAL_ERROR)
        return CONFIG_ERR_FATAL_ERROR;
      if (ret == CONFIG_ERR_SUCCESS)
        default_gateway_mac_address_checkout_success++;
      else
        rv = ret;
    }
  if (backup_gateway_ip_address_checkout)
    {
      if ((ret = _get_lan_conf_backup_gateway_address (state_data,
                                                       debug,
                                                       backup_gateway_ip_address,
                                                       BMC_MAXIPADDRLEN + 1)) == CONFIG_ERR_FATAL_ERROR)
        return CONFIG_ERR_FATAL_ERROR;
      if (ret == CONFIG_ERR_SUCCESS)
        backup_gateway_ip_address_checkout_success++;
      else
        rv = ret;
    }
  if (backup_gateway_mac_address_checkout)
    {
      if ((ret = _get_lan_conf_backup_gateway_mac_address (state_data,
                                                           debug,
                                                           backup_gateway_mac_address,
                                                           BMC_MAXMACADDRLEN+1)) == CONFIG_ERR_FATAL_ERROR)
        return CONFIG_ERR_FATAL_ERROR;
      if (ret == CONFIG_ERR_SUCCESS)
        backup_gateway_mac_address_checkout_success++;
      else
        rv = ret;
    }
  if (vlan_id_checkout) 
    {
      if ((ret = _get_lan_conf_vlan_id (state_data,
                                        debug,
                                        &vlan_id,
                                        &vlan_id_enable)) == CONFIG_ERR_FATAL_ERROR)
        return CONFIG_ERR_FATAL_ERROR;
      if (ret == CONFIG_ERR_SUCCESS)
        vlan_id_checkout_success++;
      else
        rv = ret;
    }
  if (vlan_priority_checkout)
    {
      if ((ret = _get_lan_conf_vlan_priority (state_data,
                                              debug,
                                              &vlan_priority)) == CONFIG_ERR_FATAL_ERROR)
        return CONFIG_ERR_FATAL_ERROR;
      if (ret == CONFIG_ERR_SUCCESS)
        vlan_priority_checkout_success++;
      else
        rv = ret;
    }
    
  kv = keyvalues;
  while (kv)
    {
      int temp = 0;

      assert(!kv->value_output);

      if (!strcasecmp(kv->key->key_name, KEY_NAME_IP_ADDRESS_SOURCE)
          && ip_address_source_checkout)
        temp = config_section_update_keyvalue(kv,
                                              NULL,
                                              ip_address_source_string(ip_address_source));
      else if (!strcasecmp(kv->key->key_name, KEY_NAME_IP_ADDRESS)
               && ip_address_checkout)
        temp = config_section_update_keyvalue(kv,
                                              NULL,
                                              ip_address);
      else if (!strcasecmp(kv->key->key_name, KEY_NAME_MAC_ADDRESS)
               && mac_address_checkout)
        temp = config_section_update_keyvalue(kv,
                                              NULL,
                                              mac_address);
      else if (!strcasecmp(kv->key->key_name, KEY_NAME_SUBNET_MASK)
               && subnet_mask_checkout)
        temp = config_section_update_keyvalue(kv,
                                              NULL,
                                              subnet_mask);
      else if (!strcasecmp(kv->key->key_name, KEY_NAME_DEFAULT_GATEWAY_IP_ADDRESS)
               && default_gateway_ip_address_checkout)
        temp = config_section_update_keyvalue(kv,
                                              NULL,
                                              default_gateway_ip_address);
      else if (!strcasecmp(kv->key->key_name, KEY_NAME_DEFAULT_GATEWAY_MAC_ADDRESS)
               && default_gateway_mac_address_checkout)
        temp = config_section_update_keyvalue(kv,
                                              NULL,
                                              default_gateway_mac_address);
      else if (!strcasecmp(kv->key->key_name, KEY_NAME_BACKUP_GATEWAY_IP_ADDRESS)
               && backup_gateway_ip_address_checkout)
        temp = config_section_update_keyvalue(kv,
                                              NULL,
                                              backup_gateway_ip_address);
      else if (!strcasecmp(kv->key->key_name, KEY_NAME_BACKUP_GATEWAY_MAC_ADDRESS)
               && backup_gateway_mac_address_checkout)
        temp = config_section_update_keyvalue(kv,
                                              NULL,
                                              backup_gateway_mac_address);
      else if (!strcasecmp(kv->key->key_name, KEY_NAME_VLAN_ID)
               && vlan_id_checkout_success)
        {
          snprintf(buf, BMC_CONFIG_BUFLEN, "%d", vlan_id);
          temp = config_section_update_keyvalue(kv,
                                                NULL,
                                                buf);
        }
      else if (!strcasecmp(kv->key->key_name, KEY_NAME_VLAN_ID_ENABLE)
               && vlan_id_checkout_success)
        temp = config_section_update_keyvalue(kv,
                                              NULL,
                                              vlan_id_enable ? "Yes" : "No");
      else if (!strcasecmp(kv->key->key_name, KEY_NAME_VLAN_PRIORITY)
               && vlan_priority_checkout)
        {
          snprintf(buf, BMC_CONFIG_BUFLEN, "%d", vlan_priority);
          temp = config_section_update_keyvalue(kv,
                                                NULL,
                                                buf);
        }
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
_lan_conf_commit(const char *section_name,
                 struct config_keyvalue *keyvalues,
                 int debug,
                 void *arg)
{
  bmc_config_state_data_t *state_data;
  struct config_keyvalue *kv;
  config_err_t rv = CONFIG_ERR_SUCCESS;
  config_err_t ret;
  uint8_t vlan_id_checkout = 0;
  uint8_t vlan_id_checkout_success = 0;
  uint8_t ip_address_source;
  char *ip_address = NULL;
  char *mac_address = NULL;
  char *subnet_mask = NULL;
  char *default_gateway_ip_address = NULL;
  char *default_gateway_mac_address = NULL;
  char *backup_gateway_ip_address = NULL;
  char *backup_gateway_mac_address = NULL;
  uint32_t vlan_id;
  uint8_t vlan_id_enable;
  uint8_t vlan_priority;
  uint8_t ip_address_source_commit = 0;
  uint8_t ip_address_commit = 0;
  uint8_t mac_address_commit = 0;
  uint8_t subnet_mask_commit = 0;
  uint8_t default_gateway_ip_address_commit = 0;
  uint8_t default_gateway_mac_address_commit = 0;
  uint8_t backup_gateway_ip_address_commit = 0;
  uint8_t backup_gateway_mac_address_commit = 0;
  uint8_t vlan_id_commit = 0;
  uint8_t vlan_priority_commit = 0;


  assert(section_name);
  assert(keyvalues);
  assert(arg);

  state_data = (bmc_config_state_data_t *)arg;

  kv = keyvalues;
  while (kv)
    {
      assert(kv->value_input);

      if (!strcasecmp(kv->key->key_name, KEY_NAME_IP_ADDRESS_SOURCE))
        /* no group checkout */
        ;
      else if (!strcasecmp(kv->key->key_name, KEY_NAME_IP_ADDRESS))
        /* no group checkout */
        ;
      else if (!strcasecmp(kv->key->key_name, KEY_NAME_MAC_ADDRESS))
        /* no group checkout */
        ;
      else if (!strcasecmp(kv->key->key_name, KEY_NAME_SUBNET_MASK))
        /* no group checkout */
        ;
      else if (!strcasecmp(kv->key->key_name, KEY_NAME_DEFAULT_GATEWAY_IP_ADDRESS))
        /* no group checkout */
        ;
      else if (!strcasecmp(kv->key->key_name, KEY_NAME_DEFAULT_GATEWAY_MAC_ADDRESS))
        /* no group checkout */
        ;
      else if (!strcasecmp(kv->key->key_name, KEY_NAME_BACKUP_GATEWAY_IP_ADDRESS))
        /* no group checkout */
        ;
      else if (!strcasecmp(kv->key->key_name, KEY_NAME_BACKUP_GATEWAY_MAC_ADDRESS))
        /* no group checkout */
        ;
      else if (!strcasecmp(kv->key->key_name, KEY_NAME_VLAN_ID)
               || !strcasecmp(kv->key->key_name, KEY_NAME_VLAN_ID_ENABLE))
        vlan_id_checkout++;
      else if (!strcasecmp(kv->key->key_name, KEY_NAME_VLAN_PRIORITY))
        /* no group checkout */
        ;
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

  if (vlan_id_checkout) 
    {
      if ((ret = _get_lan_conf_vlan_id (state_data,
                                        debug,
                                        &vlan_id,
                                        &vlan_id_enable)) == CONFIG_ERR_FATAL_ERROR)
        return CONFIG_ERR_FATAL_ERROR;
      if (ret == CONFIG_ERR_SUCCESS)
        vlan_id_checkout_success++;
      else
        rv = ret;
    }

  kv = keyvalues;
  while (kv)
    {
      assert(kv->value_input);

      if (!strcasecmp(kv->key->key_name, KEY_NAME_IP_ADDRESS_SOURCE))
        {
          ip_address_source = ip_address_source_number (kv->value_input);
          ip_address_source_commit++;
        }
      else if (!strcasecmp(kv->key->key_name, KEY_NAME_IP_ADDRESS))
        {
          ip_address = kv->value_input;
          ip_address_commit++;
        }
      else if (!strcasecmp(kv->key->key_name, KEY_NAME_MAC_ADDRESS))
        {
          mac_address = kv->value_input;
          mac_address_commit++;
        }
      else if (!strcasecmp(kv->key->key_name, KEY_NAME_SUBNET_MASK))
        {
          subnet_mask = kv->value_input;
          subnet_mask_commit++;
        }
      else if (!strcasecmp(kv->key->key_name, KEY_NAME_DEFAULT_GATEWAY_IP_ADDRESS))
        {
          default_gateway_ip_address = kv->value_input;
          default_gateway_ip_address_commit++;
        }
      else if (!strcasecmp(kv->key->key_name, KEY_NAME_DEFAULT_GATEWAY_MAC_ADDRESS))
        {
          default_gateway_mac_address = kv->value_input;
          default_gateway_mac_address_commit++;
        }
      else if (!strcasecmp(kv->key->key_name, KEY_NAME_BACKUP_GATEWAY_IP_ADDRESS))
        {
          backup_gateway_ip_address = kv->value_input;
          backup_gateway_ip_address_commit++;
        }
      else if (!strcasecmp(kv->key->key_name, KEY_NAME_BACKUP_GATEWAY_MAC_ADDRESS))
        {
          backup_gateway_mac_address = kv->value_input;
          backup_gateway_mac_address_commit++;
        }
      else if (!strcasecmp(kv->key->key_name, KEY_NAME_VLAN_ID)
               && vlan_id_checkout_success)
        {
          vlan_id = atoi (kv->value_input);
          vlan_id_commit++;
        }
      else if (!strcasecmp(kv->key->key_name, KEY_NAME_VLAN_ID_ENABLE)
               && vlan_id_checkout_success)
        {
          vlan_id_enable = same (kv->value_input, "yes");
          vlan_id_commit++;
        }
      else if (!strcasecmp(kv->key->key_name, KEY_NAME_VLAN_PRIORITY))
        {
          vlan_priority = atoi (kv->value_input);
          vlan_priority_commit++;
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

  if (ip_address_source_commit)
    {
      if ((ret = _set_lan_conf_ip_address_source (state_data,
                                                  debug,
                                                  ip_address_source)) == CONFIG_ERR_FATAL_ERROR)
        return CONFIG_ERR_FATAL_ERROR;
      if (ret != CONFIG_ERR_SUCCESS)
        rv = ret;
    }
  if (ip_address_commit)
    {
      if ((ret = _set_lan_conf_ip_address (state_data,
                                           debug,
                                           ip_address)) == CONFIG_ERR_FATAL_ERROR)
        return CONFIG_ERR_FATAL_ERROR;
      if (ret != CONFIG_ERR_SUCCESS)
        rv = ret;
    }
  if (mac_address_commit)
    {
      if ((ret = _set_lan_conf_mac_address (state_data,
                                            debug,
                                            mac_address)) == CONFIG_ERR_FATAL_ERROR)
        return CONFIG_ERR_FATAL_ERROR;
      if (ret != CONFIG_ERR_SUCCESS)
        rv = ret;
    }
  if (subnet_mask_commit)
    {
      if ((ret = _set_lan_conf_subnet_mask (state_data,
                                            debug,
                                            subnet_mask)) == CONFIG_ERR_FATAL_ERROR)
        return CONFIG_ERR_FATAL_ERROR;
      if (ret != CONFIG_ERR_SUCCESS)
        rv = ret;
    }
  if (default_gateway_ip_address_commit)
    {
      if ((ret = _set_lan_conf_default_gateway_address (state_data,
                                                        debug,
                                                        default_gateway_ip_address)) == CONFIG_ERR_FATAL_ERROR)
        return CONFIG_ERR_FATAL_ERROR;
      if (ret != CONFIG_ERR_SUCCESS)
        rv = ret;
    }
  if (default_gateway_mac_address_commit)
    {
      if ((ret = _set_lan_conf_default_gateway_mac_address (state_data,
                                                            debug,
                                                            default_gateway_mac_address)) == CONFIG_ERR_FATAL_ERROR)
        return CONFIG_ERR_FATAL_ERROR;
      if (ret != CONFIG_ERR_SUCCESS)
        rv = ret;
    }
  if (backup_gateway_ip_address_commit)
    {
      if ((ret = _set_lan_conf_backup_gateway_address (state_data,
                                                       debug,
                                                       backup_gateway_ip_address)) == CONFIG_ERR_FATAL_ERROR)
        return CONFIG_ERR_FATAL_ERROR;
      if (ret != CONFIG_ERR_SUCCESS)
        rv = ret;
    }
  if (backup_gateway_mac_address_commit)
    {
      if ((ret = _set_lan_conf_backup_gateway_mac_address (state_data,
                                                           debug,
                                                           backup_gateway_mac_address)) == CONFIG_ERR_FATAL_ERROR)
        return CONFIG_ERR_FATAL_ERROR;
      if (ret != CONFIG_ERR_SUCCESS)
        rv = ret;
    }
  if (vlan_id_commit) 
    {
      if ((ret = _set_lan_conf_vlan_id (state_data,
                                        debug,
                                        vlan_id,
                                        vlan_id_enable)) == CONFIG_ERR_FATAL_ERROR)
        return CONFIG_ERR_FATAL_ERROR;
      if (ret != CONFIG_ERR_SUCCESS)
        rv = ret;
    }
  if (vlan_priority_commit)
    {
      if ((ret = _set_lan_conf_vlan_priority (state_data,
                                              debug,
                                              vlan_priority)) == CONFIG_ERR_FATAL_ERROR)
        return CONFIG_ERR_FATAL_ERROR;
      if (ret != CONFIG_ERR_SUCCESS)
        rv = ret;
    }

  return rv;
}

struct config_section *
bmc_config_lan_conf_section_get (bmc_config_state_data_t *state_data)
{
  struct config_section *lan_conf_section = NULL;
  char *section_comment = 
    "In the Lan_Conf section, typical networking configuration is setup.  "
    "Most users will choose to set \"Static\" for the \"IP_Address_Source\" "
    "and set the appropriate \"IP_Address\", \"MAC_Address\", "
    "\"Subnet_Mask\", etc. for the machine.";

  if (!(lan_conf_section = config_section_create ("Lan_Conf",
                                                  "Lan_Conf",
                                                  section_comment,
                                                  0,
                                                  _lan_conf_checkout,
                                                  _lan_conf_commit)))
    goto cleanup;
  
  if (config_section_add_key (lan_conf_section,
                              KEY_NAME_IP_ADDRESS_SOURCE,
                              "Possible values: Unspecified/Static/Use_DHCP/Use_BIOS/Use_Others",
                              0,
                              ip_address_source_number_validate) < 0) 
    goto cleanup;

  if (config_section_add_key (lan_conf_section,
                              KEY_NAME_IP_ADDRESS,
                              "Give valid IP address",
                              0,
                              config_ip_address_validate) < 0) 
    goto cleanup;

  if (config_section_add_key (lan_conf_section,
                              KEY_NAME_MAC_ADDRESS,
                              "Give valid MAC address",
                              0,
                              config_mac_address_validate) < 0) 
    goto cleanup;

  if (config_section_add_key (lan_conf_section,
                              KEY_NAME_SUBNET_MASK,
                              "Give valid Subnet Mask",
                              0,
                              config_ip_address_validate) < 0) 
    goto cleanup;

  if (config_section_add_key (lan_conf_section,
                              KEY_NAME_DEFAULT_GATEWAY_IP_ADDRESS,
                              "Give valid IP address",
                              0,
                              config_ip_address_validate) < 0) 
    goto cleanup;

  if (config_section_add_key (lan_conf_section,
                              KEY_NAME_DEFAULT_GATEWAY_MAC_ADDRESS,
                              "Give valid MAC address",
                              0,
                              config_mac_address_validate) < 0) 
    goto cleanup;

  if (config_section_add_key (lan_conf_section,
                              KEY_NAME_BACKUP_GATEWAY_IP_ADDRESS,
                              "Give valid IP address",
                              0,
                              config_ip_address_validate) < 0) 
    goto cleanup;

  if (config_section_add_key (lan_conf_section,
                              KEY_NAME_BACKUP_GATEWAY_MAC_ADDRESS,
                              "Give valid MAC address",
                              0,
                              config_mac_address_validate) < 0) 
    goto cleanup;

  if (config_section_add_key (lan_conf_section,
                              KEY_NAME_VLAN_ID,
                              "Give valid unsigned number",
                              0,
                              config_number_range_twelve_bits) < 0) 
    goto cleanup;

  if (config_section_add_key (lan_conf_section,
                              KEY_NAME_VLAN_ID_ENABLE,
                              "Possible values: Yes/No",
                              0,
                              config_yes_no_validate) < 0) 
    goto cleanup;

  if (config_section_add_key (lan_conf_section,
                              KEY_NAME_VLAN_PRIORITY,
                              "Give valid unsigned number",
                              0,
                              config_number_range_one_byte) < 0) 
    goto cleanup;

  return lan_conf_section;

 cleanup:
  if (lan_conf_section)
    config_section_destroy(lan_conf_section);
  return NULL;
}

