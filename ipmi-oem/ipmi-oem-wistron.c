/*
 * Copyright (C) 2008-2012 FreeIPMI Core Team
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
#if HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */
#include <limits.h>
#include <assert.h>

#include <freeipmi/freeipmi.h>

#include "ipmi-oem.h"
#include "ipmi-oem-argp.h"
#include "ipmi-oem-common.h"
#include "ipmi-oem-wistron.h"
#include "ipmi-oem-thirdparty.h"

#include "freeipmi-portability.h"
#include "pstdout.h"

int
ipmi_oem_wistron_get_nic_mode (ipmi_oem_state_data_t *state_data)
{
  assert (state_data);
  assert (!state_data->prog_data->args->oem_options_count);

  return (ipmi_oem_thirdparty_get_nic_mode (state_data));
}

int
ipmi_oem_wistron_set_nic_mode (ipmi_oem_state_data_t *state_data)
{
  assert (state_data);
  assert (state_data->prog_data->args->oem_options_count == 1);

  return (ipmi_oem_thirdparty_set_nic_mode (state_data));
}

int
ipmi_oem_wistron_get_shared_nic_selection (ipmi_oem_state_data_t *state_data)
{
  uint32_t tmpvalue;
  int rv = -1;

  assert (state_data);
  assert (!state_data->prog_data->args->oem_options_count);

  if (ipmi_oem_thirdparty_get_extended_config_value (state_data,
						     IPMI_OEM_WISTRON_EXTENDED_CONFIGURATION_ID_NIC,
						     IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_NIC_SHARED_NIC_SELECTION,
						     0,
						     1,
						     &tmpvalue) < 0)
    goto cleanup;

  switch (tmpvalue)
    {
    case IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_NIC_SHARED_NIC_SELECTION_RESERVED:
      pstdout_printf (state_data->pstate, "clear\n");
      break;
    case IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_NIC_SHARED_NIC_SELECTION_NIC_1:
      pstdout_printf (state_data->pstate, "nic1\n");
      break;
    case IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_NIC_SHARED_NIC_SELECTION_NIC_2:
      pstdout_printf (state_data->pstate, "nic2\n");
      break;
    case IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_NIC_SHARED_NIC_SELECTION_NIC_3:
      pstdout_printf (state_data->pstate, "nic3\n");
      break;
    case IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_NIC_SHARED_NIC_SELECTION_NIC_4:
      pstdout_printf (state_data->pstate, "nic4\n");
      break;
    default:
      pstdout_printf (state_data->pstate, "unknown shared NIC selection: %Xh\n", tmpvalue);
      break;
    }

  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_oem_wistron_set_shared_nic_selection (ipmi_oem_state_data_t *state_data)
{
  uint8_t mode;
  int rv = -1;

  assert (state_data);
  assert (state_data->prog_data->args->oem_options_count == 1);

  if (strcasecmp (state_data->prog_data->args->oem_options[0], "nic1")
      && strcasecmp (state_data->prog_data->args->oem_options[0], "nic2")
      && strcasecmp (state_data->prog_data->args->oem_options[0], "nic3")
      && strcasecmp (state_data->prog_data->args->oem_options[0], "nic4")
      && strcasecmp (state_data->prog_data->args->oem_options[0], "clear"))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "%s:%s invalid OEM option argument '%s'\n",
                       state_data->prog_data->args->oem_id,
                       state_data->prog_data->args->oem_command,
                       state_data->prog_data->args->oem_options[0]);
      goto cleanup;
    }

  if (!strcasecmp (state_data->prog_data->args->oem_options[0], "nic1"))
    mode = IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_NIC_SHARED_NIC_SELECTION_NIC_1;
  else if (!strcasecmp (state_data->prog_data->args->oem_options[0], "nic2"))
    mode = IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_NIC_SHARED_NIC_SELECTION_NIC_2;
  else if (!strcasecmp (state_data->prog_data->args->oem_options[0], "nic3"))
    mode = IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_NIC_SHARED_NIC_SELECTION_NIC_3;
  else if (!strcasecmp (state_data->prog_data->args->oem_options[0], "nic4"))
    mode = IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_NIC_SHARED_NIC_SELECTION_NIC_4;
  else
    mode = IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_NIC_SHARED_NIC_SELECTION_RESERVED;

  if (ipmi_oem_thirdparty_set_extended_config_value (state_data,
						     IPMI_OEM_WISTRON_EXTENDED_CONFIGURATION_ID_NIC,
						     IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_NIC_SHARED_NIC_SELECTION,
						     0,
						     1,
						     (uint32_t)mode) < 0)
    goto cleanup;

  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_oem_wistron_get_bmc_services (ipmi_oem_state_data_t *state_data)
{
  uint8_t services = 0;
  int rv = -1;

  assert (state_data);
  assert (!state_data->prog_data->args->oem_options_count);

  if (ipmi_oem_thirdparty_get_bmc_services_bitmask (state_data, &services) < 0)
    goto cleanup;

  if (services)
    {
      /* achu: it is not clear if only one bit or multiple bits can be
       * set.  I'm assuming if the "all" bit is set, there is no need
       * to output anything else.
       */
      if (services & IPMI_OEM_WISTRON_EXTENDED_CONFIG_SECURITY_SERVICES_DISABLED_BITMASK_ALL)
        {
          pstdout_printf (state_data->pstate, "All services except IPMI disabled\n");
          goto out;
        }
      if (services & IPMI_OEM_WISTRON_EXTENDED_CONFIG_SECURITY_SERVICES_DISABLED_BITMASK_KVM_VIRTUAL_STORAGE)
        pstdout_printf (state_data->pstate, "KVM/Virtual Storage disabled\n");
      if (services & IPMI_OEM_WISTRON_EXTENDED_CONFIG_SECURITY_SERVICES_DISABLED_BITMASK_HTTP_HTTPS)
        pstdout_printf (state_data->pstate, "HTTP/HTTPS disabled\n");
      if (services & IPMI_OEM_WISTRON_EXTENDED_CONFIG_SECURITY_SERVICES_DISABLED_BITMASK_SSH)
        pstdout_printf (state_data->pstate, "SSH disabled\n");
      if (services & IPMI_OEM_WISTRON_EXTENDED_CONFIG_SECURITY_SERVICES_DISABLED_BITMASK_SNMP_V2C_AGENT)
        pstdout_printf (state_data->pstate, "SNMP v2c agent disabled\n");
      if (services & IPMI_OEM_WISTRON_EXTENDED_CONFIG_SECURITY_SERVICES_DISABLED_BITMASK_TELNET)
        pstdout_printf (state_data->pstate, "Telnet disabled\n");
    }
  else
    pstdout_printf (state_data->pstate, "All services enabled\n");

 out:
  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_oem_wistron_set_bmc_services (ipmi_oem_state_data_t *state_data)
{
  int enable = 0;
  uint8_t services = 0;
  int rv = -1;

  assert (state_data);
  assert (state_data->prog_data->args->oem_options_count == 2);

  if (strcasecmp (state_data->prog_data->args->oem_options[0], "enable")
      && strcasecmp (state_data->prog_data->args->oem_options[0], "disable"))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "%s:%s invalid OEM option argument '%s'\n",
                       state_data->prog_data->args->oem_id,
                       state_data->prog_data->args->oem_command,
                       state_data->prog_data->args->oem_options[0]);
      goto cleanup;
    }

  if (strcasecmp (state_data->prog_data->args->oem_options[1], "all")
      && strcasecmp (state_data->prog_data->args->oem_options[1], "kvm")
      && strcasecmp (state_data->prog_data->args->oem_options[1], "http")
      && strcasecmp (state_data->prog_data->args->oem_options[1], "ssh")
      && strcasecmp (state_data->prog_data->args->oem_options[1], "snmp")
      && strcasecmp (state_data->prog_data->args->oem_options[1], "telnet"))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "%s:%s invalid OEM option argument '%s'\n",
                       state_data->prog_data->args->oem_id,
                       state_data->prog_data->args->oem_command,
                       state_data->prog_data->args->oem_options[1]);
      goto cleanup;
    }

  if (!strcasecmp (state_data->prog_data->args->oem_options[0], "enable"))
    enable = 1;
        
  /* if all, it's an easy special case */
  if (!strcasecmp (state_data->prog_data->args->oem_options[1], "all"))
    {
      if (enable)
        services = IPMI_OEM_WISTRON_EXTENDED_CONFIG_SECURITY_SERVICES_DISABLED_ENABLE_ALL;
      else
        services = IPMI_OEM_WISTRON_EXTENDED_CONFIG_SECURITY_SERVICES_DISABLED_BITMASK_ALL;
    }
  else
    {
      if (ipmi_oem_thirdparty_get_bmc_services_bitmask (state_data, &services) < 0)
        goto cleanup;

      if (enable && (services & IPMI_OEM_WISTRON_EXTENDED_CONFIG_SECURITY_SERVICES_DISABLED_BITMASK_ALL))
        {
          /* clear out "all" bit, and replace with remaining bits */
          services &= (~IPMI_OEM_WISTRON_EXTENDED_CONFIG_SECURITY_SERVICES_DISABLED_BITMASK_ALL);
          services |= IPMI_OEM_WISTRON_EXTENDED_CONFIG_SECURITY_SERVICES_DISABLED_BITMASK_KVM_VIRTUAL_STORAGE;
          services |= IPMI_OEM_WISTRON_EXTENDED_CONFIG_SECURITY_SERVICES_DISABLED_BITMASK_HTTP_HTTPS;
          services |= IPMI_OEM_WISTRON_EXTENDED_CONFIG_SECURITY_SERVICES_DISABLED_BITMASK_SSH;
          services |= IPMI_OEM_WISTRON_EXTENDED_CONFIG_SECURITY_SERVICES_DISABLED_BITMASK_SNMP_V2C_AGENT;
          services |= IPMI_OEM_WISTRON_EXTENDED_CONFIG_SECURITY_SERVICES_DISABLED_BITMASK_TELNET;
        }

      if (!strcasecmp (state_data->prog_data->args->oem_options[1], "kvm"))
        {
          if (enable)
            services &= (~IPMI_OEM_WISTRON_EXTENDED_CONFIG_SECURITY_SERVICES_DISABLED_BITMASK_KVM_VIRTUAL_STORAGE);
          else
            services |= IPMI_OEM_WISTRON_EXTENDED_CONFIG_SECURITY_SERVICES_DISABLED_BITMASK_KVM_VIRTUAL_STORAGE;
        }
      else if (!strcasecmp (state_data->prog_data->args->oem_options[1], "http"))
        {
          if (enable)
            services &= (~IPMI_OEM_WISTRON_EXTENDED_CONFIG_SECURITY_SERVICES_DISABLED_BITMASK_HTTP_HTTPS);
          else
            services |= IPMI_OEM_WISTRON_EXTENDED_CONFIG_SECURITY_SERVICES_DISABLED_BITMASK_HTTP_HTTPS;
        }
      else if (!strcasecmp (state_data->prog_data->args->oem_options[1], "ssh"))
        {
          if (enable)
            services &= (~IPMI_OEM_WISTRON_EXTENDED_CONFIG_SECURITY_SERVICES_DISABLED_BITMASK_SSH);
          else
            services |= IPMI_OEM_WISTRON_EXTENDED_CONFIG_SECURITY_SERVICES_DISABLED_BITMASK_SSH;
        }
      else if (!strcasecmp (state_data->prog_data->args->oem_options[1], "snmp"))
        {
          if (enable)
            services &= (~IPMI_OEM_WISTRON_EXTENDED_CONFIG_SECURITY_SERVICES_DISABLED_BITMASK_SNMP_V2C_AGENT);
          else
            services |= IPMI_OEM_WISTRON_EXTENDED_CONFIG_SECURITY_SERVICES_DISABLED_BITMASK_SNMP_V2C_AGENT;
        }
      else if (!strcasecmp (state_data->prog_data->args->oem_options[1], "telnet"))
        {
          if (enable)
            services &= (~IPMI_OEM_WISTRON_EXTENDED_CONFIG_SECURITY_SERVICES_DISABLED_BITMASK_TELNET);
          else
            services |= IPMI_OEM_WISTRON_EXTENDED_CONFIG_SECURITY_SERVICES_DISABLED_BITMASK_TELNET;
        }
    }

  if (ipmi_oem_thirdparty_set_extended_config_value (state_data,
						     IPMI_OEM_WISTRON_EXTENDED_CONFIGURATION_ID_SECURITY,
						     IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_SECURITY_SERVICE_DISABLED,
						     0,
						     1,
						     (uint32_t)services) < 0)
    goto cleanup;

  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_oem_wistron_get_account_status (ipmi_oem_state_data_t *state_data)
{
  assert (state_data);
  assert (!state_data->prog_data->args->oem_options_count);

  return (ipmi_oem_thirdparty_get_account_status (state_data));
}

int
ipmi_oem_wistron_get_dns_config (ipmi_oem_state_data_t *state_data)
{
  uint32_t tmpvalue;
  uint8_t dnsdhcpenable;
  uint32_t dnsserver1;
  uint32_t dnsserver2;
  uint8_t dnsregisterbmc;
  char dnsbmchostname[IPMI_OEM_WISTRON_EXTENDED_CONFIG_DNS_DNS_BMC_HOST_NAME_MAX + 1];
  uint8_t dnsdomainnamedhcpenable;
  char dnsdomainname[IPMI_OEM_WISTRON_EXTENDED_CONFIG_DNS_DNS_DOMAIN_NAME_MAX + 1];
  uint8_t dnsregistrationdelay;
  int rv = -1;

  assert (state_data);
  assert (!state_data->prog_data->args->oem_options_count);

  memset (dnsbmchostname, '\0', IPMI_OEM_WISTRON_EXTENDED_CONFIG_DNS_DNS_BMC_HOST_NAME_MAX + 1);
  memset (dnsdomainname, '\0', IPMI_OEM_WISTRON_EXTENDED_CONFIG_DNS_DNS_DOMAIN_NAME_MAX + 1);

  if (ipmi_oem_thirdparty_get_extended_config_value (state_data,
						     IPMI_OEM_WISTRON_EXTENDED_CONFIGURATION_ID_DNS,
						     IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_DNS_DNS_DHCP_ENABLE,
						     0,
						     1,
						     &tmpvalue) < 0)
    goto cleanup;
  dnsdhcpenable = tmpvalue;

  if (ipmi_oem_thirdparty_get_extended_config_value (state_data,
						     IPMI_OEM_WISTRON_EXTENDED_CONFIGURATION_ID_DNS,
						     IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_DNS_DNS_SERVER1,
						     0,
						     4,
						     &tmpvalue) < 0)
    goto cleanup;
  dnsserver1 = tmpvalue;

  if (ipmi_oem_thirdparty_get_extended_config_value (state_data,
						     IPMI_OEM_WISTRON_EXTENDED_CONFIGURATION_ID_DNS,
						     IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_DNS_DNS_SERVER2,
						     0,
						     4,
						     &tmpvalue) < 0)
    goto cleanup;
  dnsserver2 = tmpvalue;
  
  if (ipmi_oem_thirdparty_get_extended_config_value (state_data,
						     IPMI_OEM_WISTRON_EXTENDED_CONFIGURATION_ID_DNS,
						     IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_DNS_DNS_REGISTER_BMC,
						     0,
						     1,
						     &tmpvalue) < 0)
    goto cleanup;
  dnsregisterbmc = tmpvalue;

  if (ipmi_oem_thirdparty_get_extended_config_string (state_data,
						      IPMI_OEM_WISTRON_EXTENDED_CONFIGURATION_ID_DNS,
						      IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_DNS_DNS_BMC_HOST_NAME,
						      0,
						      dnsbmchostname,
						      IPMI_OEM_WISTRON_EXTENDED_CONFIG_DNS_DNS_BMC_HOST_NAME_MAX) < 0)
    goto cleanup;

  if (ipmi_oem_thirdparty_get_extended_config_value (state_data,
						     IPMI_OEM_WISTRON_EXTENDED_CONFIGURATION_ID_DNS,
						     IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_DNS_DNS_DOMAIN_NAME_DHCP_ENABLE,
						     0,
						     1,
						     &tmpvalue) < 0)
    goto cleanup;
  dnsdomainnamedhcpenable = tmpvalue;

  if (ipmi_oem_thirdparty_get_extended_config_string (state_data,
						      IPMI_OEM_WISTRON_EXTENDED_CONFIGURATION_ID_DNS,
						      IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_DNS_DNS_DOMAIN_NAME,
						      0,
						      dnsdomainname,
						      IPMI_OEM_WISTRON_EXTENDED_CONFIG_DNS_DNS_DOMAIN_NAME_MAX) < 0)
    goto cleanup;

  if (ipmi_oem_thirdparty_get_extended_config_value (state_data,
						     IPMI_OEM_WISTRON_EXTENDED_CONFIGURATION_ID_DNS,
						     IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_DNS_DNS_REGISTRATION_DELAY,
						     0,
						     1,
						     &tmpvalue) < 0)
    goto cleanup;
  dnsregistrationdelay = tmpvalue;

  pstdout_printf (state_data->pstate,
		  "DNS DHCP               : %s\n",
		  (dnsdhcpenable) ? "Enabled" : "Disabled");

  pstdout_printf (state_data->pstate,
                  "DNS Server 1           : %u.%u.%u.%u\n",
                  (dnsserver1 & 0x000000FF),
                  (dnsserver1 & 0x0000FF00) >> 8,
                  (dnsserver1 & 0x00FF0000) >> 16,
                  (dnsserver1 & 0xFF000000) >> 24);

  pstdout_printf (state_data->pstate,
                  "DNS Server 2           : %u.%u.%u.%u\n",
                  (dnsserver2 & 0x000000FF),
                  (dnsserver2 & 0x0000FF00) >> 8,
                  (dnsserver2 & 0x00FF0000) >> 16,
                  (dnsserver2 & 0xFF000000) >> 24);
  
  pstdout_printf (state_data->pstate,
		  "DNS Register BMC       : %s\n",
		  (dnsdomainnamedhcpenable) ? "Enabled" : "Disabled");
  
  pstdout_printf (state_data->pstate,
                  "DNS BMC Host Name      : %s\n",
                  dnsbmchostname);
  
  pstdout_printf (state_data->pstate,
		  "DNS Domain Name DHCP   : %s\n",
		  (dnsdomainnamedhcpenable) ? "Enabled" : "Disabled");
  
  pstdout_printf (state_data->pstate,
                  "DNS Domain Name        : %s\n",
                  dnsdomainname);
  
  pstdout_printf (state_data->pstate,
		  "DNS Registration Delay : %u seconds\n",
		  dnsregistrationdelay);

  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_oem_wistron_set_dns_config (ipmi_oem_state_data_t *state_data)
{
  uint8_t dnsdhcpenable = 0;
  uint32_t dnsserver1 = 0;
  uint32_t dnsserver2 = 0;
  uint8_t dnsregisterbmc = 0;
  char dnsbmchostname[IPMI_OEM_WISTRON_EXTENDED_CONFIG_DNS_DNS_BMC_HOST_NAME_MAX + 1];
  uint8_t dnsdomainnamedhcpenable = 0;
  char dnsdomainname[IPMI_OEM_WISTRON_EXTENDED_CONFIG_DNS_DNS_DOMAIN_NAME_MAX + 1];
  uint8_t dnsregistrationdelay = 0;
  int rv = -1;
  unsigned int i;

  assert (state_data);

  memset (dnsbmchostname, '\0', IPMI_OEM_WISTRON_EXTENDED_CONFIG_DNS_DNS_BMC_HOST_NAME_MAX + 1);
  memset (dnsdomainname, '\0', IPMI_OEM_WISTRON_EXTENDED_CONFIG_DNS_DNS_DOMAIN_NAME_MAX + 1);

  if (!state_data->prog_data->args->oem_options_count)
    {
      pstdout_printf (state_data->pstate,
		      "Option: dnsdhcp=enable|disable\n"
                      "Option: dnsserver1=ipaddress\n"
                      "Option: dnsserver2=ipaddress\n"
		      "Option: dnsregisterbmc=enable|disable\n"
                      "Option: dnsbmchostname=string\n"
		      "Option: dnsdomainnamedhcp=enable|disable\n"
                      "Option: dnsdomainname=string\n"
                      "Option: dnsregistrationdelay=seconds\n");
      return (0); 
    }

  for (i = 0; i < state_data->prog_data->args->oem_options_count; i++)
    {
      char *key = NULL;
      char *value = NULL;
      
      if (ipmi_oem_parse_key_value (state_data,
                                    i,
                                    &key,
                                    &value) < 0)
        goto cleanup;

      if (!strcasecmp (key, "dnsdhcp"))
        {
          if (ipmi_oem_parse_enable (state_data, i, value, &dnsdhcpenable) < 0)
            goto cleanup;
          
          
          if (ipmi_oem_thirdparty_set_extended_config_value (state_data,
							     IPMI_OEM_WISTRON_EXTENDED_CONFIGURATION_ID_DNS,
							     IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_DNS_DNS_DHCP_ENABLE,
							     0,
							     1,
							     (uint32_t)dnsdhcpenable) < 0)
            goto cleanup;
        }
      else if (!strcasecmp (key, "dnsserver1"))
        {
          if (ipmi_oem_parse_ip_address (state_data, i, value, &dnsserver1) < 0)
            goto cleanup;
          
          
          if (ipmi_oem_thirdparty_set_extended_config_value (state_data,
							     IPMI_OEM_WISTRON_EXTENDED_CONFIGURATION_ID_DNS,
							     IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_DNS_DNS_SERVER1,
							     0,
							     4,
							     (uint32_t)dnsserver1) < 0)
            goto cleanup;
        }
      else if (!strcasecmp (key, "dnsserver2"))
        {
          if (ipmi_oem_parse_ip_address (state_data, i, value, &dnsserver2) < 0)
            goto cleanup;
          
          
          if (ipmi_oem_thirdparty_set_extended_config_value (state_data,
							     IPMI_OEM_WISTRON_EXTENDED_CONFIGURATION_ID_DNS,
							     IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_DNS_DNS_SERVER2,
							     0,
							     4,
							     (uint32_t)dnsserver2) < 0)
            goto cleanup;
        }
      else if (!strcasecmp (key, "dnsregisterbmc"))
        {
          if (ipmi_oem_parse_enable (state_data, i, value, &dnsregisterbmc) < 0)
            goto cleanup;
          
          
          if (ipmi_oem_thirdparty_set_extended_config_value (state_data,
							     IPMI_OEM_WISTRON_EXTENDED_CONFIGURATION_ID_DNS,
							     IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_DNS_DNS_REGISTER_BMC,
							     0,
							     1,
							     (uint32_t)dnsregisterbmc) < 0)
            goto cleanup;
        }
      else if (!strcasecmp (key, "dnsbmchostname"))
        {
          uint8_t string_length = 0;

          if (ipmi_oem_parse_string (state_data,
                                     i,
                                     value,
                                     &string_length,
                                     dnsbmchostname,
                                     IPMI_OEM_WISTRON_EXTENDED_CONFIG_DNS_DNS_BMC_HOST_NAME_MAX) < 0)
            goto cleanup;
          
          if (ipmi_oem_thirdparty_set_extended_config_string (state_data,
							      IPMI_OEM_WISTRON_EXTENDED_CONFIGURATION_ID_DNS,
							      IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_DNS_DNS_BMC_HOST_NAME,
							      0,
							      dnsbmchostname,
							      (unsigned int)string_length) < 0)
            goto cleanup;
        }
      else if (!strcasecmp (key, "dnsdomainnamedhcp"))
        {
          if (ipmi_oem_parse_enable (state_data, i, value, &dnsdomainnamedhcpenable) < 0)
            goto cleanup;
          
          
          if (ipmi_oem_thirdparty_set_extended_config_value (state_data,
							     IPMI_OEM_WISTRON_EXTENDED_CONFIGURATION_ID_DNS,
							     IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_DNS_DNS_DOMAIN_NAME_DHCP_ENABLE,
							     0,
							     1,
							     (uint32_t)dnsdomainnamedhcpenable) < 0)
            goto cleanup;
        }
      else if (!strcasecmp (key, "dnsdomainname"))
        {
          uint8_t string_length = 0;

          if (ipmi_oem_parse_string (state_data,
                                     i,
                                     value,
                                     &string_length,
                                     dnsdomainname,
                                     IPMI_OEM_WISTRON_EXTENDED_CONFIG_DNS_DNS_DOMAIN_NAME_MAX) < 0)
            goto cleanup;
          
          if (ipmi_oem_thirdparty_set_extended_config_string (state_data,
							      IPMI_OEM_WISTRON_EXTENDED_CONFIGURATION_ID_DNS,
							      IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_DNS_DNS_DOMAIN_NAME,
							      0,
							      dnsdomainname,
							      (unsigned int)string_length) < 0)
            goto cleanup;
        }
      else if (!strcasecmp (key, "dnsregistrationdelay"))
        {
          if (ipmi_oem_parse_1_byte_field (state_data, i, value, &dnsregistrationdelay) < 0)
            goto cleanup;
          
          if (ipmi_oem_thirdparty_set_extended_config_value (state_data,
							     IPMI_OEM_WISTRON_EXTENDED_CONFIGURATION_ID_DNS,
							     IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_DNS_DNS_REGISTRATION_DELAY,
							     0,
							     1,
							     (uint32_t)dnsregistrationdelay) < 0)
            goto cleanup;
        }
      else
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "%s:%s invalid OEM option argument '%s' : invalid key\n",
                           state_data->prog_data->args->oem_id,
                           state_data->prog_data->args->oem_command,
                           state_data->prog_data->args->oem_options[i]);
          goto cleanup;
        }
      
      free (key);
      free (value);
    }

  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_oem_wistron_get_web_server_config (ipmi_oem_state_data_t *state_data)
{
  assert (state_data);
  assert (!state_data->prog_data->args->oem_options_count);

  /* Wistron/Dell Poweredge C6220
   *
   * Web server config supports more values that v1 config, but we
   * split that out into server services config since they don't have
   * much related to "web".
   */
  return (ipmi_oem_thirdparty_get_web_server_config_v1 (state_data));
}

int
ipmi_oem_wistron_set_web_server_config (ipmi_oem_state_data_t *state_data)
{
  assert (state_data);

  /* Wistron/Dell Poweredge C6220
   *
   * Web server config supports more values that v1 config, but we
   * split that out into server services config since they don't have
   * much related to "web".
   */
  return (ipmi_oem_thirdparty_set_web_server_config_v1 (state_data));
}

int
ipmi_oem_wistron_get_server_services_config (ipmi_oem_state_data_t *state_data)
{
  uint32_t tmpvalue;
  uint16_t kvmportnum;
  uint16_t telnetportnum;
  uint16_t sshportnum;
  int rv = -1;

  assert (state_data);
  assert (!state_data->prog_data->args->oem_options_count);

  if (ipmi_oem_thirdparty_get_extended_config_value (state_data,
						     IPMI_OEM_WISTRON_EXTENDED_CONFIGURATION_ID_WEB_SERVER_CONFIGURATION,
						     IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_WEB_SERVER_CONFIGURATION_KVM_PORT_NUM,
						     0,
						     2,
						     &tmpvalue) < 0)
    goto cleanup;
  kvmportnum = tmpvalue;

  if (ipmi_oem_thirdparty_get_extended_config_value (state_data,
						     IPMI_OEM_WISTRON_EXTENDED_CONFIGURATION_ID_WEB_SERVER_CONFIGURATION,
						     IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_WEB_SERVER_CONFIGURATION_TELNET_PORT_NUM,
						     0,
						     2,
						     &tmpvalue) < 0)
    goto cleanup;
  telnetportnum = tmpvalue;

  if (ipmi_oem_thirdparty_get_extended_config_value (state_data,
						     IPMI_OEM_WISTRON_EXTENDED_CONFIGURATION_ID_WEB_SERVER_CONFIGURATION,
						     IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_WEB_SERVER_CONFIGURATION_SSH_PORT_NUM,
						     0,
						     2,
						     &tmpvalue) < 0)
    goto cleanup;
  sshportnum = tmpvalue;

  pstdout_printf (state_data->pstate,
		  "KVM Port Number    : %u\n",
		  kvmportnum);

  pstdout_printf (state_data->pstate,
		  "telnet Port Number : %u\n",
		  telnetportnum);

  pstdout_printf (state_data->pstate,
		  "SSH Port Number    : %u\n",
		  sshportnum);

  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_oem_wistron_set_server_services_config (ipmi_oem_state_data_t *state_data)
{
  uint16_t kvmportnumber = 0;
  uint16_t telnetportnumber = 0;
  uint16_t sshportnumber = 0;
  int rv = -1;
  unsigned int i;

  assert (state_data);

  if (!state_data->prog_data->args->oem_options_count)
    {
      pstdout_printf (state_data->pstate,
		      "Option: kvmportnumber=num\n"
		      "Option: telnetportnumber=num\n"
		      "Option: sshportnumber=num\n");
      return (0); 
    }

  for (i = 0; i < state_data->prog_data->args->oem_options_count; i++)
    {
      char *key = NULL;
      char *value = NULL;
      
      if (ipmi_oem_parse_key_value (state_data,
                                    i,
                                    &key,
                                    &value) < 0)
        goto cleanup;

      if (!strcasecmp (key, "kvmportnumber"))
        {
          if (ipmi_oem_parse_2_byte_field (state_data, i, value, &kvmportnumber) < 0)
            goto cleanup;
          
          if (ipmi_oem_thirdparty_set_extended_config_value (state_data,
							     IPMI_OEM_WISTRON_EXTENDED_CONFIGURATION_ID_WEB_SERVER_CONFIGURATION,
							     IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_WEB_SERVER_CONFIGURATION_KVM_PORT_NUM,
							     0,
							     2,
							     (uint32_t)kvmportnumber) < 0)
            goto cleanup;
        }
      else if (!strcasecmp (key, "telnetportnumber"))
        {
          if (ipmi_oem_parse_2_byte_field (state_data, i, value, &telnetportnumber) < 0)
            goto cleanup;
          
          if (ipmi_oem_thirdparty_set_extended_config_value (state_data,
							     IPMI_OEM_WISTRON_EXTENDED_CONFIGURATION_ID_WEB_SERVER_CONFIGURATION,
							     IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_WEB_SERVER_CONFIGURATION_TELNET_PORT_NUM,
							     0,
							     2,
							     (uint32_t)telnetportnumber) < 0)
            goto cleanup;
        }
      else if (!strcasecmp (key, "sshportnumber"))
        {
          if (ipmi_oem_parse_2_byte_field (state_data, i, value, &sshportnumber) < 0)
            goto cleanup;
          
          if (ipmi_oem_thirdparty_set_extended_config_value (state_data,
							     IPMI_OEM_WISTRON_EXTENDED_CONFIGURATION_ID_WEB_SERVER_CONFIGURATION,
							     IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_WEB_SERVER_CONFIGURATION_SSH_PORT_NUM,
							     0,
							     2,
							     (uint32_t)sshportnumber) < 0)
            goto cleanup;
        }
      else
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "%s:%s invalid OEM option argument '%s' : invalid key\n",
                           state_data->prog_data->args->oem_id,
                           state_data->prog_data->args->oem_command,
                           state_data->prog_data->args->oem_options[i]);
          goto cleanup;
        }

      free (key);
      free (value);
    }

  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_oem_wistron_get_power_management_config (ipmi_oem_state_data_t *state_data)
{
  uint32_t tmpvalue;
  uint8_t powerstaggeringacrecovery;
  uint16_t powerondelay;
  uint16_t minpowerondelay;
  uint16_t maxpowerondelay;
  int rv = -1;

  assert (state_data);
  assert (!state_data->prog_data->args->oem_options_count);

  if (ipmi_oem_thirdparty_get_extended_config_value (state_data,
						     IPMI_OEM_WISTRON_EXTENDED_CONFIGURATION_ID_POWER_MANAGEMENT,
						     IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_POWER_MANAGEMENT_POWER_STAGGERING_AC_RECOVERY,
						     0,
						     1,
						     &tmpvalue) < 0)
    goto cleanup;
  powerstaggeringacrecovery = tmpvalue;
  
  if (ipmi_oem_thirdparty_get_extended_config_value (state_data,
						     IPMI_OEM_WISTRON_EXTENDED_CONFIGURATION_ID_POWER_MANAGEMENT,
						     IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_POWER_MANAGEMENT_POWER_ON_DELAY,
						     0,
						     2,
						     &tmpvalue) < 0)
    goto cleanup;
  powerondelay = tmpvalue;

  if (ipmi_oem_thirdparty_get_extended_config_value (state_data,
						     IPMI_OEM_WISTRON_EXTENDED_CONFIGURATION_ID_POWER_MANAGEMENT,
						     IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_POWER_MANAGEMENT_MINIMUM_POWER_ON_DELAY,
						     0,
						     2,
						     &tmpvalue) < 0)
    goto cleanup;
  minpowerondelay = tmpvalue; 

  if (ipmi_oem_thirdparty_get_extended_config_value (state_data,
						     IPMI_OEM_WISTRON_EXTENDED_CONFIGURATION_ID_POWER_MANAGEMENT,
						     IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_POWER_MANAGEMENT_MAXIMUM_POWER_ON_DELAY,
						     0,
						     2,
						     &tmpvalue) < 0)
    goto cleanup;
  maxpowerondelay = tmpvalue; 
  
  if (powerstaggeringacrecovery == IPMI_OEM_WISTRON_EXTENDED_CONFIG_POWER_STAGGERING_AC_RECOVERY_IMMEDIATE)
    pstdout_printf (state_data->pstate,
                    "Power Staggering AC Recovery : Immediate\n");
  else if (powerstaggeringacrecovery == IPMI_OEM_WISTRON_EXTENDED_CONFIG_POWER_STAGGERING_AC_RECOVERY_AUTO)
    pstdout_printf (state_data->pstate,
                    "Power Staggering AC Recovery : Auto\n");
  else if (powerstaggeringacrecovery == IPMI_OEM_WISTRON_EXTENDED_CONFIG_POWER_STAGGERING_AC_RECOVERY_USER_DEFINED)
    pstdout_printf (state_data->pstate,
                    "Power Staggering AC Recovery : User Defined\n");
  else
    pstdout_printf (state_data->pstate,
                    "Power Staggering AC Recovery : %Xh\n",
                    powerstaggeringacrecovery);
  
  pstdout_printf (state_data->pstate,
		  "Power On Delay               : %u seconds\n",
                  powerondelay);
  
  pstdout_printf (state_data->pstate,
		  "Minimum Power On Delay       : %u seconds\n",
                  minpowerondelay);

  pstdout_printf (state_data->pstate,
		  "Maximum Power On Delay       : %u seconds\n",
                  maxpowerondelay);

  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_oem_wistron_set_power_management_config (ipmi_oem_state_data_t *state_data)
{
  uint8_t powerstaggeringacrecovery = 0;
  uint16_t powerondelay = 0;
  uint16_t maxpowerondelay = 0;
  int rv = -1;
  unsigned int i;

  assert (state_data);

  if (!state_data->prog_data->args->oem_options_count)
    {
      pstdout_printf (state_data->pstate,
		      "Option: powerstaggeringacrecovery=immediate|auto|user\n"
		      "Option: powerondelay=seconds\n"
		      "Option: maxpowerondelay=seconds\n");
      return (0); 
    }

  for (i = 0; i < state_data->prog_data->args->oem_options_count; i++)
    {
      char *key = NULL;
      char *value = NULL;
      
      if (ipmi_oem_parse_key_value (state_data,
                                    i,
                                    &key,
                                    &value) < 0)
        goto cleanup;

      if (!strcasecmp (key, "powerstaggeringacrecovery"))
        {
          if (strcasecmp (value, "immediate")
              && strcasecmp (value, "auto")
              && strcasecmp (value, "user"))
            {
              pstdout_fprintf (state_data->pstate,
                               stderr,
                               "%s:%s invalid OEM option argument '%s' : invalid value\n",
                               state_data->prog_data->args->oem_id,
                               state_data->prog_data->args->oem_command,
                               state_data->prog_data->args->oem_options[i]);
              goto cleanup;
            }

          if (!strcasecmp (value, "immediate"))
            powerstaggeringacrecovery = IPMI_OEM_WISTRON_EXTENDED_CONFIG_POWER_STAGGERING_AC_RECOVERY_IMMEDIATE;
          else if (!strcasecmp (value, "auto"))
            powerstaggeringacrecovery = IPMI_OEM_WISTRON_EXTENDED_CONFIG_POWER_STAGGERING_AC_RECOVERY_AUTO;
          else /* !strcasecmp (value, "user")) */
            powerstaggeringacrecovery = IPMI_OEM_WISTRON_EXTENDED_CONFIG_POWER_STAGGERING_AC_RECOVERY_USER_DEFINED;

          if (ipmi_oem_thirdparty_set_extended_config_value (state_data,
							     IPMI_OEM_WISTRON_EXTENDED_CONFIGURATION_ID_POWER_MANAGEMENT,
							     IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_POWER_MANAGEMENT_POWER_STAGGERING_AC_RECOVERY,
							     0,
							     1,
							     (uint32_t)powerstaggeringacrecovery) < 0)
            goto cleanup;
        }
      else if (!strcasecmp (key, "powerondelay"))
        {
          if (ipmi_oem_parse_2_byte_field (state_data, i, value, &powerondelay) < 0)
            goto cleanup;
          
          if (ipmi_oem_thirdparty_set_extended_config_value (state_data,
							     IPMI_OEM_WISTRON_EXTENDED_CONFIGURATION_ID_POWER_MANAGEMENT,
							     IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_POWER_MANAGEMENT_POWER_ON_DELAY,
							     0,
							     2,
							     (uint32_t)powerondelay) < 0)
            goto cleanup;
        }
      else if (!strcasecmp (key, "maxpowerondelay"))
        {
          if (ipmi_oem_parse_2_byte_field (state_data, i, value, &maxpowerondelay) < 0)
            goto cleanup;
          
          if (ipmi_oem_thirdparty_set_extended_config_value (state_data,
							     IPMI_OEM_WISTRON_EXTENDED_CONFIGURATION_ID_POWER_MANAGEMENT,
							     IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_POWER_MANAGEMENT_MAXIMUM_POWER_ON_DELAY,
							     0,
							     2,
							     (uint32_t)maxpowerondelay) < 0)
            goto cleanup;
        }
      else
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "%s:%s invalid OEM option argument '%s' : invalid key\n",
                           state_data->prog_data->args->oem_id,
                           state_data->prog_data->args->oem_command,
                           state_data->prog_data->args->oem_options[i]);
          goto cleanup;
        }

      free (key);
      free (value);
    }

  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_oem_wistron_get_firmware_information (ipmi_oem_state_data_t *state_data)
{
  uint32_t tmpvalue;
  char name[IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_FIRMWARE_INFORMATION_NAME_LEN + 1];
  char description[IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_FIRMWARE_INFORMATION_DESCRIPTION_LEN + 1];
  uint8_t entity;
  char *entity_str = NULL;
  char product_info[IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_FIRMWARE_INFORMATION_PRODUCT_INFO_LEN + 1];
  char firmware_version[IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_FIRMWARE_INFORMATION_FIRMWARE_VERSION_LEN + 1];
  char branch[IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_FIRMWARE_INFORMATION_BRANCH_LEN + 1];
  char build_information[IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_FIRMWARE_INFORMATION_BUILD_INFORMATION_LEN + 1];
  int rv = -1;

  assert (state_data);
  assert (!state_data->prog_data->args->oem_options_count);

  memset (name, '\0', IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_FIRMWARE_INFORMATION_NAME_LEN + 1);
  memset (description, '\0', IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_FIRMWARE_INFORMATION_DESCRIPTION_LEN + 1);
  memset (product_info, '\0', IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_FIRMWARE_INFORMATION_PRODUCT_INFO_LEN + 1);
  memset (firmware_version, '\0', IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_FIRMWARE_INFORMATION_FIRMWARE_VERSION_LEN + 1);
  memset (branch, '\0', IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_FIRMWARE_INFORMATION_BRANCH_LEN + 1);
  memset (build_information, '\0', IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_FIRMWARE_INFORMATION_BUILD_INFORMATION_LEN + 1);

  if (ipmi_oem_thirdparty_get_extended_config_string (state_data,
						      IPMI_OEM_WISTRON_EXTENDED_CONFIGURATION_ID_FIRMWARE_INFORMATION,
						      IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_FIRMWARE_INFORMATION_NAME,
						      0,
						      name,
						      IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_FIRMWARE_INFORMATION_NAME_LEN) < 0)
    goto cleanup;

  if (ipmi_oem_thirdparty_get_extended_config_string (state_data,
						      IPMI_OEM_WISTRON_EXTENDED_CONFIGURATION_ID_FIRMWARE_INFORMATION,
						      IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_FIRMWARE_INFORMATION_DESCRIPTION,
						      0,
						      description,
						      IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_FIRMWARE_INFORMATION_DESCRIPTION_LEN) < 0)
    goto cleanup;

  if (ipmi_oem_thirdparty_get_extended_config_value (state_data,
						     IPMI_OEM_WISTRON_EXTENDED_CONFIGURATION_ID_FIRMWARE_INFORMATION,
						     IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_FIRMWARE_INFORMATION_ENTITY,
						     0,
						     1,
						     &tmpvalue) < 0)
    goto cleanup;
  entity = tmpvalue;

  if (ipmi_oem_thirdparty_get_extended_config_string (state_data,
						      IPMI_OEM_WISTRON_EXTENDED_CONFIGURATION_ID_FIRMWARE_INFORMATION,
						      IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_FIRMWARE_INFORMATION_PRODUCT_INFO,
						      0,
						      product_info,
						      IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_FIRMWARE_INFORMATION_PRODUCT_INFO_LEN) < 0)
    goto cleanup;
 
  if (ipmi_oem_thirdparty_get_extended_config_string (state_data,
						      IPMI_OEM_WISTRON_EXTENDED_CONFIGURATION_ID_FIRMWARE_INFORMATION,
						      IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_FIRMWARE_INFORMATION_FIRMWARE_VERSION,
						      0,
						      firmware_version,
						      IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_FIRMWARE_INFORMATION_FIRMWARE_VERSION_LEN) < 0)
    goto cleanup;

  if (ipmi_oem_thirdparty_get_extended_config_string (state_data,
						      IPMI_OEM_WISTRON_EXTENDED_CONFIGURATION_ID_FIRMWARE_INFORMATION,
						      IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_FIRMWARE_INFORMATION_BRANCH,
						      0,
						      branch,
						      IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_FIRMWARE_INFORMATION_BRANCH_LEN) < 0)
    goto cleanup;

  if (ipmi_oem_thirdparty_get_extended_config_string (state_data,
						      IPMI_OEM_WISTRON_EXTENDED_CONFIGURATION_ID_FIRMWARE_INFORMATION,
						      IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_FIRMWARE_INFORMATION_BUILD_INFORMATION,
						      0,
						      build_information,
						      IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_FIRMWARE_INFORMATION_BUILD_INFORMATION_LEN) < 0)
    goto cleanup;

  pstdout_printf (state_data->pstate,
                  "BMC Name               : %s\n",
                  name);

  pstdout_printf (state_data->pstate,
                  "Controller Description : %s\n",
                  description);

  switch (entity)
    {
    case IPMI_OEM_WISTRON_EXTENDED_CONFIG_FIRMWARE_INFORMATION_ENTITY_BMC:
      entity_str = "BMC";
      break;
    case IPMI_OEM_WISTRON_EXTENDED_CONFIG_FIRMWARE_INFORMATION_ENTITY_SYSTEM_BIOS:
      entity_str = "System (BIOS)";
      break;
    case IPMI_OEM_WISTRON_EXTENDED_CONFIG_FIRMWARE_INFORMATION_ENTITY_PDB:
      entity_str = "PDB";
      break;
    case IPMI_OEM_WISTRON_EXTENDED_CONFIG_FIRMWARE_INFORMATION_ENTITY_FCB:
      entity_str = "FCB";
      break;
    default:
      entity_str = "Unrecognized";
    }

  pstdout_printf (state_data->pstate,
                  "Controller Entity      : %s\n",
                  entity_str);

  pstdout_printf (state_data->pstate,
                  "Product Info           : %s\n",
                  product_info);

  pstdout_printf (state_data->pstate,
                  "Firmware Version       : %s\n",
                  firmware_version);

  pstdout_printf (state_data->pstate,
                  "Branch                 : %s\n",
                  branch);

  pstdout_printf (state_data->pstate,
                  "Build Information      : %s\n",
                  build_information);

  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_oem_wistron_user_default_setting (ipmi_oem_state_data_t *state_data)
{
  uint8_t setting;
  int rv = -1;

  assert (state_data);
  assert (state_data->prog_data->args->oem_options_count == 1);

  if (strcasecmp (state_data->prog_data->args->oem_options[0], "set")
      && strcasecmp (state_data->prog_data->args->oem_options[0], "reset"))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "%s:%s invalid OEM option argument '%s'\n",
                       state_data->prog_data->args->oem_id,
                       state_data->prog_data->args->oem_command,
                       state_data->prog_data->args->oem_options[0]);
      goto cleanup;
    }

  if (!strcasecmp (state_data->prog_data->args->oem_options[0], "set"))
    setting = IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_FIRMWARE_INFORMATION_USER_DEFAULT_SETTING_SET_DEFAULT;
  else
    setting = IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_FIRMWARE_INFORMATION_USER_DEFAULT_SETTING_RESTORE_DEFAULT;

  if (ipmi_oem_thirdparty_set_extended_config_value (state_data,
						     IPMI_OEM_WISTRON_EXTENDED_CONFIGURATION_ID_FIRMWARE_INFORMATION,
						     IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_FIRMWARE_INFORMATION_USER_DEFAULT_SETTING,
						     0,
						     1,
						     (uint32_t)setting) < 0)
    goto cleanup;

  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_oem_wistron_get_ipv6_settings (ipmi_oem_state_data_t *state_data)
{
  uint32_t tmpvalue;
  uint8_t ipv6enable;
  char ipv6address[IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_IPV6_SETTING_IPV6_ADDRESS_LEN + 1];
  char ipv6gatewayipaddress[IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_IPV6_SETTING_IPV6_GATEWAY_IP_ADDRESS_LEN + 1];
  uint8_t ipv6prefixlength;
  uint8_t ipv6autoconfig;
  char ipv6linklocaladdress[IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_IPV6_SETTING_IPV6_LINK_LOCAL_ADDRESS_LEN + 1];
  uint8_t ipv6autodns;
  char ipv6dnsserver1[IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_IPV6_SETTING_IPV6_DNS_SERVER1_LEN];
  char ipv6dnsserver2[IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_IPV6_SETTING_IPV6_DNS_SERVER2_LEN];
  int rv = -1;

  assert (state_data);
  assert (!state_data->prog_data->args->oem_options_count);

  memset (ipv6address, '\0', IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_IPV6_SETTING_IPV6_ADDRESS_LEN + 1);
  memset (ipv6gatewayipaddress, '\0', IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_IPV6_SETTING_IPV6_GATEWAY_IP_ADDRESS_LEN + 1);
  memset (ipv6linklocaladdress, '\0', IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_IPV6_SETTING_IPV6_LINK_LOCAL_ADDRESS_LEN + 1);
  memset (ipv6dnsserver1, '\0', IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_IPV6_SETTING_IPV6_DNS_SERVER1_LEN);
  memset (ipv6dnsserver2, '\0', IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_IPV6_SETTING_IPV6_DNS_SERVER2_LEN);

  if (ipmi_oem_thirdparty_get_extended_config_value (state_data,
						     IPMI_OEM_WISTRON_EXTENDED_CONFIGURATION_ID_IPV6_SETTING,
						     IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_IPV6_SETTING_IPV6_ENABLE,
						     0,
						     1,
						     &tmpvalue) < 0)
    goto cleanup;
  ipv6enable = tmpvalue;

  if (ipmi_oem_thirdparty_get_extended_config_string (state_data,
						      IPMI_OEM_WISTRON_EXTENDED_CONFIGURATION_ID_IPV6_SETTING,
						      IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_IPV6_SETTING_IPV6_ADDRESS,
						      0,
						      ipv6address,
						      IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_IPV6_SETTING_IPV6_ADDRESS_LEN) < 0)
    goto cleanup;

  if (ipmi_oem_thirdparty_get_extended_config_string (state_data,
						      IPMI_OEM_WISTRON_EXTENDED_CONFIGURATION_ID_IPV6_SETTING,
						      IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_IPV6_SETTING_IPV6_GATEWAY_IP_ADDRESS,
						      0,
						      ipv6gatewayipaddress,
						      IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_IPV6_SETTING_IPV6_GATEWAY_IP_ADDRESS_LEN) < 0)
    goto cleanup;

  if (ipmi_oem_thirdparty_get_extended_config_value (state_data,
						     IPMI_OEM_WISTRON_EXTENDED_CONFIGURATION_ID_IPV6_SETTING,
						     IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_IPV6_SETTING_IPV6_PREFIX_LENGTH,
						     0,
						     1,
						     &tmpvalue) < 0)
    goto cleanup;
  ipv6prefixlength = tmpvalue;

  if (ipmi_oem_thirdparty_get_extended_config_value (state_data,
						     IPMI_OEM_WISTRON_EXTENDED_CONFIGURATION_ID_IPV6_SETTING,
						     IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_IPV6_SETTING_IPV6_AUTOCONFIG,
						     0,
						     1,
						     &tmpvalue) < 0)
    goto cleanup;
  ipv6autoconfig = tmpvalue;

  if (ipmi_oem_thirdparty_get_extended_config_string (state_data,
						      IPMI_OEM_WISTRON_EXTENDED_CONFIGURATION_ID_IPV6_SETTING,
						      IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_IPV6_SETTING_IPV6_LINK_LOCAL_ADDRESS,
						      0,
						      ipv6linklocaladdress,
						      IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_IPV6_SETTING_IPV6_LINK_LOCAL_ADDRESS_LEN) < 0)
    goto cleanup;

  if (ipmi_oem_thirdparty_get_extended_config_value (state_data,
						     IPMI_OEM_WISTRON_EXTENDED_CONFIGURATION_ID_IPV6_SETTING,
						     IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_IPV6_SETTING_IPV6_AUTO_DNS,
						     0,
						     1,
						     &tmpvalue) < 0)
    goto cleanup;
  ipv6autodns = tmpvalue;

  if (ipmi_oem_thirdparty_get_extended_config_string (state_data,
						      IPMI_OEM_WISTRON_EXTENDED_CONFIGURATION_ID_IPV6_SETTING,
						      IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_IPV6_SETTING_IPV6_DNS_SERVER1,
						      0,
						      ipv6dnsserver1,
						      IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_IPV6_SETTING_IPV6_DNS_SERVER1_LEN) < 0)
    goto cleanup;

  if (ipmi_oem_thirdparty_get_extended_config_string (state_data,
						      IPMI_OEM_WISTRON_EXTENDED_CONFIGURATION_ID_IPV6_SETTING,
						      IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_IPV6_SETTING_IPV6_DNS_SERVER2,
						      0,
						      ipv6dnsserver2,
						      IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_IPV6_SETTING_IPV6_DNS_SERVER2_LEN) < 0)
    goto cleanup;

  pstdout_printf (state_data->pstate,
		  "IPv6 Enable             : %s\n",
		  (ipv6enable == IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_IPV6_SETTING_IPV6_ENABLE_FALSE) ? "false" : "true"); 

  pstdout_printf (state_data->pstate,
		  "IPv6 Address            : %s\n",
		  ipv6address);

  pstdout_printf (state_data->pstate,
		  "IPv6 Gateway IP Address : %s\n",
		  ipv6gatewayipaddress);

  pstdout_printf (state_data->pstate,
		  "IPv6 Prefix Length      : %u\n",
		  ipv6prefixlength);

  pstdout_printf (state_data->pstate,
		  "IPv6 Auto Config        : %s\n",
		  (ipv6autoconfig == IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_IPV6_SETTING_IPV6_AUTOCONFIG_ENABLE_FALSE) ? "false" : "true"); 

  pstdout_printf (state_data->pstate,
		  "IPv6 Link Local Address : %s\n",
		  ipv6linklocaladdress);

  pstdout_printf (state_data->pstate,
		  "IPv6 Auto DNS           : %s\n",
		  (ipv6autodns == IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_IPV6_SETTING_IPV6_AUTO_DNS_ENABLE_FALSE) ? "false" : "true"); 

  pstdout_printf (state_data->pstate,
		  "IPv6 DNS Server 1       : %s\n",
		  ipv6dnsserver1);

  pstdout_printf (state_data->pstate,
		  "IPv6 DNS Server 2       : %s\n",
		  ipv6dnsserver2);

  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_oem_wistron_set_ipv6_settings (ipmi_oem_state_data_t *state_data)
{
  uint8_t tmpenablevalue;
  uint8_t ipv6enable;
  char ipv6address[IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_IPV6_SETTING_IPV6_ADDRESS_LEN + 1];
  char ipv6gatewayipaddress[IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_IPV6_SETTING_IPV6_GATEWAY_IP_ADDRESS_LEN + 1];
  uint8_t ipv6prefixlength;
  uint8_t ipv6autoconfig;
  char ipv6linklocaladdress[IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_IPV6_SETTING_IPV6_LINK_LOCAL_ADDRESS_LEN + 1];
  uint8_t ipv6autodns;
  char ipv6dnsserver1[IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_IPV6_SETTING_IPV6_DNS_SERVER1_LEN];
  char ipv6dnsserver2[IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_IPV6_SETTING_IPV6_DNS_SERVER2_LEN];
  int rv = -1;
  unsigned int i;

  assert (state_data);

  memset (ipv6address, '\0', IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_IPV6_SETTING_IPV6_ADDRESS_LEN + 1);
  memset (ipv6gatewayipaddress, '\0', IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_IPV6_SETTING_IPV6_GATEWAY_IP_ADDRESS_LEN + 1);
  memset (ipv6linklocaladdress, '\0', IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_IPV6_SETTING_IPV6_LINK_LOCAL_ADDRESS_LEN + 1);
  memset (ipv6dnsserver1, '\0', IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_IPV6_SETTING_IPV6_DNS_SERVER1_LEN);
  memset (ipv6dnsserver2, '\0', IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_IPV6_SETTING_IPV6_DNS_SERVER2_LEN);

  if (!state_data->prog_data->args->oem_options_count)
    {
      pstdout_printf (state_data->pstate,
		      "Option: ipv6=enable|disable\n"
                      "Option: ipv6address=ipaddress\n"
                      "Option: ipv6gatewayaddress=ipaddress\n"
		      "Option: ipv6prefixlength=length\n"
		      "Option: ipv6autoconfig=enable|disable\n"
		      "Option: ipv6autodns=enable|disable\n"
		      "Option: ipv6dnsserver1=ipaddress\n"
                      "Option: ipv6dnsserver2=ipaddress\n");
      return (0); 
    }

  for (i = 0; i < state_data->prog_data->args->oem_options_count; i++)
    {
      char *key = NULL;
      char *value = NULL;
      
      if (ipmi_oem_parse_key_value (state_data,
                                    i,
                                    &key,
                                    &value) < 0)
        goto cleanup;

      if (!strcasecmp (key, "ipv6"))
        {
          if (ipmi_oem_parse_enable (state_data, i, value, &tmpenablevalue) < 0)
            goto cleanup;
          
	  if (tmpenablevalue)
	    ipv6enable = IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_IPV6_SETTING_IPV6_ENABLE_TRUE;
	  else
	    ipv6enable = IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_IPV6_SETTING_IPV6_ENABLE_FALSE;
	  
          if (ipmi_oem_thirdparty_set_extended_config_value (state_data,
							     IPMI_OEM_WISTRON_EXTENDED_CONFIGURATION_ID_IPV6_SETTING,
							     IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_IPV6_SETTING_IPV6_ENABLE,
							     0,
							     1,
							     (uint32_t)ipv6enable) < 0)
            goto cleanup;
        }
      else if (!strcasecmp (key, "ipv6address"))
        {
          uint8_t string_length = 0;

          if (ipmi_oem_parse_string (state_data,
                                     i,
                                     value,
                                     &string_length,
                                     ipv6address,
				     IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_IPV6_SETTING_IPV6_ADDRESS_LEN) < 0)
            goto cleanup;
          
          if (ipmi_oem_thirdparty_set_extended_config_string (state_data,
							      IPMI_OEM_WISTRON_EXTENDED_CONFIGURATION_ID_IPV6_SETTING,
							      IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_IPV6_SETTING_IPV6_ADDRESS,
							      0,
							      ipv6address,
							      (unsigned int)string_length) < 0)
            goto cleanup;
        }
      else if (!strcasecmp (key, "ipv6gatewayipaddress"))
        {
          uint8_t string_length = 0;

          if (ipmi_oem_parse_string (state_data,
                                     i,
                                     value,
                                     &string_length,
                                     ipv6gatewayipaddress,
				     IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_IPV6_SETTING_IPV6_GATEWAY_IP_ADDRESS_LEN) < 0)
            goto cleanup;
          
          if (ipmi_oem_thirdparty_set_extended_config_string (state_data,
							      IPMI_OEM_WISTRON_EXTENDED_CONFIGURATION_ID_IPV6_SETTING,
							      IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_IPV6_SETTING_IPV6_GATEWAY_IP_ADDRESS,
							      0,
							      ipv6gatewayipaddress,
							      (unsigned int)string_length) < 0)
            goto cleanup;
        }
      else if (!strcasecmp (key, "ipv6prefixlength"))
        {
	  if (ipmi_oem_parse_1_byte_field (state_data, i, value, &ipv6prefixlength) < 0)
            goto cleanup;
          
          if (ipmi_oem_thirdparty_set_extended_config_value (state_data,
							     IPMI_OEM_WISTRON_EXTENDED_CONFIGURATION_ID_IPV6_SETTING,
							     IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_IPV6_SETTING_IPV6_PREFIX_LENGTH,
							     0,
							     1,
							     (uint32_t)ipv6prefixlength) < 0)
            goto cleanup;
        }
      else if (!strcasecmp (key, "ipv6autoconfig"))
        {
          if (ipmi_oem_parse_enable (state_data, i, value, &tmpenablevalue) < 0)
            goto cleanup;
          
	  if (tmpenablevalue)
	    ipv6autoconfig = IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_IPV6_SETTING_IPV6_AUTOCONFIG_ENABLE_TRUE;
	  else
	    ipv6autoconfig = IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_IPV6_SETTING_IPV6_AUTOCONFIG_ENABLE_FALSE;

          if (ipmi_oem_thirdparty_set_extended_config_value (state_data,
							     IPMI_OEM_WISTRON_EXTENDED_CONFIGURATION_ID_IPV6_SETTING,
							     IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_IPV6_SETTING_IPV6_AUTOCONFIG,
							     0,
							     1,
							     (uint32_t)ipv6autoconfig) < 0)
            goto cleanup;
        }
      else if (!strcasecmp (key, "ipv6autodns"))
        {
          if (ipmi_oem_parse_enable (state_data, i, value, &tmpenablevalue) < 0)
            goto cleanup;
          
	  if (tmpenablevalue)
	    ipv6autodns = IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_IPV6_SETTING_IPV6_AUTO_DNS_ENABLE_TRUE;
	  else
	    ipv6autodns = IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_IPV6_SETTING_IPV6_AUTO_DNS_ENABLE_FALSE;

          if (ipmi_oem_thirdparty_set_extended_config_value (state_data,
							     IPMI_OEM_WISTRON_EXTENDED_CONFIGURATION_ID_IPV6_SETTING,
							     IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_IPV6_SETTING_IPV6_AUTO_DNS,
							     0,
							     1,
							     (uint32_t)ipv6autodns) < 0)
            goto cleanup;
        }
      else if (!strcasecmp (key, "ipv6dnsserver1"))
        {
          uint8_t string_length = 0;

          if (ipmi_oem_parse_string (state_data,
                                     i,
                                     value,
                                     &string_length,
                                     ipv6dnsserver1,
				     IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_IPV6_SETTING_IPV6_DNS_SERVER1_LEN) < 0)
            goto cleanup;
          
          if (ipmi_oem_thirdparty_set_extended_config_string (state_data,
							      IPMI_OEM_WISTRON_EXTENDED_CONFIGURATION_ID_IPV6_SETTING,
							      IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_IPV6_SETTING_IPV6_DNS_SERVER1,
							      0,
							      ipv6dnsserver1,
							      (unsigned int)string_length) < 0)
            goto cleanup;
        }
      else if (!strcasecmp (key, "ipv6dnsserver2"))
        {
          uint8_t string_length = 0;

          if (ipmi_oem_parse_string (state_data,
                                     i,
                                     value,
                                     &string_length,
                                     ipv6dnsserver2,
				     IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_IPV6_SETTING_IPV6_DNS_SERVER2_LEN) < 0)
            goto cleanup;
          
          if (ipmi_oem_thirdparty_set_extended_config_string (state_data,
							      IPMI_OEM_WISTRON_EXTENDED_CONFIGURATION_ID_IPV6_SETTING,
							      IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_IPV6_SETTING_IPV6_DNS_SERVER2,
							      0,
							      ipv6dnsserver2,
							      (unsigned int)string_length) < 0)
            goto cleanup;
        }
      else
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "%s:%s invalid OEM option argument '%s' : invalid key\n",
                           state_data->prog_data->args->oem_id,
                           state_data->prog_data->args->oem_command,
                           state_data->prog_data->args->oem_options[i]);
          goto cleanup;
        }
      
      free (key);
      free (value);
    }

  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_oem_wistron_get_ipv6_trap_settings (ipmi_oem_state_data_t *state_data)
{
  uint32_t tmpvalue;
  uint32_t ipv6snmptrapdestinationsetting;
  uint8_t destination_type;
  uint8_t alertacktimeout;
  uint8_t retries;
  char ipv6snmptrapaddress[IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_IPV6_SNMP_TRAP_DESTINATION_ADDRESS_LEN + 1];
  uint8_t numberofdestinations;
  int rv = -1;
  int i;

  assert (state_data);
  assert (!state_data->prog_data->args->oem_options_count);

  memset (ipv6snmptrapaddress, '\0', IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_IPV6_SNMP_TRAP_DESTINATION_ADDRESS_LEN + 1);

  if (ipmi_oem_thirdparty_get_extended_config_value (state_data,
						     IPMI_OEM_WISTRON_EXTENDED_CONFIGURATION_ID_IPV6_TRAP_SETTING,
						     IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_IPV6_SNMP_TRAP_DESTINATION_SETTING,
						     0,
						     1,
						     &tmpvalue) < 0)
    goto cleanup;
  numberofdestinations = tmpvalue;

  for (i = 0; i < numberofdestinations; i++)
    {
      if (ipmi_oem_thirdparty_get_extended_config_value (state_data,
							 IPMI_OEM_WISTRON_EXTENDED_CONFIGURATION_ID_IPV6_TRAP_SETTING,
							 IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_IPV6_SNMP_TRAP_DESTINATION_SETTING,
							 i,
							 3,
							 &tmpvalue) < 0)
	goto cleanup;
      ipv6snmptrapdestinationsetting = tmpvalue;

      /* achu: this is stupid, why do they return these 3 values in
       * one int?  Why couldn't this be three fields.
       */
      destination_type = (ipv6snmptrapdestinationsetting & 0x000000FF);
      alertacktimeout = (ipv6snmptrapdestinationsetting & 0x0000FF00) >> 8; 
      retries = (ipv6snmptrapdestinationsetting & 0x00FF0000) >> 16; 

      if (ipmi_oem_thirdparty_get_extended_config_string (state_data,
							  IPMI_OEM_WISTRON_EXTENDED_CONFIGURATION_ID_IPV6_TRAP_SETTING,
							  IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_IPV6_SNMP_TRAP_DESTINATION_ADDRESS,
							  i,
							  ipv6snmptrapaddress,
							  IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_IPV6_SNMP_TRAP_DESTINATION_ADDRESS_LEN) < 0)
	goto cleanup;

      switch (destination_type)
	{
	case IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_IPV6_SNMP_TRAP_DESTINATION_SETTING_DESTINATION_TYPE_PET:
	  pstdout_printf (state_data->pstate,
			  "%d: Alert Destination Type                : PET Trap\n",
			  i);
	  break;
	case IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_IPV6_SNMP_TRAP_DESTINATION_SETTING_DESTINATION_TYPE_OEM1:
	  pstdout_printf (state_data->pstate,
			  "%d: Alert Destination Type                : OEM1\n",
			  i);
	  break;
	case IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_IPV6_SNMP_TRAP_DESTINATION_SETTING_DESTINATION_TYPE_OEM2:
	  pstdout_printf (state_data->pstate,
			  "%d: Alert Destination Type                : OEM2\n",
			  i);
	  break;
	default:
	  pstdout_printf (state_data->pstate,
			  "%d: Alert Destination Type                : Unknown (%Xh)\n",
			  i,
			  destination_type);
	  break;
	}

      pstdout_printf (state_data->pstate,
		      "%d: Alert Acknowledge Timeout             : %u seconds\n",
		      i,
		      alertacktimeout);

      pstdout_printf (state_data->pstate,
		      "%d: Alert Retries                         : %u\n",
		      i,
		      retries);

      pstdout_printf (state_data->pstate,
		      "%d: IPv6 SNMP Trap Destination IP Address : %s\n",
		      i,
		      ipv6snmptrapaddress);
    }
  
  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_oem_wistron_set_ipv6_trap_settings (ipmi_oem_state_data_t *state_data)
{
  uint32_t tmpvalue;
  uint8_t index;
  uint32_t ipv6snmptrapdestinationsetting;
  uint8_t destination_type;
  uint8_t alertacktimeout;
  uint8_t retries;
  char ipv6snmptrapaddress[IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_IPV6_SNMP_TRAP_DESTINATION_ADDRESS_LEN + 1];
  uint8_t numberofdestinations;
  int rv = -1;
  unsigned int i;
  char *key = NULL;
  char *value = NULL;

  assert (state_data);

  memset (ipv6snmptrapaddress, '\0', IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_IPV6_SNMP_TRAP_DESTINATION_ADDRESS_LEN + 1);

  if (!state_data->prog_data->args->oem_options_count
      || state_data->prog_data->args->oem_options_count == 1) 
    {
      pstdout_printf (state_data->pstate,
		      "Option: destinationtype=pet|oem1|oem2\n"
		      "Option: alertacktimeout=seconds\n"
		      "Option: retries=count\n"
                      "Option: ipv6snmptrapaddress=ipaddress\n");
      return (0); 
    }

  /* first field is the index, get that first */
  if (ipmi_oem_parse_key_value (state_data,
				0,
				&key,
				&value) < 0)
    goto cleanup;

  if (ipmi_oem_parse_1_byte_field (state_data, 0, value, &index) < 0)
    goto cleanup;

  /* now compare to the number of destinations */

  if (ipmi_oem_thirdparty_get_extended_config_value (state_data,
						     IPMI_OEM_WISTRON_EXTENDED_CONFIGURATION_ID_IPV6_TRAP_SETTING,
						     IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_IPV6_SNMP_TRAP_DESTINATION_SETTING,
						     0,
						     1,
						     &tmpvalue) < 0)
    goto cleanup;
  numberofdestinations = tmpvalue;

  if (index >= numberofdestinations)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "%s:%s invalid OEM option argument '%s' : index out of range\n",
                       state_data->prog_data->args->oem_id,
                       state_data->prog_data->args->oem_command,
                       state_data->prog_data->args->oem_options[0]);
      return (-1);
    } 

  for (i = 1; i < state_data->prog_data->args->oem_options_count; i++)
    {
      if (ipmi_oem_parse_key_value (state_data,
                                    i,
                                    &key,
                                    &value) < 0)
        goto cleanup;
      if (!strcasecmp (key, "destinationtype"))
        {
	  if (!strcasecmp (value, "pet"))
	    destination_type = IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_IPV6_SNMP_TRAP_DESTINATION_SETTING_DESTINATION_TYPE_PET;
	  else if (!strcasecmp (value, "oem1"))
	    destination_type = IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_IPV6_SNMP_TRAP_DESTINATION_SETTING_DESTINATION_TYPE_OEM1;
	  else if (!strcasecmp (value, "oem2"))
	    destination_type = IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_IPV6_SNMP_TRAP_DESTINATION_SETTING_DESTINATION_TYPE_OEM2;
	  else
	    {
	      pstdout_fprintf (state_data->pstate,
			       stderr,
			       "%s:%s invalid OEM option argument '%s' : invalid value\n",
			       state_data->prog_data->args->oem_id,
			       state_data->prog_data->args->oem_command,
			       state_data->prog_data->args->oem_options[i]);
	      goto cleanup;
	    }

	  if (ipmi_oem_thirdparty_get_extended_config_value (state_data,
							     IPMI_OEM_WISTRON_EXTENDED_CONFIGURATION_ID_IPV6_TRAP_SETTING,
							     IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_IPV6_SNMP_TRAP_DESTINATION_SETTING,
							     index,
							     3,
							     &tmpvalue) < 0)
	    goto cleanup;
	  ipv6snmptrapdestinationsetting = tmpvalue;

	  ipv6snmptrapdestinationsetting &= (0x00FFFF00);
	  ipv6snmptrapdestinationsetting |= destination_type;

          if (ipmi_oem_thirdparty_set_extended_config_value (state_data,
							     IPMI_OEM_WISTRON_EXTENDED_CONFIGURATION_ID_IPV6_TRAP_SETTING,
							     IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_IPV6_SNMP_TRAP_DESTINATION_SETTING,
							     index,
							     3,
							     (uint32_t)ipv6snmptrapdestinationsetting) < 0)
            goto cleanup;
        }
      else if (!strcasecmp (key, "alertacktimeout"))
	{
	  if (ipmi_oem_parse_1_byte_field (state_data, i, value, &alertacktimeout) < 0)
	    goto cleanup;

	  if (ipmi_oem_thirdparty_get_extended_config_value (state_data,
							     IPMI_OEM_WISTRON_EXTENDED_CONFIGURATION_ID_IPV6_TRAP_SETTING,
							     IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_IPV6_SNMP_TRAP_DESTINATION_SETTING,
							     index,
							     3,
							     &tmpvalue) < 0)
	    goto cleanup;
	  ipv6snmptrapdestinationsetting = tmpvalue;

	  ipv6snmptrapdestinationsetting &= (0x00FF00FF);
	  ipv6snmptrapdestinationsetting |= (alertacktimeout << 8);

          if (ipmi_oem_thirdparty_set_extended_config_value (state_data,
							     IPMI_OEM_WISTRON_EXTENDED_CONFIGURATION_ID_IPV6_TRAP_SETTING,
							     IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_IPV6_SNMP_TRAP_DESTINATION_SETTING,
							     index,
							     3,
							     (uint32_t)ipv6snmptrapdestinationsetting) < 0)
            goto cleanup;
	}
      else if (!strcasecmp (key, "retries"))
	{
	  if (ipmi_oem_parse_1_byte_field (state_data, i, value, &retries) < 0)
	    goto cleanup;

	  if (ipmi_oem_thirdparty_get_extended_config_value (state_data,
							     IPMI_OEM_WISTRON_EXTENDED_CONFIGURATION_ID_IPV6_TRAP_SETTING,
							     IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_IPV6_SNMP_TRAP_DESTINATION_SETTING,
							     index,
							     3,
							     &tmpvalue) < 0)
	    goto cleanup;
	  ipv6snmptrapdestinationsetting = tmpvalue;

	  ipv6snmptrapdestinationsetting &= (0x0000FFFF);
	  ipv6snmptrapdestinationsetting |= (retries << 16);

          if (ipmi_oem_thirdparty_set_extended_config_value (state_data,
							     IPMI_OEM_WISTRON_EXTENDED_CONFIGURATION_ID_IPV6_TRAP_SETTING,
							     IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_IPV6_SNMP_TRAP_DESTINATION_SETTING,
							     index,
							     3,
							     (uint32_t)ipv6snmptrapdestinationsetting) < 0)
            goto cleanup;
	}
      else if (!strcasecmp (key, "ipv6snmptrapaddress"))
        {
          uint8_t string_length = 0;
	  
          if (ipmi_oem_parse_string (state_data,
                                     i,
                                     value,
                                     &string_length,
				     ipv6snmptrapaddress,
				     IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_IPV6_SNMP_TRAP_DESTINATION_ADDRESS_LEN) < 0)
            goto cleanup;
          
          if (ipmi_oem_thirdparty_set_extended_config_string (state_data,
							      IPMI_OEM_WISTRON_EXTENDED_CONFIGURATION_ID_IPV6_TRAP_SETTING,
							      IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_IPV6_SNMP_TRAP_DESTINATION_ADDRESS,
							      index,
							      ipv6snmptrapaddress,
							      (unsigned int)string_length) < 0)
            goto cleanup;
        }
      else
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "%s:%s invalid OEM option argument '%s' : invalid key\n",
                           state_data->prog_data->args->oem_id,
                           state_data->prog_data->args->oem_command,
                           state_data->prog_data->args->oem_options[i]);
          goto cleanup;
        }
      
      free (key);
      free (value);
    }

  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_oem_wistron_get_sol_idle_timeout (ipmi_oem_state_data_t *state_data)
{
  assert (state_data);
  assert (!state_data->prog_data->args->oem_options_count);

  return (ipmi_oem_thirdparty_get_sol_idle_timeout (state_data));
}

int
ipmi_oem_wistron_set_sol_idle_timeout (ipmi_oem_state_data_t *state_data)
{
  assert (state_data);
  assert (state_data->prog_data->args->oem_options_count == 1);

  return (ipmi_oem_thirdparty_set_sol_idle_timeout (state_data));
}

static int
_wistron_get_telnet_ssh_redirect_function (ipmi_oem_state_data_t *state_data, uint8_t attribute_id)
{
  uint32_t tmpvalue;
  int rv = -1;

  assert (state_data);
  assert (!state_data->prog_data->args->oem_options_count);
  assert (attribute_id == IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_SOL_TELNET_REDIRECT_FUNCTION_SELECTION
	  || attribute_id == IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_SOL_SSH_REDIRECT_FUNCTION_SELECTION);
  assert (IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_SOL_TELNET_REDIRECT_FUNCTION_SELECTION_DISABLE == IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_SOL_SSH_REDIRECT_FUNCTION_SELECTION_DISABLE);
  assert (IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_SOL_TELNET_REDIRECT_FUNCTION_SELECTION_SOL_ENABLED == IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_SOL_SSH_REDIRECT_FUNCTION_SELECTION_SOL_ENABLED);
  assert (IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_SOL_TELNET_REDIRECT_FUNCTION_SELECTION_SMASH_ENABLED == IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_SOL_SSH_REDIRECT_FUNCTION_SELECTION_SMASH_ENABLED);

  if (ipmi_oem_thirdparty_get_extended_config_value (state_data,
						     IPMI_OEM_WISTRON_EXTENDED_CONFIGURATION_ID_SOL,
						     attribute_id,
						     0,
						     1,
						     &tmpvalue) < 0)
    goto cleanup;
  
  switch (tmpvalue)
    {
    case IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_SOL_TELNET_REDIRECT_FUNCTION_SELECTION_DISABLE:
      pstdout_printf (state_data->pstate, "disabled\n");
      break;
    case IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_SOL_TELNET_REDIRECT_FUNCTION_SELECTION_SOL_ENABLED:
      pstdout_printf (state_data->pstate, "SOL enabled\n");
      break;
    case IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_SOL_TELNET_REDIRECT_FUNCTION_SELECTION_SMASH_ENABLED:
      pstdout_printf (state_data->pstate, "SMASH enabled\n");
      break;
    default:
      pstdout_printf (state_data->pstate, "unknown redirect: %Xh\n", tmpvalue);
      break;
    }
  
  rv = 0;
 cleanup:
  return (rv);
}

static int
_wistron_set_telnet_ssh_redirect_function (ipmi_oem_state_data_t *state_data, uint8_t attribute_id)
{
  uint8_t tmpvalue = 0;
  int rv = -1;
  
  assert (state_data);
  assert (state_data->prog_data->args->oem_options_count == 1);
  assert (attribute_id == IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_SOL_TELNET_REDIRECT_FUNCTION_SELECTION
	  || attribute_id == IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_SOL_SSH_REDIRECT_FUNCTION_SELECTION);
  assert (IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_SOL_TELNET_REDIRECT_FUNCTION_SELECTION_DISABLE == IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_SOL_SSH_REDIRECT_FUNCTION_SELECTION_DISABLE);
  assert (IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_SOL_TELNET_REDIRECT_FUNCTION_SELECTION_SOL_ENABLED == IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_SOL_SSH_REDIRECT_FUNCTION_SELECTION_SOL_ENABLED);
  assert (IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_SOL_TELNET_REDIRECT_FUNCTION_SELECTION_SMASH_ENABLED == IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_SOL_SSH_REDIRECT_FUNCTION_SELECTION_SMASH_ENABLED);
  
  if (strcasecmp (state_data->prog_data->args->oem_options[0], "disable")
      && strcasecmp (state_data->prog_data->args->oem_options[0], "solenable")
      && strcasecmp (state_data->prog_data->args->oem_options[0], "smashenable"))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "%s:%s invalid OEM option argument '%s'\n",
                       state_data->prog_data->args->oem_id,
                       state_data->prog_data->args->oem_command,
                       state_data->prog_data->args->oem_options[0]);
      goto cleanup;
    }
  
  if (!strcasecmp (state_data->prog_data->args->oem_options[0], "disable"))
    tmpvalue = IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_SOL_TELNET_REDIRECT_FUNCTION_SELECTION_DISABLE;
  else if (!strcasecmp (state_data->prog_data->args->oem_options[0], "solenable"))
    tmpvalue = IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_SOL_TELNET_REDIRECT_FUNCTION_SELECTION_SOL_ENABLED;
  else
    tmpvalue = IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_SOL_TELNET_REDIRECT_FUNCTION_SELECTION_SMASH_ENABLED;
  
  if (ipmi_oem_thirdparty_set_extended_config_value (state_data,
						     IPMI_OEM_WISTRON_EXTENDED_CONFIGURATION_ID_SOL,
						     attribute_id,
						     0,
						     1,
						     (uint32_t)tmpvalue) < 0)
    goto cleanup;

  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_oem_wistron_get_telnet_redirect_function (ipmi_oem_state_data_t *state_data)
{
  assert (state_data);
  assert (!state_data->prog_data->args->oem_options_count);

  return (_wistron_get_telnet_ssh_redirect_function (state_data, IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_SOL_TELNET_REDIRECT_FUNCTION_SELECTION));
}

int
ipmi_oem_wistron_set_telnet_redirect_function (ipmi_oem_state_data_t *state_data)
{
  assert (state_data);
  assert (state_data->prog_data->args->oem_options_count == 1);

  return (_wistron_set_telnet_ssh_redirect_function (state_data, IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_SOL_TELNET_REDIRECT_FUNCTION_SELECTION));
}

int
ipmi_oem_wistron_get_ssh_redirect_function (ipmi_oem_state_data_t *state_data)
{
  assert (state_data);
  assert (!state_data->prog_data->args->oem_options_count);

  return (_wistron_get_telnet_ssh_redirect_function (state_data, IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_SOL_SSH_REDIRECT_FUNCTION_SELECTION));
}

int
ipmi_oem_wistron_set_ssh_redirect_function (ipmi_oem_state_data_t *state_data)
{
  assert (state_data);
  assert (state_data->prog_data->args->oem_options_count == 1);

  return (_wistron_set_telnet_ssh_redirect_function (state_data, IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_SOL_SSH_REDIRECT_FUNCTION_SELECTION));
}

