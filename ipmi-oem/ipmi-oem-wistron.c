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

  return (ipmi_oem_thirdparty_get_web_server_config_v1 (state_data));
}

int
ipmi_oem_wistron_set_web_server_config (ipmi_oem_state_data_t *state_data)
{
  assert (state_data);

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

