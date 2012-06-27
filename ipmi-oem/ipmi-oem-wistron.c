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
