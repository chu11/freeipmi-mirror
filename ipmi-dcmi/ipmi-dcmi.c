/*****************************************************************************\
 *  $Id: ipmi-dcmi.c,v 1.15 2010-07-27 18:01:43 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2009-2014 Lawrence Livermore National Security, LLC.
 *  Produced at Lawrence Livermore National Laboratory (cf, DISCLAIMER).
 *  Written by Albert Chu <chu11@llnl.gov>
 *  LLNL-CODE-413270
 *
 *  This file is part of Ipmi-Dcmi, tools and libraries to support the
 *  data center manageability interface (DCMI).  For details, see
 *  http://www.llnl.gov/linux/.
 *
 *  Ipmi-Dcmi is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by the
 *  Free Software Foundation; either version 3 of the License, or (at your
 *  option) any later version.
 *
 *  Ipmi-Dcmi is distributed in the hope that it will be useful, but
 *  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 *  for more details.
 *
 *  You should have received a copy of the GNU General Public License along
 *  with Ipmi-Dcmi.  If not, see <http://www.gnu.org/licenses/>.
\*****************************************************************************/

#if HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
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
#include <errno.h>

#include <freeipmi/freeipmi.h>

#include "ipmi-dcmi.h"
#include "ipmi-dcmi-argp.h"

#include "freeipmi-portability.h"
#include "pstdout.h"
#include "tool-common.h"
#include "tool-cmdline-common.h"
#include "tool-hostrange-common.h"
#include "tool-util-common.h"

#define IPMI_DCMI_ROLLING_AVERAGE_TIME_PERIOD_BUFLEN 4096

#define IPMI_DCMI_MAX_RECORD_IDS_BUFLEN 1024

#define IPMI_DCMI_ERROR_BUFLEN          1024

#define IPMI_DCMI_TIME_BUFLEN           512

/* return 1 on output success, 0 on no output, -1 on error */
static int
_dcmi_specification_conformance (ipmi_dcmi_state_data_t *state_data, uint8_t *parameter_revision)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint8_t major, minor;
  uint64_t val;
  int rv = -1;

  assert (state_data);
  assert (parameter_revision);

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_dcmi_get_dcmi_capability_info_supported_dcmi_capabilities_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if (ipmi_cmd_dcmi_get_dcmi_capability_info_supported_dcmi_capabilities (state_data->ipmi_ctx,
                                                                          obj_cmd_rs) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_cmd_dcmi_get_dcmi_capability_info_supported_dcmi_capabilities: %s\n",
                       ipmi_ctx_errormsg (state_data->ipmi_ctx));
      goto cleanup;
    }

  if (FIID_OBJ_GET (obj_cmd_rs,
                    "dcmi_specification_conformance.major_version",
                    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'dcmi_specification_conformance.major_version': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  major = val;

  if (FIID_OBJ_GET (obj_cmd_rs,
                    "dcmi_specification_conformance.minor_version",
                    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'dcmi_specification_conformance.minor_version': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  minor = val;

  /* XXX: achu: The spec does not say how these version numbers are
   * formmatted.  decimal?  BCD?  On the one hand, I think to be
   * consistent to the "IPMI Version" of a Get Device ID call, it
   * should be BCD.  But, these are 8 bit fields instead of 4 bit
   * fields (e.g. would I output "01.00" instead of "1.0"?).  So I'm
   * going to assume decimal for now.
   */
  pstdout_printf (state_data->pstate,
                  "DCMI Specification Conformance                     : %u.%u\n",
                  major,
                  minor);

  if (FIID_OBJ_GET (obj_cmd_rs,
                    "parameter_revision",
                    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'parameter_revision': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  (*parameter_revision) = val;

  rv = 1;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

/* return 1 on output success, 0 on no output, -1 on error */
static int
_supported_dcmi_capabilities (ipmi_dcmi_state_data_t *state_data)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint8_t parameter_revision;
  uint64_t val;
  int rv = -1;

  assert (state_data);

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_dcmi_get_dcmi_capability_info_supported_dcmi_capabilities_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if (ipmi_cmd_dcmi_get_dcmi_capability_info_supported_dcmi_capabilities (state_data->ipmi_ctx,
                                                                          obj_cmd_rs) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_cmd_dcmi_get_dcmi_capability_info_supported_dcmi_capabilities: %s\n",
                       ipmi_ctx_errormsg (state_data->ipmi_ctx));
      goto cleanup;
    }
  
  if (FIID_OBJ_GET (obj_cmd_rs,
                    "parameter_revision",
                    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'mandatory_platform_capabilities.identification_support': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  parameter_revision = val;

  /* See errata 1.0 */
  if (!(parameter_revision >= 0x02))
    {
      if (FIID_OBJ_GET (obj_cmd_rs,
                        "mandatory_platform_capabilities.identification_support",
                        &val) < 0)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "fiid_obj_get: 'mandatory_platform_capabilities.identification_support': %s\n",
                           fiid_obj_errormsg (obj_cmd_rs));
          goto cleanup;
        }
      
      pstdout_printf (state_data->pstate,
                      "Identification Support                             : %s\n",
                      val ? "Compliant with DCMI Specification" : "Not Compliant with DCMI Specification");
      
      if (FIID_OBJ_GET (obj_cmd_rs,
                        "mandatory_platform_capabilities.sel_logging",
                        &val) < 0)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "fiid_obj_get: 'mandatory_platform_capabilities.sel_logging': %s\n",
                           fiid_obj_errormsg (obj_cmd_rs));
          goto cleanup;
        }
      
      pstdout_printf (state_data->pstate,
                      "SEL logging                                        : %s\n",
                      val ? "Compliant with DCMI Specification" : "Not Compliant with DCMI Specification");
      
      if (FIID_OBJ_GET (obj_cmd_rs,
                        "mandatory_platform_capabilities.chassis_power",
                        &val) < 0)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "fiid_obj_get: 'mandatory_platform_capabilities.chassis_power': %s\n",
                           fiid_obj_errormsg (obj_cmd_rs));
          goto cleanup;
        }
      
      pstdout_printf (state_data->pstate,
                      "Chassis Power                                      : %s\n",
                      val ? "Compliant with DCMI Specification" : "Not Compliant with DCMI Specification");
      
      if (FIID_OBJ_GET (obj_cmd_rs,
                        "mandatory_platform_capabilities.temperature_monitor",
                        &val) < 0)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "fiid_obj_get: 'mandatory_platform_capabilities.temperature_monitor': %s\n",
                           fiid_obj_errormsg (obj_cmd_rs));
          goto cleanup;
        }
      
      pstdout_printf (state_data->pstate,
                      "Temperature Monitor                                : %s\n",
                      val ? "Compliant with DCMI Specification" : "Not Compliant with DCMI Specification");
    }

  if (FIID_OBJ_GET (obj_cmd_rs,
                    "optional_platform_capabilities.power_management_monitoring_support",
                    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'optional_platform_capabilities.power_management_monitoring_support': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }

  pstdout_printf (state_data->pstate,
                  "Power Management / Monitoring Support              : %s\n",
                  val ? "Available" : "Not present");

  if (FIID_OBJ_GET (obj_cmd_rs,
                    "manageability_access_capabilities.in_band_system_interface_channel_available",
                    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'manageability_access_capabilities.in_band_system_interface_channel_available': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }

  pstdout_printf (state_data->pstate,
                  "In-band System Interface Channel                   : %s\n",
                  val ? "Available" : "Not present");

  if (FIID_OBJ_GET (obj_cmd_rs,
                    "manageability_access_capabilities.serial_tmode_available",
                    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'manageability_access_capabilities.serial_tmode_available': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }

  pstdout_printf (state_data->pstate,
                  "Serial TMODE                                       : %s\n",
                  val ? "Available" : "Not present");

  if (FIID_OBJ_GET (obj_cmd_rs,
                    "manageability_access_capabilities.out_of_band_secondary_lan_channel_available",
                    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'manageability_access_capabilities.out_of_band_secondary_lan_channel_available': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }

  pstdout_printf (state_data->pstate,
                  "Out-Of-Band Secondary LAN Channel                  : %s\n",
                  val ? "Available" : "Not present");

  /* See errata 1.0 */
  if (!(parameter_revision >= 0x02))
    {
      if (FIID_OBJ_GET (obj_cmd_rs,
                        "manageability_access_capabilities.out_of_band_primary_lan_channel_available",
                        &val) < 0)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "fiid_obj_get: 'manageability_access_capabilities.out_of_band_primary_lan_channel_available': %s\n",
                           fiid_obj_errormsg (obj_cmd_rs));
          goto cleanup;
        }
      
      pstdout_printf (state_data->pstate,
                      "Out-Of-Band Primary LAN Channel                    : %s\n",
                      val ? "Available" : "Not present");
      
      if (FIID_OBJ_GET (obj_cmd_rs,
                        "manageability_access_capabilities.sol_supported",
                        &val) < 0)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "fiid_obj_get: 'manageability_access_capabilities.sol_supported': %s\n",
                           fiid_obj_errormsg (obj_cmd_rs));
          goto cleanup;
        }
      
      /* SOL Supported - removed "supported" */
      pstdout_printf (state_data->pstate,
                      "SOL                                                : %s\n",
                      val ? "Available" : "Not present");
      
      if (FIID_OBJ_GET (obj_cmd_rs,
                        "manageability_access_capabilities.vlan_capable",
                        &val) < 0)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "fiid_obj_get: 'manageability_access_capabilities.vlan_capable': %s\n",
                           fiid_obj_errormsg (obj_cmd_rs));
          goto cleanup;
        }

      /* VLAN Capable - removed "capable" */
      pstdout_printf (state_data->pstate,
                      "VLAN                                               : %s\n",
                      val ? "Available" : "Not present");
    }

  rv = 1;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

/* return 1 on output success, 0 on no output, -1 on error */
static int
_mandatory_platform_attributes (ipmi_dcmi_state_data_t *state_data)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint8_t parameter_revision;
  uint16_t number_of_sel_entries;
  uint64_t val;
  int rv = -1;

  assert (state_data);

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_dcmi_get_dcmi_capability_info_mandatory_platform_attributes_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if (ipmi_cmd_dcmi_get_dcmi_capability_info_mandatory_platform_attributes (state_data->ipmi_ctx,
                                                                            obj_cmd_rs) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_cmd_dcmi_get_dcmi_capability_info_mandatory_platform_attributes: %s\n",
                       ipmi_ctx_errormsg (state_data->ipmi_ctx));
      goto cleanup;
    }

  if (FIID_OBJ_GET (obj_cmd_rs,
                    "parameter_revision",
                    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'mandatory_platform_capabilities.identification_support': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  parameter_revision = val;

  if (FIID_OBJ_GET (obj_cmd_rs,
                    "sel_attributes.number_of_sel_entries",
                    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'sel_attributes.number_of_sel_entries': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  number_of_sel_entries = val;

  pstdout_printf (state_data->pstate,
                  "Number of SEL entries                              : %u\n",
                  number_of_sel_entries);

  /* In DCMI v1.1 */
  if (parameter_revision >= 0x02)
    {
      if (FIID_OBJ_GET (obj_cmd_rs,
                        "sel_attributes.record_level_sel_flush_upon_rollover",
                        &val) < 0)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "fiid_obj_get: 'sel_attributes.record_level_sel_flush_upon_rollover': %s\n",
                           fiid_obj_errormsg (obj_cmd_rs));
          goto cleanup;
        }
      
      pstdout_printf (state_data->pstate,
                      "Record Level SEL Flush upon Rollover               : %s\n",
                      val ? "Available" : "Not present");

      if (FIID_OBJ_GET (obj_cmd_rs,
                        "sel_attributes.entire_sel_flush_upon_rollover",
                        &val) < 0)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "fiid_obj_get: 'sel_attributes.entire_sel_flush_upon_rollover': %s\n",
                           fiid_obj_errormsg (obj_cmd_rs));
          goto cleanup;
        }
      
      pstdout_printf (state_data->pstate,
                      "Entire SEL Flush upon Rollover                     : %s\n",
                      val ? "Available" : "Not present");
    }
  
  if (FIID_OBJ_GET (obj_cmd_rs,
                    "sel_attributes.sel_automatic_rollover_enabled",
                    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'sel_attributes.sel_automatic_rollover_enabled': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }

  pstdout_printf (state_data->pstate,
                  "SEL automatic rollover                             : %s\n",
                  val ? "Available" : "Not present");

  /* See errata 1.0 */
  if (!(parameter_revision >= 0x02))
    {
      if (FIID_OBJ_GET (obj_cmd_rs,
                        "identification_attributes.guid_support",
                        &val) < 0)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "fiid_obj_get: 'identification_attributes.guid_support': %s\n",
                           fiid_obj_errormsg (obj_cmd_rs));
          goto cleanup;
        }
      
      pstdout_printf (state_data->pstate,
                      "GUID                                               : %s\n",
                      val ? "Available" : "Not present");
    }

  if (FIID_OBJ_GET (obj_cmd_rs,
                    "identification_attributes.dhcp_host_name_support",
                    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'identification_attributes.dhcp_host_name_support': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }

  pstdout_printf (state_data->pstate,
                  "DHCP Host Name                                     : %s\n",
                  val ? "Available" : "Not present");

  if (FIID_OBJ_GET (obj_cmd_rs,
                    "identification_attributes.asset_tag_support",
                    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'identification_attributes.asset_tag_support': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }

  pstdout_printf (state_data->pstate,
                  "Asset Tag                                          : %s\n",
                  val ? "Available" : "Not present");

  /* See errata 1.0 */
  if (!(parameter_revision >= 0x02))
    {
      if (FIID_OBJ_GET (obj_cmd_rs,
                    "temperature_monitoring.inlet_temperature",
                        &val) < 0)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "fiid_obj_get: 'temperature_monitoring.inlet_temperature': %s\n",
                           fiid_obj_errormsg (obj_cmd_rs));
          goto cleanup;
        }
      
      pstdout_printf (state_data->pstate,
                      "Inlet temperature                                  : %s\n",
                      val ? "At least 1 present" : "Not present");
      
      if (FIID_OBJ_GET (obj_cmd_rs,
                        "temperature_monitoring.processors_temperature",
                        &val) < 0)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "fiid_obj_get: 'temperature_monitoring.processors_temperature': %s\n",
                           fiid_obj_errormsg (obj_cmd_rs));
          goto cleanup;
        }
      
      pstdout_printf (state_data->pstate,
                      "Processors temperature                             : %s\n",
                      val ? "At least 1 present" : "Not present");
      
      if (FIID_OBJ_GET (obj_cmd_rs,
                        "temperature_monitoring.baseboard_temperature",
                        &val) < 0)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "fiid_obj_get: 'temperature_monitoring.baseboard_temperature': %s\n",
                           fiid_obj_errormsg (obj_cmd_rs));
          goto cleanup;
        }
      
      pstdout_printf (state_data->pstate,
                      "Baseboard temperature                              : %s\n",
                      val ? "At least 1 present" : "Not present");
    }

  /* In DCMI v1.1 */
  if (parameter_revision >= 0x02)
    {
      int flag;

      if ((flag = fiid_obj_get (obj_cmd_rs,
				"temperature_monitoring.sampling_period",
				&val)) < 0)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "fiid_obj_get: 'temperature_monitoring.sampling_period': %s\n",
                           fiid_obj_errormsg (obj_cmd_rs));
          goto cleanup;
        }

      if (flag)
        {
          pstdout_printf (state_data->pstate,
                      "Sampling frequency for Temperature Monitoring      : Every %u Second(s)\n",
                      val);
        }
    }

  rv = 1;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

/* return 1 on output success, 0 on no output, -1 on error */
static int
_optional_platform_attributes (ipmi_dcmi_state_data_t *state_data)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint8_t slave_address;
  uint8_t device_revision;
  uint8_t channel_number;
  uint64_t val;
  int rv = -1;

  assert (state_data);

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_dcmi_get_dcmi_capability_info_optional_platform_attributes_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if (ipmi_cmd_dcmi_get_dcmi_capability_info_optional_platform_attributes (state_data->ipmi_ctx,
                                                                           obj_cmd_rs) < 0)
    {
      /* IPMI Workaround?
       *
       * Technically, I believe this should be mandatory, as this is
       * not listed as optional in the DCMI spec (enhanced system
       * power statistics attributes is the only one that is
       * optional).  However, this parameter specifically lists
       * attributes for optional parts of DCMI.
       *
       * This has been seen as non-implemented on atleast two
       * motherboards (one undocumented motherboard and also the Intel
       * S2600JF/Appro 512X).
       */
      if (ipmi_ctx_errnum (state_data->ipmi_ctx) == IPMI_ERR_BAD_COMPLETION_CODE
          && ((ipmi_check_completion_code (obj_cmd_rs,
					   IPMI_COMP_CODE_REQUEST_PARAMETER_NOT_SUPPORTED) == 1)
	      || (ipmi_check_completion_code (obj_cmd_rs,
					      IPMI_COMP_CODE_REQUESTED_SENSOR_DATA_OR_RECORD_NOT_PRESENT) == 1)))
        {
          rv = 0;
          goto cleanup;
        }

      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_cmd_dcmi_get_dcmi_capability_info_optional_platform_attributes: %s\n",
                       ipmi_ctx_errormsg (state_data->ipmi_ctx));
      goto cleanup;
    }

  if (FIID_OBJ_GET (obj_cmd_rs,
                    "power_management_device_slave_address.slave_address",
                    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'power_management_device_slave_address.slave_address': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  slave_address = val;

  pstdout_printf (state_data->pstate,
                  "Power Management Device Slave Address              : %02Xh\n",
                  slave_address);

  if (FIID_OBJ_GET (obj_cmd_rs,
                    "power_management_controller_channel_number.device_revision",
                    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'power_management_controller_channel_number.device_revision': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  device_revision = val;

  /* revision is decimal?  Lets assume so */
  pstdout_printf (state_data->pstate,
                  "Power Management Controller Device Revision        : %u\n",
                  device_revision);

  if (FIID_OBJ_GET (obj_cmd_rs,
                    "power_management_controller_channel_number.channel_number",
                    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'power_management_controller_channel_number.channel_number': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  channel_number = val;

  pstdout_printf (state_data->pstate,
                  "Power Management Controller Channel Number         : %u\n",
                  channel_number);

  rv = 1;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

/* return 1 on output success, 0 on no output, -1 on error */
static int
_manageability_access_attributes (ipmi_dcmi_state_data_t *state_data)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint8_t channel_number;
  uint64_t val;
  int rv = -1;

  assert (state_data);

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_dcmi_get_dcmi_capability_info_manageability_access_attributes_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if (ipmi_cmd_dcmi_get_dcmi_capability_info_manageability_access_attributes (state_data->ipmi_ctx,
                                                                              obj_cmd_rs) < 0)
    {     
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_cmd_dcmi_get_dcmi_capability_info_manageability_access_attributes: %s\n",
                       ipmi_ctx_errormsg (state_data->ipmi_ctx));
      goto cleanup;
    }

  if (FIID_OBJ_GET (obj_cmd_rs,
                    "mandatory_primary_lan_out_of_band_support_channel_number",
                    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'mandatory_primary_lan_out_of_band_support_channel_number': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  channel_number = val;

  if (channel_number == IPMI_DCMI_CHANNEL_NOT_SUPPORTED)
    pstdout_printf (state_data->pstate,
                    "Primary LAN Out-of-band Channel Number             : Not supported\n");
  else
    pstdout_printf (state_data->pstate,
                    "Primary LAN Out-of-band Channel Number             : %u\n",
                    channel_number);

  if (FIID_OBJ_GET (obj_cmd_rs,
                    "optional_secondary_lan_out_of_band_support_channel_number",
                    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'optional_secondary_lan_out_of_band_support_channel_number': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  channel_number = val;

  if (channel_number == IPMI_DCMI_CHANNEL_NOT_SUPPORTED)
    pstdout_printf (state_data->pstate,
                    "Secondary LAN Out-of-band Channel Number           : Not supported\n");
  else
    pstdout_printf (state_data->pstate,
                    "Secondary LAN Out-of-band Channel Number           : %u\n",
                    channel_number);

  if (FIID_OBJ_GET (obj_cmd_rs,
                    "optional_serial_out_of_band_tmode_capability_channel_number",
                    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'optional_serial_out_of_band_tmode_capability_channel_number': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  channel_number = val;

  if (channel_number == IPMI_DCMI_CHANNEL_NOT_SUPPORTED)
    pstdout_printf (state_data->pstate,
                    "Serial Out-of-band TMODE Capability Channel Number : Not supported\n");
  else
    pstdout_printf (state_data->pstate,
                    "Serial Out-of-band TMODE Capability Channel Number : %u\n",
                    channel_number);

  rv = 1;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

/* return 1 on output success, 0 on no output, -1 on error */
static int
_get_enhanced_system_power_statistics_attributes (ipmi_dcmi_state_data_t *state_data,
                                                  uint8_t *number_of_supported_rolling_average_time_periods,
                                                  uint8_t *rolling_average_time_periods,
                                                  unsigned int rolling_average_time_periods_buflen)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  int len;
  int rv = -1;

  assert (state_data);
  assert (number_of_supported_rolling_average_time_periods);
  assert (rolling_average_time_periods);
  assert (rolling_average_time_periods_buflen);

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_dcmi_get_dcmi_capability_info_enhanced_system_power_statistics_attributes_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if (ipmi_cmd_dcmi_get_dcmi_capability_info_enhanced_system_power_statistics_attributes (state_data->ipmi_ctx,
                                                                                          obj_cmd_rs) < 0)
    {
      /* this optional parameter is not supported */
      if (ipmi_ctx_errnum (state_data->ipmi_ctx) == IPMI_ERR_BAD_COMPLETION_CODE
          && ((ipmi_check_completion_code (obj_cmd_rs,
					   IPMI_COMP_CODE_REQUEST_PARAMETER_NOT_SUPPORTED) == 1)
	      || (ipmi_check_completion_code (obj_cmd_rs,
					      IPMI_COMP_CODE_REQUESTED_SENSOR_DATA_OR_RECORD_NOT_PRESENT) == 1)))
        {
          rv = 0;
          goto cleanup;
        }

      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_cmd_dcmi_get_dcmi_capability_info_enhanced_system_power_statistics_attributes: %s\n",
                       ipmi_ctx_errormsg (state_data->ipmi_ctx));
      goto cleanup;
    }

  if (FIID_OBJ_GET (obj_cmd_rs,
                    "number_of_supported_rolling_average_time_periods",
                    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'number_of_supported_rolling_average_time_periods': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  (*number_of_supported_rolling_average_time_periods) = val;

  if ((len = fiid_obj_get_data (obj_cmd_rs,
                                "rolling_average_time_periods",
                                rolling_average_time_periods,
                                rolling_average_time_periods_buflen)) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get_data: 'rolling_average_time_periods': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  
  if ((*number_of_supported_rolling_average_time_periods) != len)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "invalid number of supported rolling average time periods reported: %u vs. %u\n",
                       (*number_of_supported_rolling_average_time_periods),
                       len);
      goto cleanup;
    }
  
  if ((len = fiid_template_len_bytes (tmpl_dcmi_rolling_average_time_period)) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_template_len_bytes: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  /* an "assertion" */
  if (len != 1)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "rolling average time period length invalid: %u\n",
                       len);
      goto cleanup;
    }

  rv = 1;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static int
_get_time_duration_info (ipmi_dcmi_state_data_t *state_data,
                         uint8_t rolling_average_time_period,
                         uint8_t *time_duration,
                         char **time_duration_units_str)
{
  fiid_obj_t obj_rolling_average_time_period = NULL;
  uint8_t time_duration_units;
  uint64_t val;
  int rv = -1;

  assert (state_data);
  assert (time_duration);
  assert (time_duration_units_str);

  if (!(obj_rolling_average_time_period = fiid_obj_create (tmpl_dcmi_rolling_average_time_period)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if (fiid_obj_set_all (obj_rolling_average_time_period,
                        &rolling_average_time_period,
                        1) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_set_all: %s\n",
                       fiid_obj_errormsg (obj_rolling_average_time_period));
      goto cleanup;
    }

  if (FIID_OBJ_GET (obj_rolling_average_time_period,
                    "time_duration",
                    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'time_duration': %s\n",
                       fiid_obj_errormsg (obj_rolling_average_time_period));
      goto cleanup;
    }
  (*time_duration) = val;
  
  if (FIID_OBJ_GET (obj_rolling_average_time_period,
                    "time_duration_units",
                    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'time_duration_units': %s\n",
                       fiid_obj_errormsg (obj_rolling_average_time_period));
      goto cleanup;
    }
  time_duration_units = val;
  
  if (time_duration_units == IPMI_DCMI_TIME_DURATION_UNITS_SECONDS)
    (*time_duration_units_str) = "Seconds";
  else if (time_duration_units == IPMI_DCMI_TIME_DURATION_UNITS_MINUTES)
    (*time_duration_units_str) = "Minutes";
  else if (time_duration_units == IPMI_DCMI_TIME_DURATION_UNITS_HOURS)
    (*time_duration_units_str) = "Hours";
  else
    (*time_duration_units_str) = "Days";
  
  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_rolling_average_time_period);
  return (rv);
}

/* return 1 on output success, 0 on no output, -1 on error */
static int
_enhanced_system_power_statistics_attributes (ipmi_dcmi_state_data_t *state_data)
{
  uint8_t rolling_average_time_periods[IPMI_DCMI_ROLLING_AVERAGE_TIME_PERIOD_BUFLEN];
  uint8_t number_of_supported_rolling_average_time_periods;
  int i;

  assert (state_data);

  if (_get_enhanced_system_power_statistics_attributes (state_data,
                                                        &number_of_supported_rolling_average_time_periods,
                                                        rolling_average_time_periods,
                                                        IPMI_DCMI_ROLLING_AVERAGE_TIME_PERIOD_BUFLEN) < 0)
    return (-1);

  for (i = 0; i < number_of_supported_rolling_average_time_periods; i++)
    {
      uint8_t time_duration;
      char *time_duration_units_str = NULL;

      if (_get_time_duration_info (state_data,
                                   rolling_average_time_periods[i],
                                   &time_duration,
                                   &time_duration_units_str) < 0)
        return (-1);

      pstdout_printf (state_data->pstate,
                      "Available Rolling Average Time Period              : %u %s\n",
                      time_duration,
                      time_duration_units_str);
    }

  return (1);
}

static int
get_dcmi_capability_info (ipmi_dcmi_state_data_t *state_data)
{
  uint8_t parameter_revision;
  int ret;

  assert (state_data);

  if ((ret = _dcmi_specification_conformance (state_data, &parameter_revision)) < 0)
    return (-1);
  
  if (ret)
    pstdout_printf (state_data->pstate, "\n");

  if ((ret = _supported_dcmi_capabilities (state_data)) < 0)
    return (-1);

  if (ret)
    pstdout_printf (state_data->pstate, "\n");

  if ((ret = _mandatory_platform_attributes (state_data)) < 0)
    return (-1);

  if (ret)
    pstdout_printf (state_data->pstate, "\n");

  if ((ret = _optional_platform_attributes (state_data)) < 0)
    return (-1);

  if (ret)
    pstdout_printf (state_data->pstate, "\n");

  if ((ret = _manageability_access_attributes (state_data)) < 0)
    return (-1);

  if (parameter_revision >= 0x02)
    {
      if (ret)
        pstdout_printf (state_data->pstate, "\n");
      
      if ((ret = _enhanced_system_power_statistics_attributes (state_data)) < 0)
        return (-1);
    }

  return (0);
}

static int
get_asset_tag (ipmi_dcmi_state_data_t *state_data)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint8_t asset_tag_data[IPMI_DCMI_MAX_ASSET_TAG_LENGTH + 1];
  int data_len;
  unsigned int offset = 0;
  uint8_t total_asset_tag_length = 0;
  uint8_t bytes_to_read = IPMI_DCMI_ASSET_TAG_NUMBER_OF_BYTES_TO_READ_MAX;
  int rv = -1;

  assert (state_data);

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_dcmi_get_asset_tag_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  memset (asset_tag_data, '\0', IPMI_DCMI_MAX_ASSET_TAG_LENGTH + 1);

  while (1)
    {
      uint64_t val;

      if (!offset
          || ((total_asset_tag_length - offset) >= IPMI_DCMI_ASSET_TAG_NUMBER_OF_BYTES_TO_READ_MAX))
        bytes_to_read = IPMI_DCMI_ASSET_TAG_NUMBER_OF_BYTES_TO_READ_MAX;
      else 
        bytes_to_read = total_asset_tag_length - offset;
      
      if (ipmi_cmd_dcmi_get_asset_tag (state_data->ipmi_ctx,
                                       offset,
                                       bytes_to_read,
                                       obj_cmd_rs) < 0)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "ipmi_cmd_dcmi_get_asset_tag: %s\n",
                           ipmi_ctx_errormsg (state_data->ipmi_ctx));
          goto cleanup;
        }

      if (FIID_OBJ_GET (obj_cmd_rs,
                        "total_asset_tag_length",
                        &val) < 0)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "fiid_obj_get: 'total_asset_tag_length': %s\n",
                           fiid_obj_errormsg (obj_cmd_rs));
          goto cleanup;
        }
      total_asset_tag_length = val;

      if (!total_asset_tag_length)
        break;

      if ((data_len = fiid_obj_get_data (obj_cmd_rs,
                                         "data",
                                         asset_tag_data + offset,
                                         IPMI_DCMI_MAX_ASSET_TAG_LENGTH - offset)) < 0)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "fiid_obj_get_data: 'data': %s\n",
                           fiid_obj_errormsg (obj_cmd_rs));
          goto cleanup;
        }
      offset += data_len;

      if (offset >= total_asset_tag_length)
        break;
    }

  if (total_asset_tag_length)
    {
      /* Handle special case UTF-8 encoding w/ BOM prefix */
      if (asset_tag_data[0] == IPMI_DCMI_ASSET_TAG_UTF8_BOM_BYTE0
          && asset_tag_data[1] == IPMI_DCMI_ASSET_TAG_UTF8_BOM_BYTE1
          && asset_tag_data[2] == IPMI_DCMI_ASSET_TAG_UTF8_BOM_BYTE2)
	/* achu: I think this is right for UTF-8 in libc and is
	 * portable, but I would bet some systems won't like this.
	 */
        pstdout_printf (state_data->pstate,
                        "%ls\n",
                        &asset_tag_data[3]);
      else
        pstdout_printf (state_data->pstate,
                        "%s\n",
                        asset_tag_data);
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static int
set_asset_tag (ipmi_dcmi_state_data_t *state_data)
{
  fiid_obj_t obj_cmd_rs = NULL;
  unsigned int offset = 0;
  char data_buf[IPMI_DCMI_MAX_ASSET_TAG_LENGTH + 1];
  unsigned int data_len;
  uint8_t bytes_to_write = IPMI_DCMI_ASSET_TAG_NUMBER_OF_BYTES_TO_WRITE_MAX;
  int rv = -1;

  assert (state_data);

  /* achu:
   *
   * DCMI v1.1 spec is unclear if the entire buffer needs to be
   * written or just the amount you desire.
   *
   * DCMI v1.5 spec strongly suggests you don't write the entire
   * buffer due to the results of the "total_asset_tag_length_written"
   * field.
   *
   * "Total Asset Tag Length. This is the length in bytes of the stored
   * Asset Tag after the Set operation has completed. The Asset Tag
   * length shall be set to the sum of the offset to write plus bytes
   * to write. For example, if offset to write is 32 and bytes to
   * write is 4, the Total Asset Tag Length returned will be 36."
   */

  data_len = strlen (state_data->prog_data->args->set_asset_tag_arg);

  memset (data_buf, '\0', IPMI_DCMI_MAX_ASSET_TAG_LENGTH + 1);

  memcpy (data_buf,
          state_data->prog_data->args->set_asset_tag_arg,
          strlen (state_data->prog_data->args->set_asset_tag_arg));

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_dcmi_set_asset_tag_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  while (1)
    {
      uint64_t val;

      if ((data_len - offset) >= IPMI_DCMI_ASSET_TAG_NUMBER_OF_BYTES_TO_WRITE_MAX)
        bytes_to_write = IPMI_DCMI_ASSET_TAG_NUMBER_OF_BYTES_TO_WRITE_MAX;
      else 
        bytes_to_write = data_len - offset;
      
      if (ipmi_cmd_dcmi_set_asset_tag (state_data->ipmi_ctx,
                                       offset,
                                       bytes_to_write,
                                       data_buf + offset,
                                       data_len,
                                       obj_cmd_rs) < 0)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "ipmi_cmd_dcmi_set_asset_tag: %s\n",
                           ipmi_ctx_errormsg (state_data->ipmi_ctx));
          goto cleanup;
        }

      if (FIID_OBJ_GET (obj_cmd_rs,
                        "total_asset_tag_length_written",
                        &val) < 0)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "fiid_obj_get: 'total_asset_tag_length': %s\n",
                           fiid_obj_errormsg (obj_cmd_rs));
          goto cleanup;
        }

      /* DCMI 1.1 spec is unclear on "total_length_written", is it the
       * number of bytes just written or total bytes written so far?
       * 
       * DCMI 1.5 spec makes it clear that this is the number of bytes
       * written in total.  To defend against vendor mistakes, we
       * handle both situations.
       */
      if (val > bytes_to_write)
        offset += bytes_to_write;
      else
        offset += val;

      if (offset >= data_len)
        break;
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static int
get_management_controller_identifier_string (ipmi_dcmi_state_data_t *state_data)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint8_t management_controller_identifier_string_data[IPMI_DCMI_MAX_MANAGEMENT_CONTROLLER_IDENTIFIER_STRING_LENGTH];
  int data_len;
  unsigned int offset = 0;
  uint8_t total_length = 0;
  uint8_t bytes_to_read = IPMI_DCMI_MANAGEMENT_CONTROLLER_IDENTIFIER_STRING_NUMBER_OF_BYTES_TO_READ_MAX;
  int rv = -1;

  assert (state_data);

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_dcmi_get_management_controller_identifier_string_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  memset (management_controller_identifier_string_data, '\0', IPMI_DCMI_MAX_MANAGEMENT_CONTROLLER_IDENTIFIER_STRING_LENGTH);

  while (1)
    {
      uint64_t val;

      if (!offset
          || ((total_length - offset) >= IPMI_DCMI_MANAGEMENT_CONTROLLER_IDENTIFIER_STRING_NUMBER_OF_BYTES_TO_READ_MAX))
        bytes_to_read = IPMI_DCMI_MANAGEMENT_CONTROLLER_IDENTIFIER_STRING_NUMBER_OF_BYTES_TO_READ_MAX;
      else 
        bytes_to_read = total_length - offset;
      
      if (ipmi_cmd_dcmi_get_management_controller_identifier_string (state_data->ipmi_ctx,
                                       offset,
                                       bytes_to_read,
                                       obj_cmd_rs) < 0)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "ipmi_cmd_dcmi_get_management_controller_identifier_string: %s\n",
                           ipmi_ctx_errormsg (state_data->ipmi_ctx));
          goto cleanup;
        }

      if (FIID_OBJ_GET (obj_cmd_rs,
                        "total_length",
                        &val) < 0)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "fiid_obj_get: 'total_length': %s\n",
                           fiid_obj_errormsg (obj_cmd_rs));
          goto cleanup;
        }
      total_length = val;

      if (!total_length)
        break;

      if ((data_len = fiid_obj_get_data (obj_cmd_rs,
                                         "data",
                                         management_controller_identifier_string_data + offset,
                                         IPMI_DCMI_MAX_MANAGEMENT_CONTROLLER_IDENTIFIER_STRING_LENGTH - offset)) < 0)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "fiid_obj_get_data: 'data': %s\n",
                           fiid_obj_errormsg (obj_cmd_rs));
          goto cleanup;
        }
      offset += data_len;

      if (offset >= total_length)
        break;
    }

  if (total_length)
    pstdout_printf (state_data->pstate,
                    "%s\n",
                    management_controller_identifier_string_data);

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static int
set_management_controller_identifier_string (ipmi_dcmi_state_data_t *state_data)
{
  fiid_obj_t obj_cmd_rs = NULL;
  unsigned int offset = 0;
  char data_buf[IPMI_DCMI_MAX_MANAGEMENT_CONTROLLER_IDENTIFIER_STRING_LENGTH];
  unsigned int data_len;
  uint8_t bytes_to_write = IPMI_DCMI_MANAGEMENT_CONTROLLER_IDENTIFIER_STRING_NUMBER_OF_BYTES_TO_WRITE_MAX;
  int rv = -1;

  assert (state_data);

  /* achu:
   *
   * According to DCMI v1.5 draft
   * 
   * "The presence of the null terminator among bytes to shall be
   * considered as indicating the last transfer of the Management
   * Controller Identifier string"
   *
   * So I am assuming we don't need to write the entire buffer.  But
   * we must include the NUL byte at the end.
   */

  /* +1 for NUL char */
  data_len = strlen (state_data->prog_data->args->set_management_controller_identifier_string_arg) + 1;

  memset (data_buf, '\0', IPMI_DCMI_MAX_MANAGEMENT_CONTROLLER_IDENTIFIER_STRING_LENGTH);

  memcpy (data_buf,
          state_data->prog_data->args->set_management_controller_identifier_string_arg,
          strlen (state_data->prog_data->args->set_management_controller_identifier_string_arg));

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_dcmi_set_management_controller_identifier_string_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  while (1)
    {
      uint64_t val;

      if ((data_len - offset) >= IPMI_DCMI_MANAGEMENT_CONTROLLER_IDENTIFIER_STRING_NUMBER_OF_BYTES_TO_WRITE_MAX)
        bytes_to_write = IPMI_DCMI_MANAGEMENT_CONTROLLER_IDENTIFIER_STRING_NUMBER_OF_BYTES_TO_WRITE_MAX;
      else 
        bytes_to_write = data_len - offset;
      
      if (ipmi_cmd_dcmi_set_management_controller_identifier_string (state_data->ipmi_ctx,
                                                                     offset,
                                                                     bytes_to_write,
                                                                     data_buf + offset,
                                                                     data_len,
                                                                     obj_cmd_rs) < 0)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "ipmi_cmd_dcmi_set_management_controller_identifier_string: %s\n",
                           ipmi_ctx_errormsg (state_data->ipmi_ctx));
          goto cleanup;
        }

      if (FIID_OBJ_GET (obj_cmd_rs,
                        "total_length_written",
                        &val) < 0)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "fiid_obj_get: 'total_management_controller_identifier_string_length': %s\n",
                           fiid_obj_errormsg (obj_cmd_rs));
          goto cleanup;
        }


      /* DCMI 1.1 spec is unclear on "total_length_written", is it the
       * number of bytes just written or total bytes written so far?
       * 
       * DCMI 1.5 spec makes it clear that this is the number of bytes
       * written in total.  To defend against vendor mistakes, we
       * handle both situations.
       */
      if (val > bytes_to_write)
        offset += bytes_to_write;
      else
        offset += val;

      if (offset >= data_len)
        break;
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static char *
_entity_string (uint8_t entity_id)
{
  assert (IPMI_DCMI_ENTITY_ID_VALID (entity_id));

  if (entity_id == IPMI_DCMI_ENTITY_ID_INLET_TEMPERATURE)
    return IPMI_DCMI_ENTITY_ID_INLET_TEMPERATURE_STR;
  else if (entity_id == IPMI_DCMI_ENTITY_ID_CPU_TEMPERATURE)
    return IPMI_DCMI_ENTITY_ID_CPU_TEMPERATURE_STR;
  else
    return IPMI_DCMI_ENTITY_ID_BASEBOARD_TEMPERATURE_STR;
}

static int
_sensor_info_output (ipmi_dcmi_state_data_t *state_data,
                     uint8_t sensor_type,
                     uint8_t entity_id)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint8_t entity_instance_start = 0x01; /* starts at 1, not 0 */
  unsigned int total_entity_instances_parsed = 0;
  int rv = -1;

  assert (state_data);
  assert (IPMI_SENSOR_TYPE_VALID (sensor_type));
  assert (IPMI_DCMI_ENTITY_ID_VALID (entity_id));
  
  pstdout_printf (state_data->pstate,
                  "%s SDR Record IDs\n",
                  _entity_string (entity_id));

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_dcmi_get_dcmi_sensor_info_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  while (1)
    {
      uint64_t val;
      uint8_t total_number_of_available_instances;
      uint8_t number_of_record_ids_in_this_response;
      uint8_t sdr_record_ids[IPMI_DCMI_MAX_RECORD_IDS_BUFLEN];
      int sdr_record_ids_len;
      int i;

      fiid_obj_clear (obj_cmd_rs);

      if (ipmi_cmd_dcmi_get_dcmi_sensor_info (state_data->ipmi_ctx,
                                              sensor_type,
                                              entity_id,
                                              IPMI_DCMI_ENTITY_INSTANCE_ALL,
                                              entity_instance_start,
                                              obj_cmd_rs) < 0)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "ipmi_cmd_dcmi_get_dcmi_sensor_info: %s\n",
                           ipmi_ctx_errormsg (state_data->ipmi_ctx));
          goto cleanup;
        }

      if (FIID_OBJ_GET (obj_cmd_rs,
                        "total_number_of_available_instances",
                        &val) < 0)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "fiid_obj_get: 'total_number_of_available_instances': %s\n",
                           fiid_obj_errormsg (obj_cmd_rs));
          goto cleanup;
        }
      total_number_of_available_instances = val;

      if (!total_number_of_available_instances)
        break;

      if (FIID_OBJ_GET (obj_cmd_rs,
                        "number_of_record_ids_in_this_response",
                        &val) < 0)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "fiid_obj_get: 'number_of_record_ids_in_this_response': %s\n",
                           fiid_obj_errormsg (obj_cmd_rs));
          goto cleanup;
        }
      number_of_record_ids_in_this_response = val;

      if (!number_of_record_ids_in_this_response)
        break;

      if ((sdr_record_ids_len = fiid_obj_get_data (obj_cmd_rs,
                                                   "sdr_record_ids",
                                                   sdr_record_ids,
                                                   IPMI_DCMI_MAX_RECORD_IDS_BUFLEN)) < 0)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "fiid_obj_get_data: 'sdr_record_ids': %s\n",
                           fiid_obj_errormsg (obj_cmd_rs));
          goto cleanup;
        }
      
      if (sdr_record_ids_len % 2)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "invalid sdr_record_ids length returned: %u\n",
                           sdr_record_ids_len);
          goto cleanup;
        }

      if (number_of_record_ids_in_this_response > (sdr_record_ids_len / 2))
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "invalid sdr_record_ids returned: %u > %u\n",
                           number_of_record_ids_in_this_response,
                           (sdr_record_ids_len / 2));
          goto cleanup;
        }

      for (i = 0; i < (number_of_record_ids_in_this_response * 2); i += 2)
        {
          uint16_t record_id = 0;
          
          record_id |= sdr_record_ids[i];
          record_id |= (sdr_record_ids[i+1] << 8);
          
          pstdout_printf (state_data->pstate,
                          "%u\n",
                          record_id);
          total_entity_instances_parsed++;
        }
      
      /* achu: entity IDs are returned sequentially?  If not, I'm not
       * sure how this API can even work, you wouldn't know where to
       * start the next time around.  Hopefully this is a correct
       * assumption
       */
      /* HLiebig: Note: Intel simply increments the offset by 8 (max number of 
       * SDR Id's per response.
       * See dcmitool from www.intel.com/go/DCMI (a modified ipmitool)
       */
  
      entity_instance_start += number_of_record_ids_in_this_response;

      if (total_entity_instances_parsed >= total_number_of_available_instances)
        break;
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static int
get_dcmi_sensor_info (ipmi_dcmi_state_data_t *state_data)
{
  int rv = -1;

  assert (state_data);

  if (_sensor_info_output (state_data,
                           IPMI_SENSOR_TYPE_TEMPERATURE,
                           IPMI_DCMI_ENTITY_ID_INLET_TEMPERATURE) < 0)
    goto cleanup;

  pstdout_printf (state_data->pstate, "\n");

  if (_sensor_info_output (state_data,
                           IPMI_SENSOR_TYPE_TEMPERATURE,
                           IPMI_DCMI_ENTITY_ID_CPU_TEMPERATURE) < 0)
    goto cleanup;
  
  pstdout_printf (state_data->pstate, "\n");

  if (_sensor_info_output (state_data,
                           IPMI_SENSOR_TYPE_TEMPERATURE,
                           IPMI_DCMI_ENTITY_ID_BASEBOARD_TEMPERATURE) < 0)
    goto cleanup;

  rv = 0;
 cleanup:
  return (rv);
}

static int
_output_power_statistics (ipmi_dcmi_state_data_t *state_data,
                          uint8_t mode,
                          uint8_t mode_attributes)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint16_t current_power;
  uint16_t minimum_power_over_sampling_duration;
  uint16_t maximum_power_over_sampling_duration;
  uint16_t average_power_over_sampling_duration;
  uint32_t time_stamp;
  uint32_t statistics_reporting_time_period;
  uint8_t power_measurement;
  uint64_t val;
  char timestr[IPMI_DCMI_TIME_BUFLEN + 1];
  int rv = -1;

  assert (state_data);

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_dcmi_get_power_reading_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if (ipmi_cmd_dcmi_get_power_reading (state_data->ipmi_ctx,
                                       mode,
                                       mode_attributes,
                                       obj_cmd_rs) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_cmd_dcmi_get_power_reading: %s\n",
                       ipmi_ctx_errormsg (state_data->ipmi_ctx));
      goto cleanup;
    }

  if (FIID_OBJ_GET (obj_cmd_rs,
                    "current_power",
                    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'current_power': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  current_power = val;

  if (FIID_OBJ_GET (obj_cmd_rs,
                    "minimum_power_over_sampling_duration",
                    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'minimum_power_over_sampling_duration': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  minimum_power_over_sampling_duration = val;

  if (FIID_OBJ_GET (obj_cmd_rs,
                    "maximum_power_over_sampling_duration",
                    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'maximum_power_over_sampling_duration': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  maximum_power_over_sampling_duration = val;

  if (FIID_OBJ_GET (obj_cmd_rs,
                    "average_power_over_sampling_duration",
                    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'average_power_over_sampling_duration': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  average_power_over_sampling_duration = val;

  if (FIID_OBJ_GET (obj_cmd_rs,
                    "time_stamp",
                    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'time_stamp': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  time_stamp = val;

  if (FIID_OBJ_GET (obj_cmd_rs,
                    "statistics_reporting_time_period",
                    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'statistics_reporting_time_period': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  statistics_reporting_time_period = val;

  if (FIID_OBJ_GET (obj_cmd_rs,
                    "power_reading_state.power_measurement",
                    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'power_measurement': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  power_measurement = val;

  pstdout_printf (state_data->pstate,
                  "Current Power                        : %u Watts\n",
                  current_power);

  pstdout_printf (state_data->pstate,
                  "Minimum Power over sampling duration : %u watts\n",
                  minimum_power_over_sampling_duration);

  pstdout_printf (state_data->pstate,
                  "Maximum Power over sampling duration : %u watts\n",
                  maximum_power_over_sampling_duration);

  pstdout_printf (state_data->pstate,
                  "Average Power over sampling duration : %u watts\n",
                  average_power_over_sampling_duration);

  memset (timestr, '\0', IPMI_DCMI_TIME_BUFLEN + 1);

  if (ipmi_timestamp_string (time_stamp,
			     state_data->prog_data->args->common_args.utc_offset,
			     get_timestamp_flags (&(state_data->prog_data->args->common_args),
						  IPMI_TIMESTAMP_FLAG_DEFAULT), 
			     "%m/%d/%Y - %H:%M:%S",
			     timestr,
			     IPMI_DCMI_TIME_BUFLEN) < 0)
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "ipmi_timestamp_string: %s\n",
		       strerror (errno));
      goto cleanup;
    }

  pstdout_printf (state_data->pstate,
                  "Time Stamp                           : %s\n",
                  timestr);

  pstdout_printf (state_data->pstate,
                  "Statistics reporting time period     : %u milliseconds\n",
                  statistics_reporting_time_period);

  pstdout_printf (state_data->pstate,
                  "Power Measurement                    : %s\n",
                  (power_measurement) ? "Active" : "Not Available");

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static int
get_system_power_statistics (ipmi_dcmi_state_data_t *state_data)
{
  assert (state_data);

  if (_output_power_statistics (state_data,
                                IPMI_DCMI_POWER_READING_MODE_SYSTEM_POWER_STATISTICS,
                                0) < 0)
    return (-1);

  return (0);
}

static int
get_enhanced_system_power_statistics (ipmi_dcmi_state_data_t *state_data)
{
  uint8_t rolling_average_time_periods[IPMI_DCMI_ROLLING_AVERAGE_TIME_PERIOD_BUFLEN];
  uint8_t number_of_supported_rolling_average_time_periods = 0;
  int i;

  assert (state_data);

  if (_get_enhanced_system_power_statistics_attributes (state_data,
                                                        &number_of_supported_rolling_average_time_periods,
                                                        rolling_average_time_periods,
                                                        IPMI_DCMI_ROLLING_AVERAGE_TIME_PERIOD_BUFLEN) < 0)
    return (-1);


  for (i = 0; i < number_of_supported_rolling_average_time_periods; i++)
    {
      uint8_t time_duration;
      char *time_duration_units_str = NULL;

      if (_get_time_duration_info (state_data,
                                   rolling_average_time_periods[i],
                                   &time_duration,
                                   &time_duration_units_str) < 0)
        return (-1);

      pstdout_printf (state_data->pstate,
                      "Power Statistics for Rolling Average Time Period %u %s\n",
                      time_duration,
                      time_duration_units_str);
      
      pstdout_printf (state_data->pstate, "\n");
      
      if (_output_power_statistics (state_data,
                                    IPMI_DCMI_POWER_READING_MODE_ENHANCED_SYSTEM_POWER_STATISTICS,
                                    rolling_average_time_periods[i]) < 0)
        return (-1);
    }

  return (0);
}

static int
_get_power_limit (ipmi_dcmi_state_data_t *state_data,
                  uint8_t *exception_actions,
                  uint16_t *power_limit_requested,
                  uint32_t *correction_time_limit,
                  uint16_t *management_application_statistics_sampling_period,
                  uint8_t *comp_code,
                  char *errorbuf,
                  unsigned int errorbuflen)

{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;
  int no_set_power_limit_error_flag = 0;
  int rv = -1;

  assert (state_data);
  assert (exception_actions);
  assert (power_limit_requested);
  assert (correction_time_limit);
  assert (management_application_statistics_sampling_period);
  assert (errorbuf);
  assert (errorbuflen);

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_dcmi_get_power_limit_rs)))
    {
      snprintf (errorbuf,
                errorbuflen,
                "fiid_obj_create: %s",
                strerror (errno));
      goto cleanup;
    }

  /* IPMI Workaround/Interpretation
   *
   * The DCMI spec indicates a potential completion code for the "Get
   * Power Limit" command as "No Set Power Limit" (0x80).  FreeIPMI
   * originally interpreted this to mean the "Set Power Limit" command
   * was not available.  Atleast one vendor interpreted this to mean
   * "No Power Limit Set".  One can consider this an English
   * interpretation issue of 'No set POWER LIMIT' vs. 'No SET POWER
   * LIMIT' (i.e. is "set" a verb or part of a proper noun referencing
   * the DCMI command).  Confounding this issue is the fact that the
   * example implementation in Intel's DCMItool implements the former,
   * while the DCMI Conformance test suite implements the latter.  In
   * addition to this, with the latter interpretation, it need not be
   * an indication of an error, but rather a flag.  So the rest of the
   * packet can be completely full of legitimate data.
   *
   * So how do we handle this?
   *
   * If we hit "No Set Power Limit", try to read data.  If we can't
   * read data (b/c it's not set), fail out, but preserve the "No Set
   * Power Limit" error message.
   */

  if (ipmi_cmd_dcmi_get_power_limit (state_data->ipmi_ctx,
                                     obj_cmd_rs) < 0)
    {
      if (ipmi_ctx_errnum (state_data->ipmi_ctx) == IPMI_ERR_BAD_COMPLETION_CODE
          && comp_code)
        {
          (*comp_code) = 0;
          if (FIID_OBJ_GET (obj_cmd_rs, "comp_code", &val) < 0)
            {
              snprintf (errorbuf,
                        errorbuflen,
                        "fiid_obj_get: 'comp_code': %s",
                        fiid_obj_errormsg (obj_cmd_rs));
              goto cleanup;
            }
          (*comp_code) = val;
        }
      
      if (ipmi_ctx_errnum (state_data->ipmi_ctx) == IPMI_ERR_BAD_COMPLETION_CODE
          && ipmi_check_completion_code (obj_cmd_rs,
                                         IPMI_COMP_CODE_DCMI_NO_SET_POWER_LIMIT) == 1)
        {
          snprintf (errorbuf,
                    errorbuflen,
                    "ipmi_cmd_dcmi_get_power_limit: %s",
                    IPMI_COMP_CODE_DCMI_NO_SET_POWER_LIMIT_STR);
          no_set_power_limit_error_flag++;
          goto read_data;
        }
      else
        snprintf (errorbuf,
                  errorbuflen,
                  "ipmi_cmd_dcmi_get_power_limit: %s",
                  ipmi_ctx_errormsg (state_data->ipmi_ctx));

      goto cleanup;
    }

 read_data:

  if (FIID_OBJ_GET (obj_cmd_rs,
                    "exception_actions",
                    &val) < 0)
    {
      if (!no_set_power_limit_error_flag
          || fiid_obj_errnum (obj_cmd_rs) != FIID_ERR_DATA_NOT_AVAILABLE)
        snprintf (errorbuf,
                  errorbuflen,
                  "fiid_obj_get: 'exception_actions': %s",
                  fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  (*exception_actions) = val;
  
  if (FIID_OBJ_GET (obj_cmd_rs,
                    "power_limit_requested",
                    &val) < 0)
    {
      if (!no_set_power_limit_error_flag
          || fiid_obj_errnum (obj_cmd_rs) != FIID_ERR_DATA_NOT_AVAILABLE)
        snprintf (errorbuf,
                  errorbuflen,
                  "fiid_obj_get: 'power_limit_requested': %s",
                  fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  (*power_limit_requested) = val;

  if (FIID_OBJ_GET (obj_cmd_rs,
                    "correction_time_limit",
                    &val) < 0)
    {
      if (!no_set_power_limit_error_flag
          || fiid_obj_errnum (obj_cmd_rs) != FIID_ERR_DATA_NOT_AVAILABLE)
        snprintf (errorbuf,
                  errorbuflen,
                  "fiid_obj_get: 'correction_time_limit': %s",
                  fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  (*correction_time_limit) = val;

  if (FIID_OBJ_GET (obj_cmd_rs,
                    "management_application_statistics_sampling_period",
                    &val) < 0)
    {
      if (!no_set_power_limit_error_flag
          || fiid_obj_errnum (obj_cmd_rs) != FIID_ERR_DATA_NOT_AVAILABLE)
        snprintf (errorbuf,
                  errorbuflen,
                  "fiid_obj_get: 'management_application_statistics_sampling_period': %s",
                  fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  (*management_application_statistics_sampling_period) = val;

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}


static int
get_power_limit (ipmi_dcmi_state_data_t *state_data)
{
  uint8_t exception_actions;
  uint16_t power_limit_requested;
  uint32_t correction_time_limit;
  uint16_t management_application_statistics_sampling_period;
  char errorbuf[IPMI_DCMI_ERROR_BUFLEN + 1];

  assert (state_data);

  memset (errorbuf, '\0', IPMI_DCMI_ERROR_BUFLEN + 1);

  if (_get_power_limit (state_data,
                        &exception_actions,
                        &power_limit_requested,
                        &correction_time_limit,
                        &management_application_statistics_sampling_period,
                        NULL,   /* comp_code */
                        errorbuf,
                        IPMI_DCMI_ERROR_BUFLEN) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "%s\n",
                       errorbuf);
      return (-1);
    }

  /* XXX: figure out OEM specifics, and list details given manufacturer ID/product ID */
  /* 

  From Holger Liebig at Fujitsu

  "Regarding OEM exception action we currently do support 'continue'
  (0x02) and 'shutdown' (0x03) in addition to the 'hard power off'."

  But I don't know what the manufacturer ID and product ID are, so we leave it out for now.

  */

  if (exception_actions == IPMI_DCMI_EXCEPTION_ACTION_NO_ACTION)
    pstdout_printf (state_data->pstate,
                    "Exception Actions                                 : No Action (%Xh)\n",
                    exception_actions);
  else if (exception_actions == IPMI_DCMI_EXCEPTION_ACTION_HARD_POWER_OFF_SYSTEM)
    pstdout_printf (state_data->pstate,
                    "Exception Actions                                 : Hard Power Off system (%Xh)\n",
                    exception_actions);
  else if (exception_actions == IPMI_DCMI_EXCEPTION_ACTION_LOG_EVENT_TO_SEL_ONLY)
    pstdout_printf (state_data->pstate,
                    "Exception Actions                                 : Log event to SEL only (%Xh)\n",
                    exception_actions);
  else if ((exception_actions >= IPMI_DCMI_EXCEPTION_ACTION_OEM_MIN)
           && (exception_actions <= IPMI_DCMI_EXCEPTION_ACTION_OEM_MAX))
    pstdout_printf (state_data->pstate,
                    "Exception Actions                                 : OEM action (%Xh)\n",
                    exception_actions);
  else 
    pstdout_printf (state_data->pstate,
                    "Exception Actions                                 : Unknown action (%Xh)\n",
                    exception_actions);
   
  pstdout_printf (state_data->pstate,
                  "Power Limit Requested                             : %u watts\n",
                  power_limit_requested);

  pstdout_printf (state_data->pstate,
                  "Correction time limit                             : %u milliseconds\n",
                  correction_time_limit);

  pstdout_printf (state_data->pstate,
                  "Management application Statistics Sampling period : %u seconds\n",
                  management_application_statistics_sampling_period);

  return (0);
}

static int
set_power_limit (ipmi_dcmi_state_data_t *state_data)
{
  struct ipmi_dcmi_arguments *args;
  fiid_obj_t obj_cmd_rs = NULL;
  uint8_t comp_code = 0;
  uint8_t exception_actions;
  uint16_t power_limit_requested;
  uint32_t correction_time_limit;
  uint16_t management_application_statistics_sampling_period;
  char errorbuf[IPMI_DCMI_ERROR_BUFLEN + 1];
  int rv = -1;

  assert (state_data);
  assert (state_data->prog_data->args->exception_actions
          || state_data->prog_data->args->power_limit_requested
          || state_data->prog_data->args->correction_time_limit
          || state_data->prog_data->args->statistics_sampling_period);

  args = state_data->prog_data->args;

  memset (errorbuf, '\0', IPMI_DCMI_ERROR_BUFLEN + 1);

  if (_get_power_limit (state_data,
                        &exception_actions,
                        &power_limit_requested,
                        &correction_time_limit,
                        &management_application_statistics_sampling_period,
                        &comp_code,
                        errorbuf,
                        IPMI_DCMI_ERROR_BUFLEN) < 0)
    {
      /* IPMI Workaround/Interpretation
       *
       * The DCMI spec indicates a potential completion code for the
       * "Get Power Limit" command as "No Set Power Limit" (0x80).
       * FreeIPMI originally interpreted this to mean the "Set Power
       * Limit" command was not available.  Atleast one vendor
       * interpreted this to mean "No Power Limit Set".  One can
       * consider this an English interpretation issue of 'No set
       * POWER LIMIT' vs. 'No SET POWER LIMIT' (i.e. is "set" a verb
       * or part of a proper noun referencing the DCMI command).
       * Confounding this issue is the fact that the example
       * implementation in Intel's DCMItool implements the former,
       * while the DCMI Conformance test suite implements the latter.
       * In addition to this, with the latter interpretation, it need
       * not be an indication of an error, but rather a flag.  So the
       * rest of the packet can be completely full of legitimate data.
       * 
       * So we will do the following.
       *
       * If the "No Set Power Limit" completion code is returned and 
       * we were able to read all of the fields, _get_power_limit() will
       * return normally and this error fallthrough won't occur.
       *
       * If the "No Set Power Limit", completion code is returned and
       * we were *not* able to read all of the fields, we won't have
       * values from "Get Power Limit" and won't know how to do the
       * configuration properly in "Set Power Limit".  So we will
       * require that the user input all fields for "Set Power Limit".
       */
      if (comp_code == IPMI_COMP_CODE_DCMI_NO_SET_POWER_LIMIT)
        {
          if (!args->exception_actions
              || !args->power_limit_requested
              || !args->correction_time_limit
              || !args->statistics_sampling_period)
            {
              pstdout_fprintf (state_data->pstate,
                               stderr,
                               "Must specify --exception-actions, --power-limit-requested, "
                               "--correction-time-limit, and --statistics-sampling-period\n");
              goto cleanup;
            }
        }
      else
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "%s\n",
                           errorbuf);
          goto cleanup;
        }
    }

  if (!args->exception_actions)
    args->exception_actions_arg = exception_actions;

  if (!args->power_limit_requested)
    args->power_limit_requested_arg = power_limit_requested;

  if (!args->correction_time_limit)
    args->correction_time_limit_arg = correction_time_limit;

  if (!args->statistics_sampling_period)
    args->statistics_sampling_period_arg = management_application_statistics_sampling_period;

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_dcmi_set_power_limit_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if (ipmi_cmd_dcmi_set_power_limit (state_data->ipmi_ctx,
                                     args->exception_actions_arg,
                                     args->power_limit_requested_arg,
                                     args->correction_time_limit_arg,
                                     args->statistics_sampling_period_arg,
                                     obj_cmd_rs) < 0)
    {
      if (ipmi_ctx_errnum (state_data->ipmi_ctx) == IPMI_ERR_BAD_COMPLETION_CODE
          && ipmi_check_completion_code (obj_cmd_rs,
                                         IPMI_COMP_CODE_DCMI_POWER_LIMIT_OUT_OF_RANGE) == 1)
        pstdout_fprintf (state_data->pstate,
                         stderr,
                         "ipmi_cmd_dcmi_set_power_limit: %s\n",
                         IPMI_COMP_CODE_DCMI_POWER_LIMIT_OUT_OF_RANGE_STR);
      else if (ipmi_ctx_errnum (state_data->ipmi_ctx) == IPMI_ERR_BAD_COMPLETION_CODE
               && ipmi_check_completion_code (obj_cmd_rs,
                                              IPMI_COMP_CODE_DCMI_CORRECTION_TIME_OUT_OF_RANGE) == 1)
        pstdout_fprintf (state_data->pstate,
                         stderr,
                         "ipmi_cmd_dcmi_set_power_limit: %s\n",
                         IPMI_COMP_CODE_DCMI_CORRECTION_TIME_OUT_OF_RANGE_STR);
      else if (ipmi_ctx_errnum (state_data->ipmi_ctx) == IPMI_ERR_BAD_COMPLETION_CODE
               && ipmi_check_completion_code (obj_cmd_rs,
                                              IPMI_COMP_CODE_DCMI_STATISTICS_REPORTING_PERIOD_OUT_OF_RANGE) == 1)
        pstdout_fprintf (state_data->pstate,
                         stderr,
                         "ipmi_cmd_dcmi_set_power_limit: %s\n",
                         IPMI_COMP_CODE_DCMI_STATISTICS_REPORTING_PERIOD_OUT_OF_RANGE_STR);
      else
        pstdout_fprintf (state_data->pstate,
                         stderr,
                         "ipmi_cmd_dcmi_set_power_limit: %s\n",
                         ipmi_ctx_errormsg (state_data->ipmi_ctx));
      goto cleanup;
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static int
activate_deactivate_power_limit (ipmi_dcmi_state_data_t *state_data)
{
  struct ipmi_dcmi_arguments *args;
  fiid_obj_t obj_cmd_rs = NULL;
  int rv = -1;

  assert (state_data);

  args = state_data->prog_data->args;

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_dcmi_activate_deactivate_power_limit_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if (ipmi_cmd_dcmi_activate_deactivate_power_limit (state_data->ipmi_ctx,
                                                     args->activate_deactivate_power_limit_arg,
                                                     obj_cmd_rs) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_cmd_dcmi_activate_deactivate_power_limit: %s\n",
                       ipmi_ctx_errormsg (state_data->ipmi_ctx));
      goto cleanup;
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static int
run_cmd_args (ipmi_dcmi_state_data_t *state_data)
{
  struct ipmi_dcmi_arguments *args;
  int rv = -1;

  assert (state_data);

  args = state_data->prog_data->args;

  if (args->interpret_oem_data)
    {
      if (ipmi_get_oem_data (state_data->pstate,
                             state_data->ipmi_ctx,
                             &state_data->oem_data) < 0)
        goto cleanup;
    }

  if (args->get_dcmi_capability_info)
    return (get_dcmi_capability_info (state_data));

  if (args->get_system_power_statistics)
    return (get_system_power_statistics (state_data));

  if (args->get_enhanced_system_power_statistics)
    return (get_enhanced_system_power_statistics (state_data));

  if (args->get_asset_tag)
    return (get_asset_tag (state_data));

  if (args->set_asset_tag)
    return (set_asset_tag (state_data));

  if (args->get_management_controller_identifier_string)
    return (get_management_controller_identifier_string (state_data));

  if (args->set_management_controller_identifier_string)
    return (set_management_controller_identifier_string (state_data));

  if (args->get_dcmi_sensor_info)
    return (get_dcmi_sensor_info (state_data));

  if (args->get_power_limit)
    return (get_power_limit (state_data));

  if (args->set_power_limit)
    return (set_power_limit (state_data));

  if (args->activate_deactivate_power_limit)
    return (activate_deactivate_power_limit (state_data));

  rv = 0;
 cleanup:
  return (rv);
}

static int
_ipmi_dcmi (pstdout_state_t pstate,
            const char *hostname,
            void *arg)
{
  ipmi_dcmi_state_data_t state_data;
  ipmi_dcmi_prog_data_t *prog_data;
  int exit_code = EXIT_FAILURE;

  assert (pstate);
  assert (arg);

  prog_data = (ipmi_dcmi_prog_data_t *)arg;
  memset (&state_data, '\0', sizeof (ipmi_dcmi_state_data_t));

  state_data.prog_data = prog_data;
  state_data.pstate = pstate;

  if (!(state_data.ipmi_ctx = ipmi_open (prog_data->progname,
                                         hostname,
                                         &(prog_data->args->common_args),
					 state_data.pstate)))
    goto cleanup;

  if (run_cmd_args (&state_data) < 0)
    goto cleanup;

  exit_code = EXIT_SUCCESS;
 cleanup:
  ipmi_ctx_close (state_data.ipmi_ctx);
  ipmi_ctx_destroy (state_data.ipmi_ctx);
  return (exit_code);
}

int
main (int argc, char **argv)
{
  ipmi_dcmi_prog_data_t prog_data;
  struct ipmi_dcmi_arguments cmd_args;
  int hosts_count;
  int rv;

  ipmi_disable_coredump ();

  memset (&prog_data, '\0', sizeof (ipmi_dcmi_prog_data_t));
  prog_data.progname = argv[0];
  ipmi_dcmi_argp_parse (argc, argv, &cmd_args);
  prog_data.args = &cmd_args;

  if ((hosts_count = pstdout_setup (&(prog_data.args->common_args.hostname),
				    &(prog_data.args->common_args))) < 0)
    return (EXIT_FAILURE);

  if (!hosts_count)
    return (EXIT_SUCCESS);

  if ((rv = pstdout_launch (prog_data.args->common_args.hostname,
                            _ipmi_dcmi,
                            &prog_data)) < 0)
    {
      fprintf (stderr,
               "pstdout_launch: %s\n",
               pstdout_strerror (pstdout_errnum));
      return (EXIT_FAILURE);
    }

  return (rv);
}
