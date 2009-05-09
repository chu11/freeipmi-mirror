/*
  Copyright (C) 2003-2009 FreeIPMI Core Team

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2, or (at your option)
  any later version.

  This program is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA
*/

#if HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <assert.h>
#include <errno.h>

#include <freeipmi/freeipmi.h>

#include "bmc-info.h"
#include "bmc-info-argp.h"

#include "freeipmi-portability.h"
#include "pstdout.h"
#include "tool-common.h"
#include "tool-cmdline-common.h"
#include "tool-hostrange-common.h"

typedef struct channel_info
{
  uint8_t actual_channel_number;
  uint8_t channel_medium_type;
  uint8_t channel_protocol_type;
  uint8_t active_session_count;
  uint8_t session_support;
  uint32_t vendor_id;
} channel_info_t;

#define NUM_CHANNELS 8

static int
display_get_device_guid (bmc_info_state_data_t *state_data)
{
  uint8_t guidbuf[1024];
  fiid_obj_t obj_cmd_rs = NULL;
  int rv = -1;

  assert (state_data);

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_get_device_guid_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if (ipmi_cmd_get_device_guid (state_data->ipmi_ctx, obj_cmd_rs) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_cmd_get_device_guid: %s\n",
                       ipmi_ctx_errormsg (state_data->ipmi_ctx));
      goto cleanup;
    }

  if (fiid_obj_get_data (obj_cmd_rs,
                         "guid",
                         guidbuf,
                         1024) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get_data: 'guid': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }

  /* IPMI transfers the guid in least significant bit order and the
   * fields are reverse from the "Wired for Management
   * Specification".
   *
   * For output format details see Appendix 1 "String Representation
   * of UUIDs" in the above document.  Note that the output is
   * supposed to be output in most significant byte order.
   */
  if (!state_data->prog_data->args->get_device_guid)
    pstdout_printf (state_data->pstate, "GUID : ");

  pstdout_printf (state_data->pstate,
                  "%02X%02X%02X%02X-%02X%02X-%02X%02X-%02X%02X-%02X%02X%02X%02X%02X%02X\n",
                  guidbuf[15],  /* time low */
                  guidbuf[14],
                  guidbuf[13],
                  guidbuf[12],
                  guidbuf[11],  /* time mid */
                  guidbuf[10],
                  guidbuf[9],   /* time high and version */
                  guidbuf[8],
                  guidbuf[6],   /* clock seq high and reserved - comes before clock seq low */
                  guidbuf[7],   /* clock seq low */
                  guidbuf[5],   /* node */
                  guidbuf[4],
                  guidbuf[3],
                  guidbuf[2],
                  guidbuf[1],
                  guidbuf[0]);
  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static int
display_intel (bmc_info_state_data_t *state_data, fiid_obj_t device_id_rs)
{
  uint8_t boot_code_major, boot_code_minor, pia_major, pia_minor;
  uint64_t val;
  fiid_obj_t obj_intel_rs = NULL;
  int rv = -1;

  assert (state_data);

  if (!(obj_intel_rs = fiid_obj_copy (device_id_rs, tmpl_cmd_get_device_id_sr870bn4_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_copy: %s\n",
                       fiid_obj_errormsg (device_id_rs));
      goto cleanup;
    }

  if (FIID_OBJ_GET (obj_intel_rs,
                    "auxiliary_firmware_revision_information.boot_code.major",
                    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'auxiliary_firmware_revision_information.boot_code.major': %s\n",
                       fiid_obj_errormsg (obj_intel_rs));
      goto cleanup;
    }
  boot_code_major = val;

  if (FIID_OBJ_GET (obj_intel_rs,
                    "auxiliary_firmware_revision_information.boot_code.minor",
                    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'auxiliary_firmware_revision_information.boot_code.minor': %s\n",
                       fiid_obj_errormsg (obj_intel_rs));
      goto cleanup;
    }
  boot_code_minor = val;

  if (FIID_OBJ_GET (obj_intel_rs,
                    "auxiliary_firmware_revision_information.pia.major",
                    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'auxiliary_firmware_revision_information.pia.major': %s\n",
                       fiid_obj_errormsg (obj_intel_rs));
      goto cleanup;
    }
  pia_major = val;

  if (FIID_OBJ_GET (obj_intel_rs,
                    "auxiliary_firmware_revision_information.pia.minor",
                    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'auxiliary_firmware_revision_information.pia.minor': %s\n",
                       fiid_obj_errormsg (obj_intel_rs));
      goto cleanup;
    }
  pia_minor = val;

  pstdout_printf (state_data->pstate,
                  "Auxiliary Firmware Revision Information :\n",
                  "Boot Code : v%02x.%2x\n"
                  "PIA       : v%02x.%2x\n",
                  boot_code_major,
                  boot_code_minor,
                  pia_major,
                  pia_minor);

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_intel_rs);
  return (rv);
}

static int
display_get_device_id (bmc_info_state_data_t *state_data)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint8_t device_id;
  uint8_t revision;
  uint8_t major, minor;
  uint32_t manufacturer_id;
  uint16_t product_id;
  uint32_t auxiliary_firmware_revision_information;
  int flag;
  uint64_t val = 0;
  int rv = -1;

  assert (state_data);

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_get_device_id_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if (ipmi_cmd_get_device_id (state_data->ipmi_ctx, obj_cmd_rs) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_cmd_get_device_id: %s\n",
                       ipmi_ctx_errormsg (state_data->ipmi_ctx));
      goto cleanup;
    }

  if (FIID_OBJ_GET (obj_cmd_rs, "device_id", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'device_id': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  device_id = val;

  pstdout_printf (state_data->pstate,
                  "Device ID             : %u\n", device_id);

  if (FIID_OBJ_GET (obj_cmd_rs, "device_revision.revision", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'device_revision.revision': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  revision = val;

  pstdout_printf (state_data->pstate,
                  "Device Revision       : %u\n",
                  revision);

  if (FIID_OBJ_GET (obj_cmd_rs, "device_revision.sdr_support", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'device_revision.sdr_support': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }

  pstdout_printf (state_data->pstate,
                  "Device SDRs           : %s\n",
                  val ? "supported" : "unsupported");

  if (FIID_OBJ_GET (obj_cmd_rs, "firmware_revision1.major_revision", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'firmware_revision1.major_revision': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  major = val;

  if (FIID_OBJ_GET (obj_cmd_rs, "firmware_revision2.minor_revision", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'firmware_revision2.minor_revision': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  minor = val;

  /* achu: minor revision is BCD encoded and is 8 bits, output w/ %x */
  pstdout_printf (state_data->pstate,
                  "Firmware Revision     : %u.%02x\n",
                  major,
                  minor);

  if (FIID_OBJ_GET (obj_cmd_rs,
                    "firmware_revision1.device_available",
                    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'firmware_revision1.device_available': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }

  /* The "yes" vs. "no" is backwards from normal logic */
  pstdout_printf (state_data->pstate,
                  "Device Available      : %s\n",
                  val ? "no (device firmware, SDR Repository update or self initilization in progress)" : "yes (normal operation)");

  if (FIID_OBJ_GET (obj_cmd_rs, "ipmi_version_major", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'ipmi_version_major': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  major = val;
  
  if (FIID_OBJ_GET (obj_cmd_rs, "ipmi_version_minor", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'ipmi_version_minor': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  minor = val;

  /* achu: ipmi version is BCD encoded, but major/minor are only 4 bits */
  pstdout_printf (state_data->pstate,
                  "IPMI Version          : %u.%u\n",
                  major,
                  minor);

  if (FIID_OBJ_GET (obj_cmd_rs, "additional_device_support.sensor_device", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'additional_device_support.sensor_device': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }

  pstdout_printf (state_data->pstate,
                  "Sensor Device         : %s\n",
                  val ? "supported" : "unsupported");

  if (FIID_OBJ_GET (obj_cmd_rs, "additional_device_support.sdr_repository_device", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'additional_device_support.sdr_repository_device': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }

  pstdout_printf (state_data->pstate,
                  "SDR Repository Device : %s\n",
                  val ? "supported" : "unsupported");

  if (FIID_OBJ_GET (obj_cmd_rs, "additional_device_support.sel_device", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'additional_device_support.sel_device': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }

  pstdout_printf (state_data->pstate,
                  "SEL Device            : %s\n",
                  val ? "supported" : "unsupported");

  if (FIID_OBJ_GET (obj_cmd_rs, "additional_device_support.fru_inventory_device", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'additional_device_support.fru_inventory_device': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }

  pstdout_printf (state_data->pstate,
                  "FRU Inventory Device  : %s\n",
                  val ? "supported" : "unsupported");

  if (FIID_OBJ_GET (obj_cmd_rs, "additional_device_support.ipmb_event_receiver", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'additional_device_support.ipmb_event_receiver': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }

  pstdout_printf (state_data->pstate,
                  "IPMB Event Receiver   : %s\n",
                  val ? "supported" : "unsupported");

  if (FIID_OBJ_GET (obj_cmd_rs, "additional_device_support.ipmb_event_generator", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'additional_device_support.ipmb_event_generator': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }

  pstdout_printf (state_data->pstate,
                  "IPMB Event Generator  : %s\n",
                  val ? "supported" : "unsupported");

  if (FIID_OBJ_GET (obj_cmd_rs, "additional_device_support.bridge", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'additional_device_support.bridge': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }

  pstdout_printf (state_data->pstate,
                  "Bridge                : %s\n",
                  val ? "supported" : "unsupported");

  if (FIID_OBJ_GET (obj_cmd_rs, "additional_device_support.chassis_device", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'additional_device_support.chassis_device': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }

  pstdout_printf (state_data->pstate,
                  "Chassis Device        : %s\n",
                  val ? "supported" : "unsupported");

  if (FIID_OBJ_GET (obj_cmd_rs, "manufacturer_id.id", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'manufacturer_id.id': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  manufacturer_id = val;

  if (IPMI_IANA_ENTERPRISE_ID_VALID (manufacturer_id)
      && ipmi_iana_enterprise_numbers[manufacturer_id])
    pstdout_printf (state_data->pstate,
                    "Manufacturer ID       : %s (%u)\n",
                    ipmi_iana_enterprise_numbers[manufacturer_id],
                    manufacturer_id);
  else
    pstdout_printf (state_data->pstate,
                    "Manufacturer ID       : %u\n",
                    manufacturer_id);

  if (FIID_OBJ_GET (obj_cmd_rs, "product_id", &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'product_id': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  product_id = val;

  pstdout_printf (state_data->pstate,
                  "Product ID            : %u\n",
                  product_id);

  /* auxiliary firmware info is optional */
  if ((flag = fiid_obj_get (obj_cmd_rs,
                            "auxiliary_firmware_revision_information",
                            &val)) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'auxiliary_firmware_revision_information': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  auxiliary_firmware_revision_information = val;

  if (flag)
    {
      switch (manufacturer_id)
        {
        case IPMI_MANUFACTURER_ID_INTEL:
          switch (product_id)
            {
            case IPMI_PRODUCT_ID_SR870BN4:
              if (display_intel (state_data, obj_cmd_rs) < 0)
                goto cleanup;
              break;
	    default:
              break;
            }
          break;
        default:
          pstdout_printf (state_data->pstate,
                          "Auxiliary Firmware Revision Information : %08Xh\n",
                          auxiliary_firmware_revision_information);
        }
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static int
get_channel_info_list (bmc_info_state_data_t *state_data, channel_info_t *channel_info_list)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint8_t i;
  uint8_t ci;
  uint64_t val;
  int rv = -1;

  assert (state_data);
  assert (channel_info_list);

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_get_channel_info_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  for (i = 0, ci = 0; i < NUM_CHANNELS; i++)
    {
      if (ipmi_cmd_get_channel_info (state_data->ipmi_ctx,
                                     i,
                                     obj_cmd_rs) < 0)
        continue;

      if (FIID_OBJ_GET (obj_cmd_rs,
                        "actual_channel_number",
                        &val) < 0)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "fiid_obj_get: 'actual_channel_number': %s\n",
                           fiid_obj_errormsg (obj_cmd_rs));
          goto cleanup;
        }

      channel_info_list[ci].actual_channel_number = val;

      if (FIID_OBJ_GET (obj_cmd_rs,
                        "channel_medium_type",
                        &val) < 0)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "fiid_obj_get: 'channel_medium_type': %s\n",
                           fiid_obj_errormsg (obj_cmd_rs));
          goto cleanup;
        }

      channel_info_list[ci].channel_medium_type = val;

      if (FIID_OBJ_GET (obj_cmd_rs,
                        "channel_protocol_type",
                        &val) < 0)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "fiid_obj_get: 'channel_protocol_type': %s\n",
                           fiid_obj_errormsg (obj_cmd_rs));
          goto cleanup;
        }

      channel_info_list[ci].channel_protocol_type = val;

      if (FIID_OBJ_GET (obj_cmd_rs,
                        "active_session_count",
                        &val) < 0)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "fiid_obj_get: 'active_session_count': %s\n",
                           fiid_obj_errormsg (obj_cmd_rs));
          goto cleanup;
        }

      channel_info_list[ci].active_session_count = val;

      if (FIID_OBJ_GET (obj_cmd_rs,
                        "session_support",
                        &val) < 0)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "fiid_obj_get: 'session_support': %s\n",
                           fiid_obj_errormsg (obj_cmd_rs));
          goto cleanup;
        }

      channel_info_list[ci].session_support = val;

      if (FIID_OBJ_GET (obj_cmd_rs,
                        "vendor_id",
                        &val) < 0)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "fiid_obj_get: 'vendor_id': %s\n",
                           fiid_obj_errormsg (obj_cmd_rs));
          goto cleanup;
        }

      channel_info_list[ci].vendor_id = val;

      ci++;
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static int
display_channel_info (bmc_info_state_data_t *state_data)
{
  channel_info_t channel_info_list[NUM_CHANNELS];
  int first_newline_output = 0;
  uint8_t i;

  assert (state_data);

  memset (channel_info_list, '\0', sizeof (channel_info_t) * NUM_CHANNELS);
  if (get_channel_info_list (state_data, channel_info_list) < 0)
    return (-1);

  if (!state_data->prog_data->args->get_channel_info)
    pstdout_printf (state_data->pstate, "Channel Information\n");

  for (i = 0; i < NUM_CHANNELS; i++)
    {
      char *medium_type_str = NULL;
      char *protocol_type_str = NULL;
      char *session_support_str = NULL;

      if (IPMI_CHANNEL_MEDIUM_TYPE_IS_RESERVED (channel_info_list[i].channel_medium_type))
        continue;

      if (IPMI_CHANNEL_MEDIUM_TYPE_IS_RESERVED (channel_info_list[i].channel_medium_type))
        medium_type_str = "Reserved";
      else if (channel_info_list[i].channel_medium_type == IPMI_CHANNEL_MEDIUM_TYPE_IPMB)
        medium_type_str = "IPMB (I2C)";
      else if (channel_info_list[i].channel_medium_type == IPMI_CHANNEL_MEDIUM_TYPE_ICMB_10)
        medium_type_str = "ICMB v1.0";
      else if (channel_info_list[i].channel_medium_type == IPMI_CHANNEL_MEDIUM_TYPE_ICMB_09)
        medium_type_str = "ICMB v0.9";
      else if (channel_info_list[i].channel_medium_type == IPMI_CHANNEL_MEDIUM_TYPE_LAN_802_3)
        medium_type_str = "802.3 LAN";
      else if (channel_info_list[i].channel_medium_type == IPMI_CHANNEL_MEDIUM_TYPE_RS232)
        medium_type_str = "Asynch. Serial/Modem (RS-232)";
      else if (channel_info_list[i].channel_medium_type == IPMI_CHANNEL_MEDIUM_TYPE_OTHER_LAN)
        medium_type_str = "Other LAN";
      else if (channel_info_list[i].channel_medium_type == IPMI_CHANNEL_MEDIUM_TYPE_PCI_SMBUS)
        medium_type_str = "PCI SMBus";
      else if (channel_info_list[i].channel_medium_type == IPMI_CHANNEL_MEDIUM_TYPE_SMBUS_10_11)
        medium_type_str = "SMBus v1.0/1.1";
      else if (channel_info_list[i].channel_medium_type == IPMI_CHANNEL_MEDIUM_TYPE_SMBUS_20)
        medium_type_str = "SMBus v2.0";
      else if (channel_info_list[i].channel_medium_type == IPMI_CHANNEL_MEDIUM_TYPE_USB_1X)
        medium_type_str = "USB 1.x";
      else if (channel_info_list[i].channel_medium_type == IPMI_CHANNEL_MEDIUM_TYPE_USB_2X)
        medium_type_str = "USB 2.x";
      else if (channel_info_list[i].channel_medium_type == IPMI_CHANNEL_MEDIUM_TYPE_SYS_IFACE)
        medium_type_str = "System Interface (KCS, SMIC, or BT)";
      else if (IPMI_CHANNEL_MEDIUM_TYPE_IS_OEM (channel_info_list[i].channel_medium_type))
        medium_type_str = "OEM";
      else
        medium_type_str = "unknown";

      if (IPMI_CHANNEL_PROTOCOL_TYPE_IS_RESERVED (channel_info_list[i].channel_protocol_type))
        protocol_type_str = "Reserved";
      else if (channel_info_list[i].channel_protocol_type == IPMI_CHANNEL_PROTOCOL_TYPE_IPMB)
        protocol_type_str = "IPMB-1.0";
      else if (channel_info_list[i].channel_protocol_type == IPMI_CHANNEL_PROTOCOL_TYPE_ICMB_10)
        protocol_type_str = "ICMB-1.0";
      else if (channel_info_list[i].channel_protocol_type == IPMI_CHANNEL_PROTOCOL_TYPE_SMBUS_1X_2X)
        protocol_type_str = "IPMI-SMBus";
      else if (channel_info_list[i].channel_protocol_type == IPMI_CHANNEL_PROTOCOL_TYPE_KCS)
        protocol_type_str = "KCS";
      else if (channel_info_list[i].channel_protocol_type == IPMI_CHANNEL_PROTOCOL_TYPE_SMIC)
        protocol_type_str = "SMIC";
      else if (channel_info_list[i].channel_protocol_type == IPMI_CHANNEL_PROTOCOL_TYPE_BT_10)
        protocol_type_str = "BT-10";
      else if (channel_info_list[i].channel_protocol_type == IPMI_CHANNEL_PROTOCOL_TYPE_BT_15)
        protocol_type_str = "BT-15";
      else if (channel_info_list[i].channel_protocol_type == IPMI_CHANNEL_PROTOCOL_TYPE_TMODE)
        protocol_type_str = "TMODE";
      else if (IPMI_CHANNEL_PROTOCOL_TYPE_IS_OEM (channel_info_list[i].channel_protocol_type))
        protocol_type_str = "OEM";
      else
        protocol_type_str = "unknown";

      if (channel_info_list[i].session_support == IPMI_SESSION_SUPPORT_SESSION_LESS)
        session_support_str = "session-less";
      else if (channel_info_list[i].session_support == IPMI_SESSION_SUPPORT_SINGLE_SESSION)
        session_support_str = "single-session";
      else if (channel_info_list[i].session_support == IPMI_SESSION_SUPPORT_MULTI_SESSION)
        session_support_str = "multi-session";
      else
        session_support_str = "session-based";
        
      if (!state_data->prog_data->args->get_channel_info || first_newline_output)
        pstdout_printf (state_data->pstate, "\n");

      pstdout_printf (state_data->pstate,
                      "Channel Number       : %u\n",
                      channel_info_list[i].actual_channel_number);

      pstdout_printf (state_data->pstate,
                      "Medium Type          : %s\n",
                      medium_type_str);

      pstdout_printf (state_data->pstate,
                      "Protocol Type        : %s\n",
                      protocol_type_str);

      pstdout_printf (state_data->pstate,
                      "Active Session Count : %u\n",
                      channel_info_list[i].active_session_count);

      pstdout_printf (state_data->pstate,
                      "Session Support      : %s\n",
                      session_support_str);

      if (IPMI_IANA_ENTERPRISE_ID_VALID (channel_info_list[i].vendor_id)
          && ipmi_iana_enterprise_numbers[channel_info_list[i].vendor_id])
        pstdout_printf (state_data->pstate,
                        "Vendor ID            : %s (%u)\n",
                        ipmi_iana_enterprise_numbers[channel_info_list[i].vendor_id],
                        channel_info_list[i].vendor_id);
      else
        pstdout_printf (state_data->pstate,
                        "Vendor ID             : %u\n",
                        channel_info_list[i].vendor_id);

      first_newline_output++;
    }

  return (0);
}

static int
run_cmd_args (bmc_info_state_data_t *state_data)
{
  struct bmc_info_arguments *args;
  int rv = -1;

  assert (state_data);

  args = state_data->prog_data->args;

  if (args->get_device_id)
    return (display_get_device_id (state_data));

  if (args->get_device_guid)
    return (display_get_device_guid (state_data));
 
  if (args->get_channel_info)
    return (display_channel_info (state_data));

  if (display_get_device_id (state_data) < 0)
    goto cleanup;

  pstdout_printf (state_data->pstate, "\n");

  if (display_get_device_guid (state_data) < 0)
    goto cleanup;

  pstdout_printf (state_data->pstate, "\n");

  if (display_channel_info (state_data) < 0)
    goto cleanup;

  rv = 0;
 cleanup:
  return (rv);
}

static int
_bmc_info (pstdout_state_t pstate,
           const char *hostname,
           void *arg)
{
  bmc_info_state_data_t state_data;
  bmc_info_prog_data_t *prog_data;
  char errmsg[IPMI_OPEN_ERRMSGLEN];
  int exit_code = -1;

  prog_data = (bmc_info_prog_data_t *)arg;
  memset (&state_data, '\0', sizeof (bmc_info_state_data_t));

  state_data.prog_data = prog_data;
  state_data.pstate = pstate;

  if (!(state_data.ipmi_ctx = ipmi_open (prog_data->progname,
                                         hostname,
                                         &(prog_data->args->common),
                                         errmsg,
                                         IPMI_OPEN_ERRMSGLEN)))
    {
      pstdout_fprintf (pstate,
                       stderr,
                       "%s\n",
                       errmsg);
      exit_code = EXIT_FAILURE;
      goto cleanup;
    }

  if (run_cmd_args (&state_data) < 0)
    {
      exit_code = EXIT_FAILURE;
      goto cleanup;
    }

  exit_code = 0;
 cleanup:
  if (state_data.ipmi_ctx)
    {
      ipmi_ctx_close (state_data.ipmi_ctx);
      ipmi_ctx_destroy (state_data.ipmi_ctx);
    }
  return (exit_code);
}

int
main (int argc, char **argv)
{
  bmc_info_prog_data_t prog_data;
  struct bmc_info_arguments cmd_args;
  int exit_code;
  int rv;

  ipmi_disable_coredump ();

  memset (&prog_data, '\0', sizeof (bmc_info_prog_data_t));
  prog_data.progname = argv[0];
  bmc_info_argp_parse (argc, argv, &cmd_args);
  prog_data.args = &cmd_args;

  if (pstdout_setup (&(prog_data.args->common.hostname),
                     prog_data.args->hostrange.buffer_output,
                     prog_data.args->hostrange.consolidate_output,
                     prog_data.args->hostrange.fanout,
                     prog_data.args->hostrange.eliminate,
                     prog_data.args->hostrange.always_prefix) < 0)
    {
      exit_code = EXIT_FAILURE;
      goto cleanup;
    }

  if ((rv = pstdout_launch (prog_data.args->common.hostname,
                            _bmc_info,
                            &prog_data)) < 0)
    {
      fprintf (stderr,
               "pstdout_launch: %s\n",
               pstdout_strerror (pstdout_errnum));
      exit_code = EXIT_FAILURE;
      goto cleanup;
    }

  exit_code = rv;
 cleanup:
  return (exit_code);
}
