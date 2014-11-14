/*
 * Copyright (C) 2003-2014 FreeIPMI Core Team
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
#include "tool-util-common.h"

typedef struct channel_info
{
  uint8_t actual_channel_number;
  uint8_t channel_medium_type;
  uint8_t channel_protocol_type;
  uint8_t active_session_count;
  uint8_t session_support;
  uint32_t vendor_id;
} channel_info_t;

#define BMC_INFO_SYSTEM_INFO_STRING_MAX 512

#define BMC_INFO_IANA_STRING_MAX 1024

#define BMC_INFO_BUFLEN 1024

typedef int (*Bmc_info_system_info_first_set)(ipmi_ctx_t ctx,
                                              uint8_t get_parameter,
                                              uint8_t set_selector,
                                              uint8_t block_selector,
                                              fiid_obj_t obj_cmd_rs);

typedef int (*Bmc_info_system_info)(ipmi_ctx_t ctx,
                                    uint8_t get_parameter,
                                    uint8_t set_selector,
                                    uint8_t block_selector,
                                    fiid_obj_t obj_cmd_rs);

fiid_template_t tmpl_cmd_get_device_id_sr870bn4_rs =
  {
    { 8,  "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8,  "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8,  "device_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4,  "device_revision.revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},  /* binary encoded */
    { 3,  "device_revision.reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1,  "device_revision.sdr_support", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 7,  "firmware_revision1.major_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1,  "firmware_revision1.device_available", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8,  "firmware_revision2.minor_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},  /* BCD encoded */
    { 4,  "ipmi_version.ms_bits", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4,  "ipmi_version.ls_bits", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1,  "additional_device_support.sensor_device", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1,  "additional_device_support.sdr_repository_device", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1,  "additional_device_support.sel_device", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1,  "additional_device_support.fru_inventory_device", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1,  "additional_device_support.ipmb_event_receiver", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1,  "additional_device_support.ipmb_event_generator", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1,  "additional_device_support.bridge", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1,  "additional_device_support.chassis_device", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 20, "manufacturer_id.id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4,  "manufacturer_id.reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "product_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8,  "auxiliary_firmware_revision_information.boot_code.major", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8,  "auxiliary_firmware_revision_information.boot_code.minor", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8,  "auxiliary_firmware_revision_information.pia.major", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8,  "auxiliary_firmware_revision_information.pia.minor", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0,  "", 0}
  };

static int
display_intel_sr870bn4 (bmc_info_state_data_t *state_data, fiid_obj_t device_id_rs)
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
                  "Boot Code             : v%02x.%2x\n"
                  "PIA                   : v%02x.%2x\n",
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
  char iana_buf[BMC_INFO_IANA_STRING_MAX + 1];
  int ret;
  int rv = -1;

  assert (state_data);

  memset (iana_buf, '\0', BMC_INFO_IANA_STRING_MAX + 1);

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

  /* if ret == 0 means no string, < 0 means bad manufacturer id
   * either way, output just the number
   */
  ret = ipmi_iana_enterprise_numbers_string (manufacturer_id,
                                             iana_buf,
                                             BMC_INFO_IANA_STRING_MAX);

  if (ret > 0)
    pstdout_printf (state_data->pstate,
                    "Manufacturer ID       : %s (%u)\n",
                    iana_buf,
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
      /* OEM Interpretation
       *
       * Intel SR870BN4/Tiger 4
       */
      if (state_data->prog_data->args->interpret_oem_data
          && manufacturer_id == IPMI_IANA_ENTERPRISE_ID_INTEL
          && product_id == IPMI_INTEL_PRODUCT_ID_SR870BN4)
        {
          if (display_intel_sr870bn4 (state_data, obj_cmd_rs) < 0)
            goto cleanup;
        }
      /* OEM Interpretation
       *
       * Dell Poweredge R610
       * Dell Poweredge R710
       */
      else if (state_data->prog_data->args->interpret_oem_data
               && manufacturer_id ==  IPMI_IANA_ENTERPRISE_ID_DELL
               && (product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_R610
                   || product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_R710))
        {
          pstdout_printf (state_data->pstate,
                          "Build Number          : %u\n",
                          auxiliary_firmware_revision_information);
        }
      else
        pstdout_printf (state_data->pstate,
                        "Auxiliary Firmware Revision Information : %08Xh\n",
                        auxiliary_firmware_revision_information);
    }

  /* output newline if we're outputting all sections */
  if (!state_data->prog_data->args->get_device_id)
    pstdout_printf (state_data->pstate, "\n");

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static int
display_guid (bmc_info_state_data_t *state_data,
	      fiid_field_t *tmpl_cmd_get_guid_rs,
	      int (*ipmi_cmd_get_guid_func)(ipmi_ctx_t, fiid_obj_t),
	      char *ipmi_cmd_get_guid_description,
	      int get_guid_only_flag,
	      char *guid_description)
{
  uint8_t guidbuf[BMC_INFO_BUFLEN];
  fiid_obj_t obj_cmd_rs = NULL;
  int len;
  int rv = -1;

  assert (state_data);
  assert (tmpl_cmd_get_guid_rs);
  assert (ipmi_cmd_get_guid_func);
  assert (ipmi_cmd_get_guid_description);
  assert (guid_description);

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_get_guid_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if (ipmi_cmd_get_guid_func (state_data->ipmi_ctx, obj_cmd_rs) < 0)
    {
      if (!state_data->prog_data->args->get_device_guid
          && ipmi_ctx_errnum (state_data->ipmi_ctx) == IPMI_ERR_COMMAND_INVALID_OR_UNSUPPORTED
          && ipmi_check_completion_code (obj_cmd_rs,
                                         IPMI_COMP_CODE_INVALID_COMMAND) == 1)
        {
          rv = 0;
          goto cleanup;
        }

      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "%s: %s\n",
		       ipmi_cmd_get_guid_description,
                       ipmi_ctx_errormsg (state_data->ipmi_ctx));
      goto cleanup;
    }

  if ((len = fiid_obj_get_data (obj_cmd_rs,
                                "guid",
                                guidbuf,
                                BMC_INFO_BUFLEN)) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get_data: 'guid': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }

  if (len < IPMI_SYSTEM_GUID_LENGTH)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "guid length invalid: %d\n",
                       len);
      goto cleanup;
    }

  if (!get_guid_only_flag)
    pstdout_printf (state_data->pstate, "%s : ", guid_description);

  /* IPMI transfers the guid in least significant bit order and the
   * fields are reverse from the "Wired for Management
   * Specification".
   *
   * For output format details see Appendix 1 "String Representation
   * of UUIDs" in the above document.  Note that the output is
   * supposed to be output in most significant byte order and hex
   * characters are to be output lower case.
   */
  if (!(state_data->prog_data->args->common_args.section_specific_workaround_flags & IPMI_PARSE_SECTION_SPECIFIC_WORKAROUND_FLAGS_GUID_FORMAT))
    pstdout_printf (state_data->pstate,
		    "%02x%02x%02x%02x-%02x%02x-%02x%02x-%02x%02x-%02x%02x%02x%02x%02x%02x\n",
		    guidbuf[15],  /* time low */
		    guidbuf[14],
		    guidbuf[13],
		    guidbuf[12],
		    guidbuf[11],  /* time mid */
		    guidbuf[10],
		    guidbuf[9],   /* time high and version */
		    guidbuf[8],
		    guidbuf[7],   /* clock seq and reserved */
		    guidbuf[6],
		    guidbuf[5],   /* node */
		    guidbuf[4],
		    guidbuf[3],
		    guidbuf[2],
		    guidbuf[1],
		    guidbuf[0]);
  else
    /* It appears some vendors are not sending the GUID in the right
     * format, basically sending it in the format from the Wired for
     * Management Specification.
     */
    pstdout_printf (state_data->pstate,
		    "%02x%02x%02x%02x-%02x%02x-%02x%02x-%02x%02x-%02x%02x%02x%02x%02x%02x\n",
		    guidbuf[3],   /* time low */
		    guidbuf[2],
		    guidbuf[1],
		    guidbuf[0],
		    guidbuf[5],   /* time mid */
		    guidbuf[4],
		    guidbuf[7],   /* time high and version */
		    guidbuf[6],
		    guidbuf[8],	  /* clock seq high and reserved - not little endian*/
		    guidbuf[9],   /* clock seq low */
		    guidbuf[10],   /* node - assume sent in correct order */
		    guidbuf[11],
		    guidbuf[12],
		    guidbuf[13],
		    guidbuf[14],
		    guidbuf[15]);

  /* output newline if we're outputting all sections */
  if (!get_guid_only_flag)
    pstdout_printf (state_data->pstate, "\n");

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static int
display_get_device_guid (bmc_info_state_data_t *state_data)
{
  return display_guid (state_data,
		       tmpl_cmd_get_device_guid_rs,
		       ipmi_cmd_get_device_guid,
		       "ipmi_cmd_get_device_guid",
		       state_data->prog_data->args->get_device_guid,
		       "Device GUID");
}

static int
display_get_system_guid (bmc_info_state_data_t *state_data)
{
  return display_guid (state_data,
		       tmpl_cmd_get_system_guid_rs,
		       ipmi_cmd_get_system_guid,
		       "ipmi_cmd_get_system_guid",
		       state_data->prog_data->args->get_system_guid,
		       "System GUID");
}

/* return 1 if supported, 0 if not */
static int
display_system_info_common (bmc_info_state_data_t *state_data,
                            fiid_field_t *tmpl_cmd_first_set,
                            fiid_field_t *tmpl_cmd,
                            Bmc_info_system_info_first_set func_cmd_first_set,
                            const char *func_cmd_first_set_str,
                            Bmc_info_system_info func_cmd,
                            const char *func_cmd_str,
                            const char *string_prefix)
{
  fiid_obj_t obj_cmd_first_set_rs = NULL;
  fiid_obj_t obj_cmd_rs = NULL;
  uint8_t encoding, string_length;
  char string[BMC_INFO_SYSTEM_INFO_STRING_MAX];
  uint64_t val;
  uint8_t set_selector = 0;
  unsigned int string_count = 0;
  unsigned int orig_flags = 0;
  int ret, len;
  int rv = -1;

  assert (state_data);
  assert (tmpl_cmd_first_set);
  assert (tmpl_cmd);
  assert (func_cmd_first_set);
  assert (func_cmd_first_set_str);
  assert (func_cmd);
  assert (func_cmd_str);
  assert (string_prefix);

  assert (state_data);

  memset (string, '\0', BMC_INFO_SYSTEM_INFO_STRING_MAX);

  if (!(obj_cmd_first_set_rs = fiid_obj_create (tmpl_cmd_first_set)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  /* IPMI Workaround
   *
   * Bull 510 Blade
   *
   * The first call to retrieve the Operating System name does not
   * return a set_selector, encoding, or string_length.  If it does
   * not, we assume the string is empty.
   */

  if (ipmi_ctx_get_flags (state_data->ipmi_ctx, &orig_flags) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_ctx_get_flags: %s\n",
                       ipmi_ctx_errormsg (state_data->ipmi_ctx));
      goto cleanup;
    }

  if (ipmi_ctx_set_flags (state_data->ipmi_ctx, orig_flags | IPMI_FLAGS_NO_VALID_CHECK) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_ctx_set_flags: %s\n",
                       ipmi_ctx_errormsg (state_data->ipmi_ctx));
      goto cleanup;
    }

  if (func_cmd_first_set (state_data->ipmi_ctx,
                          IPMI_GET_SYSTEM_INFO_PARAMETER,
                          set_selector,
                          IPMI_SYSTEM_INFO_PARAMETERS_NO_BLOCK_SELECTOR,
                          obj_cmd_first_set_rs) < 0)
    {
      if ((ipmi_ctx_errnum (state_data->ipmi_ctx) == IPMI_ERR_COMMAND_INVALID_OR_UNSUPPORTED
	   && (ipmi_check_completion_code (obj_cmd_first_set_rs,
					   IPMI_COMP_CODE_INVALID_COMMAND) == 1
	       || ipmi_check_completion_code (obj_cmd_first_set_rs,
					      IPMI_COMP_CODE_INVALID_DATA_FIELD_IN_REQUEST) == 1))
	  || (ipmi_ctx_errnum (state_data->ipmi_ctx) == IPMI_ERR_BAD_COMPLETION_CODE
	      && ipmi_check_completion_code (obj_cmd_first_set_rs,
					     IPMI_COMP_CODE_GET_SYSTEM_INFO_PARAMETERS_PARAMETER_NOT_SUPPORTED) == 1))
        {
          rv = 0;
          goto cleanup;
        }
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "%s: %s\n",
                       func_cmd_first_set_str,
                       ipmi_ctx_errormsg (state_data->ipmi_ctx));
      goto cleanup;
    }
  
  if ((ret = fiid_obj_get (obj_cmd_first_set_rs,
			   "encoding",
			   &val)) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'encoding': %s\n",
                       fiid_obj_errormsg (obj_cmd_first_set_rs));
      goto cleanup;
    }
  
  if (!ret)
    goto output;

  encoding = val;
  
  if ((ret = fiid_obj_get (obj_cmd_first_set_rs,
			   "string_length",
			   &val)) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get: 'string_length': %s\n",
                       fiid_obj_errormsg (obj_cmd_first_set_rs));
      goto cleanup;
    }

  if (!ret)
    goto output;

  string_length = val;

  /* no string */
  if (!string_length)
    goto output;

  if ((len = fiid_obj_get_data (obj_cmd_first_set_rs,
                                "string",
                                string + string_count,
                                BMC_INFO_SYSTEM_INFO_STRING_MAX - string_count)) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get_data: 'string': %s\n",
                       fiid_obj_errormsg (obj_cmd_first_set_rs));
      goto cleanup;
    }
  string_count += len;

  /* string_length is 8 bits, so we should not call >= 17 times,
   *
   * ceiling ( (255 - 14) / 16 ) + 1 = 17
   *
   */

  set_selector++;
  while (string_count < string_length && set_selector < 17)
    {
      if (fiid_obj_clear (obj_cmd_rs) < 0)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "fiid_obj_clear: %s\n", 
                           fiid_obj_errormsg (obj_cmd_rs));
          goto cleanup;
        }
      
      if (func_cmd (state_data->ipmi_ctx,
                    IPMI_GET_SYSTEM_INFO_PARAMETER,
                    set_selector,
                    IPMI_SYSTEM_INFO_PARAMETERS_NO_BLOCK_SELECTOR,
                    obj_cmd_rs) < 0)
        {
          if ((ipmi_ctx_errnum (state_data->ipmi_ctx) == IPMI_ERR_COMMAND_INVALID_OR_UNSUPPORTED
	       && (ipmi_check_completion_code (obj_cmd_rs,
					       IPMI_COMP_CODE_INVALID_COMMAND) == 1
		   || ipmi_check_completion_code (obj_cmd_rs,
						  IPMI_COMP_CODE_INVALID_DATA_FIELD_IN_REQUEST) == 1))
	      || (ipmi_ctx_errnum (state_data->ipmi_ctx) == IPMI_ERR_BAD_COMPLETION_CODE
		  && ipmi_check_completion_code (obj_cmd_rs,
						 IPMI_COMP_CODE_GET_SYSTEM_INFO_PARAMETERS_PARAMETER_NOT_SUPPORTED) == 1))
            {
              rv = 0;
              goto cleanup;
            }

          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "%s: %s\n",
                           func_cmd_str,
                           ipmi_ctx_errormsg (state_data->ipmi_ctx));
          goto cleanup;
        }

      if ((len = fiid_obj_get_data (obj_cmd_rs,
                                    "string",
                                    string + string_count,
                                    BMC_INFO_SYSTEM_INFO_STRING_MAX - string_count)) < 0)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "fiid_obj_get_data: 'string': %s\n",
                           fiid_obj_errormsg (obj_cmd_rs));
          goto cleanup;
        }
      string_count += len;
      set_selector++;
    }


 output:
  
  /* XXX: assume ascii, or if not, user has set locale properly?? */
  pstdout_printf (state_data->pstate,
                  "%s %s\n",
                  string_prefix,
                  string);

  rv = 1;
 cleanup:
  if (ipmi_ctx_set_flags (state_data->ipmi_ctx, orig_flags) < 0)
    pstdout_fprintf (state_data->pstate,
		     stderr,
		     "ipmi_ctx_set_flags: %s\n",
		     ipmi_ctx_errormsg (state_data->ipmi_ctx));
  fiid_obj_destroy (obj_cmd_first_set_rs);
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);

}

/* return 1 if supported, 0 if not */
static int
display_system_info_system_firmware_version (bmc_info_state_data_t *state_data)
{
  assert (state_data);

  return display_system_info_common (state_data,
                                     tmpl_cmd_get_system_info_parameters_system_firmware_version_first_set_rs,
                                     tmpl_cmd_get_system_info_parameters_system_firmware_version_rs,
                                     ipmi_cmd_get_system_info_parameters_system_firmware_version_first_set,
                                     "ipmi_cmd_get_system_info_parameters_system_firmware_version_first_set",
                                     ipmi_cmd_get_system_info_parameters_system_firmware_version,
                                     "ipmi_cmd_get_system_info_parameters_system_firmware_version",
                                     "System Firmware Version       :");
}

/* return 1 if supported, 0 if not */
static int
display_system_info_system_name (bmc_info_state_data_t *state_data)
{
  assert (state_data);

  return display_system_info_common (state_data,
                                     tmpl_cmd_get_system_info_parameters_system_name_first_set_rs,
                                     tmpl_cmd_get_system_info_parameters_system_name_rs,
                                     ipmi_cmd_get_system_info_parameters_system_name_first_set,
                                     "ipmi_cmd_get_system_info_parameters_system_name_first_set",
                                     ipmi_cmd_get_system_info_parameters_system_name,
                                     "ipmi_cmd_get_system_info_parameters_system_name",
                                     "System Name                   :");
}

/* return 1 if supported, 0 if not */
static int
display_system_info_primary_operating_system_name (bmc_info_state_data_t *state_data)
{
  assert (state_data);

  return display_system_info_common (state_data,
                                     tmpl_cmd_get_system_info_parameters_primary_operating_system_name_first_set_rs,
                                     tmpl_cmd_get_system_info_parameters_primary_operating_system_name_rs,
                                     ipmi_cmd_get_system_info_parameters_primary_operating_system_name_first_set,
                                     "ipmi_cmd_get_system_info_parameters_primary_operating_system_name_first_set",
                                     ipmi_cmd_get_system_info_parameters_primary_operating_system_name,
                                     "ipmi_cmd_get_system_info_parameters_primary_operating_system_name",
                                     "Primary Operating System Name :");
}

/* return 1 if supported, 0 if not */
static int
display_system_info_operating_system_name (bmc_info_state_data_t *state_data)
{
  assert (state_data);

  return display_system_info_common (state_data,
                                     tmpl_cmd_get_system_info_parameters_operating_system_name_first_set_rs,
                                     tmpl_cmd_get_system_info_parameters_operating_system_name_rs,
                                     ipmi_cmd_get_system_info_parameters_operating_system_name_first_set,
                                     "ipmi_cmd_get_system_info_parameters_operating_system_name_first_set",
                                     ipmi_cmd_get_system_info_parameters_operating_system_name,
                                     "ipmi_cmd_get_system_info_parameters_operating_system_name",
                                     "Operating System Name         :");
}

/* return 1 if supported, 0 if not */
static int
display_system_info_present_os_version_number (bmc_info_state_data_t *state_data)
{
  assert (state_data);

  return display_system_info_common (state_data,
                                     tmpl_cmd_get_system_info_parameters_present_os_version_number_first_set_rs,
                                     tmpl_cmd_get_system_info_parameters_present_os_version_number_rs,
                                     ipmi_cmd_get_system_info_parameters_present_os_version_number_first_set,
                                     "ipmi_cmd_get_system_info_parameters_present_os_version_number_first_set",
                                     ipmi_cmd_get_system_info_parameters_present_os_version_number,
                                     "ipmi_cmd_get_system_info_parameters_present_os_version_number",
                                     "Present OS Version Number     :");
}

/* return 1 if supported, 0 if not */
static int
display_system_info_bmc_url (bmc_info_state_data_t *state_data)
{
  assert (state_data);

  return display_system_info_common (state_data,
                                     tmpl_cmd_get_system_info_parameters_bmc_url_first_set_rs,
                                     tmpl_cmd_get_system_info_parameters_bmc_url_rs,
                                     ipmi_cmd_get_system_info_parameters_bmc_url_first_set,
                                     "ipmi_cmd_get_system_info_parameters_bmc_url_first_set",
                                     ipmi_cmd_get_system_info_parameters_bmc_url,
                                     "ipmi_cmd_get_system_info_parameters_bmc_url",
                                     "BMC URL                       :");
}

/* return 1 if supported, 0 if not */
static int
display_system_info_base_os_hypervisor_url (bmc_info_state_data_t *state_data)
{
  assert (state_data);

  return display_system_info_common (state_data,
                                     tmpl_cmd_get_system_info_parameters_base_os_hypervisor_url_first_set_rs,
                                     tmpl_cmd_get_system_info_parameters_base_os_hypervisor_url_rs,
                                     ipmi_cmd_get_system_info_parameters_base_os_hypervisor_url_first_set,
                                     "ipmi_cmd_get_system_info_parameters_base_os_hypervisor_url_first_set",
                                     ipmi_cmd_get_system_info_parameters_base_os_hypervisor_url,
                                     "ipmi_cmd_get_system_info_parameters_base_os_hypervisor_url",
                                     "Base OS/Hypervisor URL        :");
}

static int
display_system_info (bmc_info_state_data_t *state_data)
{
  int ret;

  assert (state_data);

  if ((ret = display_system_info_system_firmware_version (state_data)) < 0)
    return (-1);

  if (!ret)
    return (0);

  if ((ret = display_system_info_system_name (state_data)) < 0)
    return (-1);

  if (!ret)
    goto newline_cleanup;

  if ((ret = display_system_info_primary_operating_system_name (state_data)) < 0)
    return (-1);

  if (!ret)
    goto newline_cleanup;

  if ((ret = display_system_info_operating_system_name (state_data)) < 0)
    return (-1);

  if (!ret)
    goto newline_cleanup;

  /* New, may not be supported */
  if ((ret = display_system_info_present_os_version_number (state_data)) < 0)
    return (-1);

  if (!ret)
    goto newline_cleanup;

  /* optional - if ret == 0, can still go on */
  if ((ret = display_system_info_bmc_url (state_data)) < 0)
    return (-1);

  /* optional - if ret == 0, can still go on */
  if ((ret = display_system_info_base_os_hypervisor_url (state_data)) < 0)
    return (-1);

 newline_cleanup:
  /* output newline if we're outputting all sections */
  if (!state_data->prog_data->args->get_system_info)
    pstdout_printf (state_data->pstate, "\n");

  return (0);
}

static int
get_channel_info_list (bmc_info_state_data_t *state_data, channel_info_t *channel_info_list)
{
  fiid_obj_t obj_cmd_rs = NULL;
  unsigned int i;
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

  for (i = IPMI_CHANNEL_NUMBER_PRIMARY_IPMB, ci = 0;
       i <= IPMI_CHANNEL_NUMBER_IMPLEMENTATION_SPECIFIC_MAX; i++)
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
  channel_info_t channel_info_list[IPMI_CHANNEL_NUMBERS_MAX];
  int first_newline_output = 0;
  unsigned int i;

  assert (state_data);

  memset (channel_info_list, '\0', sizeof (channel_info_t) * IPMI_CHANNEL_NUMBERS_MAX);
  if (get_channel_info_list (state_data, channel_info_list) < 0)
    return (-1);

  if (!state_data->prog_data->args->get_channel_info)
    pstdout_printf (state_data->pstate, "Channel Information\n");

  for (i = 0; i < IPMI_CHANNEL_NUMBERS_MAX; i++)
    {
      char *medium_type_str = NULL;
      char *protocol_type_str = NULL;
      char *session_support_str = NULL;
      char iana_buf[BMC_INFO_IANA_STRING_MAX + 1];
      int ret;
      
      memset (iana_buf, '\0', BMC_INFO_IANA_STRING_MAX + 1);

      if (IPMI_CHANNEL_MEDIUM_TYPE_IS_RESERVED (channel_info_list[i].channel_medium_type))
        continue;

      switch (channel_info_list[i].channel_medium_type)
	{
	case IPMI_CHANNEL_MEDIUM_TYPE_IPMB:
	  medium_type_str = "IPMB (I2C)";
	  break;
	case IPMI_CHANNEL_MEDIUM_TYPE_ICMB_10:
	  medium_type_str = "ICMB v1.0";
	  break;
	case IPMI_CHANNEL_MEDIUM_TYPE_ICMB_09:
	  medium_type_str = "ICMB v0.9";
	  break;
	case IPMI_CHANNEL_MEDIUM_TYPE_LAN_802_3:
	  medium_type_str = "802.3 LAN";
	  break;
	case IPMI_CHANNEL_MEDIUM_TYPE_RS232:
	  medium_type_str = "Asynch. Serial/Modem (RS-232)";
	  break;
	case IPMI_CHANNEL_MEDIUM_TYPE_OTHER_LAN:
	  medium_type_str = "Other LAN";
	  break;
	case IPMI_CHANNEL_MEDIUM_TYPE_PCI_SMBUS:
	  medium_type_str = "PCI SMBus";
	  break;
	case IPMI_CHANNEL_MEDIUM_TYPE_SMBUS_10_11:
	  medium_type_str = "SMBus v1.0/1.1";
	  break;
	case IPMI_CHANNEL_MEDIUM_TYPE_SMBUS_20:
	  medium_type_str = "SMBus v2.0";
	  break;
	case IPMI_CHANNEL_MEDIUM_TYPE_USB_1X:
	  medium_type_str = "USB 1.x";
	  break;
	case IPMI_CHANNEL_MEDIUM_TYPE_USB_2X:
	  medium_type_str = "USB 2.x";
	  break;
	case IPMI_CHANNEL_MEDIUM_TYPE_SYSTEM_INTERFACE:
	  medium_type_str = "System Interface (KCS, SMIC, or BT)";
	  break;
	default:
	  if (IPMI_CHANNEL_MEDIUM_TYPE_IS_OEM (channel_info_list[i].channel_medium_type))
	    medium_type_str = "OEM";
	  else
	    medium_type_str = "unknown";
	}

      switch (channel_info_list[i].channel_protocol_type)
	{
	case IPMI_CHANNEL_PROTOCOL_TYPE_IPMB:
	  protocol_type_str = "IPMB-1.0";
	  break;
	case IPMI_CHANNEL_PROTOCOL_TYPE_ICMB_10:
	  protocol_type_str = "ICMB-1.0";
	  break;
	case IPMI_CHANNEL_PROTOCOL_TYPE_SMBUS_1X_2X:
	  protocol_type_str = "IPMI-SMBus";
	  break;
	case IPMI_CHANNEL_PROTOCOL_TYPE_KCS:
	  protocol_type_str = "KCS";
	  break;
	case IPMI_CHANNEL_PROTOCOL_TYPE_SMIC:
	  protocol_type_str = "SMIC";
	  break;
	case IPMI_CHANNEL_PROTOCOL_TYPE_BT_10:
	  protocol_type_str = "BT-10";
	  break;
	case IPMI_CHANNEL_PROTOCOL_TYPE_BT_15:
	  protocol_type_str = "BT-15";
	  break;
	case IPMI_CHANNEL_PROTOCOL_TYPE_TMODE:
	  protocol_type_str = "TMODE";
	  break;
	default:
	  if (IPMI_CHANNEL_PROTOCOL_TYPE_IS_RESERVED (channel_info_list[i].channel_protocol_type))
	    protocol_type_str = "Reserved";
	  else if (IPMI_CHANNEL_PROTOCOL_TYPE_IS_OEM (channel_info_list[i].channel_protocol_type))
	    protocol_type_str = "OEM";
	  else
	    protocol_type_str = "unknown";
	}
      
      switch (channel_info_list[i].session_support)
	{
	case IPMI_SESSION_SUPPORT_SESSION_LESS:
	  session_support_str = "session-less";
	  break;
	case IPMI_SESSION_SUPPORT_SINGLE_SESSION:
	  session_support_str = "single-session";
	  break;
	case IPMI_SESSION_SUPPORT_MULTI_SESSION:
	  session_support_str = "multi-session";
	  break;
	case IPMI_SESSION_SUPPORT_SESSION_BASED:
	  session_support_str = "session-based";
	  break;
	default:
	  session_support_str = "unknown";
	}
        
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

      /* if ret == 0 means no string, < 0 means bad manufacturer id
       * either way, output just the number
       */
      ret = ipmi_iana_enterprise_numbers_string (channel_info_list[i].vendor_id,
                                                 iana_buf,
                                                 BMC_INFO_IANA_STRING_MAX);
      
      if (ret > 0)
        pstdout_printf (state_data->pstate,
                        "Vendor ID            : %s (%u)\n",
                        iana_buf,
                        channel_info_list[i].vendor_id);
      else
        pstdout_printf (state_data->pstate,
                        "Vendor ID             : %u\n",
                        channel_info_list[i].vendor_id);

      first_newline_output++;
    }

  /* don't output, we're the last output when we output "all" */
#if 0
  if (!state_data->prog_data->args->get_channel_info)
    pstdout_printf (state_data->pstate, "\n");
#endif

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

  if (args->get_system_guid)
    return (display_get_system_guid (state_data));
 
  if (args->get_system_info)
    return (display_system_info (state_data));

  if (args->get_channel_info)
    return (display_channel_info (state_data));

  /* else display all */

  if (display_get_device_id (state_data) < 0)
    goto cleanup;

  if (display_get_device_guid (state_data) < 0)
    goto cleanup;

  if (display_get_system_guid (state_data) < 0)
    goto cleanup;

  if (display_system_info (state_data) < 0)
    goto cleanup;

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
  int exit_code = EXIT_FAILURE;

  assert (pstate);
  assert (arg);

  prog_data = (bmc_info_prog_data_t *)arg;
  memset (&state_data, '\0', sizeof (bmc_info_state_data_t));

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
  bmc_info_prog_data_t prog_data;
  struct bmc_info_arguments cmd_args;
  int hosts_count;
  int rv;

  ipmi_disable_coredump ();

  memset (&prog_data, '\0', sizeof (bmc_info_prog_data_t));
  prog_data.progname = argv[0];
  bmc_info_argp_parse (argc, argv, &cmd_args);
  prog_data.args = &cmd_args;

  if ((hosts_count = pstdout_setup (&(prog_data.args->common_args.hostname),
                                    &(prog_data.args->common_args))) < 0)
    return (EXIT_FAILURE);

  if (!hosts_count)
    return (EXIT_SUCCESS);

  if ((rv = pstdout_launch (prog_data.args->common_args.hostname,
                            _bmc_info,
                            &prog_data)) < 0)
    {
      fprintf (stderr,
               "pstdout_launch: %s\n",
               pstdout_strerror (pstdout_errnum));
      return (EXIT_FAILURE);
    }

  return (rv);
}
