/*
 * Copyright (C) 2003-2015 FreeIPMI Core Team
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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#ifdef STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <sys/types.h>
#include <assert.h>
#include <errno.h>

#include "freeipmi/spec/oem/ipmi-sensor-and-event-code-tables-oem-intel-spec.h"
#include "freeipmi/fiid/fiid.h"

#include "libcommon/ipmi-fiid-util.h"
#include "libcommon/ipmi-trace.h"

#include "freeipmi-portability.h"

/*
 * Intel S2600BPB
 */

const char * const ipmi_oem_intel_s2600bpb_specific_remote_debug[] =
  {
    "Remote JTAG Consent",
    "Remote JTAG Enabled",
    "Remote JTAG Session",
    "Remote PECI Enabled",
    "Remote PECI Session",
    NULL
  };
unsigned int ipmi_oem_intel_s2600bpb_specific_remote_debug_max_index = 0x04;

const char * const ipmi_oem_intel_s2600bpb_specific_system_firmware_security[] =
  {
    "Authentication Failure of BMC Firmware Image During Boot",
    "Invalid Security Revision of BMC Firmware Image During Boot",
    "Authentication Failure of BMC Firmware Image During Update",
    "Invalid Security Revision of BMC Firmware Image During Update",
    "Authentication Failure of Signed Region During Update",
    "Authentication Failure of Signed Region During Boot or Runtime",
    "Invalid Security Revision of Signed Region During Update",
    "Invalid Security Revision of Signed Region During Boot or Runtime",
    "reserved",
    "reserved",
    "reserved",
    "reserved",
    "Factory Image Booted",
    "Factory Security Revision Downgraded",
    NULL
  };
unsigned int ipmi_oem_intel_s2600bpb_specific_system_firmware_security_max_index = 0x0D;

const char * const ipmi_oem_intel_s2600bpb_specific_kcs_policy[] =
  {
    "reserved",
    "reserved",
    "reserved",
    "KCS Allow all Mode",
    "KCS Restricted mode",
    "KCS Deny all mode",
    NULL
  };
unsigned int ipmi_oem_intel_s2600bpb_specific_kcs_policy_max_index = 0x05;
