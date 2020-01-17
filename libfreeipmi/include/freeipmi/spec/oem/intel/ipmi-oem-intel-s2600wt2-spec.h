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

#ifndef IPMI_OEM_INTEL_S2600WT2_SPEC_H
#define IPMI_OEM_INTEL_S2600WT2_SPEC_H

#ifdef __cplusplus
extern "C" {
#endif

/*
 * Intel S2600WT2
 */

/* w/ IPMI_CMD_OEM_INTEL_S2600WT2_GET_BMC_SERVICE_STATUS / IPMI_CMD_OEM_INTEL_S2600WT2_CONTROL_BMC_SERVICES */

#define IPMI_OEM_INTEL_S2600WT2_DISABLE_SERVICES 0x00
#define IPMI_OEM_INTEL_S2600WT2_ENABLE_SERVICES  0x01

#define IPMI_OEM_INTEL_S2600WT2_STANDARD_SERVICES_SSH  0x80
#define IPMI_OEM_INTEL_S2600WT2_STANDARD_SERVICES_HTTP 0x20
#define IPMI_OEM_INTEL_S2600WT2_STANDARD_SERVICES_RMCP 0x08

#define IPMI_OEM_INTEL_S2600WT2_OEM_SPECIFIC_SERVICES_KVM 0x80

#ifdef __cplusplus
}
#endif

#endif /* IPMI_OEM_INTEL_S2600WT2_SPEC_H */
