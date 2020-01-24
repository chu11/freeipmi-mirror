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

#ifndef IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_S2600BPB_SPEC_H
#define IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_S2600BPB_SPEC_H

#ifdef __cplusplus
extern "C" {
#endif

#include <freeipmi/spec/oem/intel/ipmi-event-reading-type-code-oem-intel-common-spec.h>

/*
 * S2600BPB
 */
/* achu: unfortunately conflicts with PCIE_CORRECTABLE_ERROR */
#define IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_S2600BPB_REMOTE_DEBUG             0x71
/* achu: unfortunately conflicts with QPI_FATAL_ERROR */
#define IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_S2600BPB_NVME_CRITICAL_WARNING    0x73
#define IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_S2600BPB_KCS_POLICY               0x7A
#define IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_S2600BPB_SYSTEM_FIRMWARE_SECURITY 0x7B

#ifdef __cplusplus
}
#endif

#endif /* IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_S2600BPB_SPEC_H */
