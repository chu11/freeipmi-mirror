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

#ifndef IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_SPEC_H
#define IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_SPEC_H

#ifdef __cplusplus
extern "C" {
#endif

#include <freeipmi/spec/oem/ipmi-event-reading-type-code-oem-intel-s5500wb-spec.h>
#include <freeipmi/spec/oem/ipmi-event-reading-type-code-oem-intel-quanta-qssc-s4r-spec.h>
#include <freeipmi/spec/oem/ipmi-event-reading-type-code-oem-intel-s2600jf-spec.h>
#include <freeipmi/spec/oem/ipmi-event-reading-type-code-oem-intel-s2600wp-spec.h>
#include <freeipmi/spec/oem/ipmi-event-reading-type-code-oem-intel-windmill-spec.h>

/*
 * Intel S2600KP
 * Intel S2600WT2
 * Intel S2600WTT
 * Intel S2600GZ
 */
#define IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_E52600V3_QPI_CORRECTABLE_ERROR          0x72
#define IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_E52600V3_QPI_FATAL_ERROR                0x73
#define IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_E52600V3_QPI_FATAL_ERROR_2              0x74
#define IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_E52600V3_QPI_LINK_WIDTH_REDUCED         0x77

#define IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_E52600V3_PCI_EXPRESS_FATAL_ERRORS       0x70
#define IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_E52600V3_PCI_EXPRESS_FATAL_ERRORS_2     0x76
#define IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_E52600V3_PCI_EXPRESS_CORRECTABLE_ERRORS 0x71

#define IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_E52600V3_FIRMWARE_UPDATE_STATUS_SENSOR  0x70

#define IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_E52600V3_BIOS_RECOVERY_START            0x70
#define IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_E52600V3_BIOS_RECOVERY_FINISH           0xF0

#define IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_E52600V3_IERR_RECOVERY_DUMP_INFO        0x70

#ifdef __cplusplus
}
#endif

#endif /* IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_SPEC_H */
