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

#ifndef IPMI_SENSOR_NUMBERS_OEM_INTEL_S2600BPB_SPEC_H
#define IPMI_SENSOR_NUMBERS_OEM_INTEL_S2600BPB_SPEC_H

#ifdef __cplusplus
extern "C" {
#endif

#include <freeipmi/spec/oem/intel/ipmi-sensor-numbers-oem-intel-common-spec.h>

/*
 * Intel S2600BPB
 */

/* BMC owned sensors (GID = 0020h) */
#define IPMI_SENSOR_NUMBER_OEM_INTEL_S2600BPB_FIRMWARE_SECURITY 0x1A
#define IPMI_SENSOR_NUMBER_OEM_INTEL_S2600BPB_NVME1_THERM_MGN   0x91
#define IPMI_SENSOR_NUMBER_OEM_INTEL_S2600BPB_NVME2_THERM_MGN   0x92
#define IPMI_SENSOR_NUMBER_OEM_INTEL_S2600BPB_NVME3_THERM_MGN   0x93
#define IPMI_SENSOR_NUMBER_OEM_INTEL_S2600BPB_NVME1_CRIT_WARN   0x94
#define IPMI_SENSOR_NUMBER_OEM_INTEL_S2600BPB_NVME2_CRIT_WARN   0x95
#define IPMI_SENSOR_NUMBER_OEM_INTEL_S2600BPB_NVME3_CRIT_WARN   0x96
#define IPMI_SENSOR_NUMBER_OEM_INTEL_S2600BPB_BAD_USE_PWD       0xD7
#define IPMI_SENSOR_NUMBER_OEM_INTEL_S2600BPB_KCS_POLICY        0xDA
#define IPMI_SENSOR_NUMBER_OEM_INTEL_S2600BPB_REMOTE_DEBUG      0xDB

#ifdef __cplusplus
}
#endif

#endif /* IPMI_SENSOR_NUMBERS_OEM_INTEL_S2600BPB_SPEC_H */
