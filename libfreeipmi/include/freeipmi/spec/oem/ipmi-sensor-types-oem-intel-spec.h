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

#ifndef IPMI_SENSOR_TYPES_OEM_INTEL_SPEC_H
#define IPMI_SENSOR_TYPES_OEM_INTEL_SPEC_H

#ifdef __cplusplus
extern "C" {
#endif

#include <freeipmi/spec/oem/ipmi-sensor-types-oem-intel-s5500wb-spec.h>
#include <freeipmi/spec/oem/ipmi-sensor-types-oem-intel-sr1625-spec.h>
#include <freeipmi/spec/oem/ipmi-sensor-types-oem-intel-quanta-qssc-s4r-spec.h>

/*
 * Intel S5000PAL
 */
#define IPMI_SENSOR_TYPE_OEM_INTEL_NMI_STATE                    0xC0

/*
 * Intel Windmill
 * (Quanta Winterfell)
 * (Wiwynn Windmill)
 */
#define IPMI_SENSOR_TYPE_OEM_INTEL_WINDMILL_ME_FW_HEALTH_SENSOR 0xDC

/* Used by many sensors */
#define IPMI_SENSOR_TYPE_OEM_INTEL_WINDMILL_GENERIC             0xC0

/*
 * Intel S2600KP
 * Intel S2600WT2
 * Intel S2600WTT
 * Intel S2600GZ
 */
#define IPMI_SENSOR_TYPE_OEM_INTEL_E52600V3_IERR_RECOVERY_DUMP_INFO 0xD1

#ifdef __cplusplus
}
#endif

#endif /* IPMI_SENSOR_TYPES_OEM_INTEL_SPEC_H */
