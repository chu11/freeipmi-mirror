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

#ifndef IPMI_SENSOR_NUMBERS_OEM_INTEL_WINDMILL_SPEC_H
#define IPMI_SENSOR_NUMBERS_OEM_INTEL_WINDMILL_SPEC_H

#ifdef __cplusplus
extern "C" {
#endif

/*
 * Intel Windmill
 * (Quanta Winterfell)
 * (Wiwynn Windmill)
 */
#define IPMI_SENSOR_NUMBER_OEM_INTEL_WINDMILL_ME_FW_HEALTH_SENSOR                       0x17
#define IPMI_SENSOR_NUMBER_OEM_INTEL_WINDMILL_PROC_HOT_EXTENDED_SENSOR                  0x3C
#define IPMI_SENSOR_NUMBER_OEM_INTEL_WINDMILL_MEM_HOT_EXTENDED_SENSOR                   0x3D
#define IPMI_SENSOR_NUMBER_OEM_INTEL_WINDMILL_MACHINE_CHECK_ERROR_SENSOR                0x40
#define IPMI_SENSOR_NUMBER_OEM_INTEL_WINDMILL_PCIE_ERROR_SENSOR                         0x41
#define IPMI_SENSOR_NUMBER_OEM_INTEL_WINDMILL_POST_ERROR_SENSOR                         0x2B
#define IPMI_SENSOR_NUMBER_OEM_INTEL_WINDMILL_OTHER_IIO_ERROR_SENSOR                    0x43
#define IPMI_SENSOR_NUMBER_OEM_INTEL_WINDMILL_CPU_SEL_STATUS                            0x5F
#define IPMI_SENSOR_NUMBER_OEM_INTEL_WINDMILL_MEMORY_ECC_ERROR                          0x63
#define IPMI_SENSOR_NUMBER_OEM_INTEL_WINDMILL_CHASSIS_POWER_STATUS                      0x70
#define IPMI_SENSOR_NUMBER_OEM_INTEL_WINDMILL_HOT_SWAP_CONTROLLER_0_STATUS_LOW          0x28
#define IPMI_SENSOR_NUMBER_OEM_INTEL_WINDMILL_HOT_SWAP_CONTROLLER_0_STATUS_HIGH         0x42
#define IPMI_SENSOR_NUMBER_OEM_INTEL_WINDMILL_HOT_SWAP_CONTROLLER_0_STATUS_MFR_SPECIFIC 0x5E
#define IPMI_SENSOR_NUMBER_OEM_INTEL_WINDMILL_HOT_SWAP_CONTROLLER_0_STATUS_INPUT        0x9F

#ifdef __cplusplus
}
#endif

#endif /* IPMI_SENSOR_NUMBERS_OEM_INTEL_WINDMILL_SPEC_H */
