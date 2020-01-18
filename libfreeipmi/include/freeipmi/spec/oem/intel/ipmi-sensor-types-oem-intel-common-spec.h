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

#ifndef IPMI_SENSOR_TYPES_OEM_INTEL_COMMON_SPEC_H
#define IPMI_SENSOR_TYPES_OEM_INTEL_COMMON_SPEC_H

#ifdef __cplusplus
extern "C" {
#endif

/*
 * Intel S5500WB/Penguin Computing Relion 700
 * Intel SR1625
 * Quanta QSSC-S4R/Appro GB812X-CN
 * (Quanta motherboard contains Intel manufacturer ID)
 */

#define IPMI_SENSOR_TYPE_OEM_INTEL_SMI_TIMEOUT                  0xF3

/*
 * Intel S2600KP
 * Intel S2600WT2
 * Intel S2600WTT
 * Intel S2600GZ
 * Intel S2600BPB
 */

#define IPMI_SENSOR_TYPE_OEM_INTEL_IERR_RECOVERY_DUMP_INFO 0xD1

#ifdef __cplusplus
}
#endif

#endif /* IPMI_SENSOR_TYPES_OEM_INTEL_COMMON_SPEC_H */
