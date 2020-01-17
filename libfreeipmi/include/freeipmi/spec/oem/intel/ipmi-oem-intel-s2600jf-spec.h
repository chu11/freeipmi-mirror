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

#ifndef IPMI_OEM_INTEL_S2600JF_SPEC_H
#define IPMI_OEM_INTEL_S2600JF_SPEC_H

#ifdef __cplusplus
extern "C" {
#endif

#include <freeipmi/spec/oem/intel/ipmi-oem-intel-common-spec.h>

/*
 * Intel S2600JF/Appro 512X
 */

/* w/ IPMI_CMD_OEM_INTEL_S2600JF_SET_POWER_RESTORE_DELAY / IPMI_CMD_OEM_INTEL_S2600JF_GET_POWER_RESTORE_DELAY */
#define IPMI_OEM_INTEL_S2600JF_POWER_RESTORE_DELAY_MSB_MASK 0x07
#define IPMI_OEM_INTEL_S2600JF_POWER_RESTORE_DELAY_LSB_MASK 0xFF

#define IPMI_OEM_INTEL_S2600JF_POWER_RESTORE_DELAY_MAX 0x07FF

#ifdef __cplusplus
}
#endif

#endif /* IPMI_OEM_INTEL_S2600JF_SPEC_H */
