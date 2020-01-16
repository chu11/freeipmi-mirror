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

#ifndef IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_COMMON_SPEC_H
#define IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_COMMON_SPEC_H

#ifdef __cplusplus
extern "C" {
#endif

/*
 * Intel S2600JF/Appro 512X
 * Intel S2600WP
 */
/* achu: In earlier code S2600JF was "OPI" instead of "QPI".  Typo on
 * my part, or was actually OPI in document from Intel (which also was
 * possibly typoed)
 */
#define IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_PCIE_FATAL_ERROR       0x70
#define IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_PCIE_CORRECTABLE_ERROR 0x71
#define IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_QPI_CORRECTABLE_ERRORS 0x72
#define IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_QPI_FATAL_ERROR        0x73
/* continuation for IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_QPI_FATAL_ERROR for more offsets */
#define IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_QPI_FATAL_ERROR_2      0x74
#define IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_CHIPSET_PROPRIETARY    0x75
/* continuation for IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_PCIE_FATAL_ERROR for more offsets */
#define IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_PCIE_FATAL_ERROR_2     0x76
#define IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_QPI_LINK_WIDTH_REDUCED 0x77
#define IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_MEMORY_ERROR_EXTENSION 0x7F

#ifdef __cplusplus
}
#endif

#endif /* IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_COMMON_SPEC_H */
