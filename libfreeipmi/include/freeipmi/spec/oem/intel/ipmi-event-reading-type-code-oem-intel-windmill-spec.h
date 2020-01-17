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

#ifndef IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_WINDMILL_SPEC_H
#define IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_WINDMILL_SPEC_H

#ifdef __cplusplus
extern "C" {
#endif

/*
 * Intel Windmill
 * (Quanta Winterfell)
 * (Wiwynn Windmill)
 */
/* achu: Like Intel Node Manager, but no Intel Node Manager on these nodes, so cut & paste to differentiate */
#define IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_WINDMILL_ME_FW_HEALTH_SENSOR    0x75
#define IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_WINDMILL_OTHER_IIO_ERROR_SENSOR 0x70

#ifdef __cplusplus
}
#endif

#endif /* IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_WINDMILL_SPEC_H */
