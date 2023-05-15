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

#ifndef IPMI_DEVICE_TYPES_UTIL_H
#define IPMI_DEVICE_TYPES_UTIL_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>

/* return length of string written into buffer on success, -1 on error */
int ipmi_device_type_modifier_message (uint8_t device_type,
                                       uint8_t device_modifier,
                                       char *buf,
                                       unsigned int buflen);

#ifdef __cplusplus
}
#endif

#endif /* IPMI_DEVICE_TYPES_UTIL_H */
