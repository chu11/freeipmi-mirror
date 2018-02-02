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

#ifndef IPMI_CMD_OEM_GIGABYTE_SPEC_H
#define IPMI_CMD_OEM_GIGABYTE_SPEC_H

#ifdef __cplusplus
extern "C" {
#endif

/*
 * GIGABYTE MD90-FS0-ZB
 */
/* IPMI_NET_FN_OEM_GROUP_RQ / IPMI_NET_FN_OEM_GROUP_RS */
/* achu: not official names, named based on use context */
#define IPMI_CMD_OEM_GIGABYTE_CONFIGURATION 0xCC

/* IPMI_NET_FN_OEM_GROUP_RQ / IPMI_NET_FN_OEM_GROUP_RS */
/* achu: not official names, named based on use context */
#define IPMI_CMD_OEM_GIGABYTE_PORT_CONFIGURATION  0x20
#define IPMI_CMD_OEM_GIGABYTE_PORT_CONFIGURATION2 0x21

#ifdef __cplusplus
}
#endif

#endif /* IPMI_CMD_OEM_GIGABYTE_SPEC_H */
