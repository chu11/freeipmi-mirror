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

#ifndef IPMI_OEM_GIGABYTE_SPEC_H
#define IPMI_OEM_GIGABYTE_SPEC_H

#ifdef __cplusplus
extern "C" {
#endif

/*
 * GIGABYTE MD90-FS0-ZB
 */

/* w/ IPMI_CMD_OEM_GIGABYTE_CONFIGURATION */
#define IPMI_OEM_GIGABYTE_NIC_MODE_DEDICATED   0x01
#define IPMI_OEM_GIGABYTE_NIC_MODE_SHARED      0x02
#define IPMI_OEM_GIGABYTE_NIC_MODE_FAILOVER    0x03

/* w/ IPMI_CMD_OEM_GIGABYTE_PORT_CONFIGURATION */
#define IPMI_OEM_GIGABYTE_BLOCK_PORT   0
#define IPMI_OEM_GIGABYTE_UNBLOCK_PORT 1

/* w/ IPMI_CMD_OEM_GIGABYTE_PORT_CONFIGURATION2 */
#define IPMI_OEM_GIGABYTE_PORT_DISABLED 0
#define IPMI_OEM_GIGABYTE_PORT_ENABLED  1

#ifdef __cplusplus
}
#endif

#endif /* IPMI_OEM_GIGABYTE_SPEC_H */
