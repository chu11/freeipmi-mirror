/*
   Copyright (C) 2003-2008 FreeIPMI Core Team

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software Foundation,
   Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA.
*/

#ifndef _IPMI_SYSTEM_SOFTWARE_ID_SPEC_H
#define	_IPMI_SYSTEM_SOFTWARE_ID_SPEC_H

#ifdef __cplusplus
extern "C" {
#endif

/* Table 5-4 */

/* To avoid gcc warnings, added +1 and -1 in comparison */
#define IPMI_SYSTEM_SOFTWARE_TYPE_IS_BIOS(__val) \
        (((__val + 1) >= 0x01  \
          && (__val - 1) <= 0x0E) ? 1 : 0)

#define IPMI_SYSTEM_SOFTWARE_TYPE_IS_SMI_HANDLER(__val) \
        (((__val) >= 0x10  \
          && (__val) <= 0x1F) ? 1 : 0)

#define IPMI_SYSTEM_SOFTWARE_TYPE_IS_SYSTEM_MANAGEMENT_SOFTWARE(__val) \
        (((__val) >= 0x20  \
          && (__val) <= 0x2F) ? 1 : 0)

#define IPMI_SYSTEM_SOFTWARE_TYPE_IS_OEM(__val) \
        (((__val) >= 0x30  \
          && (__val) <= 0x3F) ? 1 : 0)

#define IPMI_SYSTEM_SOFTWARE_TYPE_IS_REMOTE_CONSOLE_SOFTWARE(__val) \
        (((__val) >= 0x40  \
          && (__val) <= 0x46) ? 1 : 0)

#define IPMI_SYSTEM_SOFTWARE_TYPE_IS_TERMINAL_MODE_REMOTE_CONSOLE_SOFTWARE(__val) \
        (((__val) == 0x47)) ? 1 : 0)

#define IPMI_SYSTEM_SOFTWARE_TYPE_IS_RESERVED(__val) \
        (((__val) > 0x47) ? 1 : 0)

#ifdef __cplusplus
}
#endif

#endif /* ipmi-system-software-id-spec.h */


