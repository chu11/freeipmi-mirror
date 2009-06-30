/*
   Copyright (C) 2003-2009 FreeIPMI Core Team

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

#ifndef _IPMI_SYSTEM_INFO_PARAMETER_SPEC_H
#define _IPMI_SYSTEM_INFO_PARAMETER_SPEC_H

#ifdef __cplusplus
extern "C" {
#endif

#define IPMI_SYSTEM_INFO_PARAMETER_SET_IN_PROGRESS                  0x0
#define IPMI_SYSTEM_INFO_PARAMETER_SYSTEM_FIRMWARE_VERSION          0x1
#define IPMI_SYSTEM_INFO_PARAMETER_SYSTEM_NAME                      0x2
#define IPMI_SYSTEM_INFO_PARAMETER_PRIMARY_OPERATING_SYSTEM_NAME    0x3
#define IPMI_SYSTEM_INFO_PARAMETER_OPERATING_SYSTEM_NAME            0x4

/* Add +1 to avoid compiler warnings */
#define IPMI_SYSTEM_INFO_PARAMETER_SELECTOR_VALID(__parameter_selector)              \
  ((((__parameter_selector+1)) > (IPMI_SYSTEM_INFO_PARAMETER_SET_IN_PROGRESS+1) && \
    (__parameter_selector) <= IPMI_SYSTEM_INFO_PARAMETER_OPERATING_SYSTEM_NAME) ? 1 : 0)

/* To avoid gcc warnings, subtract -1 in comparison */
#define IPMI_SYSTEM_INFO_PARAMETER_SELECTOR_IS_OEM(__parameter_selector) \
  (((__parameter_selector) >= 196 && (__parameter_selector - 1) <= (255 - 1)) ? 1 : 0)

#define IPMI_SYSTEM_INFO_NO_SET_SELECTOR                                    0x0
#define IPMI_SYSTEM_INFO_NO_BLOCK_SELECTOR                                  0x0

#ifdef __cplusplus
}
#endif
#endif /* _IPMI_SYSTEM_INFO_PARAMETER_SPEC_H */
