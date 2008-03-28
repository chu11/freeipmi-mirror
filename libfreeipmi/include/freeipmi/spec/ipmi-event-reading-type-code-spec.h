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

#ifndef _IPMI_EVENT_READING_TYPE_CODE_SPEC_H
#define	_IPMI_EVENT_READING_TYPE_CODE_SPEC_H

#ifdef __cplusplus
extern "C" {
#endif

/* Table 42-1 */

#define IPMI_EVENT_READING_TYPE_CODE_IS_UNSPECIFIED(__val) \
        (((__val) == 0x00) ? 1 : 0)

#define IPMI_EVENT_READING_TYPE_CODE_IS_THRESHOLD(__val) \
        (((__val) == 0x01) ? 1 : 0)

#define IPMI_EVENT_READING_TYPE_CODE_IS_GENERIC(__val) \
        (((__val) >= 0x02  \
          && (__val) <= 0x0C) ? 1 : 0)

#define IPMI_EVENT_READING_TYPE_CODE_IS_SENSOR_SPECIFIC(__val) \
        (((__val) == 0x6F) ? 1 : 0)

#define IPMI_EVENT_READING_TYPE_CODE_IS_OEM(__val) \
        (((__val) >= 0x70  \
          && (__val) <= 0x7F) ? 1 : 0)

#ifdef __cplusplus
}
#endif

#endif /* ipmi-event-reading-type-code-spec.h */


