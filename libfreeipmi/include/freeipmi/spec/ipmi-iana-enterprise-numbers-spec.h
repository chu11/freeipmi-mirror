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

#ifndef _IPMI_IANA_ENTERPRISE_NUMBERS_SPEC_H
#define _IPMI_IANA_ENTERPRISE_NUMBERS_SPEC_H

#ifdef __cplusplus
extern "C" {
#endif

/* Convenience macros, will be added as needed in code */
#define IPMI_IANA_ENTERPRISE_ID_INTEL      343
#define IPMI_IANA_ENTERPRISE_ID_DELL       674
#define IPMI_IANA_ENTERPRISE_ID_INVENTEC 20569

/* As of this writing min = 0, max = 34214 */

/* To avoid gcc warnings, add +1 in comparison */
#define IPMI_IANA_ENTERPRISE_ID_VALID(__iana_enterprise_id) \
  (((__iana_enterprise_id + 1) >= (0 + 1)                   \
    && (__iana_enterprise_id) <= 34214) ? 1 : 0)

/* Some fields can be NULL if they were not assigned/removed by IANA */
extern const char *const ipmi_iana_enterprise_numbers[];

#ifdef __cplusplus
}
#endif

#endif
