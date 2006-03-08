/* 
   ipmi-cipher-suite-spec.h - IPMI Network Function Specification

   Copyright (C) 2003, 2004, 2005 FreeIPMI Core Team

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
   Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.  
*/

#ifndef _IPMI_CIPHER_SUITE_SPEC_H
#define	_IPMI_CIPHER_SUITE_SPEC_H

#ifdef __cplusplus
extern "C" {
#endif

/* Notes:
   Refer to IPMI 2.0 spec Table 22-18 and Table 22-19. 
*/

#define IPMI_CIPHER_SUITE_TAG_BITS_AUTHENTICATION_ALGORITHM   0x0
#define IPMI_CIPHER_SUITE_TAG_BITS_INTEGRITY_ALGORITHM        0x1
#define IPMI_CIPHER_SUITE_TAG_BITS_CONFIDENTIALITY_ALGORITHM  0x2
#define IPMI_CIPHER_SUITE_TAG_BITS_RECORD                     0x3

#ifdef __cplusplus
}
#endif

#endif /* ipmi-cipher_suite-spec.h */
