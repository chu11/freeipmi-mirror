/* 
   ipmi-utils.h - general utility procedures

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

#ifndef _IPMI_UTILS_H
#define	_IPMI_UTILS_H	1

#ifdef __cplusplus
extern "C" {
#endif

#ifndef __cplusplus
 
#if !defined(__STDC_VERSION__) || (__STDC_VERSION__ < 199901L)
# if !defined(__GNUC__) ||(__GNUC__ < 3)
  typedef char _Bool;           /* For C compilers without _Bool */
# endif
#endif
 
#define bool  _Bool
#define true  1
#define false 0
 
#else
 
  /* C++ */
#define bool  bool
#define true  true
#define false false
#endif
#define __bool_true_false_are_defined 1

#define FREEIPMI_MIN(x,y)  ((x < y) ? x : y)

#define IPMI_COMP_CODE(obj_cmd)  obj_cmd[1]

int8_t ipmi_chksum (uint8_t *buf, uint64_t len);
int8_t ipmi_comp_test (fiid_obj_t obj_cmd);
int ipmi_open_free_udp_port (void);

int8_t ipmi_ipv4_address_string2int(char *src, uint32_t *dest);

int8_t ipmi_mac_address_string2int(char *src, uint64_t *dest);

#ifdef __cplusplus
}
#endif

#endif /* ipmi-utils.h */


